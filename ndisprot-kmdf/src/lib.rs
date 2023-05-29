#![no_std]
#![feature(allocator_api, const_option, result_option_inspect)]

use wdf_kmdf_sys::{
    WDFDEVICE, WDFFILEOBJECT, WDFQUEUE, WDFREQUEST, WDF_FILEOBJECT_CONFIG, WDF_IO_QUEUE_CONFIG,
    WDF_IO_QUEUE_DISPATCH_TYPE, WDF_NO_OBJECT_ATTRIBUTES,
};
use windows_kernel_rs::{
    log::{self, debug, error, info, trace},
    string::{utf16str, UnicodeString},
    DriverObject,
};
use windows_kernel_sys::{Error, NTSTATUS, PDRIVER_OBJECT, PUNICODE_STRING, ULONG};

#[allow(non_snake_case)]
#[no_mangle]
unsafe extern "system" fn DriverEntry(
    driver_object: PDRIVER_OBJECT,
    registry_path: PUNICODE_STRING,
) -> NTSTATUS {
    #[global_allocator]
    static ALLOCATOR: windows_kernel_rs::allocator::KernelAlloc =
        windows_kernel_rs::allocator::KernelAlloc;

    windows_kernel_rs::init_kernel_logger!(log::COMPONENT_IHVDRIVER, log::LevelFilter::Info);

    // SAFETY: This is the driver entry point
    let driver_object = unsafe { DriverObject::new(driver_object) };
    // SAFETY: Is a copy of the original PCUNICODE_STRING, but never modified,
    // and lifetime is tied to this variable (which is bound to `DriverEntry`)
    let registry_path = unsafe { UnicodeString::from_raw(*registry_path) };

    match driver_entry(driver_object, registry_path) {
        Ok(()) => windows_kernel_sys::sys::Win32::Foundation::STATUS_SUCCESS,
        Err(err) => err.0,
    }
}

fn driver_entry(
    driver_object: DriverObject,
    registry_path: UnicodeString<'_>,
) -> Result<(), Error> {
    debug!("DriverEntry");

    wdf_kmdf::driver::Driver::<NdisProt>::create(
        driver_object,
        registry_path,
        wdf_kmdf::driver::DriverConfig {
            pnp_mode: wdf_kmdf::driver::PnpMode::NonPnp,
            pool_tag: None,
        },
        |driver| {
            {
                let p_init = unsafe {
                    wdf_kmdf::raw::WdfControlDeviceInitAllocate(
                        driver.raw_handle(),
                        core::ptr::addr_of!(
                            windows_kernel_sys::SDDL_DEVOBJ_SYS_ALL_ADM_RWX_WORLD_RW_RES_R
                        ),
                    )
                };
                if p_init.is_null() {
                    return Err(Error(
                        windows_kernel_sys::sys::Win32::Foundation::STATUS_INSUFFICIENT_RESOURCES,
                    ));
                }

                // call `create_control_device` to create the WDFDEVICE
                // representing our software device
                let control_device = create_control_device(driver, p_init).inspect_err(|err| {
                    error!("create_control_device failed with status {:#x}", err.0)
                })?;

                Ok(pinned_init::try_init!(NdisProt {
                    control_device: control_device,
                    eth_type: NPROT_ETH_TYPE,
                    partial_cancel_id: 0,
                    local_cancel_id: 0,
                    binds_complete: (),
                }? Error))
            }
        },
    )
    .inspect_err(|err| error!("WdfDriverCreate failed with status {:#x}", err.0))?;

    Ok(())
}

fn create_control_device(
    _driver: &mut wdf_kmdf::driver::DriverHandle,
    device_init: wdf_kmdf_sys::PWDFDEVICE_INIT,
) -> Result<WDFDEVICE, Error> {
    struct DeviceInit(wdf_kmdf_sys::PWDFDEVICE_INIT);

    impl Drop for DeviceInit {
        fn drop(&mut self) {
            if !self.0.is_null() {
                // Free the WDFDEVICE_INIT structure only if device creation fails
                // Otherwise, the framework has ownership of the memory and so frees
                // it itself.
                unsafe { wdf_kmdf::raw::WdfDeviceInitFree(self.0) };

                self.0 = core::ptr::null_mut();
            }
        }
    }

    let mut device_init = DeviceInit(device_init);

    // Default I/O type is Buffered
    // We want direct I/O for reads and writes so set it explicitly
    unsafe {
        wdf_kmdf::raw::WdfDeviceInitSetIoType(
            device_init.0,
            wdf_kmdf_sys::_WDF_DEVICE_IO_TYPE::WdfDeviceIoDirect,
        )
    };

    Error::to_err(unsafe {
        wdf_kmdf::raw::WdfDeviceInitAssignName(device_init.0, Some(NT_DEVICE_NAME.as_raw_ptr()))
    })?;

    // Initialize WDF_FILEOBJECT_CONFIG_INIT struct to tell the framework
    // whether you're interested in handling Create, Close, and Cleanup
    // requests that get generated when an application or another kernel
    // component opens a handle to the device.
    //
    // If you don't register, the framework's default behaviour would be
    // to complete these requests with STATUS_SUCCESS. A driver might be
    // Interested in registering these events if it wants to do security
    // validation and also wants to maintain per handle (fileobject)
    // state.
    let mut file_config = WDF_FILEOBJECT_CONFIG::init(
        Some(ndisprot_evt_device_file_create),
        Some(ndisprot_evt_file_close),
        Some(ndisprot_evt_file_cleanup),
    );

    let mut file_object_attribs =
        wdf_kmdf::object::default_object_attributes::<FileObjectContext>();

    unsafe {
        wdf_kmdf::raw::WdfDeviceInitSetFileObjectConfig(
            device_init.0,
            &mut file_config,
            Some(&mut file_object_attribs),
        )
    };

    let mut device_object_attribs = wdf_kmdf::object::default_object_attributes::<ControlDevice>();

    let mut control_device = core::ptr::null_mut();
    Error::to_err(unsafe {
        wdf_kmdf::raw::WdfDeviceCreate(
            &mut device_init.0,
            Some(&mut device_object_attribs),
            &mut control_device,
        )
    })?;
    // `device_init` is set to NULL upon successful device creation

    // Create a symbolic link so that usermode apps can open the control device
    Error::to_err(unsafe {
        wdf_kmdf::raw::WdfDeviceCreateSymbolicLink(control_device, DOS_DEVICE_NAME.as_raw_ptr())
    })?;

    // Config the default queue to recieve parallel read, write, and ioctl requests.
    // Default queues recieve all requests which are not (configure-forwarded using WdfDeviceConfigureRequestDispatching?)
    let mut io_queue_config = WDF_IO_QUEUE_CONFIG::init_default_queue(
        WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchParallel,
    );

    io_queue_config.EvtIoWrite = Some(ndisprot_evt_io_write);
    io_queue_config.EvtIoRead = Some(ndisprot_evt_io_read);
    io_queue_config.EvtIoDeviceControl = Some(ndisprot_evt_io_device_control);

    let mut queue = core::ptr::null_mut();

    Error::to_err(unsafe {
        wdf_kmdf::raw::WdfIoQueueCreate(
            control_device,
            &mut io_queue_config,
            None,
            Some(&mut queue),
        )
    })?;

    // Until we notify WDF that we're done initializing,
    // all I/O requests are rejected.
    unsafe { wdf_kmdf::raw::WdfControlFinishInitializing(control_device) };

    // Create a device object where an application to use NDIS devices
    return Ok(control_device);
}

unsafe extern "C" fn ndisprot_evt_device_file_create(
    Device: WDFDEVICE,
    Request: WDFREQUEST,
    FileObject: WDFFILEOBJECT,
) {
}
unsafe extern "C" fn ndisprot_evt_file_close(FileObject: WDFFILEOBJECT) {}
unsafe extern "C" fn ndisprot_evt_file_cleanup(FileObject: WDFFILEOBJECT) {}

unsafe extern "C" fn ndisprot_evt_io_write(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}
unsafe extern "C" fn ndisprot_evt_io_read(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}
unsafe extern "C" fn ndisprot_evt_io_device_control(
    Queue: WDFQUEUE,           // in
    Request: WDFREQUEST,       // in
    OutputBufferLength: usize, // in
    InputBufferLength: usize,  // in
    IoControlCode: ULONG,      // in
) {
}

const NT_DEVICE_NAME: UnicodeString<'static> =
    UnicodeString::new_const(utf16str!(r"\Device\Ndisprot"));
const DOS_DEVICE_NAME: UnicodeString<'static> =
    UnicodeString::new_const(utf16str!(r"\Global??\Ndisprot"));

// Following two are arranged in the way a little-endian processor would read 2 bytes from the wire
const NPROT_ETH_TYPE: u16 = 0x8e88;
const NPROT_8021P_TAG_TYPE: u16 = 0x0081;

struct NdisProt {
    control_device: WDFDEVICE,
    // todo: ndis_protocol_handle
    /// frame type of interest
    eth_type: u16,
    /// for cancelling sends
    partial_cancel_id: u8,
    local_cancel_id: u32,
    // todo: open_list: Lock<ListEntry<OpenContext>>
    /// have we seen `NetEventBindsComplete`?
    // Note: is a RKEVENT, Initialized via KeInitializeEvent
    binds_complete: (),
}

wdf_kmdf::impl_context_space!(NdisProt);

struct FileObjectContext;

wdf_kmdf::impl_context_space!(FileObjectContext);

struct ControlDevice;

wdf_kmdf::impl_context_space!(ControlDevice);

#[vtable::vtable]
impl wdf_kmdf::driver::DriverCallbacks for NdisProt {
    fn unload(&mut self) {
        debug!("Unload Enter");
        // UnregisterExCallback
        // DoProtocolUnload
        debug!("Unload Exit");
    }
}

// unfortunately we need to declare these two (__CxxFrameHandler3 & _fltused)
//
// `_fltused` is particularly problematic, since it implies that we have
// floating point operations somewhere, and on x86 kernel mode drivers
// should wrap floating point operations with state saving.
// (see https://github.com/Trantect/win_driver_example/issues/4)
//
// Since we don't plan to support the x86 arch, this isn't *too* bad,
// but still sorta bad.
//
// This maybe comes from llvm being unable to determine that panicking
// never happens, but might require having to use a crate using linker
// tricks to guarantee it
#[used]
#[no_mangle]
pub static _fltused: i32 = 0;
#[no_mangle]
pub extern "system" fn __CxxFrameHandler3() -> i32 {
    0
}

#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    windows_kernel_rs::__handle_panic(info);
}
