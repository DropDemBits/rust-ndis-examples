#![no_std]
#![allow(non_snake_case, unused_variables, dead_code)] // Cut down on the warnings for now
#![feature(allocator_api, const_option, result_option_inspect, offset_of)]

extern crate alloc;

use core::{mem::MaybeUninit, ptr::addr_of_mut};

use once_arc::OnceArc;
use pinned_init::{pin_data, pinned_drop, PinInit};
use wdf_kmdf::sync::SpinMutex;
use wdf_kmdf_sys::{
    WDFDEVICE, WDFFILEOBJECT, WDFQUEUE, WDFREQUEST, WDF_FILEOBJECT_CONFIG, WDF_IO_QUEUE_CONFIG,
    WDF_IO_QUEUE_DISPATCH_TYPE,
};
use windows_kernel_rs::{
    log::{self, debug, error, info, warn},
    string::{nt_unicode_str, unicode_string::NtUnicodeStr},
    DriverObject,
};
use windows_kernel_sys::{
    result::STATUS, Error, KeInitializeEvent, NdisGeneratePartialCancelId, KEVENT, LIST_ENTRY,
    NDIS_HANDLE, NDIS_OBJECT_TYPE_PROTOCOL_DRIVER_CHARACTERISTICS,
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1,
    NDIS_REQUEST_TYPE, NET_DEVICE_POWER_STATE, NTSTATUS, OID_GEN_CURRENT_PACKET_FILTER,
    PDRIVER_OBJECT, PUNICODE_STRING, ULONG,
};

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
    let registry_path = unsafe {
        NtUnicodeStr::from_raw_parts(
            (*registry_path).Buffer,
            (*registry_path).Length,
            (*registry_path).MaximumLength,
        )
    };

    match driver_entry(driver_object, registry_path) {
        Ok(()) => STATUS::SUCCESS,
        Err(err) => err.0,
    }
    .to_u32()
}

fn driver_entry(driver_object: DriverObject, registry_path: NtUnicodeStr<'_>) -> Result<(), Error> {
    const PROTO_NAME: NtUnicodeStr<'static> = nt_unicode_str!("NDISPROT");
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
                    return Err(Error(STATUS::INSUFFICIENT_RESOURCES));
                }

                // call `create_control_device` to create the WDFDEVICE representing our software device
                // framework manages the lifetime of the control device
                let control_device = create_control_device(driver, p_init).inspect_err(|err| {
                    error!("create_control_device failed with status {:#x?}", err.0)
                })?;

                const NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1: u16 =
                    (core::mem::offset_of!(
                        NDIS_PROTOCOL_DRIVER_CHARACTERISTICS,
                        SendNetBufferListsCompleteHandler
                    ) + core::mem::size_of::<
                        windows_kernel_sys::SEND_NET_BUFFER_LISTS_COMPLETE_HANDLER,
                    >()) as u16;

                // initialize the protocol characteristic structure
                let mut proto_char =
                    unsafe { core::mem::zeroed::<NDIS_PROTOCOL_DRIVER_CHARACTERISTICS>() };

                proto_char.Header.Type = NDIS_OBJECT_TYPE_PROTOCOL_DRIVER_CHARACTERISTICS as u8;
                proto_char.Header.Size = NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1;
                proto_char.Header.Revision = NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1 as u8;

                proto_char.MajorNdisVersion = 6;
                proto_char.MinorNdisVersion = 0;
                proto_char.Name = unsafe { *(PROTO_NAME.as_ptr().cast()) };

                proto_char.SetOptionsHandler = None;
                proto_char.OpenAdapterCompleteHandlerEx = Some(ndisbind::open_adapter_complete);
                proto_char.CloseAdapterCompleteHandlerEx = Some(ndisbind::close_adapter_complete);
                proto_char.BindAdapterHandlerEx = Some(ndisbind::bind_adapter);
                proto_char.UnbindAdapterHandlerEx = Some(ndisbind::unbind_adapter);
                proto_char.OidRequestCompleteHandler = Some(ndisbind::request_complete);
                proto_char.StatusHandlerEx = Some(ndisbind::status_handler);
                proto_char.NetPnPEventHandler = Some(ndisbind::pnp_event_handler);
                proto_char.UninstallHandler = None;

                proto_char.SendNetBufferListsCompleteHandler = Some(send::send_complete);
                proto_char.ReceiveNetBufferListsHandler = Some(recv::receive_net_buffer_lists);

                // Register as a protocol driver
                let mut handle = MaybeUninit::<NDIS_HANDLE>::uninit();
                Error::to_err(unsafe {
                    windows_kernel_sys::NdisRegisterProtocolDriver(
                        core::ptr::null_mut(),
                        &mut proto_char,
                        handle.as_mut_ptr(),
                    )
                })
                .inspect_err(|err| {
                    warn!(
                        "Failed to register protocol driver with NDIS (status code {:#x?})",
                        err.0
                    );
                })?;
                let handle = unsafe { handle.assume_init() };

                let partial_cancel_id = unsafe { NdisGeneratePartialCancelId() };

                info!("DriverEntry: Ndis Cancel Id: {:#x?}", partial_cancel_id);

                Ok(pinned_init::try_pin_init!(NdisProt {
                    control_device,
                    ndis_protocol_handle: handle,
                    eth_type: NPROT_ETH_TYPE,
                    partial_cancel_id,
                    open_list <- SpinMutex::new(ListEntry::new()),
                    binds_complete <- KeEvent::new(EventType::Notification, false),
                }? Error))
            }
        },
    )
    .inspect_err(|err| error!("WdfDriverCreate failed with status {:#x?}", err.0))?;

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
        wdf_kmdf::raw::WdfDeviceInitAssignName(device_init.0, Some(NT_DEVICE_NAME.as_ptr().cast()))
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
        wdf_kmdf::raw::WdfDeviceCreateSymbolicLink(control_device, DOS_DEVICE_NAME.as_ptr().cast())
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

const NT_DEVICE_NAME: NtUnicodeStr<'static> = nt_unicode_str!(r"\Device\Ndisprot");
const DOS_DEVICE_NAME: NtUnicodeStr<'static> = nt_unicode_str!(r"\Global??\Ndisprot");

// Following two are arranged in the way a little-endian processor would read 2 bytes from the wire
const NPROT_ETH_TYPE: u16 = 0x8e88;
const NPROT_8021P_TAG_TYPE: u16 = 0x0081;

#[pin_data]
struct NdisProt {
    control_device: WDFDEVICE,
    ndis_protocol_handle: NDIS_HANDLE,
    /// frame type of interest
    eth_type: u16,
    /// for cancelling sends
    partial_cancel_id: u8,
    // TODO: ListEntry is really just a ListHead<OpenContextList>
    #[pin]
    open_list: SpinMutex<ListEntry>,
    /// Notifiying when binding is complete (used in ioctl)
    #[pin]
    binds_complete: KeEvent,
}

wdf_kmdf::impl_context_space!(NdisProt);

struct FileObjectContext {
    open_context: OnceArc<OpenContext>,
}

wdf_kmdf::impl_context_space!(FileObjectContext);

struct ControlDevice;

wdf_kmdf::impl_context_space!(ControlDevice);

/// See <https://github.com/microsoft/Windows-driver-samples/blob/d9acf794c92ba2fb0525f6c794ef394709035ac3/network/ndis/ndisprot_kmdf/60/ndisprot.h#L54-L90>
#[pin_data(PinnedDrop)]
struct OpenContext {
    /// Link to the global `open_list`
    #[pin]
    link: ListEntry,
    inner: SpinMutex<OpenContextInner>,

    /// Set in `OPEN_DEVICE`
    file_object: WDFFILEOBJECT,

    binding_handle: NDIS_HANDLE,
    send_nbl_pool: NDIS_HANDLE,
    // every nbl contains at least one net buffer
    recv_nbl_pool: NDIS_HANDLE,

    mac_options: u32,
    max_frame_size: u32,
    data_backfill_size: u32,
    context_backfill_size: i32,

    pending_send_count: u32,

    read_queue: WDFQUEUE,
    pending_read_count: u32,
    #[pin]
    recv_nbl_queue: LIST_ENTRY,
    recv_nbl_len: u32,

    power_state: NET_DEVICE_POWER_STATE,
    /// signaled if PowerState is D0
    #[pin]
    powered_up_event: KeEvent,
    /// used in `open_adapter`
    device_name: NtUnicodeStr<'static>,
    /// device display name
    device_desc: NtUnicodeStr<'static>,

    // For open/close_adapter
    bind_status: NTSTATUS,
    #[pin]
    bind_event: KeEvent,

    // structure signature, for sanity checking
    oc_sig: u32,
    state: OpenState,
    closing_event: *mut KeEvent,
    current_address: MACAddr,
    multicast_address: [MACAddr; MAX_MULTICAST_ADDRESS],

    status_indication_queue: WDFQUEUE,
}

#[pinned_drop]
impl PinnedDrop for OpenContext {
    fn drop(self: core::pin::Pin<&mut Self>) {
        debug_assert!(self.binding_handle.is_null());
        debug_assert!(self.file_object.is_null());
    }
}

struct OpenContextInner {
    /// State information
    flags: OpenContextFlags,
}

enum OpenState {
    Initializing,
    Running,
    Pausing,
    Paused,
    Restarting,
    Closing,
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct OpenContextFlags: u32 {
        /// State of binding
        const BIND_FLAGS = 0x0000_000F;
        const BIND_IDLE = 1 << 0;
        const BIND_OPENING = 1 << 1;
        const BIND_FAILED = 1 << 2;
        const BIND_ACTIVE = 1 << 3;
        const BIND_CLOSING = 1 << 4;

        /// State of IO opening
        const OPEN_FLAGS = 0x0000_00F0;
        const OPEN_IDLE = 0;
        const OPEN_ACTIVE = 1 << 5;

        const RESET_FLAGS = Self::RESET_IN_PROGRESS.bits() | Self::NOT_RESETTING.bits();
        const RESET_IN_PROGRESS = 1 << 8;
        const NOT_RESETTING = 0;

        const MEDIA_FLAGS = Self::MEDIA_CONNECTED.bits() | Self::MEDIA_DISCONNECTED.bits();
        const MEDIA_CONNECTED = 1 << 10;
        const MEDIA_DISCONNECTED = 0;

        const READ_FLAGS = Self::READ_SERVICING.bits();
        /// Is the read service routine running?
        const READ_SERVICING = 1 << 21;

        const UNBIND_FLAGS = Self::UNBIND_RECEIVED.bits();
        /// Seen NDIS Unbind?
        const UNBIND_RECEIVED = 1 << 28;

        // is 1 << 28 in original example but likely a mistake
        const ALLOCATED_NBL = 1 << 29;
        // is 1 << 29 in original example but adjusted due to prev
        const NBL_RETREAT_RECV_RSVD = 1 << 30;
    }
}

const MAX_MULTICAST_ADDRESS: usize = 32;

const OC_STRUCTURE_SIG: u32 = u32::from_be_bytes(['N' as u8, 'u' as u8, 'i' as u8, 'o' as u8]);

struct MACAddr([u8; 6]);

#[vtable::vtable]
impl wdf_kmdf::driver::DriverCallbacks for NdisProt {
    fn unload(&mut self) {
        debug!("Unload Enter");
        // UnregisterExCallback

        ndisbind::unload_protocol(self);

        // the framework handles deleting of the control object
        self.control_device = core::ptr::null_mut();
        debug!("Unload Exit");
    }
}

// Dummy type so that we can store a list entry in `NdisProt`
struct ListEntry {
    entry: LIST_ENTRY,
}

impl ListEntry {
    fn new() -> impl PinInit<Self, Error> {
        unsafe {
            pinned_init::init_from_closure(|slot: *mut Self| {
                let entry = addr_of_mut!((*slot).entry);
                addr_of_mut!((*entry).Flink).write(entry);
                addr_of_mut!((*entry).Blink).write(entry);
                Ok(())
            })
        }
    }
}

#[pin_data]
struct KeEvent {
    #[pin]
    event: KEVENT,
}

impl KeEvent {
    fn new(event_type: EventType, start_signaled: bool) -> impl PinInit<Self, Error> {
        unsafe {
            pinned_init::pin_init_from_closure(move |slot: *mut Self| {
                KeInitializeEvent(
                    addr_of_mut!((*slot).event),
                    windows_kernel_sys::_EVENT_TYPE(event_type as i32),
                    start_signaled.into(),
                );

                Ok(())
            })
        }
    }
}

#[repr(i32)]
enum EventType {
    Notification = windows_kernel_sys::_EVENT_TYPE::NotificationEvent.0,
    Synchronization = windows_kernel_sys::_EVENT_TYPE::SynchronizationEvent.0,
}

/// Called when the framework recives a IRP_MJ_CREATE request (e.g. when an application opens the device
/// to perform IO operations on it).
///
/// ## Note
/// This is called synchronously and in the context of the thread which created the IRP_MJ_CREATE request.
unsafe extern "C" fn ndisprot_evt_device_file_create(
    Device: WDFDEVICE,
    Request: WDFREQUEST,
    FileObject: WDFFILEOBJECT,
) {
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    debug!("Open: FileObject {:#x?}", file_object);

    file_object.get_context_mut().open_context = OnceArc::new(None);

    wdf_kmdf::raw::WdfRequestComplete(Request, STATUS::SUCCESS.to_u32());
}

/// Called when all of the handles representing the FileObject are closed, as well as all of the references being removed.
///
/// ## Note
/// This is called in an arbitrary thread context, so any context that was created in `FileCreate` should be done in the
/// cleanup callback.
unsafe extern "C" fn ndisprot_evt_file_close(FileObject: WDFFILEOBJECT) {
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    debug!("Close: FileObject {:#x?}", file_object);

    file_object.get_context_mut().open_context.take();
}

/// Called when the handle representing the `FileObject` is closed.
///
/// ## Note
/// This is called in the context of the thread which originally closed the handle
unsafe extern "C" fn ndisprot_evt_file_cleanup(FileObject: WDFFILEOBJECT) {
    //
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    let open_context = file_object.get_context_mut().open_context.take();

    debug!(
        "Cleanup: FileObject {:#x?}, Open: {:#x?}",
        file_object,
        open_context.is_some()
    );

    if let Some(open_context) = open_context {
        // Mark this endpoint
        {
            let mut inner = open_context.inner.lock();
            // Note: This is equivalent to setting the open flags to idle
            inner.flags.remove(OpenContextFlags::OPEN_FLAGS);
        }

        // Set packet filter to 0, telling NDIS we aren't interested in any more receives.
        // We do not want to wait for the device to be powered on
        let packet_filter = [0u8; 4];
        let mut bytes_processed = 0u32;
        let status = ndisbind::validate_open_and_do_request(
            &open_context,
            NDIS_REQUEST_TYPE::NdisRequestSetInformation,
            OID_GEN_CURRENT_PACKET_FILTER,
            packet_filter.as_ptr(),
            packet_filter.len(),
            &mut bytes_processed,
            false,
        );
        if let Err(err) = Error::to_err(status) {
            debug!(
                "Cleanup: set packet filter ({:#x?}) failed {:#x?}",
                packet_filter, err
            );

            // Ignore the result
            // We may stil continue to get (indicated?) receives,
            // which will be handled approriately
        }

        {
            let inner = open_context.inner.lock();

            if inner.flags.contains(OpenContextFlags::BIND_ACTIVE) {
                drop(inner);

                // Cancel any pending reads
                wdf_kmdf::raw::WdfIoQueuePurgeSynchronously(open_context.read_queue);
                // Cancel pending ioctl request for status indication
                wdf_kmdf::raw::WdfIoQueuePurgeSynchronously(open_context.status_indication_queue);
            }
        }

        // Cleanup the receive packet queue
        ndisbind::flush_receive_queue(&open_context);
    }

    debug!("Cleanup");
}

unsafe extern "C" fn ndisprot_evt_io_device_control(
    Queue: WDFQUEUE,           // in
    Request: WDFREQUEST,       // in
    OutputBufferLength: usize, // in
    InputBufferLength: usize,  // in
    IoControlCode: ULONG,      // in
) {
}

unsafe extern "C" fn ndisprot_evt_io_read(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}

unsafe extern "C" fn ndisprot_evt_io_write(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}

// unfortunately we need to declare _fltused
//
// `_fltused` is problematic since it implies that we have floating point
// operations somewhere, and on x86 kernel mode drivers should wrap floating
// point operations with state saving.
// (see https://github.com/Trantect/win_driver_example/issues/4)
//
// Since we don't plan to support the x86 arch, this isn't *too* bad,
// but still sorta bad.
#[used]
#[no_mangle]
pub static _fltused: i32 = 0;

#[cfg(not(test))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    windows_kernel_rs::__handle_panic(info);
}

mod ndisbind {
    //! NDIS protocol entry points as well as handling binding and unbinding from adapters
    use alloc::sync::Arc;
    use windows_kernel_sys::{
        result::STATUS, NdisDeregisterProtocolDriver, NDIS_HANDLE, NDIS_OID, NDIS_REQUEST_TYPE,
        NTSTATUS, OID_802_11_ADD_WEP, OID_802_11_AUTHENTICATION_MODE, OID_802_11_BSSID,
        OID_802_11_BSSID_LIST, OID_802_11_BSSID_LIST_SCAN, OID_802_11_CONFIGURATION,
        OID_802_11_DISASSOCIATE, OID_802_11_INFRASTRUCTURE_MODE, OID_802_11_NETWORK_TYPE_IN_USE,
        OID_802_11_POWER_MODE, OID_802_11_RELOAD_DEFAULTS, OID_802_11_REMOVE_WEP, OID_802_11_RSSI,
        OID_802_11_SSID, OID_802_11_STATISTICS, OID_802_11_SUPPORTED_RATES, OID_802_11_WEP_STATUS,
        OID_802_3_MULTICAST_LIST, PNDIS_BIND_PARAMETERS, PNDIS_OID_REQUEST,
        PNDIS_STATUS_INDICATION, PNET_PNP_EVENT_NOTIFICATION,
    };

    use crate::{NdisProt, OpenContext};

    const SUPPORTED_SET_OIDS: &[NDIS_OID] = &[
        OID_802_11_INFRASTRUCTURE_MODE,
        OID_802_11_AUTHENTICATION_MODE,
        OID_802_11_RELOAD_DEFAULTS,
        OID_802_11_REMOVE_WEP,
        OID_802_11_WEP_STATUS,
        OID_802_11_BSSID_LIST_SCAN,
        OID_802_11_ADD_WEP,
        OID_802_11_SSID,
        OID_802_11_BSSID,
        OID_802_11_BSSID_LIST,
        OID_802_11_DISASSOCIATE,
        // the following two are apparently used by power management?
        OID_802_11_STATISTICS,
        OID_802_11_POWER_MODE,
        OID_802_11_NETWORK_TYPE_IN_USE,
        OID_802_11_RSSI,
        OID_802_11_SUPPORTED_RATES,
        OID_802_11_CONFIGURATION,
        OID_802_3_MULTICAST_LIST,
    ];

    pub(crate) unsafe extern "C" fn bind_adapter(
        ProtocolDriverContext: NDIS_HANDLE,
        BindContext: NDIS_HANDLE,
        BindParameters: PNDIS_BIND_PARAMETERS,
    ) -> NTSTATUS {
        STATUS::SUCCESS.to_u32()
    }

    pub(crate) unsafe extern "C" fn open_adapter_complete(
        ProtocolBindingContext: NDIS_HANDLE,
        Status: NTSTATUS,
    ) {
    }

    pub(crate) unsafe extern "C" fn unbind_adapter(
        UnbindContext: NDIS_HANDLE,
        ProtocolBindingContext: NDIS_HANDLE,
    ) -> NTSTATUS {
        STATUS::SUCCESS.to_u32()
    }

    pub(crate) unsafe extern "C" fn close_adapter_complete(ProtocolBindingContext: NDIS_HANDLE) {}

    pub(crate) unsafe extern "C" fn pnp_event_handler(
        ProtocolBindingContext: NDIS_HANDLE,
        NetPnPEventNotification: PNET_PNP_EVENT_NOTIFICATION,
    ) -> NTSTATUS {
        STATUS::SUCCESS.to_u32()
    }

    pub(crate) unsafe extern "C" fn NdisprotProtocolUnloadHandler() -> NTSTATUS {
        STATUS::SUCCESS.to_u32()
    }

    pub(crate) unsafe extern "C" fn request_complete(
        ProtocolBindingContext: NDIS_HANDLE,
        OidRequest: PNDIS_OID_REQUEST,
        Status: NTSTATUS,
    ) {
    }

    pub(crate) unsafe extern "C" fn status_handler(
        ProtocolBindingContext: NDIS_HANDLE,
        StatusIndication: PNDIS_STATUS_INDICATION,
    ) {
    }

    pub(crate) fn unload_protocol(context: &mut NdisProt) {
        let proto_handle =
            core::mem::replace(&mut context.ndis_protocol_handle, core::ptr::null_mut());

        if !proto_handle.is_null() {
            unsafe { NdisDeregisterProtocolDriver(proto_handle) };
        }
    }

    pub(crate) fn validate_open_and_do_request(
        open_context: &Arc<OpenContext>,
        request_type: NDIS_REQUEST_TYPE,
        oid: NDIS_OID,
        info_buffer: *const u8,
        info_len: usize,
        bytes_processed: &mut u32,
        wait_for_power_on: bool,
    ) -> NTSTATUS {
        loop {}
    }

    pub(crate) fn flush_receive_queue(open_context: &Arc<OpenContext>) {
        loop {}
    }
}

mod send {
    use windows_kernel_sys::{NDIS_HANDLE, PNET_BUFFER_LIST, ULONG};

    pub(crate) unsafe extern "C" fn send_complete(
        ProtocolBindingContext: NDIS_HANDLE,
        NetBufferList: PNET_BUFFER_LIST,
        SendCompleteFlags: ULONG,
    ) {
    }
}

mod recv {
    use windows_kernel_sys::{NDIS_HANDLE, NDIS_PORT_NUMBER, PNET_BUFFER_LIST, ULONG};

    pub(crate) unsafe extern "C" fn receive_net_buffer_lists(
        ProtocolBindingContext: NDIS_HANDLE,
        NetBufferLists: PNET_BUFFER_LIST,
        PortNumber: NDIS_PORT_NUMBER,
        NumberOfNetBufferLists: ULONG,
        ReceiveFlags: ULONG,
    ) {
    }
}
