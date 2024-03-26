#![no_std]
#![allow(non_snake_case)] // Cut down on the warnings for now
#![feature(allocator_api)]

extern crate alloc;

mod ndisbind;

mod send;

mod recv;

use core::{
    mem::ManuallyDrop,
    pin::Pin,
    ptr::addr_of_mut,
    sync::atomic::{AtomicU32, AtomicUsize, Ordering},
};

use alloc::vec::Vec;
use crossbeam_utils::atomic::AtomicCell;
use ndis_rs::NetBufferList;
use pinned_init::{pin_data, pinned_drop, PinInit};
use wdf_kmdf::{
    device::ControlDevice,
    driver::Driver,
    file_object::FileObject,
    handle::{HandleWrapper, HasContext, Ref},
    io_queue::IoQueue,
    object::GeneralObject,
    request::FileRequest,
    sync::{SpinMutex, SpinPinMutex},
};
use wdf_kmdf_sys::{
    WDFDEVICE, WDFFILEOBJECT, WDFQUEUE, WDFREQUEST, WDF_FILEOBJECT_CONFIG, WDF_IO_QUEUE_CONFIG,
    WDF_IO_QUEUE_DISPATCH_TYPE,
};
use windows_kernel_rs::{
    ioctl::{DeviceType, IoControlCode, RequiredAccess, TransferMethod},
    log::{self, debug, error, info, warn},
    string::{
        nt_unicode_str,
        unicode_string::{NtUnicodeStr, NtUnicodeString},
    },
    DriverObject,
};
use windows_kernel_sys::{
    result::STATUS, Error, KeInitializeEvent, KeSetEvent, KeWaitForSingleObject, NdisFreeMemory,
    NdisFreeNetBufferListPool, NdisGeneratePartialCancelId, KEVENT, NDIS_HANDLE,
    NDIS_OBJECT_TYPE_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_OID, NDIS_PACKET_TYPE_BROADCAST,
    NDIS_PACKET_TYPE_DIRECTED, NDIS_PACKET_TYPE_MULTICAST, NDIS_PORT_NUMBER,
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1,
    NDIS_REQUEST_TYPE, NDIS_STATUS, NET_DEVICE_POWER_STATE, NTSTATUS,
    OID_GEN_CURRENT_PACKET_FILTER, PDRIVER_OBJECT, PUNICODE_STRING, ULONG, ULONG_PTR,
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

    windows_kernel_rs::init_kernel_logger!(log::COMPONENT_IHVDRIVER, log::LevelFilter::Trace);

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

    let driver = wdf_kmdf::driver::Driver::<NdisProt>::create(
        driver_object,
        registry_path,
        wdf_kmdf::driver::DriverConfig {
            pnp_mode: wdf_kmdf::driver::PnpMode::NonPnp,
            pool_tag: None,
        },
        |driver| {
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

            let cancel_id_gen = CancelIdGen::new(unsafe { NdisGeneratePartialCancelId() });

            info!(
                "DriverEntry: Ndis Cancel Id: {:#x?}",
                cancel_id_gen.partial_id
            );

            Ok(pinned_init::try_pin_init! {
                NdisProt {
                    control_device,
                    ndis_protocol_handle: ProtocolHandle(AtomicCell::new(core::ptr::null_mut())),
                    eth_type: NPROT_ETH_TYPE,
                    cancel_id_gen,
                    open_list <- SpinPinMutex::new(Vec::new()),
                    binds_complete <- KeEvent::new(EventType::Notification, false),
                }? Error
            })
        },
    )
    .inspect_err(|err| error!("WdfDriverCreate failed with status {:#x?}", err.0))?;

    // Register as a protocol driver
    //
    // We do this seperately from the main driver context area initialization as
    // unfortunately before `NdisRegisterProtocolDriver` returns, any of the protocol
    // callbacks can be called including `ProtocolPnPEventHandler`, which accesses
    // the driver context area. This ends up accessing the context area while it's
    // being initialized, which we panic on.
    //
    // Unfortunately this doesn't happen every single time the driver is loaded, as
    // it ends up being a race condition. We have to assume that the write to the
    // protocol driver handle place happens before any of the callbacks since
    // `ProtocolBindAdapterEx` requires it for `NdisOpenAdapterEx`.

    const NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1: u16 = memoffset::span_of!(
        NDIS_PROTOCOL_DRIVER_CHARACTERISTICS,
        SendNetBufferListsCompleteHandler
    )
    .1 as u16;

    // initialize the protocol characteristic structure
    let mut proto_char = unsafe { core::mem::zeroed::<NDIS_PROTOCOL_DRIVER_CHARACTERISTICS>() };

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
    //
    // note: this stays live as long as the driver context space is live, which is
    // the case so long as we're in `driver_entry`
    let handle_place = driver.get_context().ndis_protocol_handle.0.as_ptr();
    Error::to_err(unsafe {
        windows_kernel_sys::NdisRegisterProtocolDriver(
            // Pass in the driver context so that `BindAdapter` has access to the driver globals
            driver.raw_handle().cast(),
            &mut proto_char,
            handle_place,
        )
    })
    .inspect_err(|err| {
        warn!("Failed to register protocol driver with NDIS (status code {err:#x?})",);
    })?;

    Ok(())
}

fn create_control_device(
    _driver: &wdf_kmdf::driver::Driver<NdisProt>,
    device_init: wdf_kmdf_sys::PWDFDEVICE_INIT,
) -> Result<ControlDevice<()>, Error> {
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
        wdf_kmdf::context_space::default_object_attributes::<FileObjectContext>();

    unsafe {
        wdf_kmdf::raw::WdfDeviceInitSetFileObjectConfig(
            device_init.0,
            &mut file_config,
            Some(&mut file_object_attribs),
        )
    };

    let mut device_object_attribs = wdf_kmdf::context_space::default_object_attributes::<()>();

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

    io_queue_config.EvtIoWrite = Some(send::ndisprot_evt_io_write);
    io_queue_config.EvtIoRead = Some(recv::ndisprot_evt_io_read);
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
    //
    // SAFETY: Is the correct type of handle
    return Ok(unsafe { ControlDevice::wrap_raw(control_device.cast()) });
}

const NT_DEVICE_NAME: NtUnicodeStr<'static> = nt_unicode_str!(r"\Device\Ndisprot");
const DOS_DEVICE_NAME: NtUnicodeStr<'static> = nt_unicode_str!(r"\Global??\Ndisprot");

// Following two are arranged in the way a little-endian processor would read 2 bytes from the wire
const NPROT_ETH_TYPE: u16 = 0x8e88;
const NPROT_8021P_TAG_TYPE: u16 = 0x0081;

const NPROTO_PACKET_FILTER: u32 =
    NDIS_PACKET_TYPE_DIRECTED | NDIS_PACKET_TYPE_MULTICAST | NDIS_PACKET_TYPE_BROADCAST;

// XXX: What parts of this are common to all protocol drivers, and which are NdisProt specific?
// Common Protocol
// - ndis_protocol_handle (for creating nbl pools and opening adapters)
// - cancel_id_gen (for cancelling sends and oid requests)
// - open_list (for storing all of the bound protocol handles)
// - binds_complete (for indicating all binds are complete?)
//
// NdisProt
// - eth_type (for filtering)
// - control_device (for piping through requests to be made from userspace)
#[pin_data(PinnedDrop)]
struct NdisProt {
    // Only used by BindAdapter to create the WDFIOQUEUEs for the adapter
    control_device: ControlDevice<()>,
    // Used in
    // - ndisbind::BindAdapter
    //   - ndisbind::CreateBinding
    //     - Creating the NBL pool for send
    //     - Creating the NBL pool for recv
    //     - Opening the adapter (`NdisOpenAdapterEx`)
    // - (Dropping) ProtocolUnloadHandler (unused)
    // - (Dropping) EvtUnload
    ndis_protocol_handle: ProtocolHandle,
    /// frame type of interest
    // Used in
    // - RecieveNbl
    //   - Have access to a ProtocolBindingContext
    eth_type: u16,
    /// for cancelling sends
    // Used in
    // - send::EvtIoWrite
    //   - Have access to FileObject.ContextSpace
    // - ndisbind::DoRequest -> can we not use this?
    //   - can use a different thing!
    //   - could have like a per-file object thing?
    //   - just has to match
    //   - ahhhhh it's just supposed to be a usize
    //   - also could just not since we never use it for cancellation
    cancel_id_gen: CancelIdGen,
    // Used in
    // - ndisbind::BindAdapter (pre `CreateBinding`), mut
    //   - Have access to a ProtocolBindingContext
    // - ndisbind::QueryBinding, iter
    //   - EvtIoDeviceControl
    //     - Have access to FileObject.ContextSpace
    // - ndisbind::LookupDevice, iter
    //   - EvtIoDeviceControl
    //     - Have access to FileObject.ContextSpace
    // Is an `Option<GeneralObject<OpenContext>>` so that we can pre-allocate space to put the open context in
    #[pin]
    open_list: SpinPinMutex<Vec<Option<Ref<GeneralObject<OpenContext>>>>>,
    /// Notifiying when binding is complete (used in ioctl)
    // Used in
    // - ndisbind::PnPEventHandler
    //   - Have access to a ProtocolBindingContext
    // - NdisprotOpenDevice
    //   - IoCtl
    //     - Has access to a dev ctx?
    #[pin]
    binds_complete: KeEvent,
}

wdf_kmdf::impl_context_space!(NdisProt);

// Pretty much the only viable way to pass stuff via `ProtocolDriverContext`
// while also being able to free it is to stuff it in a WdfObj parented to the driver
//
// Alternatively could do `ProtocolHandle<DriverContext>` storing a `Box<DriverContext>`
// and then manifesting a reference in `BindAdapter`.

#[pinned_drop]
impl PinnedDrop for NdisProt {
    fn drop(self: Pin<&mut Self>) {
        debug!("Globals Drop Enter");
        // can't do resource cleanup here because the context space is considered uninitialized at this point
        debug!("Globals Drop Exit");
    }
}

struct CancelIdGen {
    partial_id: u8,
    local_id: AtomicUsize,
}

impl CancelIdGen {
    fn new(partial_cancel_id: u8) -> Self {
        Self {
            partial_id: partial_cancel_id,
            local_id: AtomicUsize::new(0),
        }
    }

    fn next(&self) -> usize {
        let mut id = self.local_id.fetch_add(1, Ordering::Relaxed).to_be_bytes();
        id[0] = self.partial_id;
        usize::from_be_bytes(id)
    }
}

struct ProtocolHandle(AtomicCell<NDIS_HANDLE>);

unsafe impl Send for ProtocolHandle {}
unsafe impl Sync for ProtocolHandle {}

struct BindingHandle(AtomicCell<NDIS_HANDLE>);

unsafe impl Send for BindingHandle {}
unsafe impl Sync for BindingHandle {}

struct NblPool(NDIS_HANDLE);

unsafe impl Send for NblPool {}
unsafe impl Sync for NblPool {}

// FIXME: This a wrapper which allows `NtUnicodeString` to be used in `Sync + Send` contexts
struct SyncWrapper<T>(T);

impl<T> core::ops::Deref for SyncWrapper<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> core::ops::DerefMut for SyncWrapper<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

unsafe impl<T> Send for SyncWrapper<T> {}
unsafe impl<T> Sync for SyncWrapper<T> {}

struct FileObjectContext {
    open_context: SpinMutex<Option<Ref<GeneralObject<OpenContext>>>>,
}

wdf_kmdf::impl_context_space!(FileObjectContext);

impl FileObjectContext {
    fn open_context(&self) -> Option<Ref<GeneralObject<OpenContext>>> {
        let guard = self.open_context.lock();
        guard.as_ref().map(|it| it.clone_ref())
    }
}

/// See <https://github.com/microsoft/Windows-driver-samples/blob/d9acf794c92ba2fb0525f6c794ef394709035ac3/network/ndis/ndisprot_kmdf/60/ndisprot.h#L54-L90>
#[pin_data(PinnedDrop)]
struct OpenContext {
    driver: Ref<Driver<NdisProt>>,

    #[pin]
    inner: SpinPinMutex<OpenContextInner>,

    // for `validate_open_and_do_request`
    binding_handle: BindingHandle,

    // set on adapter create
    // Used in
    // - EvtIoWrite
    // - CreateBinding (write, pre binding open)
    // - FreeBindResources
    //   - ShutdownBinding
    //     ...
    send_nbl_pool: NblPool,
    // every nbl contains at least one net buffer
    // set on adapter create
    // Used in
    // - BindAdapter (write, in CreateBinding pre binding open)
    // - FreeBindResources
    //   - ShutdownBinding
    //     ...
    // - AllocateReceiveNBL
    //   - ...
    recv_nbl_pool: NblPool,

    // Used in
    // - CreateBinding (write)
    // - Restart (write)
    mac_options: AtomicCell<u32>,
    // Used in
    // - EvtIoWrite (read)
    // - CreateBinding (write)
    // - Restart (write)
    max_frame_size: AtomicCell<u32>,
    // Used in
    // - CreateBinding (write)
    data_backfill_size: u32,
    // Used in
    // - CreateBinding (write)
    context_backfill_size: u32,

    // protected by `lock`
    // Used in
    // - send::SendComplete (read (== 0), write (dec))
    // - send::EvtIoWrite (write (inc))
    // - PnPEventHandler (read)
    // - ShutdownBinding (unprotected read, acqrel atomic (may dep on a queue)?)
    // - WaitForPendingIo (unprotected read)
    // - ValidateOpenAndDoRequest (read, write)
    // - QueryOidValue (read, write)
    //
    // Also used to not make the binding go away
    pended_send_count: AtomicU32,

    // Used in
    // - OpenDevice (QueueStart)
    // - EvtFileCleanup (PurgeQueueSync)
    // - BindAdapter
    //   - WdfIoQueueCreate
    //   - WdfIoQueueReadyNotify
    // - ShutdownBinding
    //   - WdfIoQueuePurgeSync
    // - FreeBindResources
    // - WaitForPendingIO
    //   - WdfIoQueuePurgeSync
    // - recv::EvtIoRead
    //   - WdfRequestForwardToIoQueue
    // - recv::ServiceReads
    //   - WdfIoQueueRetreiveNextRequest
    read_queue: IoQueue<FileRequest<FileObjectContext>>,
    // protected by `lock`
    // Used in
    // - recv::ServiceReads (write dec)
    // - WaitForPendingIO (read)
    // oh no (no inc)
    pended_read_count: AtomicU32,

    #[pin]
    recv_queue: SpinPinMutex<RecvQueue<4>>,

    // Used in
    // - recv::QueueReceiveNBL (read)
    // - PnPEventHandler (write)
    // - CreateBinding (write)
    // - ValidateOpenAndDoRequest (read)
    // - Status (read)
    power_state: AtomicCell<NET_DEVICE_POWER_STATE>,
    /// signaled if PowerState is D0
    // Used in
    // - CreateBinding (write init, signal)
    // - UnbindAdapter (signal)
    // - PnPEventHandler (write init, signal)
    //   - write init basically means that all waits in ValidateOpenAndDoRequest guarantee timeout
    // - ValidateOpenAndDoRequest (wait)
    //   - there's a lil note here too
    #[pin]
    powered_up_event: KeEvent,
    /// used in `open_adapter`
    // Used in
    // - CreateBinding (read dbg, write)
    // - FreeBindResources (free)
    device_name: SyncWrapper<NtUnicodeString>,
    /// device display name
    // Used in
    // - CreateBinding (write)
    // - FreeBindResources (free)
    // - QueryBinding (read)
    // This is manually drop since the memory is allocated by NDIS
    device_desc: SyncWrapper<ManuallyDrop<NtUnicodeString>>,

    // For open/close_adapter
    // Used in
    // - OpenAdapterComplete (write)
    // - CreateBinding (read)
    // - ShutdownBinding (read)
    //   - unused except for an assert
    bind_status: NTSTATUS,
    // Used in
    // - OpenAdapterComplete (signal)
    // - CloseAdapterComplete (signal)
    // - CreateBinding (write init, wait forever (after NdisOpenAdapterEx))
    // - ShutdownBinding (write init, wait forever (after NdisCloseAdapterEx))
    #[pin]
    bind_event: KeEvent,

    // structure signature, for sanity checking
    oc_sig: u32,
    // Used in
    // - send::EvtIoWrite (read)
    // - CreateBinding (write)
    current_address: MACAddr,
    // Unused
    multicast_address: [MACAddr; MAX_MULTICAST_ADDRESS],

    // Used in
    // - EvtFileCleanup
    //   - WdfIoQueuePurgeSync
    // - EvtIoDeviceControl
    //   - WdfRequestForwardToIoQueue
    // - OpenDevice
    //   - WdfIoQueueStart
    // - BindAdapter
    //   - WdfIoQueueCreate (pre Createbinding)
    // - ShutdownBinding
    //   - WdfIoQueuePurgeSync
    // - FreeBindResources
    // - ServiceIndicateStatusIrp
    //  - WdfIoQueueRetrieveNextRequest
    status_indication_queue: IoQueue<FileRequest<FileObjectContext>>,
}

wdf_kmdf::impl_context_space!(OpenContext);

#[pinned_drop]
impl PinnedDrop for OpenContext {
    fn drop(self: Pin<&mut Self>) {
        // SAFETY: We're never going to replace the pinned data
        let this = unsafe { self.get_unchecked_mut() };

        // we don't carry the binding handle here anymore
        // FIXME: we temporarily do
        debug_assert!(this.binding_handle.0.load().is_null());
        debug_assert!(this.inner.get_mut().file_object.is_none());

        let send_nbl_pool = core::mem::replace(&mut this.send_nbl_pool.0, core::ptr::null_mut());
        if !send_nbl_pool.is_null() {
            unsafe { NdisFreeNetBufferListPool(send_nbl_pool) }
        }

        let recv_nbl_pool = core::mem::replace(&mut this.recv_nbl_pool.0, core::ptr::null_mut());
        if !recv_nbl_pool.is_null() {
            unsafe { NdisFreeNetBufferListPool(recv_nbl_pool) }
        }

        // we allocated device_name so we don't need to explicitly drop it

        let device_desc = core::mem::take(&mut this.device_desc.0);
        {
            // This memory was allocated by NDIS
            let device_desc = unsafe {
                core::mem::transmute::<
                    ManuallyDrop<NtUnicodeString>,
                    windows_kernel_sys::UNICODE_STRING,
                >(device_desc)
            };

            unsafe { NdisFreeMemory(device_desc.Buffer.cast(), 0, 0) };
        }

        // read queue & status indication queue are automatically cleaned up
    }
}

struct OpenContextInner {
    /// State information
    flags: OpenContextFlags,
    // protected by `lock`
    // Used in
    // - send::EvtIoWrite (unprotected read)
    // - recv::ReceiveNBLs (read)
    // - recv::QueueReceiveNBL (read)
    // - BindAdapter (write, Initializing)
    // - CreateBinding (write, Paused)
    // - UnbindAdapter (unprotected write, Closing)
    // - PnPEventHandler
    //   - (write, Pausing)
    //   - (unprotected write, Paused)
    //   - (unprotected read dbg, Paused)
    //   - (unprotected write, Restart)
    state: OpenState,
    /// Set in `OPEN_DEVICE`
    // Used in
    // - EvtFileCleanup (breaking assoc)
    // - OpenDevice (setting up assoc, breaking assoc in error case of setting filter)
    //
    // Essentially just used as debugging for figuring out the assoc'd file object
    file_object: Option<Ref<FileObject<FileObjectContext>>>,
}

struct RecvQueue<const LEN: usize> {
    // Used in
    // - recv::ServiceReads (read, technically write into_iter)
    // - recv::QueueReceiveNBL (write)
    // - recv::FlushReceiveQueue (write into_iter)
    // - BindAdapter (write)
    queue: [Option<&'static mut NetBufferList>; LEN],

    // read == write
    // write is always ahead or at read
    //
    // ???: wrapping behaviour?
    //
    // replacement behaviour:
    // bump read head
    // take element
    // bump write head

    //
    // |  0  |  0  |  0  |  0  |
    // ^
    // read (0, 0)
    // write (0, 0)
    //
    // |  a  |  0  |  0  |  0  |
    // ^     ^
    // read  |
    //       write (0, 1)
    //
    // |  a  |  b  |  0  |  0  |
    // ^           ^
    // read        |
    //             write (0, 2)
    //
    // |  a  |  b  |  c  |  0  |
    // ^                 ^
    // read              |
    //                   write (0, 3)
    //
    // |  a  |  b  |  c  |  d  |
    // ^                       ^
    // read                    |
    //                         write (1, 0)

    // read head & write head are composed of a
    // - lap portion (head / LEN)
    // - index portion (head % LEN)
    //
    // the queue is empty if they're on the same index & lap
    read_head: usize,
    write_head: usize,
}

impl<const LEN: usize> RecvQueue<LEN> {
    fn new() -> Self {
        Self {
            queue: core::array::from_fn(|_| None),
            read_head: 0,
            write_head: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.read_head == self.write_head
    }

    fn len(&self) -> u32 {
        (self.write_head.wrapping_sub(self.read_head) % LEN) as u32
    }

    fn enqueue(
        &mut self,
        element: &'static mut NetBufferList,
    ) -> Option<&'static mut NetBufferList> {
        let new_write_head = self.write_head.wrapping_add(1) % LEN;

        let old_element = if new_write_head == self.read_head {
            // queue is full, need to move the read head
            let old_element = self.queue[self.read_head].replace(element);
            self.read_head = self.read_head.wrapping_add(1) % LEN;
            old_element
        } else {
            // there's still room, so we can write to the write head
            self.queue[self.write_head] = Some(element);
            None
        };

        self.write_head = new_write_head;
        old_element
    }

    fn dequeue(&mut self) -> Option<&'static mut NetBufferList> {
        if self.read_head == self.write_head {
            // queue is empty
            return None;
        }

        let element = self.queue[self.read_head].take();
        self.read_head = self.read_head.wrapping_add(1) % LEN;
        element
    }

    fn front(&self) -> Option<&NetBufferList> {
        if self.is_empty() {
            return None;
        }

        self.queue[self.read_head].as_ref().map(|it| &**it)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpenState {
    Initializing,
    Running,
    Pausing,
    Paused,
    // FIXME: Use this somewhere
    #[allow(unused)]
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
    }
}

const MAX_MULTICAST_ADDRESS: usize = 32;

const NPROT_FLAGS_ALLOCATED_NBL: u32 = 1 << 28;

#[derive(Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
struct MACAddr([u8; 6]);

impl MACAddr {
    const fn zero() -> Self {
        MACAddr([0; 6])
    }
}

#[repr(C, packed)]
struct EthHeader {
    dst_addr: MACAddr,
    src_addr: MACAddr,
    eth_type: u16,
}

#[repr(C, packed)]
struct TaggedEthHeader {
    dst_addr: MACAddr,
    src_addr: MACAddr,
    /// ether_type of an 802.1p/q frame
    tpid: u16,
    /// tag control information
    tci: u16,
    eth_type: u16,
}

#[vtable::vtable]
impl wdf_kmdf::driver::DriverCallbacks for NdisProt {
    // Need to cleanup resources here because doing it in drop means that we're just freeing memory
    fn unload(self: Pin<&Self>) {
        debug!("Unload Enter");
        // UnregisterExCallback

        ndisbind::unload_protocol(self.as_ref());

        // the framework handles deleting of the control object
        // self.control_device = core::ptr::null_mut();
        debug!("Unload Exit");
    }
}

#[pin_data]
pub struct KeEvent {
    #[pin]
    event: KEVENT,
}

impl core::fmt::Debug for KeEvent {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("KeEvent").finish_non_exhaustive()
    }
}

unsafe impl Sync for KeEvent {}

impl KeEvent {
    pub fn new(event_type: EventType, start_signaled: bool) -> impl PinInit<Self> {
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

    pub fn wait(&self, timeout: Timeout) -> Result<(), Error> {
        let timeout = timeout
            .0
            .as_ref()
            .map_or(core::ptr::null::<i64>(), |it| (it as *const i64).cast());
        let timeout = timeout.cast_mut();

        let event = &self.event as *const KEVENT;

        // SAFETY:
        // We must imagine the `KEVENT` as Thread-safe
        // or: The limits of my sanity when interfacing with implicit documentation
        //
        // We're going off of the "General Objects" description from here:
        // <https://www.geoffchappell.com/studies/windows/km/ntoskrnl/inc/ntos/ntosdef_x/dispatcher_header/index.htm>
        //
        // Calling `KeWaitForSingleObject` on a `KEVENT` likely accesses the following fields:
        // - `Signalling: UCHAR` likely to check if the `KEVENT` is in the signalling state
        // - `SignalState: LONG` for what kind of signaling to do
        // - `WaitListHead: LIST_ENTRY` to add the current thread to the waiting list
        //
        // The thing is that there's an assumption of it being possible for multiple threads
        // to access a `KEVENT` object as in <https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/defining-and-using-an-event-object>
        // there's a description of a worker thread which is separate from the main thread.
        //
        // Multiple threads may also add themselves to `WaitListHead`, which
        // can be done atomically using a CAS on a U128 or memory fence magic
        //
        // There's also the situation of this has been used in multithreaded
        // contexts without crashing or blowing up.
        //
        // By all accounts, this must operate on a *const
        Error::to_err(unsafe {
            KeWaitForSingleObject(
                event.cast::<core::ffi::c_void>().cast_mut(),
                windows_kernel_sys::KWAIT_REASON::Executive,
                windows_kernel_sys::MODE::KernelMode.0 as i8,
                false as u8,
                // LARGE_INTEGER basically has the same layout as an i64
                timeout.cast(),
            )
        })
    }

    pub fn signal(&self) {
        unsafe { KeSetEvent(core::ptr::addr_of!(self.event).cast_mut(), 1, 0) };
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Timeout(Option<i64>);

impl Timeout {
    /// Wait until reaching an absolute timestamp, relative to January 1st, 1601, in 100-nanosecond increments
    ///
    /// Also accounts for changes in the system time.
    pub fn absolute(timestamp: i64) -> Self {
        assert!(timestamp > 0);
        Self(Some(timestamp))
    }

    /// Waits for an interval (in units of 100-nanoseconds) to pass
    pub fn relative(duration: i64) -> Self {
        assert!(duration > 0);
        Self(Some(duration.wrapping_neg()))
    }

    /// Like [`Self::relative`], but in units of milliseconds
    pub fn relative_ms(duration: i64) -> Self {
        // 100 ns is basically 0.1 us
        // 1 ms = 1_000 us = 1_000_0 100-ns
        Self::relative(
            duration
                .checked_mul(1_000_0)
                .expect("overflow in ms to 100-ns conversion"),
        )
    }

    /// Don't wait and return immediately
    pub fn dont_wait() -> Self {
        Self(Some(0))
    }

    /// Wait indefinitely until the object is set to the signaled state.
    ///
    /// ## Note
    ///
    /// Caller must be at IRQL <= APC_LEVEL
    pub fn forever() -> Self {
        Self(None)
    }
}

#[repr(i32)]
pub enum EventType {
    Notification = windows_kernel_sys::_EVENT_TYPE::NotificationEvent.0,
    Synchronization = windows_kernel_sys::_EVENT_TYPE::SynchronizationEvent.0,
}

/// Called when the framework recives a IRP_MJ_CREATE request (e.g. when an application opens the device
/// to perform IO operations on it).
///
/// ## Note
/// This is called synchronously and in the context of the thread which created the IRP_MJ_CREATE request.
unsafe extern "C" fn ndisprot_evt_device_file_create(
    _Device: WDFDEVICE,
    Request: WDFREQUEST,
    FileObject: WDFFILEOBJECT,
) {
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    debug!("Open: FileObject {file_object:#x?}");

    let status = unsafe {
        file_object.init_context_space(pinned_init::try_init!(FileObjectContext {
            open_context <- SpinMutex::new(None),
        }? Error))
    };

    let status = match status {
        Ok(()) => STATUS::SUCCESS,
        Err(err) => err.0,
    };

    wdf_kmdf::raw::WdfRequestComplete(Request, status.to_u32());
}

/// Called when all of the handles representing the FileObject are closed, as well as all of the references being removed.
///
/// ## Note
/// This is called in an arbitrary thread context, so any context that was created in `FileCreate` should be done in the
/// cleanup callback.
unsafe extern "C" fn ndisprot_evt_file_close(FileObject: WDFFILEOBJECT) {
    let file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    debug!("Close: FileObject {:#x?}", file_object);

    let _ = file_object.get_context().open_context.lock().take();
}

/// Called when the handle representing the `FileObject` is closed.
///
/// ## Note
/// This is called in the context of the thread which originally closed the handle
unsafe extern "C" fn ndisprot_evt_file_cleanup(FileObject: WDFFILEOBJECT) {
    let file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    let open_object = file_object.get_context().open_context();

    debug!("Cleanup: FileObject {file_object:x?}, Open: {open_object:x?}",);

    if let Some(open_object) = &open_object {
        let open_context = open_object.get_context();

        // Mark this endpoint
        {
            let mut inner = open_context.inner.lock();
            inner.flags.set(OpenContextFlags::OPEN_IDLE, true);
            let _ = inner.file_object.take();
        }

        // Set packet filter to 0, telling NDIS we aren't interested in any more receives.
        // We do not want to wait for the device to be powered on
        let packet_filter = [0u8; 4];
        let mut bytes_processed = 0u32;
        let status = ndisbind::validate_open_and_do_request(
            &open_context,
            NDIS_REQUEST_TYPE::NdisRequestSetInformation,
            OID_GEN_CURRENT_PACKET_FILTER,
            packet_filter.as_ptr().cast_mut().cast(),
            packet_filter.len() as u32,
            &mut bytes_processed,
            false,
        );
        if let Err(err) = Error::to_err(status) {
            debug!(
                "Cleanup: open {open_object:x?}, set packet filter ({packet_filter:#x?}) failed {err:#x?}",
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
                open_context.read_queue.purge_synchronously();
                // Cancel pending ioctl request for status indication
                open_context.status_indication_queue.purge_synchronously();
            }
        }

        // Cleanup the receive packet queue
        recv::flush_receive_queue(&open_object);
    }

    debug!("Cleanup: {open_object:x?}");
}

const FSCTL_NDISPROT_BASE: DeviceType = DeviceType::Network;
const IOCTL_NDISPROT_OPEN_DEVICE: IoControlCode = IoControlCode::new(
    FSCTL_NDISPROT_BASE,
    0x200,
    TransferMethod::Buffered,
    RequiredAccess::ReadWrite,
);
const IOCTL_NDISPROT_QUERY_OID_VALUE: IoControlCode = IoControlCode::new(
    FSCTL_NDISPROT_BASE,
    0x201,
    TransferMethod::Buffered,
    RequiredAccess::ReadWrite,
);
const IOCTL_NDISPROT_SET_OID_VALUE: IoControlCode = IoControlCode::new(
    FSCTL_NDISPROT_BASE,
    0x205,
    TransferMethod::Buffered,
    RequiredAccess::ReadWrite,
);
const IOCTL_NDISPROT_QUERY_BINDING: IoControlCode = IoControlCode::new(
    FSCTL_NDISPROT_BASE,
    0x203,
    TransferMethod::Buffered,
    RequiredAccess::ReadWrite,
);
const IOCTL_NDISPROT_BIND_WAIT: IoControlCode = IoControlCode::new(
    FSCTL_NDISPROT_BASE,
    0x204,
    TransferMethod::Buffered,
    RequiredAccess::ReadWrite,
);
const IOCTL_NDISPROT_INDICATE_STATUS: IoControlCode = IoControlCode::new(
    FSCTL_NDISPROT_BASE,
    0x206,
    TransferMethod::Buffered,
    RequiredAccess::ReadWrite,
);

unsafe extern "C" fn ndisprot_evt_io_device_control(
    _Queue: WDFQUEUE,           // in
    Request: WDFREQUEST,        // in
    _OutputBufferLength: usize, // in
    _InputBufferLength: usize,  // in
    IoControlCode: ULONG,       // in
) {
    let file_object = unsafe { wdf_kmdf::raw::WdfRequestGetFileObject(Request) };

    let file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(file_object) };

    let file_object_ctx = file_object.get_context();
    let open_object = file_object_ctx.open_context();

    let mut bytes_returned = 0;

    let control_code = IoControlCode::from(IoControlCode);

    let mut nt_status;

    match control_code {
        IOCTL_NDISPROT_BIND_WAIT => {
            // Block until we've seen a `NetEventBindsComplete` event,
            // meaning that we've finished binding to all running adapters
            // that we're supposed to bind to
            //
            // Wait a max of 5 seconds to see if this is the case

            let driver = unsafe { wdf_kmdf::raw::WdfGetDriver() }
                .map(|driver| unsafe { wdf_kmdf::driver::Driver::<NdisProt>::wrap(driver) });

            if let Some(driver) = driver {
                nt_status = match driver
                    .get_context()
                    .binds_complete
                    .wait(Timeout::relative_ms(5_000))
                {
                    Ok(()) => STATUS::SUCCESS,
                    Err(_) => STATUS::TIMEOUT,
                };
            } else {
                // no open context to go off of, and would've timed out anyway
                nt_status = STATUS::TIMEOUT;
            }

            debug!("IoControl: BindWait returning {:#x?}", nt_status);
        }
        IOCTL_NDISPROT_QUERY_BINDING => {
            assert_eq!(control_code.transfer_method(), TransferMethod::Buffered);

            let mut sys_buffer = core::ptr::null_mut();
            let mut buf_size = 0;

            nt_status = unsafe {
                wdf_kmdf::raw::WdfRequestRetrieveOutputBuffer(
                    Request,
                    core::mem::size_of::<QueryBinding>(),
                    &mut sys_buffer,
                    Some(&mut buf_size),
                )
                .into()
            };

            if nt_status.is_success() {
                nt_status =
                    ndisbind::query_binding(sys_buffer, buf_size, buf_size, &mut bytes_returned)
                        .into();

                debug!("IoControl: QueryBinding returning {:#x?}", nt_status);
            } else {
                error!("WdfRequestRetrieveOutputBuffer failed {:#x?}", nt_status);
            }
        }
        IOCTL_NDISPROT_OPEN_DEVICE => {
            assert_eq!(control_code.transfer_method(), TransferMethod::Buffered);

            'out: {
                if let Some(open_object) = open_object {
                    log::warn!(
                        "IoControl: OPEN_DEVICE: FileObj {file_object:x?} already associated with open {open_object:x?}",
                    );
                    nt_status = STATUS::DEVICE_BUSY;
                    break 'out;
                }

                let mut sys_buffer = core::ptr::null_mut();
                let mut buf_size = 0;

                nt_status = unsafe {
                    wdf_kmdf::raw::WdfRequestRetrieveInputBuffer(
                        Request,
                        0,
                        &mut sys_buffer,
                        Some(&mut buf_size),
                    )
                    .into()
                };

                if !nt_status.is_success() {
                    log::warn!("WdfRequestRetrieveInputBuffer failed {nt_status:x?}");
                    break 'out;
                }

                let device_name =
                    unsafe { core::slice::from_raw_parts(sys_buffer.cast(), buf_size) };

                match open_device(device_name, file_object.clone_ref()) {
                    Ok(open_context) => {
                        log::trace!(
                            "IoControl OPEN_DEVICE: Open {open_context:x?} <-> FileObject {file_object:x?}",
                        )
                    }
                    Err(err) => nt_status = err.0,
                }
            }
        }
        IOCTL_NDISPROT_QUERY_OID_VALUE => {
            assert_eq!(control_code.transfer_method(), TransferMethod::Buffered);

            'out: {
                let mut sys_buffer = core::ptr::null_mut();
                let mut buf_size = 0;

                nt_status = unsafe {
                    wdf_kmdf::raw::WdfRequestRetrieveOutputBuffer(
                        Request,
                        core::mem::size_of::<QueryOid>(),
                        &mut sys_buffer,
                        Some(&mut buf_size),
                    )
                    .into()
                };

                if !nt_status.is_success() {
                    log::warn!("WdfRequestRetrieveOutputBuffer failed {:x?}", nt_status);
                    break 'out;
                }

                if let Some(open_context) = open_object {
                    let open_context = open_context.get_context();

                    let status = ndisbind::query_oid_value(
                        &*open_context,
                        sys_buffer,
                        buf_size,
                        &mut bytes_returned,
                    );
                    nt_status = ndis_status_to_nt_status(status).0;
                } else {
                    nt_status = STATUS::DEVICE_NOT_CONNECTED;
                }
            }
        }
        IOCTL_NDISPROT_SET_OID_VALUE => {
            assert_eq!(control_code.transfer_method(), TransferMethod::Buffered);

            'out: {
                let mut sys_buffer = core::ptr::null_mut();
                let mut buf_size = 0;

                nt_status = unsafe {
                    wdf_kmdf::raw::WdfRequestRetrieveInputBuffer(
                        Request,
                        core::mem::size_of::<SetOid>(),
                        &mut sys_buffer,
                        Some(&mut buf_size),
                    )
                    .into()
                };

                if !nt_status.is_success() {
                    log::warn!("WdfRequestRetrieveInputBuffer failed {:x?}", nt_status);
                    break 'out;
                }

                if let Some(open_context) = open_object {
                    let open_context = open_context.get_context();

                    let status = ndisbind::set_oid_value(&*open_context, sys_buffer, buf_size);

                    bytes_returned = 0;

                    nt_status = ndis_status_to_nt_status(status).0;
                } else {
                    nt_status = STATUS::DEVICE_NOT_CONNECTED;
                }
            }
        }
        IOCTL_NDISPROT_INDICATE_STATUS => {
            assert_eq!(control_code.transfer_method(), TransferMethod::Buffered);

            if let Some(open_context) = open_object {
                let open_context = open_context.get_context();

                let request = unsafe { FileRequest::<FileObjectContext>::wrap_raw(Request.cast()) };

                let status = open_context
                    .status_indication_queue
                    .forward_to_queue(request);

                nt_status = match status {
                    Ok(_) => STATUS::PENDING,
                    Err(err) => err.0,
                };
            } else {
                nt_status = STATUS::DEVICE_NOT_CONNECTED;
            }
        }
        _ => nt_status = STATUS::NOT_SUPPORTED,
    }

    if nt_status != STATUS::PENDING {
        unsafe {
            wdf_kmdf::raw::WdfRequestCompleteWithInformation(
                Request,
                nt_status.to_u32(),
                bytes_returned as ULONG_PTR,
            )
        }
    }
}

/// Helper function called to process IOCTL_NDISPROT_OPEN_DEVICE.
/// Check if there is a binding to the specified device and is not associated with a file object already.
/// If so, make an association between the binding and this file object.
fn open_device(
    device_name: &[u8],
    file_object: Ref<FileObject<FileObjectContext>>,
) -> Result<Ref<GeneralObject<OpenContext>>, Error> {
    let Some(driver) = (unsafe { wdf_kmdf::raw::WdfGetDriver() }) else {
        // Driver not initialized yet
        return Err(STATUS::UNSUCCESSFUL.into());
    };
    let driver = unsafe { wdf_kmdf::driver::Driver::<NdisProt>::wrap(driver) };
    let globals = driver.get_context();

    let file_context = file_object.get_context();

    let status = 'out: {
        // length shouldn't include nul terminator
        let device_name_len = 'len: {
            if let Some(new_len) = device_name.len().checked_sub(2) {
                if &device_name[new_len..] == &[0, 0] {
                    break 'len new_len;
                }
            }

            device_name.len()
        };
        let device_name = unsafe {
            NtUnicodeStr::from_raw_parts(
                device_name.as_ptr().cast(),
                device_name_len as u16,
                device_name.len() as u16,
            )
        };

        let Some(open_object) = ndisbind::ndisprot_lookup_device(globals, device_name) else {
            log::warn!("open_device: couldn't find device {device_name}");
            break 'out Err(Error(STATUS::OBJECT_NAME_NOT_FOUND));
        };

        let open_context = open_object.get_context();

        let mut inner = open_context.inner.lock();

        if !inner.flags.contains(OpenContextFlags::OPEN_IDLE) {
            log::warn!(
                "open_device: open {:x?}/{:x?} already associated with another file object {:x?}",
                open_object,
                inner.flags,
                inner.file_object
            );
            break 'out Err(Error(STATUS::DEVICE_BUSY));
        }

        let old_open = {
            let mut inner = file_context.open_context.lock();

            match inner.as_ref() {
                Some(old) => Err(old.clone_ref()),
                None => {
                    *inner = Some(open_object.clone_ref());
                    Ok(())
                }
            }
        };

        if let Err(current) = old_open {
            // already used by another open
            let flags = current.get_context().inner.lock().flags;

            log::warn!(
                "open_device: file object {:x?} already associated with open {:x?}/{:x?}",
                file_object,
                current,
                flags
            );
            break 'out Err(Error(STATUS::INVALID_DEVICE_REQUEST));
        }
        inner.file_object = Some(file_object.clone_ref());

        // Start the queus as they may have been in a purged state if someone
        // else has previously opened the device
        open_context.read_queue.start();
        open_context.status_indication_queue.start();

        inner.flags.remove(OpenContextFlags::OPEN_FLAGS);
        inner.flags.set(OpenContextFlags::OPEN_ACTIVE, true);

        drop(inner);

        // Set the packet filter now
        let packet_filter = NPROTO_PACKET_FILTER.to_ne_bytes();
        let mut bytes_processed = 0u32;
        let status = ndisbind::validate_open_and_do_request(
            &open_context,
            NDIS_REQUEST_TYPE::NdisRequestSetInformation,
            OID_GEN_CURRENT_PACKET_FILTER,
            packet_filter.as_ptr().cast_mut().cast(),
            packet_filter.len() as u32,
            &mut bytes_processed,
            true,
        );
        if let Err(err) = Error::to_err(status) {
            let err = ndis_status_to_nt_status(err.0.to_u32());

            log::warn!(
                "open_device: open {:x?}: set packet filter ({:x?}) failed: {:x?}",
                open_object,
                packet_filter,
                err
            );

            // undo everything we did above
            let mut inner = open_context.inner.lock();

            // Need to set OpenContext to null again, so others can open for this file object later
            let current_open_context_obj = file_context.open_context.lock().take();
            assert_eq!(current_open_context_obj.as_ref(), Some(&open_object));

            inner.flags.remove(OpenContextFlags::OPEN_FLAGS);
            inner.flags.set(OpenContextFlags::OPEN_IDLE, true);

            inner.file_object = None;

            break 'out Err(err);
        }

        Ok(open_object)
    };

    status
}

#[repr(C)]
#[derive(Clone, Copy)]
struct QueryOid {
    oid: NDIS_OID,
    port_number: NDIS_PORT_NUMBER,
    // variable length, minimum 4 bytes
    data: [u8; core::mem::size_of::<ULONG>()],
}

#[repr(C)]
#[derive(Clone, Copy)]
struct SetOid {
    oid: NDIS_OID,
    port_number: NDIS_PORT_NUMBER,
    // variable length, minimum 4 bytes
    data: [u8; core::mem::size_of::<ULONG>()],
}

#[repr(C)]
#[derive(Clone, Copy)]
struct QueryBinding {
    binding_index: u32,
    device_name_offset: u32,
    device_name_length: u32,
    device_descr_offset: u32,
    device_descr_length: u32,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct IndicateStatus {
    /// NDIS_STATUS
    indicated_status: u32,
    /// offset from the start of this struct
    status_buffer_offset: u32,
    /// length in bytes
    status_buffer_length: u32,
}

fn ndis_status_to_nt_status(status: NDIS_STATUS) -> Error {
    // handle the status codes which map to NT status codes first
    use windows_kernel_sys::result::{NtStatus, ERROR::NDIS};

    let status = match NtStatus::from(status) {
        same @ (STATUS::SUCCESS
        | STATUS::PENDING
        | STATUS::BUFFER_OVERFLOW
        | STATUS::UNSUCCESSFUL
        | STATUS::INSUFFICIENT_RESOURCES
        | STATUS::NOT_SUPPORTED) => same,
        not_same => {
            let status = windows_kernel_sys::result::HResultError::from(not_same.to_u32());

            match status {
                NDIS::BUFFER_TOO_SHORT => STATUS::BUFFER_TOO_SMALL,
                NDIS::INVALID_LENGTH => STATUS::INVALID_BUFFER_SIZE,
                NDIS::INVALID_DATA => STATUS::INVALID_PARAMETER,
                NDIS::ADAPTER_NOT_FOUND => STATUS::NO_MORE_ENTRIES,
                NDIS::ADAPTER_NOT_READY => STATUS::DEVICE_NOT_READY,
                _ => STATUS::UNSUCCESSFUL,
            }
        }
    };

    Error(status)
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

#[no_mangle]
pub extern "system" fn __CxxFrameHandler3() -> i32 {
    0
}
