#![no_std]
#![allow(non_snake_case, unused_variables, dead_code)] // Cut down on the warnings for now
#![feature(allocator_api, const_option, result_option_inspect, offset_of)]

extern crate alloc;

mod ndisbind;

mod send;

mod recv;

use core::{
    mem::MaybeUninit,
    pin::Pin,
    ptr::addr_of_mut,
    sync::atomic::{AtomicUsize, Ordering},
};

use alloc::sync::Arc;
use once_arc::OnceArc;
use pinned_init::{pin_data, pinned_drop, InPlaceInit, PinInit};
use wdf_kmdf::{object::AsObjectHandle, sync::SpinMutex};
use wdf_kmdf_sys::{
    WDFDEVICE, WDFFILEOBJECT, WDFQUEUE, WDFREQUEST, WDF_FILEOBJECT_CONFIG, WDF_IO_QUEUE_CONFIG,
    WDF_IO_QUEUE_DISPATCH_TYPE,
};
use windows_kernel_rs::{
    ioctl::{DeviceType, IoControlCode, RequiredAccess, TransferMethod},
    log::{self, debug, error, info, warn},
    string::{nt_unicode_str, unicode_string::NtUnicodeStr},
    DriverObject,
};
use windows_kernel_sys::{
    result::STATUS, Error, KeInitializeEvent, KeWaitForSingleObject, NdisGeneratePartialCancelId,
    KEVENT, LIST_ENTRY, NDIS_HANDLE, NDIS_OBJECT_TYPE_PROTOCOL_DRIVER_CHARACTERISTICS,
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1,
    NDIS_REQUEST_TYPE, NET_DEVICE_POWER_STATE, NTSTATUS, OID_GEN_CURRENT_PACKET_FILTER,
    PDRIVER_OBJECT, PUNICODE_STRING, ULONG, ULONG_PTR,
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

    windows_kernel_rs::init_kernel_logger!(log::COMPONENT_IHVDRIVER, log::LevelFilter::Debug);

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
                        // Pass in the driver context so that `BindAdapter` has access to the driver globals
                        driver.as_handle_mut(),
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

                // Note:
                // If an error occurs after a successful call to NdisRegisterProtocolDriver, the driver must call the NdisDeregisterProtocolDriver function before DriverEntry returns.
                let handle = unsafe { handle.assume_init() };

                let cancel_id_gen = CancelIdGen::new(unsafe { NdisGeneratePartialCancelId() });

                info!(
                    "DriverEntry: Ndis Cancel Id: {:#x?}",
                    cancel_id_gen.partial_id
                );

                let globals = pinned_init::try_pin_init!(Globals {
                    control_device,
                    ndis_protocol_handle: handle,
                    eth_type: NPROT_ETH_TYPE,
                    cancel_id_gen,
                    open_list <- SpinMutex::new(ListEntry::new()),
                    binds_complete <- KeEvent::new(EventType::Notification, false),
                }? Error);

                Ok(pinned_init::try_pin_init! {
                    NdisProt {
                        globals: Arc::try_pin_init(globals)?
                    }? Error
                })
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
    return Ok(control_device);
}

const NT_DEVICE_NAME: NtUnicodeStr<'static> = nt_unicode_str!(r"\Device\Ndisprot");
const DOS_DEVICE_NAME: NtUnicodeStr<'static> = nt_unicode_str!(r"\Global??\Ndisprot");

// Following two are arranged in the way a little-endian processor would read 2 bytes from the wire
const NPROT_ETH_TYPE: u16 = 0x8e88;
const NPROT_8021P_TAG_TYPE: u16 = 0x0081;

#[pin_data]
struct NdisProt {
    globals: Pin<Arc<Globals>>,
}

wdf_kmdf::impl_context_space!(NdisProt);

// Pretty much the only viable way to pass stuff via `ProtocolDriverContext`
// while also being able to free it is to stuff it in a WdfObj parented to the driver
//
// Alternatively could do `ProtocolHandle<DriverContext>` storing a `Box<DriverContext>`
// and then manifesting a reference in `BindAdapter`.

#[pin_data(PinnedDrop)]
struct Globals {
    // Only used by BindAdapter to create the WDFIOQUEUEs for the adapter
    control_device: WDFDEVICE,
    // Used in
    // - ndisbind::BindAdapter
    //   - ndisbind::CreateBinding
    //     - Creating the NBL pool for send
    //     - Creating the NBL pool for recv
    //     - Opening the adapter (`NdisOpenAdapterEx`)
    // - (Dropping) ProtocolUnloadHandler (unused)
    // - (Dropping) EvtUnload
    ndis_protocol_handle: NDIS_HANDLE,
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
    // TODO: ListEntry is really just a ListHead<OpenContextList>
    // Used in
    // - ndisbind::BindAdapter (pre `CreateBinding`), mut
    //   - Have access to a ProtocolBindingContext
    // - ndisbind::QueryBinding, iter
    //   - EvtIoDeviceControl
    //     - Have access to FileObject.ContextSpace
    // - ndisbind::LookupDevice, iter
    //   - EvtIoDeviceControl
    //     - Have access to FileObject.ContextSpace
    #[pin]
    open_list: SpinMutex<ListEntry>,
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

#[pinned_drop]
impl PinnedDrop for Globals {
    fn drop(mut self: Pin<&mut Self>) {
        debug!("Globals Drop Enter");
        // UnregisterExCallback

        ndisbind::unload_protocol(self.as_mut());

        // the framework handles deleting of the control object
        self.control_device = core::ptr::null_mut();
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

struct FileObjectContext {
    open_context: OnceArc<OpenContext>,
}

wdf_kmdf::impl_context_space!(FileObjectContext);

impl FileObjectContext {
    fn open_context(&self) -> Option<Pin<Arc<OpenContext>>> {
        self.open_context
            .get()
            .map(|arc| unsafe { Pin::new_unchecked(arc) })
    }
}

struct ControlDevice;

wdf_kmdf::impl_context_space!(ControlDevice);

/// See <https://github.com/microsoft/Windows-driver-samples/blob/d9acf794c92ba2fb0525f6c794ef394709035ac3/network/ndis/ndisprot_kmdf/60/ndisprot.h#L54-L90>
#[pin_data(PinnedDrop)]
struct OpenContext {
    globals: Pin<Arc<Globals>>,

    /// Link to the global `open_list`
    #[pin]
    link: ListEntry,
    inner: SpinMutex<OpenContextInner>,

    /// Set in `OPEN_DEVICE`
    // Used in
    // - EvtFileCleanup (breaking assoc)
    // - OpenDevice (setting up assoc, breaking assoc in error case of setting filter)
    //
    // Essentially just used as debugging for figuring out the assoc'd file object
    file_object: WDFFILEOBJECT,

    // Used in
    // - EvtIoWrite (read)
    // - ReceiveNBLs (read)
    //   - Have access to a ProtocolBindingContext
    //   - AllocateReceiveNBLs (read)
    // - FreeReceiveNBLs (read)
    //   - ServiceReads
    //     - EvtNotifyReadQueue
    //       - ReadQueue event, have access to ctx area but done before binding create?
    //     - QueueReceiveNBL
    //       - ReceiveNBLs
    //   - ReceiveNBLs
    //     - QueueReceiveNBL
    //   - FlushReceiveQueue
    //     - PnPEventHandler
    //       - Have access to a ProtocolBindingContext
    //     - ShutdownBinding
    //       - CreateBinding
    //       - UnbindAdapter
    //         - Have access to a ProtocolBindingContext
    //     - EvtFileCleanup
    binding_handle: NDIS_HANDLE,
    // set on adapter create
    // Used in
    // - EvtIoWrite
    // - CreateBinding (write, pre binding open)
    // - FreeBindResources
    //   - ShutdownBinding
    //     ...
    send_nbl_pool: NDIS_HANDLE,
    // every nbl contains at least one net buffer
    // set on adapter create
    // Used in
    // - BindAdapter (write, in CreateBinding pre binding open)
    // - FreeBindResources
    //   - ShutdownBinding
    //     ...
    // - AllocateReceiveNBL
    //   - ...
    recv_nbl_pool: NDIS_HANDLE,

    // Used in
    // - CreateBinding (write)
    // - Restart (write)
    mac_options: u32,
    // Used in
    // - EvtIoWrite (read)
    // - CreateBinding (write)
    // - Restart (write)
    max_frame_size: u32,
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
    pended_send_count: u32,

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
    read_queue: WDFQUEUE,
    // protected by `lock`
    // Used in
    // - recv::ServiceReads (write dec)
    // - WaitForPendingIO (read)
    // oh no (no inc)
    pending_read_count: u32,
    #[pin]
    // Used in
    // - recv::ServiceReads (read, technically write into_iter)
    // - recv::QueueReceiveNBL (write)
    // - recv::FlushReceiveQueue (write into_iter)
    // - BindAdapter (write)
    recv_nbl_queue: LIST_ENTRY,
    // protected by `lock`
    // - recv::ServiceReads (write dec)
    // - recv::QueueReceiveNBL (write inc + dec, read)
    // - recv::FlushReceiveQueue (write dec)
    recv_nbl_len: u32,

    // Used in
    // - recv::QueueReceiveNBL (read)
    // - PnPEventHandler (write)
    // - CreateBinding (write)
    // - ValidateOpenAndDoRequest (read)
    // - Status (read)
    power_state: NET_DEVICE_POWER_STATE,
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
    device_name: NtUnicodeStr<'static>,
    /// device display name
    // Used in
    // - CreateBinding (write)
    // - FreeBindResources (free)
    // - QueryBinding (read)
    device_desc: NtUnicodeStr<'static>,

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
    // Used in
    // - send::SendComplete (signal, write NULL)
    // - ShutdownBinding (write NULL, write local event storage + init)
    // - WaitForPendingIO (assert != NULL, wait forever)
    // - ValidateOpenAndDoRequest (assert != NULL, signal, write NULL)
    // - QueryOidValue (assert != NULL, signal, write NULL)
    // - SetOidValue (assert != NULL, signal, write NULL)
    closing_event: *mut KeEvent,
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
    status_indication_queue: WDFQUEUE,
}

#[pinned_drop]
impl PinnedDrop for OpenContext {
    fn drop(self: Pin<&mut Self>) {
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
    // This will also drop the (hopefully last remaining) reference to the globals
    fn unload(self: Pin<&mut Self>) {
        debug!("Unload Enter");
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
pub struct KeEvent {
    #[pin]
    event: KEVENT,
}

impl KeEvent {
    pub fn new(event_type: EventType, start_signaled: bool) -> impl PinInit<Self, Error> {
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
    Device: WDFDEVICE,
    Request: WDFREQUEST,
    FileObject: WDFFILEOBJECT,
) {
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    debug!("Open: FileObject {:#x?}", file_object);

    let status = unsafe {
        file_object.init_context_space(pinned_init::try_init!(FileObjectContext {
            open_context: OnceArc::new(None)
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
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    debug!("Close: FileObject {:#x?}", file_object);

    // file_object.get_context_mut().open_context.take();
}

/// Called when the handle representing the `FileObject` is closed.
///
/// ## Note
/// This is called in the context of the thread which originally closed the handle
unsafe extern "C" fn ndisprot_evt_file_cleanup(FileObject: WDFFILEOBJECT) {
    let mut file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(FileObject) };

    // let open_context = file_object.get_context_mut().open_context.take();
    let open_context = None::<Arc<OpenContext>>;

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
    Queue: WDFQUEUE,           // in
    Request: WDFREQUEST,       // in
    OutputBufferLength: usize, // in
    InputBufferLength: usize,  // in
    IoControlCode: ULONG,      // in
) {
    let file_object = unsafe { wdf_kmdf::raw::WdfRequestGetFileObject(Request) };

    let file_object =
        unsafe { wdf_kmdf::file_object::FileObject::<FileObjectContext>::wrap(file_object) };

    let open_context = file_object.get_context().open_context();

    let mut bytes_returned = 0;

    let control_code = IoControlCode::from(IoControlCode);

    let mut nt_status = STATUS::UNSUCCESSFUL;

    match control_code {
        IOCTL_NDISPROT_BIND_WAIT => {
            // Block until we've seen a `NetEventBindsComplete` event,
            // meaning that we've finished binding to all running adapters
            // that we're supposed to bind to
            //
            // Wait a max of 5 seconds to see if this is the case

            if let Some(open_context) = open_context {
                nt_status = match open_context
                    .globals
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
            // calls open_device
        }
        IOCTL_NDISPROT_QUERY_OID_VALUE => {}
        IOCTL_NDISPROT_SET_OID_VALUE => {}
        IOCTL_NDISPROT_INDICATE_STATUS => {}
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

fn open_device(
    device_name: &[u8],
    FileObject: WDFFILEOBJECT,
) -> Result<Pin<Arc<OpenContext>>, Error> {
    loop {}
}

#[repr(C)]
struct QueryBinding {
    binding_index: u32,
    device_name_offset: u32,
    device_name_length: u32,
    device_descr_offset: u32,
    device_descr_length: u32,
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
