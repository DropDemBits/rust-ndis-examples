//! NDIS protocol entry points as well as handling binding and unbinding from adapters
use core::{
    marker::PhantomData,
    mem::ManuallyDrop,
    pin::Pin,
    sync::atomic::{AtomicBool, AtomicU32, Ordering},
};

use alloc::boxed::Box;
use crossbeam_utils::atomic::AtomicCell;
use nt_list::{
    single_list::{NtSingleList, NtSingleListEntry, NtSingleListHead},
    NtListElement,
};
use pinned_init::{pin_data, InPlaceInit, PinInit};
use scopeguard::ScopeGuard;
use wdf_kmdf::{
    driver::Driver,
    handle::{HasContext, Ref},
    io_queue::IoQueue,
    object::GeneralObject,
    sync::SpinPinMutex,
};
use wdf_kmdf_sys::WDF_IO_QUEUE_CONFIG;
use windows_kernel_rs::{
    log,
    string::unicode_string::{NtUnicodeStr, NtUnicodeString},
};
use windows_kernel_sys::{
    result::STATUS, Error, NdisAllocateNetBufferListPool, NdisCloseAdapterEx,
    NdisDeregisterProtocolDriver, NdisFreeMemory, NdisFreeNetBufferListPool, NdisOidRequest,
    NdisOpenAdapterEx, NdisQueryAdapterInstanceName, NDIS_DEFAULT_PORT_NUMBER,
    NDIS_ETH_TYPE_802_1Q, NDIS_ETH_TYPE_802_1X, NDIS_HANDLE, NDIS_LINK_STATE, NDIS_MEDIUM,
    NDIS_OBJECT_HEADER, NDIS_OBJECT_TYPE_DEFAULT, NDIS_OBJECT_TYPE_OID_REQUEST,
    NDIS_OBJECT_TYPE_OPEN_PARAMETERS, NDIS_OBJECT_TYPE_STATUS_INDICATION, NDIS_OID,
    NDIS_OID_REQUEST, NDIS_OID_REQUEST_REVISION_1, NDIS_OPEN_PARAMETERS,
    NDIS_OPEN_PARAMETERS_REVISION_1, NDIS_PORT_NUMBER, NDIS_PROTOCOL_ID_IPX,
    NDIS_PROTOCOL_RESTART_PARAMETERS, NDIS_REQUEST_TYPE, NDIS_RESTART_ATTRIBUTES,
    NDIS_RESTART_GENERAL_ATTRIBUTES, NDIS_SIZEOF_LINK_STATE_REVISION_1,
    NDIS_SIZEOF_NDIS_OPEN_PARAMETERS_REVISION_1,
    NDIS_SIZEOF_NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1, NDIS_SIZEOF_OID_REQUEST_REVISION_1,
    NDIS_SIZEOF_STATUS_INDICATION_REVISION_1, NDIS_STATUS, NET_BUFFER_LIST_POOL_PARAMETERS,
    NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1, NET_DEVICE_POWER_STATE, NET_IFINDEX,
    NET_IF_MEDIA_CONNECT_STATE, NET_LUID, NET_PNP_EVENT_CODE, OID_802_11_ADD_WEP,
    OID_802_11_AUTHENTICATION_MODE, OID_802_11_BSSID, OID_802_11_BSSID_LIST,
    OID_802_11_BSSID_LIST_SCAN, OID_802_11_CONFIGURATION, OID_802_11_DISASSOCIATE,
    OID_802_11_INFRASTRUCTURE_MODE, OID_802_11_NETWORK_TYPE_IN_USE, OID_802_11_POWER_MODE,
    OID_802_11_RELOAD_DEFAULTS, OID_802_11_REMOVE_WEP, OID_802_11_RSSI, OID_802_11_SSID,
    OID_802_11_STATISTICS, OID_802_11_SUPPORTED_RATES, OID_802_11_WEP_STATUS,
    OID_802_3_MULTICAST_LIST, OID_GEN_CURRENT_PACKET_FILTER, OID_GEN_MINIPORT_RESTART_ATTRIBUTES,
    PNDIS_BIND_PARAMETERS, PNDIS_OID_REQUEST, PNDIS_STATUS_INDICATION, PNET_PNP_EVENT_NOTIFICATION,
    PUCHAR, PVOID, UINT, ULONG, WCHAR,
};

use crate::{
    recv, BindingHandle, EventType, IndicateStatus, KeEvent, MACAddr, NblPool, OpenContext,
    OpenContextFlags, OpenContextInner, OpenState, QueryBinding, QueryOid, RecvQueue, SetOid,
    SyncWrapper, Timeout, MAX_MULTICAST_ADDRESS,
};

use super::NdisProt;

#[pinned_init::pin_data]
struct Request {
    Request: NDIS_OID_REQUEST,
    #[pin]
    ReqEvent: KeEvent,
    Status: ULONG,
}

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

const NPROT_ALLOC_TAG: u32 = u32::from_be_bytes(*b"Nuio");

#[derive(Debug)]
#[pin_data]
pub(crate) struct ProtocolBindingContext {
    /// The actual open context
    pub(crate) open_context: Ref<GeneralObject<OpenContext>>,

    // Initialization context:
    // Note that we have exclusive access to this section

    // For open/close_adapter
    // Used in
    // - OpenAdapterComplete (write)
    // - CreateBinding (read)
    // - ShutdownBinding (read)
    //   - unused except for an assert
    // might not need to be atomic, but feeling a bit iffy about the exclusivity
    bind_status: AtomicU32,
    // Used in
    // - OpenAdapterComplete (signal)
    // - CloseAdapterComplete (signal)
    // - CreateBinding (write init, wait forever (after NdisOpenAdapterEx))
    // - ShutdownBinding (write init, wait forever (after NdisCloseAdapterEx))
    #[pin]
    bind_event: KeEvent,

    // role originally done by `open_context.inner.flags`, but made separate
    // as `open_context` may or may not have been initialized yet
    unbind_received: AtomicBool,

    // Note that these only become valid once the `ProtocolOpenAdapterComplete`
    // callback completes

    // Index into the selected medium array
    selected_medium_index: u32,
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
}

/// Called when NDIS is binding this protocol to an adapter. We set up a
/// binding, with `OpenContext` keeping track of the binding state.
pub(crate) unsafe extern "C" fn bind_adapter(
    ProtocolDriverContext: NDIS_HANDLE,
    BindContext: NDIS_HANDLE,
    BindParameters: PNDIS_BIND_PARAMETERS,
) -> NDIS_STATUS {
    log::debug!(
        "bind_adapter: {:#x?} {:#x?} {:#x?}",
        ProtocolDriverContext,
        BindContext,
        BindParameters
    );

    // Translation note:
    // The original code allows the state of the opening context to be visible,
    // but we only add the open context to the full list by the end, unless we split
    // open context

    let BindParameters = unsafe { &*BindParameters };

    let driver = unsafe { Driver::<NdisProt>::wrap(ProtocolDriverContext.cast()) };
    let globals = driver.get_context();

    // Reserve space for the open context in the global list
    {
        let mut open_list = globals.open_list.lock();
        if let Err(err) = open_list.try_reserve(1) {
            log::error!("reserving space for adding open context to list failed ({err})",);
            return STATUS::INSUFFICIENT_RESOURCES.to_u32();
        }

        open_list.push(None);
    }

    let adapter_name = unsafe { &*BindParameters.AdapterName };
    let adapter_name = unsafe {
        NtUnicodeStr::from_raw_parts(
            adapter_name.Buffer,
            adapter_name.Length,
            adapter_name.MaximumLength,
        )
    };

    // Check if there are any bindings yet
    //
    // ???: Okay so why do we check if there is an existing binding?
    // - is it because we could keep bindings after power run-down?
    // - could a pnp NIC reuse the same adapter name?
    // - can we concurrently bind to the same adapter multiple times?
    //   - wouldn't really make sense since there should only be one binding per adapter
    //   - also, the original code checks it way later on in `ndisprotCreateBindng`, where potetially another binding
    //     could've come in
    //
    // so this seems to be just for checking if we're leaking bindings
    if let Some(other_context) = ndisprot_lookup_device(globals, adapter_name) {
        log::warn!(
            "bind_adapter: binding to {adapter_name} already exists on binding {other_context:x?}",
        );

        return STATUS::UNSUCCESSFUL.to_u32();
    }

    // Note: if we want to async-ify this, we need to somehow break the dependency loop between OpenContext and ProtocolBindingContext
    // orrr we could just decouple open context & EvtIoDeviceControl
    let status = wdf_kmdf::object::GeneralObject::<OpenContext>::with_parent(
        &globals.control_device,
        |open_context| {
            // capture rules moment, need to borrow driver so as to not accidentally move it
            let driver = &driver;

            // initialize the binding context helper
            let protocol_open_context = open_context.clone_ref();
            let protocol_binding_context = Box::pin_init(pinned_init::pin_init! {
                ProtocolBindingContext {
                    open_context: protocol_open_context,
                    bind_status: AtomicU32::new(STATUS::SUCCESS.to_u32()),
                    bind_event <- KeEvent::new(crate::EventType::Notification, false),
                    unbind_received: AtomicBool::new(false),
                    selected_medium_index: 0,
                    binding_handle: core::ptr::null_mut(),
                }
            });

            let protocol_binding_context = match protocol_binding_context {
                Ok(it) => it,
                Err(_err) => {
                    log::error!("bind_adapter: protocol binding context alloc failed");
                    return Err(STATUS::INSUFFICIENT_RESOURCES.into());
                }
            };

            // Manual queue for pending read requests.
            // These read requests are forwarded from the original read queue and
            // serviced in the ProtocolRecv indication handler.
            let read_queue = {
                let queue_config = WDF_IO_QUEUE_CONFIG::init(
                    wdf_kmdf_sys::WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual,
                );
                let queue = match IoQueue::create(globals.control_device.raw_handle(), queue_config)
                {
                    Ok(queue) => queue,
                    Err(err) => {
                        log::error!(
                            "WdfIoQueueCreate for read queue failed {:#x?}",
                            err.0.to_u32()
                        );
                        return Err(STATUS::UNSUCCESSFUL.into());
                    }
                };

                queue
            };

            // Register a notification for when a new request gets enqueued while the queue is idle
            let status = unsafe {
                Error::to_err(wdf_kmdf::raw::WdfIoQueueReadyNotify(
                    read_queue.raw_handle(),
                    Some(crate::recv::ndisprot_evt_notify_read_queue),
                    Some(open_context.raw_handle()),
                ))
            };
            if let Err(err) = status {
                log::error!(
                    "WdfIoQueueCreate for read queue failed {:#x?}",
                    err.0.to_u32()
                );
                return Err(STATUS::UNSUCCESSFUL.into());
            }

            // Make the queue to hold the status indication requests
            let status_indication_queue = {
                let queue_config = WDF_IO_QUEUE_CONFIG::init(
                    wdf_kmdf_sys::WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual,
                );
                let queue = match IoQueue::create(globals.control_device.raw_handle(), queue_config)
                {
                    Ok(queue) => queue,
                    Err(err) => {
                        log::error!(
                            "WdfIoQueueCreate for ioctl queue failed {:#x?}",
                            err.0.to_u32()
                        );
                        return Err(STATUS::UNSUCCESSFUL.into());
                    }
                };

                queue
            };

            #[allow(unused_assignments)]
            let mut state = OpenState::Initializing;

            // Reference code bumps the ref count since ownership transfers to ndisprot_create_bindings
            // and therefore cleans it up on failure

            // region: ndisprot_create_binding

            // inlined version of `ndisprot_create_binding`
            //
            // Creates an ndis binding to the indicated device, if a binding doesn't exist yet.
            // Also creates the additional resources?
            //
            // ## Note
            //
            // Blocks & completes synchronously

            let mut flags = OpenContextFlags::empty();
            flags.remove(OpenContextFlags::BIND_FLAGS);
            flags.set(OpenContextFlags::BIND_OPENING, true);

            // soooo...
            // yeah we're opening
            // but also we don't have the context actually setup yet and we're not added to the open context list

            // Copy device name
            let device_name = match try_from_unicode_str(adapter_name) {
                Ok(it) => it,
                Err(err) => {
                    log::error!(
                        "create_binding: failed to alloc device name string ({:#x?}, {} bytes)",
                        err.0.to_u32(),
                        adapter_name.capacity()
                    );
                    return Err(STATUS::INSUFFICIENT_RESOURCES.into());
                }
            };

            let mut pool_parameters: NET_BUFFER_LIST_POOL_PARAMETERS =
                unsafe { core::mem::zeroed() };
            pool_parameters.Header.Type = NDIS_OBJECT_TYPE_DEFAULT as u8;
            pool_parameters.Header.Revision = NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1 as u8;
            pool_parameters.Header.Size = NDIS_SIZEOF_NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1;

            pool_parameters.ProtocolId = NDIS_PROTOCOL_ID_IPX as u8;
            pool_parameters.ContextSize =
                core::mem::size_of::<crate::send::NprotSendNblRsvd>() as u16;
            pool_parameters.fAllocateNetBuffer = windows_kernel_sys::TRUE as u8;
            pool_parameters.PoolTag = NPROT_ALLOC_TAG;

            let send_nbl_pool = {
                let nbl_pool = NdisAllocateNetBufferListPool(
                    globals.ndis_protocol_handle.0.load(),
                    &mut pool_parameters,
                );
                if nbl_pool.is_null() {
                    log::warn!("create_binding: failed to alloc send nbl pool");
                    return Err(STATUS::INSUFFICIENT_RESOURCES.into());
                }
                scopeguard::guard(nbl_pool, |nbl_pool| {
                    if !nbl_pool.is_null() {
                        NdisFreeNetBufferListPool(nbl_pool);
                    }
                })
            };

            // recv nbls do not by default have a context
            pool_parameters.ContextSize = 0;

            let recv_nbl_pool = {
                let nbl_pool = NdisAllocateNetBufferListPool(
                    globals.ndis_protocol_handle.0.load(),
                    &mut pool_parameters,
                );
                if nbl_pool.is_null() {
                    log::warn!("create_binding: failed to alloc recv nbl pool");
                    return Err(STATUS::INSUFFICIENT_RESOURCES.into());
                }
                scopeguard::guard(nbl_pool, |nbl_pool| {
                    if !nbl_pool.is_null() {
                        NdisFreeNetBufferListPool(nbl_pool);
                    }
                })
            };

            // assume that the device is powered up
            let power_state =
                AtomicCell::new(windows_kernel_sys::NET_DEVICE_POWER_STATE::NetDeviceStateD0);

            let mut medium_array = [NDIS_MEDIUM::NdisMedium802_3];
            let mut frame_type_array = [NDIS_ETH_TYPE_802_1X as u16, NDIS_ETH_TYPE_802_1Q as u16];

            // Note: need to leak the inner box because we want to transfer ownership to NDIS
            let protocol_binding_context =
                Box::leak(unsafe { Pin::into_inner_unchecked(protocol_binding_context) })
                    as *mut ProtocolBindingContext;

            // Open the adapter
            let mut open_parameters: NDIS_OPEN_PARAMETERS = unsafe { core::mem::zeroed() };
            open_parameters.Header.Revision = NDIS_OPEN_PARAMETERS_REVISION_1 as u8;
            open_parameters.Header.Size = NDIS_SIZEOF_NDIS_OPEN_PARAMETERS_REVISION_1;
            open_parameters.Header.Type = NDIS_OBJECT_TYPE_OPEN_PARAMETERS as u8;

            open_parameters.AdapterName = BindParameters.AdapterName;
            open_parameters.MediumArray = medium_array.as_mut_ptr();
            open_parameters.MediumArraySize = medium_array.len() as u32;
            // hmmm, if this were to be in an async context, this would have to be saved
            open_parameters.SelectedMediumIndex =
                core::ptr::addr_of_mut!((*protocol_binding_context).selected_medium_index);
            open_parameters.FrameTypeArray = frame_type_array.as_mut_ptr();
            open_parameters.FrameTypeArraySize = frame_type_array.len() as u32;

            // Note:
            // `NDIS_DECLARE_PROTOCOL_OPEN_CONTEXT(NDISPROT_OPEN_CONTEXT)`
            // is probably just some SAL stuff

            let mut status = unsafe {
                Error::to_err(NdisOpenAdapterEx(
                    globals.ndis_protocol_handle.0.load(),
                    protocol_binding_context.cast(),
                    &mut open_parameters,
                    BindContext,
                    core::ptr::addr_of_mut!((*protocol_binding_context).binding_handle),
                ))
            };

            if let Err(Error(STATUS::PENDING)) = status {
                // ???: Wait should probably have a memory fence
                let _ = {
                    unsafe { &*protocol_binding_context }
                        .bind_event
                        .wait(Timeout::forever())
                };
                status = Error::to_err(
                    unsafe { &*protocol_binding_context }
                        .bind_status
                        .load(Ordering::Relaxed)
                        .into(),
                );
            }

            if let Err(err) = status {
                log::warn!(
                    "create_binding: NdisOpenAdapterEx ({}) failed {:#x?}",
                    device_name,
                    err.0.to_u32()
                );

                // Take back ownership of the context to drop it
                drop(unsafe { Box::from_raw(protocol_binding_context) });

                return Err(err);
            }

            state = OpenState::Paused;

            // Open has been completed successfully

            //  Get the display name for the adapter (it's okay if this fails)
            let mut device_desc = ManuallyDrop::new(NtUnicodeString::new());
            let _ = unsafe {
                NdisQueryAdapterInstanceName(
                    core::ptr::addr_of_mut!(device_desc).cast(),
                    { &*protocol_binding_context }.binding_handle,
                )
            };
            let device_desc = scopeguard::guard(device_desc, |device_desc| {
                // This memory was allocated by NDIS
                let device_desc = core::mem::transmute::<
                    ManuallyDrop<NtUnicodeString>,
                    windows_kernel_sys::UNICODE_STRING,
                >(device_desc);

                NdisFreeMemory(device_desc.Buffer.cast(), 0, 0);
            });

            let mut current_address = MACAddr([0; 6]);
            current_address
                .0
                .copy_from_slice(&BindParameters.CurrentMacAddress[0..6]);

            let mac_options = BindParameters.MacOptions;
            let max_frame_size = BindParameters.MtuSize;
            let media_state = BindParameters.MediaConnectState;

            // Note: original code uses the wrong enumeration constant (`NdisMediaStateConnected`)
            if media_state
                == windows_kernel_sys::NET_IF_MEDIA_CONNECT_STATE::MediaConnectStateConnected
            {
                flags.set(OpenContextFlags::MEDIA_CONNECTED, true);
            } else {
                flags.set(OpenContextFlags::MEDIA_CONNECTED, false);
            }

            let data_backfill_size = BindParameters.DataBackFillSize;
            let context_backfill_size = BindParameters.ContextBackFillSize;

            // Mark this open.
            // Also check if we received an Unbind while we were setting this up

            flags.remove(OpenContextFlags::BIND_FLAGS);
            flags.insert(OpenContextFlags::BIND_ACTIVE);

            assert!(
                !unsafe { &*protocol_binding_context }
                    .unbind_received
                    .load(Ordering::Relaxed),
                "shouldn't be unbinding while opening"
            );

            // endregion: ndisprot_create_binding

            Ok(pinned_init::try_pin_init! {
                OpenContext {
                    oc_sig: 0x030EE030,
                    driver: driver.clone_ref(),

                    inner <- SpinPinMutex::new(OpenContextInner {
                        flags,
                        state,
                        file_object: None,
                    }),

                    binding_handle: BindingHandle(AtomicCell::new(unsafe { (*protocol_binding_context).binding_handle })),

                    device_name: SyncWrapper(device_name),
                    device_desc: SyncWrapper(ScopeGuard::into_inner(device_desc)),

                    bind_event <- KeEvent::new(crate::EventType::Notification, false),
                    bind_status: 0,

                    // start off by assuming the device is powered up
                    powered_up_event <- KeEvent::new(crate::EventType::Notification, true),
                    power_state,

                    send_nbl_pool: NblPool(ScopeGuard::into_inner(send_nbl_pool)),
                    pended_send_count: AtomicU32::new(0),

                    recv_nbl_pool: NblPool(ScopeGuard::into_inner(recv_nbl_pool)),
                    read_queue,
                    pended_read_count: AtomicU32::new(0),
                    recv_queue <- SpinPinMutex::new(RecvQueue::new()),

                    status_indication_queue,

                    current_address,
                    multicast_address: [{MACAddr::zero()}; MAX_MULTICAST_ADDRESS],
                    mac_options: AtomicCell::new(mac_options),
                    max_frame_size: AtomicCell::new(max_frame_size),
                    data_backfill_size,
                    context_backfill_size,
                }? Error
            })
        },
    );

    match status {
        Ok(it) => {
            let mut open_list = globals.open_list.lock();

            if let Some(awa) = open_list.iter_mut().find(|obj| obj.is_none()) {
                *awa = Some(it)
            } else {
                // Should've always had a space from earlier
                log::error!("putting open context into list failed",);
                return STATUS::INSUFFICIENT_RESOURCES.to_u32();
            }

            STATUS::SUCCESS.to_u32()
        }
        Err(err) => {
            // Note: here's where we'd do the `ndisprotShutdownBinding` bit which is relevant to not fully completing initialization

            // Pretty much the only thing that's relevant is `ndisprotFreeBindResources`,
            // since we only add the open context to the global list above, and since we can only fail
            // before the binding is marked active, `ndisprotShutdownBinding` doesn't do anything extra
            err.0.to_u32()
        }
    }
}

/// Completion callback called by NDIS if `NdisOpenAdapterEX` pended.
/// Wakes up the thread creating the binding.
pub(crate) unsafe extern "C" fn open_adapter_complete(
    ProtocolBindingContext: NDIS_HANDLE,
    Status: NDIS_STATUS,
) {
    // The thread that's waiting on this won't see the changes until we signal the event
    let protocol_binding_context =
        unsafe { &*ProtocolBindingContext.cast::<ProtocolBindingContext>() };

    // ughhhhhhhhhhhhhhh
    // yay for not knowing exclusivity
    // kinda pushes for preferring async
    protocol_binding_context
        .bind_status
        .store(Status, Ordering::Relaxed);
    protocol_binding_context.bind_event.signal();
}

/// Called when NDIS wants to close the binding to an adapter
pub(crate) unsafe extern "C" fn unbind_adapter(
    UnbindContext: NDIS_HANDLE,
    ProtocolBindingContext: NDIS_HANDLE,
) -> NDIS_STATUS {
    log::debug!(
        "unbind_adapter: {:#x?} {:#x?}",
        UnbindContext,
        ProtocolBindingContext
    );

    let protocol_binding_context = ProtocolBindingContext.cast::<ProtocolBindingContext>();
    {
        let protocol_binding_context = unsafe { &mut *protocol_binding_context };

        protocol_binding_context
            .unbind_received
            .store(true, Ordering::Relaxed);

        // note: while this will die if the context isn't initialized yet, there's an assert in `bind_adapter`
        // which disallows unbinding an in-progress bind
        let open_context = protocol_binding_context.open_context.get_context();

        // In case threads were waiting for the device to be powered up, wake them
        open_context.powered_up_event.signal();

        open_context.inner.lock().state = OpenState::Closing;

        shutdown_binding(protocol_binding_context);
    }

    // deallocate protocol_binding_context (WDF takes care of dropping `OpenContext`)
    drop(Box::from_raw(protocol_binding_context));

    STATUS::SUCCESS.to_u32()
}

/// Completion callback called by NDIS if `NdisCloseAdapter` pended.
/// Wakes up the thread that is closing the adapter.
pub(crate) unsafe extern "C" fn close_adapter_complete(ProtocolBindingContext: NDIS_HANDLE) {
    let protocol_binding_context =
        unsafe { &*ProtocolBindingContext.cast::<ProtocolBindingContext>() };

    protocol_binding_context.bind_event.signal();
}

fn shutdown_binding(protocol_binding_context: &mut ProtocolBindingContext) {
    let open_object = &protocol_binding_context.open_context;
    let open_context = open_object.get_context();

    let mut do_close_binding = false;

    {
        let mut inner = open_context.inner.lock();

        if inner.flags.intersects(OpenContextFlags::BIND_OPENING) {
            // still in the process of setting up this binding
            return;
        }

        if inner.flags.intersects(OpenContextFlags::BIND_ACTIVE) {
            inner.flags.remove(OpenContextFlags::BIND_FLAGS);
            inner.flags.insert(OpenContextFlags::BIND_CLOSING);

            do_close_binding = true;
        }
    }

    if do_close_binding {
        let mut bytes_read = 0u32;

        // Set packet filter to 0 before closing the binding
        let mut packet_filter = 0u32;
        let status = do_request(
            &*open_context,
            NDIS_DEFAULT_PORT_NUMBER,
            NDIS_REQUEST_TYPE::NdisRequestSetInformation,
            OID_GEN_CURRENT_PACKET_FILTER,
            core::ptr::addr_of_mut!(packet_filter).cast(),
            core::mem::size_of_val(&packet_filter) as ULONG,
            &mut bytes_read,
        );
        if let Err(err) = status {
            log::warn!(
                "shutdown_binding: set packet filter failed: {:#x?}",
                err.0.to_u32()
            );
        }

        // clear multicast list before closing the binding
        let status = do_request(
            &*open_context,
            NDIS_DEFAULT_PORT_NUMBER,
            NDIS_REQUEST_TYPE::NdisRequestSetInformation,
            OID_802_3_MULTICAST_LIST,
            core::ptr::null_mut(),
            0,
            &mut bytes_read,
        );
        if let Err(err) = status {
            log::warn!(
                "shutdown_binding: set multicast list failed: {:#x?}",
                err.0.to_u32()
            );
        }

        // cancel pending reads and status indications
        // note: this appears to be done in place of `wait_for_pending_io` since we just
        // drop all of the requests and wait for them to complete.
        open_context.read_queue.purge_synchronously();
        open_context.status_indication_queue.purge_synchronously();

        // discard any queued recieves
        recv::flush_receive_queue(&open_object);

        // close the binding now
        log::info!(
            "shutdown_binding: closing open_context {}, binding handle {:#x?}",
            open_context.device_name.0,
            protocol_binding_context.binding_handle
        );

        let _ = unsafe {
            KeEvent::new(EventType::Notification, false)
                .__pinned_init(core::ptr::addr_of_mut!(protocol_binding_context.bind_event))
        };

        let status =
            unsafe { Error::to_err(NdisCloseAdapterEx(protocol_binding_context.binding_handle)) };

        if let Err(Error(STATUS::PENDING)) = status {
            let _ = protocol_binding_context.bind_event.wait(Timeout::forever());
            // note: closing completion does not have a status
        }

        assert!(status == Ok(()) || status == Err(Error(STATUS::PENDING)));

        open_context.binding_handle.0.store(core::ptr::null_mut());

        let mut inner = open_context.inner.lock();
        inner.flags.remove(OpenContextFlags::BIND_FLAGS);
        inner.flags.insert(OpenContextFlags::BIND_IDLE);
        inner.flags.remove(OpenContextFlags::UNBIND_FLAGS);
    }

    // remove from the global list
    open_context
        .driver
        .get_context()
        .open_list
        .lock()
        .retain(|e| e.as_deref() != Some(&protocol_binding_context.open_context));
}

pub(crate) unsafe extern "C" fn pnp_event_handler(
    ProtocolBindingContext: NDIS_HANDLE,
    NetPnPEventNotification: PNET_PNP_EVENT_NOTIFICATION,
) -> NDIS_STATUS {
    let event_notif = unsafe { &*NetPnPEventNotification };
    // Note: `ProtocolBindingContext` can be `NULL`, which means that it applies to all bindings,
    // which is true for
    // - NetEventBindList
    // - NetEventBindsComplete
    // - NetEventReconfigure (sometimes)
    let protocol_binding_context = ProtocolBindingContext
        .cast::<ProtocolBindingContext>()
        .as_ref();

    let mut status = STATUS::UNSUCCESSFUL;

    match event_notif.NetPnPEvent.NetEvent {
        NET_PNP_EVENT_CODE::NetEventSetPower => {
            if let Some(binding_ctx) = protocol_binding_context {
                let open_object = &binding_ctx.open_context;
                let open_context = open_object.get_context();

                let power_state = unsafe {
                    event_notif
                        .NetPnPEvent
                        .Buffer
                        .cast::<NET_DEVICE_POWER_STATE>()
                        .read()
                };
                open_context.power_state.store(power_state);

                if open_context.power_state.load().0 > NET_DEVICE_POWER_STATE::NetDeviceStateD0.0 {
                    // ???: So we write to the open context, but the object context area holding open context is supposed
                    // to only have thread-safe references. How do we get mut access to clear the waiters but also retain
                    // shared access so as to not block the mut access?
                    //
                    // Seems like we can signal the event after setting the power state?

                    // The device below is transitioning into a low power state.
                    // We wake-up any thread attempting to query the device so that they can see the new power state.
                    open_context.powered_up_event.signal();

                    // Wait for any IO in-progress to complete
                    //
                    // Note: NDIS 6.30+ protocol drivers must not wait for the
                    // completion of any IO requests in `NetEventSetPower`.
                    // We already wait for any pending IO requests in
                    // `NetEventPause` like in the non-KMDF ndisprot driver
                    // example, so we simply just comment out this call.
                    //
                    // See the `NetEventSetPower` section in
                    // https://learn.microsoft.com/en-us/windows-hardware/drivers/network/handling-pnp-events-and-power-management-events-in-a-protocol-driver
                    // for more info.
                    // wait_for_pending_io(&open_context, false);

                    // Return any receives that we have queued up
                    recv::flush_receive_queue(open_object);

                    log::info!(
                        "pnp_event: SetPower to {}",
                        open_context.power_state.load().0
                    );
                } else {
                    // The device below is powered up
                    log::info!("pnp_event: SetPower to ON");

                    open_context.powered_up_event.signal();
                }

                status = STATUS::SUCCESS;
            }
        }
        NET_PNP_EVENT_CODE::NetEventQueryPower => {
            status = STATUS::SUCCESS;
        }
        NET_PNP_EVENT_CODE::NetEventBindsComplete => {
            // manifest a driver handle, since for binds complete we don't get a specific context
            if let Some(driver) = unsafe { wdf_kmdf::raw::WdfGetDriver() } {
                let driver = unsafe { Driver::<crate::NdisProt>::wrap(driver) };

                let globals = driver.get_context();
                globals.binds_complete.signal();

                // Note: we do not register an ExCallback right now, since we're only focused on getting NdisProt up

                status = STATUS::SUCCESS;
            } else {
                // Driver not initialized yet
                status = STATUS::UNSUCCESSFUL;
            };
        }
        NET_PNP_EVENT_CODE::NetEventPause => {
            if let Some(binding_ctx) = protocol_binding_context {
                let open_object = &binding_ctx.open_context;
                let open_context = open_object.get_context();

                // Wait for all sends to complete
                let mut open_context_inner = open_context.inner.lock();
                open_context_inner.state = OpenState::Pausing;

                // Note: could also complete the PnP event asynchronously (would need NdisCompleteNetPnPEvent)
                loop {
                    let pended_send_count = open_context.pended_send_count.load(Ordering::Relaxed);

                    if pended_send_count == 0 {
                        break;
                    }

                    drop(open_context_inner);
                    {
                        log::info!("pnp_event: outstanding send count is {}", pended_send_count);

                        // Sleep for 1 second!
                        pinned_init::stack_pin_init!(let sleep_event = KeEvent::new(EventType::Notification, false));
                        let _ = sleep_event.wait(Timeout::relative_ms(1_000));
                    }
                    open_context_inner = open_context.inner.lock();
                }
                drop(open_context_inner);

                // Return all queued receives
                recv::flush_receive_queue(&open_object);

                open_context.inner.lock().state = OpenState::Paused;

                status = STATUS::SUCCESS;
            }
        }
        NET_PNP_EVENT_CODE::NetEventRestart => {
            if let Some(binding_ctx) = protocol_binding_context {
                let open_context = binding_ctx.open_context.get_context();

                debug_assert_eq!(
                    open_context.inner.lock().state,
                    OpenState::Paused,
                    "trying to restart binding while adapter isn't paused"
                );

                // Get the updated attributes, if there are any
                let updated_attributes = 'updated_attributes: {
                    let buffer = event_notif.NetPnPEvent.Buffer;
                    if buffer.is_null() {
                        break 'updated_attributes None;
                    }

                    let buffer_length = event_notif.NetPnPEvent.BufferLength;
                    assert_eq!(
                        buffer_length as usize,
                        core::mem::size_of::<ProtocolRestartParameters>()
                    );

                    Some(unsafe { &*buffer.cast::<ProtocolRestartParameters>() })
                };

                if let Some(updated_attributes) = updated_attributes {
                    restart(&open_context, updated_attributes);

                    open_context.inner.lock().state = OpenState::Running;
                    status = STATUS::SUCCESS;
                }
            }
        }
        NET_PNP_EVENT_CODE::NetEventQueryRemoveDevice
        | NET_PNP_EVENT_CODE::NetEventCancelRemoveDevice
        | NET_PNP_EVENT_CODE::NetEventReconfigure
        | NET_PNP_EVENT_CODE::NetEventBindList
        | NET_PNP_EVENT_CODE::NetEventPnPCapabilities => {
            status = STATUS::SUCCESS;
        }
        _ => {
            // Translation note: Would return NDIS_STATUS_NOT_SUPPORTED, but
            // NDIS 6.x drivers aren't supposed to return that
            status = STATUS::UNSUCCESSFUL;
        }
    }

    log::debug!(
        "pnp_event_handler: Open {:x?}, Event Ptr {:#x?}, Event {:x?}, Status {:x}",
        protocol_binding_context,
        NetPnPEventNotification,
        event_notif.NetPnPEvent.NetEvent.0,
        status.to_u32()
    );

    status.to_u32()
}

pub(crate) fn unload_protocol(context: Pin<&NdisProt>) {
    let proto_handle = context.ndis_protocol_handle.0.swap(core::ptr::null_mut());

    if !proto_handle.is_null() {
        unsafe { NdisDeregisterProtocolDriver(proto_handle) };
    }
}

fn do_request(
    // protocol_binding_context: &ProtocolBindingContext,
    open_context: &OpenContext,
    port_number: NDIS_PORT_NUMBER,
    request_type: NDIS_REQUEST_TYPE,
    oid: NDIS_OID,
    information_buffer: PVOID,
    information_buffer_length: ULONG,
    bytes_processed: &mut ULONG,
) -> Result<(), Error> {
    // FIXME: required because OID requests via WDFREQUESTs go via
    // `OpenContext`, not `ProtocolBindingContext`
    let binding_handle = open_context.binding_handle.0.load();

    let mut request = unsafe { core::mem::zeroed::<NDIS_OID_REQUEST>() };
    request.Header.Type = NDIS_OBJECT_TYPE_OID_REQUEST as u8;
    request.Header.Revision = NDIS_OID_REQUEST_REVISION_1 as u8;
    request.Header.Size = NDIS_SIZEOF_OID_REQUEST_REVISION_1;
    request.RequestType = request_type;
    request.PortNumber = port_number;

    match request_type {
        NDIS_REQUEST_TYPE::NdisRequestQueryInformation => {
            request.DATA.QUERY_INFORMATION.Oid = oid;
            request.DATA.QUERY_INFORMATION.InformationBuffer = information_buffer;
            request.DATA.QUERY_INFORMATION.InformationBufferLength = information_buffer_length;
        }
        NDIS_REQUEST_TYPE::NdisRequestSetInformation => {
            request.DATA.SET_INFORMATION.Oid = oid;
            request.DATA.SET_INFORMATION.InformationBuffer = information_buffer;
            request.DATA.SET_INFORMATION.InformationBufferLength = information_buffer_length;
        }
        _ => unreachable!(),
    }

    request.RequestId = open_context.driver.get_context().cancel_id_gen.next() as PVOID;

    pinned_init::stack_pin_init!(let req_context = pinned_init::pin_init!(Request {
        Request: request,
        ReqEvent <- KeEvent::new(EventType::Notification, false),
        Status: 0x0,
    }));

    let mut status = unsafe {
        Error::to_err(NdisOidRequest(
            binding_handle,
            core::ptr::addr_of_mut!(req_context.as_mut().Request),
        ))
    };

    if let Err(Error(STATUS::PENDING)) = status {
        let _ = req_context.as_ref().ReqEvent.wait(Timeout::forever());
        status = Error::to_err(req_context.as_ref().Status);
    }

    if status.is_ok() {
        *bytes_processed = match request_type {
            NDIS_REQUEST_TYPE::NdisRequestQueryInformation => unsafe {
                req_context
                    .as_ref()
                    .Request
                    .DATA
                    .QUERY_INFORMATION
                    .BytesWritten
            },
            NDIS_REQUEST_TYPE::NdisRequestSetInformation => unsafe {
                req_context.as_ref().Request.DATA.SET_INFORMATION.BytesRead
            },
            _ => unreachable!(),
        };

        // The driver below should set the correct value to `BytesWritten` or `BytesRead`,
        // but for now just trucate the value to the info buffer length.
        *bytes_processed = (*bytes_processed).max(information_buffer_length);
    }

    status
}

/// Pre-validates and holds a reference to an open context before calling `do_request`.
/// This also makes sure we have a valid binding.
pub(crate) fn validate_open_and_do_request(
    open_context: &OpenContext,
    request_type: NDIS_REQUEST_TYPE,
    oid: NDIS_OID,
    info_buffer: PVOID,
    info_len: ULONG,
    bytes_processed: &mut u32,
    wait_for_power_on: bool,
) -> NDIS_STATUS {
    // ???: How do we bridge the gap from having a file open context to getting
    // the protocol binding context with a handle in it?
    //
    // if we want to maintain a divide between the wdf side and the ndis
    // side while also permitting non-blocking async (i.e. not needing to use
    // KeWaitForSingleObject on the current thread), there needs to be a way of
    // passing messages between the two sides.
    //
    // for now: just use the binding_handle we got from the protocol binding
    // context, we'll deal with WDF -> Binding communication later
    let status;

    'out: {
        let inner = open_context.inner.lock();

        // Proceed only if this is bound
        if !inner.flags.contains(OpenContextFlags::BIND_ACTIVE) {
            status = STATUS::INVALID_PARAMETER;
            break 'out;
        }

        assert!(!open_context.binding_handle.0.load().is_null());

        // Make sure the binding doesn't go away until we're finished with the request
        open_context
            .pended_send_count
            .fetch_add(1, Ordering::Relaxed);

        drop(inner);

        if wait_for_power_on {
            // Wait for the device below to be powered up.
            //
            // We don't wait forever here to avoid a `PROCESS_HAS_LOCKED_PAGES` bugcheck
            // in the event of the calling process terminating and this request not completing
            // within a reasonable amount of time.
            //
            // An alternative would be to explicitly handle cancellation of this request.
            //
            // ???: Is this still true with WDFREQUESTs?
            let _ = open_context
                .powered_up_event
                .wait(Timeout::relative_ms(4_500));
        }

        if open_context.power_state.load() == NET_DEVICE_POWER_STATE::NetDeviceStateD0 {
            let err = do_request(
                open_context,
                NDIS_DEFAULT_PORT_NUMBER,
                request_type,
                oid,
                info_buffer,
                info_len,
                bytes_processed,
            );

            if let Err(err) = err {
                status = err.0;
            } else {
                status = STATUS::SUCCESS;
            }
        } else {
            status = windows_kernel_sys::result::ERROR::NDIS::ADAPTER_NOT_READY
                .to_u32()
                .into();
        }

        // let go of the binding
        let _pended_send_count = open_context
            .pended_send_count
            .fetch_sub(1, Ordering::Relaxed);
    }

    log::debug!(
        "validate_open_and_do_request: Open {:x?}, OID {:x?}, Status {:x?}",
        open_context.inner.lock().flags,
        oid,
        status
    );

    status.to_u32()
}

/// Entry point indicating completion of a pended NDIS_REQUEST
pub(crate) unsafe extern "C" fn request_complete(
    _ProtocolBindingContext: NDIS_HANDLE,
    OidRequest: PNDIS_OID_REQUEST,
    Status: NDIS_STATUS,
) {
    // yayyyy container_of moment
    let mut req_context = unsafe {
        let containing_record = OidRequest
            .cast::<u8>()
            .sub(core::mem::offset_of!(Request, Request))
            .cast::<Request>();
        Pin::new_unchecked(&mut *containing_record)
    };

    // Stash the completion status
    req_context.Status = Status;

    // Wake up the thread blocked on this request being completed
    req_context.as_ref().ReqEvent.signal();
}

/// Processes the request based on the input arguments and completes it.
/// If the request was cancelled we'll let the cancel routine do the request completion (?)
fn service_indicate_status_irp(
    open_context: &OpenContext,
    general_status: NDIS_STATUS,
    status_buffer: PVOID,
    status_buffer_size: UINT,
) {
    log::debug!("--> ndisbind::service_indicate_status_irp");

    // Translation Note: original code acquires the open context lock, but doesn't
    // access anything fields that needs to be protected by the lock.
    // let mut inner = open_context.inner.lock();

    let mut nt_status;
    let mut bytes = 0;

    let processed_request = 'out: {
        // Get the first pended read request
        let request = match open_context.status_indication_queue.retrive_next_request() {
            Some(Ok(request)) => request,
            Some(Err(err)) => {
                assert!(
                    false,
                    "WdfIoQueeuRetrieveNextRequest failed unexpectedly {:x?}",
                    err.0
                );
                break 'out None;
            }
            None => break 'out None,
        };

        let mut indicate_status = core::ptr::null_mut();
        let mut out_buf_len = 0;

        nt_status = unsafe {
            wdf_kmdf::raw::WdfRequestRetrieveOutputBuffer(
                request.raw_handle(),
                core::mem::size_of::<IndicateStatus>(),
                &mut indicate_status,
                Some(&mut out_buf_len),
            )
        };
        if let Err(err) = Error::to_err(nt_status) {
            log::error!("WdfRequestRetrieveOutputBuffer failed {:x?}", err);
            break 'out Some((request, nt_status, bytes));
        }

        // Check if the buffer is large enough to accomodate the status buffer data.
        if out_buf_len.saturating_sub(core::mem::size_of::<IndicateStatus>())
            >= status_buffer_size as usize
        {
            let indicate_status = indicate_status.cast::<IndicateStatus>();

            unsafe {
                (*indicate_status).indicated_status = general_status;
                (*indicate_status).status_buffer_length = status_buffer_size;
                (*indicate_status).status_buffer_offset =
                    core::mem::size_of::<IndicateStatus>() as u32;
            }

            let data = unsafe {
                indicate_status
                    .cast::<u8>()
                    .add(core::mem::size_of::<IndicateStatus>())
            };

            unsafe {
                data.copy_from_nonoverlapping(status_buffer.cast(), status_buffer_size as usize)
            };

            nt_status = STATUS::SUCCESS.to_u32();
        } else {
            nt_status = STATUS::BUFFER_OVERFLOW.to_u32();
        }

        // number of bytes copied or number of bytes required.
        bytes = core::mem::size_of::<IndicateStatus>().saturating_add(status_buffer_size as usize)
            as u64;

        break 'out Some((request, nt_status, bytes));
    };

    if let Some((request, nt_status, bytes)) = processed_request {
        unsafe {
            wdf_kmdf::raw::WdfRequestCompleteWithInformation(request.raw_handle(), nt_status, bytes)
        };
    }

    log::debug!("<-- ndisbind::service_indicate_status_irp");
}

/// Protocol entry point called by NDIS to indicate a change in status at the miniport.
///
/// We make not of reset and media connect status indications.
pub(crate) unsafe extern "C" fn status_handler(
    ProtocolBindingContext: NDIS_HANDLE,
    StatusIndication: PNDIS_STATUS_INDICATION,
) {
    let protocol_binding_context =
        unsafe { &*ProtocolBindingContext.cast::<ProtocolBindingContext>() };
    let status_indication = unsafe { &*StatusIndication };

    let open = &protocol_binding_context.open_context;
    let open_context = open.get_context();

    if status_indication.Header.Type != NDIS_OBJECT_TYPE_STATUS_INDICATION as u8
        || status_indication.Header.Size != NDIS_SIZEOF_STATUS_INDICATION_REVISION_1
    {
        log::info!(
            "status: received an invalid status indication: {open:x?}, status indication {:x?}",
            core::ptr::addr_of!(*status_indication)
        );
    }

    let general_status = status_indication.StatusCode;

    log::info!("status: open {open:x?}, status {general_status:x?}");

    service_indicate_status_irp(
        &open_context,
        general_status,
        status_indication.StatusBuffer,
        status_indication.StatusBufferSize,
    );

    {
        let mut inner = open_context.inner.lock();

        if open_context.power_state.load() != NET_DEVICE_POWER_STATE::NetDeviceStateD0 {
            // The device is in a lower power state
            // We still continue to acknowledge status indications

            // NOTE: any actions we take based on these status indications
            // should take into accout the current device power state
        }

        const NDIS_STATUS_RESET_START: u32 = 0x40010004;
        const NDIS_STATUS_RESET_END: u32 = 0x40010005;
        const NDIS_STATUS_LINK_STATE: u32 = 0x40010017;

        match general_status {
            NDIS_STATUS_RESET_START => {
                assert!(!inner.flags.contains(OpenContextFlags::RESET_IN_PROGRESS));
                inner.flags.remove(OpenContextFlags::RESET_FLAGS);
                inner.flags.set(OpenContextFlags::RESET_IN_PROGRESS, true);
            }
            NDIS_STATUS_RESET_END => {
                assert!(inner.flags.contains(OpenContextFlags::RESET_IN_PROGRESS));
                inner.flags.remove(OpenContextFlags::RESET_FLAGS);
                inner.flags.set(OpenContextFlags::NOT_RESETTING, true);
            }
            NDIS_STATUS_LINK_STATE => {
                assert!(
                    status_indication.StatusBufferSize >= NDIS_SIZEOF_LINK_STATE_REVISION_1.into()
                );

                let link_state =
                    unsafe { &*status_indication.StatusBuffer.cast::<NDIS_LINK_STATE>() };

                if link_state.MediaConnectState
                    == NET_IF_MEDIA_CONNECT_STATE::MediaConnectStateConnected
                {
                    inner.flags.remove(OpenContextFlags::MEDIA_FLAGS);
                    inner.flags.set(OpenContextFlags::MEDIA_CONNECTED, true);
                } else {
                    inner.flags.remove(OpenContextFlags::MEDIA_FLAGS);
                    inner.flags.set(OpenContextFlags::MEDIA_DISCONNECTED, true);
                }
            }
            _ => {}
        }
    }
}

/// Returns information about the specified binding
pub(crate) fn query_binding(
    buffer: PVOID,
    input_length: usize,
    output_length: usize,
    bytes_returned: &mut usize,
) -> NDIS_STATUS {
    let Some(driver) = (unsafe { wdf_kmdf::raw::WdfGetDriver() }) else {
        // Driver not initialized yet
        return STATUS::UNSUCCESSFUL.to_u32();
    };
    let driver = unsafe { Driver::<NdisProt>::wrap(driver) };
    let globals = driver.get_context();

    let status = 'out: {
        if input_length < core::mem::size_of::<QueryBinding>() {
            break 'out STATUS::INSUFFICIENT_RESOURCES;
        }

        if output_length < core::mem::size_of::<QueryBinding>() {
            // need at least `QueryBinding` size bytes
            *bytes_returned = core::mem::size_of::<QueryBinding>();
            break 'out STATUS::BUFFER_TOO_SMALL;
        }

        let remaining = output_length - core::mem::size_of::<QueryBinding>();
        let binding_index = unsafe { *buffer.cast::<QueryBinding>() }.binding_index;

        let open_list = globals.open_list.lock();

        // get the `binding_index`th binding context
        let Some(open) = open_list
            .iter()
            .filter_map(|open| {
                // Skip placeholders
                let Some(open) = open.as_ref() else {
                    return None;
                };
                let open_context = open.get_context();

                // ... or if not bound.
                if !open_context
                    .inner
                    .lock()
                    .flags
                    .contains(OpenContextFlags::BIND_ACTIVE)
                {
                    return None;
                }

                Some(open)
            })
            .nth(binding_index as usize)
        else {
            break 'out STATUS::NO_MORE_ENTRIES;
        };

        let open_context = open.get_context();

        log::info!("query_binding: found open {open:x?}");
        // found the binding context we are looking for, copy device
        // name & description to the output buffer

        let name_len =
            (open_context.device_name.len() as usize).saturating_add(core::mem::size_of::<WCHAR>());
        let desc_len =
            (open_context.device_desc.len() as usize).saturating_add(core::mem::size_of::<WCHAR>());

        let name_offset = core::mem::size_of::<QueryBinding>();
        let desc_offset = name_offset.saturating_add(name_len);

        let required_size = name_len.saturating_add(desc_len);

        if remaining < required_size {
            // Indicate what size we'd need
            *bytes_returned = required_size;
            break 'out STATUS::BUFFER_TOO_SMALL;
        }

        let header = unsafe { &mut *buffer.cast::<u8>().add(0).cast::<QueryBinding>() };

        // Note: apparently `INVALID_BUFFER_SIZE` is the one we should
        // use if it's bigger than what we can handle?
        if let Ok(offset) = u32::try_from(name_offset) {
            header.device_name_offset = offset;
        } else {
            break 'out STATUS::INVALID_BUFFER_SIZE;
        }

        if let Ok(offset) = u32::try_from(desc_offset) {
            header.device_descr_offset = offset;
        } else {
            break 'out STATUS::INVALID_BUFFER_SIZE;
        }

        let name_bytes = unsafe { buffer.cast::<u8>().add(name_offset) };
        unsafe { name_bytes.write_bytes(0, name_len) };
        unsafe {
            name_bytes.copy_from_nonoverlapping(
                open_context.device_name.as_slice().as_ptr().cast(),
                open_context.device_name.len().into(),
            )
        };

        let desc_bytes = unsafe { buffer.cast::<u8>().add(desc_offset) };
        unsafe { desc_bytes.write_bytes(0, desc_len) };
        unsafe {
            desc_bytes.copy_from_nonoverlapping(
                open_context.device_desc.as_slice().as_ptr().cast(),
                open_context.device_desc.len().into(),
            );
        }

        header.device_name_length = open_context.device_name.len().into();
        header.device_descr_length = open_context.device_desc.len().into();

        *bytes_returned = required_size;

        STATUS::SUCCESS
    };

    status.to_u32()
}

/// Searches the global bindings list for an open context which has a binding to the specified adapter, and return a reference to it
pub(crate) fn ndisprot_lookup_device(
    globals: Pin<&NdisProt>,
    adapter_name: NtUnicodeStr<'_>,
) -> Option<Ref<GeneralObject<OpenContext>>> {
    globals
        .open_list
        .lock()
        .as_ref()
        .iter()
        .find_map(|open_obj| {
            let open_obj = &open_obj.as_ref()?;
            let open_context = open_obj.get_context();
            (open_context.device_name.0 == adapter_name).then(|| open_obj.clone_ref())
        })
}

/// Query an arbitrary OID value from the miniport
pub(crate) fn query_oid_value(
    open_context: &OpenContext,
    data_buffer: PVOID,
    buffer_length: usize,
    bytes_written: &mut usize,
) -> NDIS_STATUS {
    let status;
    let mut oid = 0;

    'out: {
        if buffer_length < core::mem::size_of::<QueryOid>() {
            status = STATUS::BUFFER_TOO_SMALL;
            break 'out;
        }

        let header = unsafe { *data_buffer.cast::<QueryOid>() };
        oid = header.oid;

        let data = unsafe {
            data_buffer
                .cast::<u8>()
                .add(core::mem::offset_of!(QueryOid, data))
        };
        let data_len = buffer_length.saturating_sub(core::mem::offset_of!(QueryOid, data));
        let Ok(data_len) = u32::try_from(data_len) else {
            status = STATUS::BUFFER_TOO_SMALL;
            break 'out;
        };

        {
            let inner = open_context.inner.lock();

            if !inner.flags.contains(OpenContextFlags::BIND_ACTIVE) {
                log::warn!(
                    "query_oid: open of {:x?}/{:x?} is in an invalid state",
                    open_context.device_name.0,
                    inner.flags
                );
                status = STATUS::UNSUCCESSFUL;
                break 'out;
            }

            // Make sure the binding doesn't go away
            open_context
                .pended_send_count
                .fetch_add(1, Ordering::Relaxed);
        }
        let mut bytes_processed = 0;

        if let Err(err) = do_request(
            open_context,
            header.port_number,
            NDIS_REQUEST_TYPE::NdisRequestQueryInformation,
            oid,
            data.cast(),
            data_len,
            &mut bytes_processed,
        ) {
            status = err.0
        } else {
            status = STATUS::SUCCESS;
        }

        *bytes_written = bytes_processed as usize;

        {
            // Let go of the binding
            let _pended_send_count = open_context
                .pended_send_count
                .fetch_sub(1, Ordering::Relaxed);
        }

        if status == STATUS::SUCCESS {
            *bytes_written = bytes_written.saturating_add(core::mem::offset_of!(QueryOid, data));
        }
    }

    log::debug!(
        "query_oid: open {:x?}, OID {oid:x?}, Status {status:x?}",
        open_context.inner.lock().flags,
    );

    status.to_u32()
}

pub(crate) fn set_oid_value(
    open_context: &OpenContext,
    data_buffer: PVOID,
    buffer_length: usize,
) -> NDIS_STATUS {
    let status;
    let mut oid = 0;

    'out: {
        if buffer_length < core::mem::size_of::<SetOid>() {
            status = STATUS::BUFFER_TOO_SMALL;
            break 'out;
        }

        let header = unsafe { *data_buffer.cast::<SetOid>() };
        oid = header.oid;

        // Check if this is settable by usermode apps
        if !valid_settable_oid(oid) {
            log::warn!("set_oid: oid {oid:x?} cannot be set");
            status = STATUS::INVALID_PARAMETER;
            break 'out;
        }

        let data = unsafe {
            data_buffer
                .cast::<u8>()
                .add(core::mem::offset_of!(SetOid, data))
        };
        let data_len = buffer_length.saturating_sub(core::mem::offset_of!(SetOid, data));
        let Ok(data_len) = u32::try_from(data_len) else {
            status = STATUS::BUFFER_TOO_SMALL;
            break 'out;
        };

        {
            let inner = open_context.inner.lock();

            if !inner.flags.contains(OpenContextFlags::BIND_ACTIVE) {
                log::warn!(
                    "set_oid: open of {:x?}/{:x?} is in an invalid state",
                    open_context.device_name.0,
                    inner.flags
                );
                status = STATUS::UNSUCCESSFUL;
                break 'out;
            }

            // Make sure the binding doesn't go away
            open_context
                .pended_send_count
                .fetch_add(1, Ordering::Relaxed);
        }

        let mut bytes_processed = 0;

        if let Err(err) = do_request(
            open_context,
            header.port_number,
            NDIS_REQUEST_TYPE::NdisRequestSetInformation,
            oid,
            data.cast(),
            data_len,
            &mut bytes_processed,
        ) {
            status = err.0
        } else {
            status = STATUS::SUCCESS;
        }

        {
            // Let go of the binding
            let _pended_send_count = open_context
                .pended_send_count
                .fetch_sub(1, Ordering::Relaxed);
        }
    }

    log::debug!(
        "set_oid: open {:x?}, OID {oid:x?}, Status {status:x?}",
        open_context.inner.lock().flags,
    );

    status.to_u32()
}

/// Validate whether the given set OID is settable or not.
fn valid_settable_oid(oid: NDIS_OID) -> bool {
    SUPPORTED_SET_OIDS.contains(&oid)
}

/// Handle restart attribute changes.
fn restart(open_context: &OpenContext, updated_attributes: &ProtocolRestartParameters) {
    // Check for filter stack changes
    for filter_module in updated_attributes.filter_module_names() {
        log::info!("Filter: {}", filter_module);
    }

    // Check for updated attributes
    if let Some(restart_attr) = updated_attributes
        .restart_attributes()
        .find(|attr| attr.Oid == OID_GEN_MINIPORT_RESTART_ATTRIBUTES)
    {
        let general_attributes = unsafe {
            &*restart_attr
                .data()
                .as_ptr()
                .cast::<NDIS_RESTART_GENERAL_ATTRIBUTES>()
        };

        // Pickup new attributes of interest

        open_context
            .mac_options
            .store(general_attributes.MacOptions);

        open_context
            .max_frame_size
            .store(general_attributes.MtuSize);
    }
}

/// Note: always adds a nul-terminator
fn try_from_unicode_str(unicode_str: NtUnicodeStr<'_>) -> Result<NtUnicodeString, Error> {
    use windows_kernel_rs::string::NtStringError;

    let mut string = NtUnicodeString::new();
    let capacity = unicode_str
        .capacity()
        .checked_add(core::mem::size_of::<u16>() as u16)
        .ok_or(STATUS::BUFFER_OVERFLOW)?;
    string.try_reserve(capacity).map_err(|err| match err {
        NtStringError::BufferSizeExceedsU16 => STATUS::BUFFER_OVERFLOW,
        _ => STATUS::UNSUCCESSFUL,
    })?;

    let raw_string = unsafe {
        core::ptr::addr_of_mut!(string)
            .cast::<windows_kernel_sys::UNICODE_STRING>()
            .read()
    };
    // note: nt-string doesn't deal with allocation failures (i.e. `alloc::alloc` or `alloc::realloc` returning null pointer),
    // so we introspect the original buffer pointer to see if it's null.
    if raw_string.Buffer.is_null() {
        return Err(STATUS::INSUFFICIENT_RESOURCES.into());
    }

    // push it all!
    if !unicode_str.is_empty() {
        string
            .try_push_u16(unicode_str.as_slice())
            .expect("should have reserved enough space, and is a valid nt unicode string");
    }

    Ok(string)
}

// Helpers for navigating the fields of NDIS_PROTOCOL_RESTART_PARAMETERS easier

#[repr(C)]
struct ProtocolRestartParameters {
    _Header: NDIS_OBJECT_HEADER,
    FilterModuleNameBuffer: PUCHAR,
    FilterModuleNameBufferLength: ULONG,
    RestartAttributes: NtSingleListHead<RestartAttributes, RestartAttributesList>,
    BoundIfIndex: NET_IFINDEX,
    BoundIfNetLuid: NET_LUID,
    _Flags: ULONG,
}

// Should be the same as NDIS_PROTOCOL_RESTART_PARAMETERS
static_assertions::assert_eq_align!(ProtocolRestartParameters, NDIS_PROTOCOL_RESTART_PARAMETERS);
static_assertions::assert_eq_size!(ProtocolRestartParameters, NDIS_PROTOCOL_RESTART_PARAMETERS);

impl ProtocolRestartParameters {
    fn filter_module_names(&self) -> impl Iterator<Item = NtUnicodeStr<'_>> {
        unsafe {
            FilterModuleNameIter::new(
                self.FilterModuleNameBuffer,
                self.FilterModuleNameBufferLength,
            )
        }
    }

    fn restart_attributes(&self) -> impl Iterator<Item = &RestartAttributes> {
        unsafe { self.RestartAttributes.iter() }
    }
}

struct FilterModuleNameIter<'a> {
    _restart_parameters: PhantomData<&'a ()>,
    current: PUCHAR,
    end: PUCHAR,
}

impl<'a> FilterModuleNameIter<'a> {
    unsafe fn new(buffer: PUCHAR, length: ULONG) -> Self {
        let current = buffer;

        let end = if buffer.is_null() {
            // No entries, so we make end to be the same as the current
            current
        } else {
            unsafe { current.add(length as usize) }
        };

        FilterModuleNameIter {
            _restart_parameters: PhantomData,
            current,
            end,
        }
    }
}

impl<'a> Iterator for FilterModuleNameIter<'a> {
    type Item = NtUnicodeStr<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        #[repr(C)]
        struct NameElement {
            len: u16,
            buffer: [u16; 1],
        }

        if self.current == self.end {
            None
        } else {
            // Format of the buffer elements:
            // | byte_len: USHORT | buffer: [u8; byte_len] |

            let element = self.current.cast::<NameElement>();
            let buffer_len = unsafe { (*element).len };
            let buffer = unsafe { core::ptr::addr_of!((*element).buffer) }.cast();

            let str = unsafe { NtUnicodeStr::from_raw_parts(buffer, buffer_len, buffer_len) };

            // Bump by the element length, which is the buffer length plus the size at the front.
            // `buffer_len` is only for the size of the buffer and not the offset to bump by.
            let element_len = usize::from(buffer_len).saturating_add(2);
            self.current = unsafe { self.current.add(element_len) };

            Some(str)
        }
    }
}

#[derive(NtSingleList)]
enum RestartAttributesList {}

#[derive(NtListElement)]
#[repr(C, align(16))]
struct RestartAttributes {
    /// Next set of restart attributes
    Next: NtSingleListEntry<Self, RestartAttributesList>,
    /// NDIS object identifer of what `Data` contains
    Oid: NDIS_OID,
    /// Length (in bytes) of `Data`
    DataLength: ULONG,
    // Impl Note: This cannot be a [u8] since that'd mean that pointers to restart attributes
    // become wide-pointers, whereas the original definition of `NDIS_RESTART_ATTRIBUTES` uses
    // thin-pointers for the `Next` pointer.
    Data: [u8; 1],
}

// Should be the same as NDIS_RESTART_ATTRIBUTES
static_assertions::assert_eq_align!(RestartAttributes, NDIS_RESTART_ATTRIBUTES);
static_assertions::assert_eq_size!(RestartAttributes, NDIS_RESTART_ATTRIBUTES);

impl RestartAttributes {
    fn data(&self) -> &[u8] {
        let data = core::ptr::addr_of!(self.Data).cast::<u8>();
        // SAFETY:
        // - We assume that NDIS contains the full `DataLength` bytes of `Data` in a single allocation
        // - Since the alignment of `u8` is 1, `Data` is already aligned
        // - NDIS owns the allocation containing `Data`, so it is okay to assume that it won't mutate `Data`
        // - on x86_64, ULONG::MAX (u32::MAX) is less than isize::MAX
        unsafe { core::slice::from_raw_parts(data, self.DataLength as usize) }
    }
}
