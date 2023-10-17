//! NDIS protocol entry points as well as handling binding and unbinding from adapters
use core::{
    mem::ManuallyDrop,
    pin::Pin,
    sync::atomic::{AtomicBool, AtomicU32, Ordering},
};

use alloc::{boxed::Box, sync::Arc};
use pinned_init::{pin_data, InPlaceInit, PinInit};
use scopeguard::ScopeGuard;
use wdf_kmdf::{object::GeneralObject, sync::SpinMutex};
use wdf_kmdf_sys::WDF_IO_QUEUE_CONFIG;
use windows_kernel_rs::{
    log,
    string::unicode_string::{NtUnicodeStr, NtUnicodeString},
};
use windows_kernel_sys::{
    result::STATUS, Error, NdisAllocateNetBufferListPool, NdisCloseAdapterEx,
    NdisDeregisterProtocolDriver, NdisFreeMemory, NdisFreeNetBufferListPool, NdisOpenAdapterEx,
    NdisQueryAdapterInstanceName, NDIS_DEFAULT_PORT_NUMBER, NDIS_ETH_TYPE_802_1Q,
    NDIS_ETH_TYPE_802_1X, NDIS_HANDLE, NDIS_MEDIUM, NDIS_OBJECT_TYPE_DEFAULT,
    NDIS_OBJECT_TYPE_OPEN_PARAMETERS, NDIS_OID, NDIS_OPEN_PARAMETERS,
    NDIS_OPEN_PARAMETERS_REVISION_1, NDIS_PORT_NUMBER, NDIS_PROTOCOL_ID_IPX, NDIS_REQUEST_TYPE,
    NET_BUFFER_LIST_POOL_PARAMETERS, NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1, NTSTATUS,
    OID_802_11_ADD_WEP, OID_802_11_AUTHENTICATION_MODE, OID_802_11_BSSID, OID_802_11_BSSID_LIST,
    OID_802_11_BSSID_LIST_SCAN, OID_802_11_CONFIGURATION, OID_802_11_DISASSOCIATE,
    OID_802_11_INFRASTRUCTURE_MODE, OID_802_11_NETWORK_TYPE_IN_USE, OID_802_11_POWER_MODE,
    OID_802_11_RELOAD_DEFAULTS, OID_802_11_REMOVE_WEP, OID_802_11_RSSI, OID_802_11_SSID,
    OID_802_11_STATISTICS, OID_802_11_SUPPORTED_RATES, OID_802_11_WEP_STATUS,
    OID_802_3_MULTICAST_LIST, OID_GEN_CURRENT_PACKET_FILTER, PNDIS_BIND_PARAMETERS,
    PNDIS_OID_REQUEST, PNDIS_STATUS_INDICATION, PNET_PNP_EVENT_NOTIFICATION, PVOID,
};

use crate::{
    EventType, Globals, KeEvent, ListEntry, MACAddr, OpenContext, OpenContextFlags,
    OpenContextInner, OpenState, Timeout, MAX_MULTICAST_ADDRESS,
};

use super::NdisProt;

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

#[pin_data]
struct ProtocolBindingContext {
    // The actual open context
    open_context: GeneralObject<OpenContext>,

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
) -> NTSTATUS {
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

    let driver =
        unsafe { wdf_kmdf::driver::Driver::<NdisProt>::wrap(ProtocolDriverContext.cast()) };
    let globals = match driver.get_context() {
        Ok(ctx) => ctx.globals.clone(),
        Err(err) => return Error::from(err).0.to_u32(),
    };

    // Reserve space for the open context in the global list
    {
        let mut open_list = globals.open_list.lock();
        if let Err(err) = open_list.try_reserve(1) {
            log::error!(
                "reserving space for adding open context to list failed ({})",
                err
            );
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
    if let Some(other_context) = ndisprot_lookup_device(globals.as_ref(), adapter_name) {
        log::warn!(
            "bind_adapter: binding to {} already exists on binding {:#x?}",
            adapter_name,
            other_context.raw_handle()
        );

        return STATUS::UNSUCCESSFUL.to_u32();
    }

    let status = wdf_kmdf::object::GeneralObject::<OpenContext>::with_parent(
        &wdf_kmdf::object::RawObjectHandle(globals.control_device.cast()),
        |open_context| {
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
                Err(err) => {
                    log::error!("bind_adapter: protocol binding context alloc failed");
                    return Err(STATUS::INSUFFICIENT_RESOURCES.into());
                }
            };

            // Manual queue for pending read requests.
            // These read requests are forwarded from the original read queue and
            // serviced in the ProtocolRecv indication handler.
            let read_queue = {
                let mut io_queue = core::ptr::null_mut();
                let mut queue_config = WDF_IO_QUEUE_CONFIG::init(
                    wdf_kmdf_sys::WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual,
                );
                let status = Error::to_err(unsafe {
                    wdf_kmdf::raw::WdfIoQueueCreate(
                        globals.control_device,
                        &mut queue_config,
                        None,
                        Some(&mut io_queue),
                    )
                });
                if let Err(err) = status {
                    log::error!(
                        "WdfIoQueueCreate for read queue failed {:#x?}",
                        err.0.to_u32()
                    );
                    return Err(STATUS::UNSUCCESSFUL.into());
                }

                scopeguard::guard(io_queue, |io_queue| unsafe {
                    wdf_kmdf::raw::WdfObjectDelete(io_queue.cast())
                })
            };

            // Register a notification for when a new request gets enqueued while the queue is idle
            let status = unsafe {
                Error::to_err(wdf_kmdf::raw::WdfIoQueueReadyNotify(
                    *read_queue,
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
                let mut io_queue = core::ptr::null_mut();
                let mut queue_config = WDF_IO_QUEUE_CONFIG::init(
                    wdf_kmdf_sys::WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual,
                );
                let status = Error::to_err(unsafe {
                    wdf_kmdf::raw::WdfIoQueueCreate(
                        globals.control_device,
                        &mut queue_config,
                        None,
                        Some(&mut io_queue),
                    )
                });
                if let Err(err) = status {
                    log::error!(
                        "WdfIoQueueCreate for ioctl queue failed {:#x?}",
                        err.0.to_u32()
                    );
                    return Err(STATUS::UNSUCCESSFUL.into());
                }

                scopeguard::guard(io_queue, |io_queue| unsafe {
                    wdf_kmdf::raw::WdfObjectDelete(io_queue.cast())
                })
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

            const NDIS_SIZEOF_NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1: u16 =
                (core::mem::offset_of!(NET_BUFFER_LIST_POOL_PARAMETERS, DataSize)
                    + core::mem::size_of::<windows_kernel_sys::ULONG>()) as u16;

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
                    globals.ndis_protocol_handle,
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
                    globals.ndis_protocol_handle,
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
            let power_state = windows_kernel_sys::NET_DEVICE_POWER_STATE::NetDeviceStateD0;

            let mut medium_array = [NDIS_MEDIUM::NdisMedium802_3];
            let mut frame_type_array = [NDIS_ETH_TYPE_802_1X as u16, NDIS_ETH_TYPE_802_1Q as u16];

            // Note: need to leak the inner box because we want to transfer ownership to NDIS
            let protocol_binding_context =
                Box::leak(unsafe { Pin::into_inner_unchecked(protocol_binding_context) })
                    as *mut ProtocolBindingContext;

            const NDIS_SIZEOF_NDIS_OPEN_PARAMETERS_REVISION_1: u16 =
                (core::mem::offset_of!(NDIS_OPEN_PARAMETERS, FrameTypeArraySize)
                    + core::mem::size_of::<windows_kernel_sys::UINT>()) as u16;

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
                    globals.ndis_protocol_handle,
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

                    inner <- SpinMutex::new(OpenContextInner {
                        flags,
                        state,
                        closing_event: core::ptr::null_mut(),
                    }),

                    file_object: core::ptr::null_mut(),

                    device_name,
                    device_desc: ScopeGuard::into_inner(device_desc),

                    bind_event <- KeEvent::new(crate::EventType::Notification, false),
                    bind_status: 0,

                    // start off by assuming the device is powered up
                    powered_up_event <- KeEvent::new(crate::EventType::Notification, true),
                    power_state,

                    send_nbl_pool: ScopeGuard::into_inner(send_nbl_pool),
                    pended_send_count: 0,

                    recv_nbl_pool: ScopeGuard::into_inner(recv_nbl_pool),
                    read_queue: ScopeGuard::into_inner(read_queue),
                    pending_read_count: 0,
                    recv_nbl_queue <- ListEntry::new(),
                    recv_nbl_len: 0,

                    status_indication_queue: ScopeGuard::into_inner(status_indication_queue),

                    current_address,
                    multicast_address: [{MACAddr::zero()}; MAX_MULTICAST_ADDRESS],
                    mac_options,
                    max_frame_size,
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
    Status: NTSTATUS,
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
) -> NTSTATUS {
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
        let open_context = protocol_binding_context
            .open_context
            .get_context()
            .expect("binding context should be initialized at unload");

        // In case threads were waiting for the device to be powered up, wake them
        open_context.powered_up_event.signal();

        open_context.inner.lock().state = OpenState::Closing;

        drop(open_context);

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
    let Ok(open_context) = protocol_binding_context.open_context.get_context() else {
        // either setting up or already unbound
        return;
    };

    pinned_init::stack_pin_init!(let closing_event = KeEvent::new(crate::EventType::Notification, false));
    let mut do_close_binding = false;

    {
        let mut inner = open_context.inner.lock();

        if inner.flags.intersects(OpenContextFlags::BIND_OPENING) {
            // still in the process of setting up this binding
            return;
        }

        if inner.flags.intersects(OpenContextFlags::BIND_ACTIVE) {
            assert!(inner.closing_event.is_null());
            inner.closing_event = core::ptr::null_mut();

            inner.flags.remove(OpenContextFlags::BIND_FLAGS);
            inner.flags.insert(OpenContextFlags::BIND_CLOSING);

            if open_context.pended_send_count != 0 {
                inner.closing_event = unsafe { closing_event.get_unchecked_mut() } as *mut _;
            }

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
            &mut packet_filter as *mut _,
            core::mem::size_of_val(&packet_filter),
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
        unsafe {
            wdf_kmdf::raw::WdfIoQueuePurgeSynchronously(open_context.read_queue);
        }
        unsafe {
            wdf_kmdf::raw::WdfIoQueuePurgeSynchronously(open_context.status_indication_queue);
        }

        // discard any queued recieves
        flush_receive_queue(&*open_context);

        // close the binding now
        log::info!(
            "shutdown_binding: closing open_context {}, binding handle {:#x?}",
            open_context.device_name,
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

        let mut inner = open_context.inner.lock();
        inner.flags.remove(OpenContextFlags::BIND_FLAGS);
        inner.flags.insert(OpenContextFlags::BIND_IDLE);
        inner.flags.remove(OpenContextFlags::UNBIND_FLAGS);
    }

    // remove from the global list
    open_context
        .driver
        .get_context()
        .expect("driver context should be available")
        .globals
        .open_list
        .lock()
        .retain(|e| e != &protocol_binding_context.open_context);
}

pub(crate) unsafe extern "C" fn pnp_event_handler(
    ProtocolBindingContext: NDIS_HANDLE,
    NetPnPEventNotification: PNET_PNP_EVENT_NOTIFICATION,
) -> NTSTATUS {
    log::debug!(
        "pnp_event_handler: {:#x?} {:#x?}",
        ProtocolBindingContext,
        NetPnPEventNotification
    );
    STATUS::UNSUCCESSFUL.to_u32()
}

pub(crate) unsafe extern "C" fn NdisprotProtocolUnloadHandler() -> NTSTATUS {
    log::debug!("unload_handler",);
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

pub(crate) fn unload_protocol(mut context: Pin<&mut Globals>) {
    let proto_handle = core::mem::replace(&mut context.ndis_protocol_handle, core::ptr::null_mut());

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

fn do_request(
    open_context: &OpenContext,
    port_number: NDIS_PORT_NUMBER,
    request_type: NDIS_REQUEST_TYPE,
    oid: NDIS_OID,
    information_buffer: *mut u32,
    information_buffer_length: usize,
    bytes_read: &mut u32,
) -> Result<(), Error> {
    Ok(())
}

pub(crate) fn flush_receive_queue(open_context: &OpenContext) {
    loop {}
}

/// Returns information about the specified binding
pub(crate) fn query_binding(
    buffer: PVOID,
    input_length: usize,
    output_length: usize,
    bytes_returned: &mut usize,
) -> NTSTATUS {
    loop {}
}

/// Searches the global bindings list for an open context which has a binding to the specified adapter, and return a reference to it
pub(crate) fn ndisprot_lookup_device(
    globals: Pin<&Globals>,
    adapter_name: NtUnicodeStr<'_>,
) -> Option<GeneralObject<OpenContext>> {
    globals
        .open_list
        .lock()
        .as_ref()
        .iter()
        .find_map(|open_obj| {
            let open_obj = &open_obj.as_ref()?;
            let open_context = open_obj.get_context().ok()?;
            (open_context.device_name == adapter_name).then(|| open_obj.clone_ref())
        })
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
