//! NDIS Protocol entry points and utility functions for the NDIS MUX
//! intermediate miniport sample.
//!
//! The protocol edge binds to Ethernet (NdisMedium802_3) adapters, and
//! initiates creation of zero or more Virtual Ethernet LAN (VELAN) miniport
//! instances by calling `NdisIMInitializeDeviceInstanceEx` once for each VELAN
//! configured over a lower binding.

use core::{mem::ManuallyDrop, sync::atomic::AtomicU32};

use wdf_kmdf::{handle::HasContext, miniport::MiniportDriver};
use windows_kernel_rs::{
    log::debug,
    string::unicode_string::{NtUnicodeStr, NtUnicodeString},
    sync::{EventType, KeEvent, Timeout},
};
use windows_kernel_sys::{
    result::STATUS, Error, NdisAllocateMemoryWithTagPriority, NdisFreeMemory, NdisOpenAdapterEx,
    EX_POOL_PRIORITY, NDIS_BIND_PARAMETERS_REVISION_1, NDIS_DEVICE_POWER_STATE, NDIS_HANDLE,
    NDIS_LINK_STATE, NDIS_LINK_STATE_REVISION_1, NDIS_OBJECT_TYPE_BIND_PARAMETERS,
    NDIS_OBJECT_TYPE_DEFAULT, NDIS_OBJECT_TYPE_OPEN_PARAMETERS, NDIS_OPEN_PARAMETERS,
    NDIS_OPEN_PARAMETERS_REVISION_1, NDIS_PORT_NUMBER, NDIS_SIZEOF_LINK_STATE_REVISION_1,
    NDIS_SIZEOF_NDIS_OPEN_PARAMETERS_REVISION_1, NDIS_STATUS, PNDIS_BIND_PARAMETERS,
    PNDIS_OID_REQUEST, PNDIS_STATUS_INDICATION, PNET_BUFFER_LIST, PNET_PNP_EVENT_NOTIFICATION,
    ULONG, WCHAR,
};

use crate::{
    Adapt, AdaptInner, AdapterBindingState, BindInfo, MediumArray, Mux, NdisHandle, NdisRwLock,
    NdisSpinMutex, SyncWrapper, MUX_BINDING_ACTIVE, MUX_TAG,
};

/// Called by NDIS to bind to a miniport below. This routine creates a
/// binding by calling `NdisOpenAdapterEx`, and then initiates creation of all
/// configured VELANs on this binding.
pub(crate) extern "C" fn pt_bind_adapter(
    _ProtocolDriverContext: NDIS_HANDLE,
    _BindContext: NDIS_HANDLE,
    BindParameters: PNDIS_BIND_PARAMETERS,
) -> NDIS_STATUS {
    let driver = MiniportDriver::<Mux>::get();
    let globals = driver.get_context();

    let BindParameters = unsafe { &*BindParameters };

    let config_string = unsafe {
        NtUnicodeStr::from_raw_parts(
            (*BindParameters.ProtocolSection).Buffer,
            (*BindParameters.ProtocolSection).Length,
            (*BindParameters.ProtocolSection).MaximumLength,
        )
    };

    debug!("==> Protocol BindAdapter: {config_string}");

    let status = 'error: {
        if BindParameters.Header.Type != NDIS_OBJECT_TYPE_BIND_PARAMETERS as u8
            || BindParameters.Header.Revision != NDIS_BIND_PARAMETERS_REVISION_1 as u8
        {
            break 'error Err(Error::from(STATUS::UNSUCCESSFUL));
        }

        // Allocate memory for the config string with two extra WCHARs for NULL termination.
        let capacity = config_string.capacity() as usize + core::mem::size_of::<WCHAR>();
        let config_string = {
            let buffer = unsafe {
                NdisAllocateMemoryWithTagPriority(
                    globals.ProtHandle.get(),
                    capacity as u32,
                    MUX_TAG,
                    EX_POOL_PRIORITY::LowPoolPriority,
                )
                .cast::<u16>()
            };

            if buffer.is_null() {
                break 'error Err(STATUS::INSUFFICIENT_RESOURCES.into());
            }

            // Zero out the buffer and copy the string
            unsafe {
                buffer.write_bytes(0, capacity);
                buffer.copy_from_nonoverlapping(
                    config_string.as_slice().as_ptr(),
                    config_string.as_slice().len(),
                )
            };

            let string = unsafe {
                core::mem::transmute::<
                    windows_kernel_sys::UNICODE_STRING,
                    ManuallyDrop<NtUnicodeString>,
                >(windows_kernel_sys::UNICODE_STRING {
                    Buffer: buffer,
                    MaximumLength: capacity as u16,
                    Length: config_string.len(),
                })
            };

            scopeguard::guard(string, |string| {
                let string = unsafe {
                    core::mem::transmute::<
                        ManuallyDrop<NtUnicodeString>,
                        windows_kernel_sys::UNICODE_STRING,
                    >(string)
                };

                // Length is not required if the memory is allocated with NdisAllocateMemoryWithTagPriority
                unsafe { NdisFreeMemory(string.Buffer.cast(), 0, 0) };
            })
        };

        // Initialize the adapter structure
        let adapt = wdf_kmdf::object::GeneralObject::<Adapt>::create_unique(|_| {
            let PtDevicePowerState = NDIS_DEVICE_POWER_STATE::NdisDeviceStateD0;

            // Copy the link state, this could be updated by PtStatus soon after open operation is complete
            let mut LastIndicatedLinkState = unsafe { core::mem::zeroed::<NDIS_LINK_STATE>() };
            LastIndicatedLinkState.Header.Revision = NDIS_LINK_STATE_REVISION_1 as u8;
            LastIndicatedLinkState.Header.Type = NDIS_OBJECT_TYPE_DEFAULT as u8;
            LastIndicatedLinkState.Header.Size = NDIS_SIZEOF_LINK_STATE_REVISION_1;
            LastIndicatedLinkState.MediaConnectState = BindParameters.MediaConnectState;
            LastIndicatedLinkState.MediaDuplexState = BindParameters.MediaDuplexState;
            LastIndicatedLinkState.XmitLinkSpeed = BindParameters.XmitLinkSpeed;
            LastIndicatedLinkState.RcvLinkSpeed = BindParameters.RcvLinkSpeed;

            Ok(pinned_init::try_pin_init!(
                Adapt {
                    BindingHandle: NdisHandle::empty(),
                    VElanList: (),
                    VElanCount: 0,
                    ConfigString: SyncWrapper(scopeguard::ScopeGuard::into_inner(config_string)),
                    Status: 0,
                    Event <- KeEvent::new(EventType::Notification, false),
                    PacketFilter: 0,
                    PtDevicePowerState,
                    Medium: MediumArray[0],
                    PowerManagementCaps: unsafe { *BindParameters.PowerManagementCapabilities },
                    RcvScaleCapabilities: unsafe { *BindParameters.RcvScaleCapabilities },
                    BindParameters: BindInfo::from_bind_parameters(BindParameters),
                    LastIndicatedLinkState,
                    BindingState: AdapterBindingState::Opening,
                    OutstandingSends: AtomicU32::new(0),
                    PauseEvent: None,
                    Lock <- NdisSpinMutex::new(AdaptInner {
                        Flags: 0,
                    }),
                    RWLock <- NdisRwLock::new(),

                    OutstandingRequests: AtomicU32::new(0),
                    CloseEvent: None,
                }? Error
            ))
        });

        let Ok(mut adapt) = adapt else {
            break 'error Err(STATUS::INSUFFICIENT_RESOURCES.into());
        };

        let mut MediumIndex = 0;

        // Now open the adapter below and complete the initialization
        let mut open_parameters = unsafe { core::mem::zeroed::<NDIS_OPEN_PARAMETERS>() };
        open_parameters.Header.Type = NDIS_OBJECT_TYPE_OPEN_PARAMETERS as u8;
        open_parameters.Header.Revision = NDIS_OPEN_PARAMETERS_REVISION_1 as u8;
        open_parameters.Header.Size = NDIS_SIZEOF_NDIS_OPEN_PARAMETERS_REVISION_1;
        open_parameters.AdapterName = BindParameters.AdapterName;
        open_parameters.MediumArray = MediumArray.as_ptr().cast_mut();
        open_parameters.MediumArraySize = MediumArray.len() as u32;
        open_parameters.SelectedMediumIndex = &mut MediumIndex;

        open_parameters.FrameTypeArray = core::ptr::null_mut();
        open_parameters.FrameTypeArraySize = 0;

        // ???: Do we have exclusive access to the protocol bind context?
        let status = Error::to_err(unsafe {
            NdisOpenAdapterEx(
                globals.ProtHandle.get(),
                adapt.raw_handle(),
                &mut open_parameters,
                _BindContext,
                adapt.get_context().BindingHandle.as_ptr(),
            )
        });

        let status = if let Err(Error(STATUS::PENDING)) = status {
            let _ = adapt.get_context().Event.wait(Timeout::forever());
            Error::to_err(adapt.get_context().Status)
        } else {
            status
        };

        if let Err(_) = status {
            break 'error status;
        }

        let mut adapt_context = adapt.get_context_mut();
        adapt_context.Lock.get_mut().Flags |= MUX_BINDING_ACTIVE;
        adapt_context.Medium = MediumArray[MediumIndex as usize];

        // Copy all the relevant information about the adapter into the local structure
        if !BindParameters.RcvScaleCapabilities.is_null() {
            adapt_context.RcvScaleCapabilities =
                unsafe { BindParameters.RcvScaleCapabilities.read() };
        }

        adapt_context.PowerManagementCaps =
            unsafe { BindParameters.PowerManagementCapabilities.read() };

        // PtPostProcessPnPCapabilities
        // The following fields must be overwritten by an IM driver.
        debug!("==> pt_post_process_pnp_capabilities");
        let wakeup_caps = &mut adapt_context.PowerManagementCaps.WakeUpCapabilities;
        wakeup_caps.MinMagicPacketWakeUp = NDIS_DEVICE_POWER_STATE::NdisDeviceStateUnspecified;
        wakeup_caps.MinPatternWakeUp = NDIS_DEVICE_POWER_STATE::NdisDeviceStateUnspecified;
        wakeup_caps.MinLinkChangeWakeUp = NDIS_DEVICE_POWER_STATE::NdisDeviceStateUnspecified;
        debug!("<== pt_post_process_pnp_capabilities");

        let adapt = {
            let adapt = adapt.into_shared();
            // Note: we have to leak a ref since it's owned by NDIS
            core::mem::forget(wdf_kmdf::clone!(adapt));
            adapt
        };

        // Add this adapter to the global AdapterList
        let mut adapter_list = globals.AdapterList.lock();
        let Ok(()) = adapter_list.try_reserve(1) else {
            break 'error Err(STATUS::INSUFFICIENT_RESOURCES.into());
        };
        adapter_list.push(adapt);

        // Start all VELANS configured on this adapter

        Ok(())
    };

    if let Err(err) = status {
        // todo
    }

    STATUS::UNSUCCESSFUL.to_u32()
}

/// Completion routine for `NdisOpenAdapter` issued from within
/// `pt_bind_adapter`. Simply unblock the caller.
pub(crate) unsafe extern "C" fn pt_open_adapter_complete(
    ProtocolBindingContext: NDIS_HANDLE,
    Status: NDIS_STATUS,
) {
    todo!()
}

fn pt_query_adapter_info() {
    todo!()
}

fn pt_request_adapter_sync() {
    todo!()
}

fn pt_request_adapter_async() {
    todo!()
}

fn pt_close_adapter(adapt: &Adapt) {
    todo!()
}

/// Called by NDIS when we are required to unbind the adapter below.
/// Go through all VELANs on the adapter and shut them down.
pub(crate) extern "C" fn pt_unbind_adapter(
    UnbindContext: NDIS_HANDLE,
    ProtocolBindingContext: NDIS_HANDLE,
) -> NDIS_STATUS {
    STATUS::UNSUCCESSFUL.to_u32()
}

/// Completion for the `pt_close_adapter` call.
pub(crate) extern "C" fn pt_close_adapter_complete(ProtocolBindingContext: NDIS_HANDLE) {
    todo!()
}

/// Completion handler for an NDIS request sent to a lower miniport.
pub(crate) extern "C" fn pt_request_complete(
    ProtocolBindingContext: NDIS_HANDLE,
    NdisRequest: PNDIS_OID_REQUEST,
    Status: NDIS_STATUS,
) {
    todo!()
}

fn pt_complete_forwarded_request() {
    todo!()
}

fn pt_post_process_pnp_capabilities() {
    todo!()
}

fn pt_complete_blocking_request() {
    todo!()
}

fn pt_discard_completed_request() {
    todo!()
}

/// Handle a status indication on the lower binding (ADAPT). If this is a media
/// status indication, we also pass this on to all associated VELANs.
pub(crate) extern "C" fn pt_status(
    ProtocolBindingContext: NDIS_HANDLE,
    StatusIndication: PNDIS_STATUS_INDICATION,
) {
    todo!()
}

fn pt_multicast_match() {
    todo!()
}

fn pt_pnp_net_event_set_power() {
    todo!()
}

/// Called by NDIS to notify us of a PNP event related to a lower binding. Based
/// on the event, this dispatches to other helper routines.
pub(crate) extern "C" fn pt_pnp_handler(
    ProtocolBindingContext: NDIS_HANDLE,
    pNetPnPEventNotification: PNET_PNP_EVENT_NOTIFICATION,
) -> NDIS_STATUS {
    STATUS::UNSUCCESSFUL.to_u32()
}

fn pt_create_and_start_velan() {
    todo!()
}

fn pt_allocate_and_initialize_velan() {
    todo!()
}

fn pt_deallocate_velan() {
    todo!()
}

fn pt_stop_velan() {
    todo!()
}

fn pt_unlink_velan_from_adapter() {
    todo!()
}

fn pt_find_velan() {
    todo!()
}

fn pt_bootstrap_velans() {
    todo!()
}

fn pt_reference_velan() {
    todo!()
}

fn pt_dereference_velan() {
    todo!()
}

fn pt_reference_adapter() {
    todo!()
}

fn pt_dereference_adapter() {
    todo!()
}

/// ReceiveNetBufferList handler
pub(crate) extern "C" fn pt_receive_nbl(
    ProtocolBindingContext: NDIS_HANDLE,
    NetBufferLists: PNET_BUFFER_LIST,
    PortNumber: NDIS_PORT_NUMBER,
    NumberOfNetBufferLists: ULONG,
    ReceiveFlags: ULONG,
) {
    todo!()
}

/// Called by NDIS when the miniport below has completed a send.
/// We complete the corresponding upper-edge send this represents.
pub(crate) extern "C" fn pt_send_nbl_complete(
    ProtocolBindingContext: NDIS_HANDLE,
    NetBufferLists: PNET_BUFFER_LIST,
    SendCompleteFlags: ULONG,
) {
    todo!()
}

fn pt_handle_receive_tagging_nb() {
    todo!()
}

fn pt_strip_vlan_tag_nb() {
    todo!()
}

fn pt_restore_receive_nbl() {
    todo!()
}
