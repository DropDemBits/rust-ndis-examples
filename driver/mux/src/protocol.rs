//! NDIS Protocol entry points and utility functions for the NDIS MUX
//! intermediate miniport sample.
//!
//! The protocol edge binds to Ethernet (NdisMedium802_3) adapters, and
//! initiates creation of zero or more Virtual Ethernet LAN (VELAN) miniport
//! instances by calling `NdisIMInitializeDeviceInstanceEx` once for each VELAN
//! configured over a lower binding.

use windows_kernel_sys::{
    result::STATUS, NDIS_HANDLE, NDIS_PORT_NUMBER, NDIS_STATUS, PNDIS_BIND_PARAMETERS,
    PNDIS_OID_REQUEST, PNDIS_STATUS_INDICATION, PNET_BUFFER_LIST, PNET_PNP_EVENT_NOTIFICATION,
    ULONG,
};

/// Called by NDIS to bind to a miniport below. This routine creates a
/// binding by calling `NdisOpenAdapterEx`, and then initiates creation of all
/// configured VELANs on this binding.
pub(crate) extern "C" fn pt_bind_adapter(
    ProtocolDriverContext: NDIS_HANDLE,
    BindContext: NDIS_HANDLE,
    BindParameters: PNDIS_BIND_PARAMETERS,
) -> NDIS_STATUS {
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

fn pt_close_adapter() {
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
