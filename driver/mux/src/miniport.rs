//! NDIS Miniport entry points and utilities for the NDIS MUX intermediate
//! miniport sample. The driver exposes zero or more Virtual Ethernet LANs
//! (VELANSs) as NDIS miniport instances over each lower (protocol-edge) binding
//! to an underlying adapter.

use wdf_kmdf::miniport::MiniportDriver;
use windows_kernel_rs::log::debug;
use windows_kernel_sys::{
    result::STATUS, NdisCancelSendNetBufferLists, NDIS_HALT_ACTION, NDIS_HANDLE, NDIS_PORT_NUMBER,
    NDIS_SHUTDOWN_ACTION, NDIS_STATUS, PDRIVER_OBJECT, PNDIS_MINIPORT_INIT_PARAMETERS,
    PNDIS_MINIPORT_PAUSE_PARAMETERS, PNDIS_MINIPORT_RESTART_PARAMETERS, PNDIS_OID_REQUEST,
    PNET_BUFFER_LIST, PNET_DEVICE_PNP_EVENT, PVOID, ULONG,
};

use crate::{Mux, VELan};

/// This is the miniport initialize routine which gets called as a result of
/// our call to NdisIMInitializeDeviceInstanceEx.
///
/// The context parameter which we pass there is the VELan structure which we
/// retrieve here.
pub(crate) unsafe extern "C" fn mp_initialize(
    MiniportAdapterHandle: NDIS_HANDLE,
    MiniportDriverContext: NDIS_HANDLE,
    MiniportInitParameters: PNDIS_MINIPORT_INIT_PARAMETERS,
) -> NDIS_STATUS {
    STATUS::UNSUCCESSFUL.to_u32()
}

fn mp_query_information() {
    todo!()
}

fn mp_set_information() {
    todo!()
}

fn mp_method_request() {
    todo!()
}

/// Miniport OID request dispatch handler
pub(crate) unsafe extern "C" fn mp_oid_request(
    MiniportAdapterContext: NDIS_HANDLE,
    NdisRequest: PNDIS_OID_REQUEST,
) -> NDIS_STATUS {
    STATUS::NOT_SUPPORTED.to_u32()
}

/// Halt handler. Add any further clean-up for the VELAN to this function.
///
/// We wait for all pending IO on the VELAN to complete and then unlink the
/// VELAN from the adapter.
pub(crate) unsafe extern "C" fn mp_halt(
    MiniportAdapterContext: NDIS_HANDLE,
    HaltAction: NDIS_HALT_ACTION,
) {
    todo!()
}

fn mp_forward_oid_request() {
    todo!()
}

fn mp_set_packet_filter() {
    todo!()
}

fn mp_set_multicast_list() {
    todo!()
}

fn mp_generate_mac_addr() {
    todo!()
}

/// Notifies when PnP events are direceted to our miniport device object.
pub(crate) unsafe extern "C" fn mp_device_pnp_event(
    MiniportAdapterContext: NDIS_HANDLE,
    NetDevicePnPEvent: PNET_DEVICE_PNP_EVENT,
) {
    // NOTE: This isn't implemented in the original sample code.
    // XXX: Where can we find out what we could implement?

    debug!("==> mp_device_pnp_event: AdapterContext: {MiniportAdapterContext:x?}, DevicePnPEvent: {NetDevicePnPEvent:x?}");
    debug!("<== mp_device_pnp_event: AdapterContext: {MiniportAdapterContext:x?}, DevicePnPEvent: {NetDevicePnPEvent:x?}");
}

/// Notifies us of an impending system shutdown.
/// Since this is not a hardware driver, there isn't anything specific we need to do about this.
pub(crate) unsafe extern "C" fn mp_adapter_shutdown(
    MiniportAdapterContext: NDIS_HANDLE,
    ShutdownAction: NDIS_SHUTDOWN_ACTION,
) {
    debug!(
        "==> mp_adapter_shutdown: VELan: {MiniportAdapterContext:x?}, ShutdownAction: {:x?}",
        ShutdownAction.0
    );
    debug!(
        "<== mp_adapter_shutdown: VELan: {MiniportAdapterContext:x?}, ShutdownAction: {:x?}",
        ShutdownAction.0
    );
}

/// Handler that unloads the miniport
pub(crate) unsafe extern "C" fn mp_unload(DriverObject: PDRIVER_OBJECT) {
    debug!("==> mp_unload: DriverObject {DriverObject:x?}");
    // SAFETY: This is the port-defined unload callback.
    // We don't call any more WDF functions after this.
    unsafe { MiniportDriver::<Mux>::unload() };
    debug!("<== mp_unload: DriverObject {DriverObject:x?}");
}

/// Pauses the miniport. No NBLs will be indicated to the upper binding as well
/// as status indications.
pub(crate) unsafe extern "C" fn mp_pause(
    MiniportAdapterContext: NDIS_HANDLE,
    MiniportPauseParameters: PNDIS_MINIPORT_PAUSE_PARAMETERS,
) -> NDIS_STATUS {
    todo!()
}

/// Restarts the miniport. When the miniport is back in the restarted state, it
/// can indicate NBLs to the upper binding.
pub(crate) unsafe extern "C" fn mp_restart(
    MiniportAdapterContext: NDIS_HANDLE,
    MiniportRestartParameters: PNDIS_MINIPORT_RESTART_PARAMETERS,
) -> NDIS_STATUS {
    todo!()
}

/// Send the NBLs to the lower binding.
pub(crate) unsafe extern "C" fn mp_send_nbls(
    MiniportAdapterContext: NDIS_HANDLE,
    NetBufferLists: PNET_BUFFER_LIST,
    PortNumber: NDIS_PORT_NUMBER,
    SendFlags: ULONG,
) {
    todo!()
}

/// Called whenever protocols are done with a packet that we had indicated up
/// and they had queued up for returning later.
pub(crate) unsafe extern "C" fn mp_return_nbls(
    MiniportAdapterContext: NDIS_HANDLE,
    NetBufferLists: PNET_BUFFER_LIST,
    ReturnFlags: ULONG,
) {
    todo!()
}

/// The miniport entry point to handle cancellation of all send packets that
/// match the given `CancelId`. If we have queued any packets that match this,
/// then we should dequeue them and call `NdisMSendCompleteNetBufferLists` for
/// all such packets, with a status of NDIS_STATUS_REQUEST_ABORTED.
///
/// We should also call `NdisCancelSendPackets` in turn, on each lower binding
/// that this adapter corresponds to. This is to let miniports below cancel any
/// matching packets.
pub(crate) unsafe extern "C" fn mp_cancel_send_nbls(
    MiniportAdapterContext: NDIS_HANDLE,
    CancelId: PVOID,
) {
    let velan = unsafe { &*MiniportAdapterContext.cast::<VELan>() };

    debug!("==> mp_cancel_send_nbls: VELan {MiniportAdapterContext:x?}, CancelId {CancelId:?}");

    unsafe { NdisCancelSendNetBufferLists((&*velan.pAdapt).BindingHandle, CancelId) }

    debug!("<== mp_cancel_send_nbls: VELan {MiniportAdapterContext:x?}, CancelId {CancelId:?}");
}

/// Miniport entry point to handle cancellation of a request. This function
/// checks to see if the CancelRequest should be terminated at this level, or
/// passed down to the next driver.
pub(crate) unsafe extern "C" fn mp_cancel_oid_request(
    MiniportAdapterContext: NDIS_HANDLE,
    RequestId: PVOID,
) {
    todo!()
}

fn mux_allocate_mdl() {}

fn mp_handle_send_tagging_nb() {}

fn mp_restore_send_nb() {
    todo!()
}
