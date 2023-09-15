//! NDIS protocol entry points as well as handling binding and unbinding from adapters
use core::pin::Pin;

use alloc::sync::Arc;
use windows_kernel_rs::log;
use windows_kernel_sys::{
    result::STATUS, NdisDeregisterProtocolDriver, NDIS_HANDLE, NDIS_OID, NDIS_REQUEST_TYPE,
    NTSTATUS, OID_802_11_ADD_WEP, OID_802_11_AUTHENTICATION_MODE, OID_802_11_BSSID,
    OID_802_11_BSSID_LIST, OID_802_11_BSSID_LIST_SCAN, OID_802_11_CONFIGURATION,
    OID_802_11_DISASSOCIATE, OID_802_11_INFRASTRUCTURE_MODE, OID_802_11_NETWORK_TYPE_IN_USE,
    OID_802_11_POWER_MODE, OID_802_11_RELOAD_DEFAULTS, OID_802_11_REMOVE_WEP, OID_802_11_RSSI,
    OID_802_11_SSID, OID_802_11_STATISTICS, OID_802_11_SUPPORTED_RATES, OID_802_11_WEP_STATUS,
    OID_802_3_MULTICAST_LIST, PNDIS_BIND_PARAMETERS, PNDIS_OID_REQUEST, PNDIS_STATUS_INDICATION,
    PNET_PNP_EVENT_NOTIFICATION, PVOID,
};

use crate::{Globals, OpenContext};

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
    log::debug!(
        "bind_adapter: {:#x?} {:#x?} {:#x?}",
        ProtocolDriverContext,
        BindContext,
        BindParameters
    );
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
    log::debug!(
        "unbind_adapter: {:#x?} {:#x?}",
        UnbindContext,
        ProtocolBindingContext
    );
    STATUS::SUCCESS.to_u32()
}

pub(crate) unsafe extern "C" fn close_adapter_complete(ProtocolBindingContext: NDIS_HANDLE) {}

pub(crate) unsafe extern "C" fn pnp_event_handler(
    ProtocolBindingContext: NDIS_HANDLE,
    NetPnPEventNotification: PNET_PNP_EVENT_NOTIFICATION,
) -> NTSTATUS {
    log::debug!(
        "pnp_event_handler: {:#x?} {:#x?}",
        ProtocolBindingContext,
        NetPnPEventNotification
    );
    STATUS::SUCCESS.to_u32()
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

pub(crate) fn flush_receive_queue(open_context: &Arc<OpenContext>) {
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
