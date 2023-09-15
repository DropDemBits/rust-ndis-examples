use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_sys::{NDIS_HANDLE, PNET_BUFFER_LIST, ULONG};

pub(crate) unsafe extern "C" fn ndisprot_evt_io_write(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}

pub(crate) unsafe extern "C" fn send_complete(
    ProtocolBindingContext: NDIS_HANDLE,
    NetBufferList: PNET_BUFFER_LIST,
    SendCompleteFlags: ULONG,
) {
}
