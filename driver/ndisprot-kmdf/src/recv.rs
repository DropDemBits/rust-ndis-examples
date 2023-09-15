use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_sys::{NDIS_HANDLE, NDIS_PORT_NUMBER, PNET_BUFFER_LIST, ULONG};

pub(crate) unsafe extern "C" fn ndisprot_evt_io_read(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}

pub(crate) unsafe extern "C" fn receive_net_buffer_lists(
    ProtocolBindingContext: NDIS_HANDLE,
    NetBufferLists: PNET_BUFFER_LIST,
    PortNumber: NDIS_PORT_NUMBER,
    NumberOfNetBufferLists: ULONG,
    ReceiveFlags: ULONG,
) {
}
