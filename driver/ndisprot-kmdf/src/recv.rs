use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_sys::{LIST_ENTRY, NDIS_HANDLE, NDIS_PORT_NUMBER, PNET_BUFFER_LIST, ULONG};

/// `ProtocolReserved` in received packets: we link these packets up in a queue waiting for read io requests.
pub(crate) struct NprotRecvNblRsvd {
    link: LIST_ENTRY,
    /// Used if we had to partial-map
    nbl: PNET_BUFFER_LIST,
}

pub(crate) unsafe extern "C" fn ndisprot_evt_io_read(
    Queue: WDFQUEUE,     // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
}

pub(crate) unsafe extern "C" fn ndisprot_evt_notify_read_queue(
    Queue: WDFQUEUE,
    Context: wdf_kmdf_sys::WDFCONTEXT,
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
