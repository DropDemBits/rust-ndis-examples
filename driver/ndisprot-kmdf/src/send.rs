use core::sync::atomic::AtomicU32;

use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_sys::{NDIS_HANDLE, PNET_BUFFER_LIST, ULONG};

/// `ProtocolReserved` in sent packets. We save a handle to the io request
/// which generated the send.
///
pub(crate) struct NprotSendNblRsvd {
    request: WDFREQUEST,
    /// Used to determine when to free the packet back to its pool.
    /// It is used to synchronize between a thread completing a send and a thread attempting
    /// to cancel a send.
    ref_count: AtomicU32,
}

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
