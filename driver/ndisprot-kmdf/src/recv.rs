use core::pin::Pin;

use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_rs::log;
use windows_kernel_sys::{LIST_ENTRY, NDIS_HANDLE, NDIS_PORT_NUMBER, PNET_BUFFER_LIST, ULONG};

use crate::OpenContext;

#[derive(nt_list::list::NtList)]
pub(crate) enum RecvNblList {}

/// `ProtocolReserved` in received packets: we link these packets up in a queue waiting for read io requests.
/// We stuff this inside of NET_BUFFER_LIST.ProtocolReserved
#[repr(C)]
#[derive(nt_list::NtListElement)]
pub(crate) struct RecvNblRsvd {
    link: nt_list::list::NtListEntry<Self, RecvNblList>,
    /// Used if we had to partial-map
    nbl: PNET_BUFFER_LIST,
    // FIXME: want to have this in here so that open context is alive while there are enqueued recvs.
    // Would likely need to be `Ref<GeneralObject<OpenContext>>`
    // _open_context: GeneralObject<OpenContext>,
}

// `ProtocolReserved` is a `[PVOID; 4]`
static_assertions::const_assert!(
    core::mem::size_of::<RecvNblRsvd>() <= core::mem::size_of::<[PVOID; 4]>(),
);

impl RecvNblRsvd {
    fn containing_nbl(self: Pin<&mut Self>) -> PNET_BUFFER_LIST {
        self.nbl
    }

    unsafe fn from_nbl<'a>(net_buffer_list: PNET_BUFFER_LIST) -> Pin<&'a mut Self> {
        let element: *mut Self =
            unsafe { core::ptr::addr_of_mut!((*net_buffer_list).ProtocolReserved) }.cast();
        let element = unsafe { &mut *element };
        unsafe { Pin::new_unchecked(element) }
    }

    fn take_nbl(self: Pin<&mut Self>) -> PNET_BUFFER_LIST {
        let this = unsafe { self.get_unchecked_mut() };
        core::mem::replace(&mut this.nbl, core::ptr::null_mut())
    }
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

pub(crate) fn flush_receive_queue(open_context: &OpenContext) {
    // FIXME: Fill in once we're adding NBLs to the recv list
    log::error!("unimplemented");
}
