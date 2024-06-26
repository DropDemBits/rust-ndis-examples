use core::sync::atomic::{AtomicU32, Ordering};

use ndis_rs::{Mdl, MdlMappingFlags, NblChain, NetBufferList};
use wdf_kmdf::{
    handle::{HandleWrapper, HasContext, Ref, WithContext},
    object::GeneralObject,
    raw,
    request::FileRequest,
};
use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_rs::log;
use windows_kernel_sys::{
    result::STATUS, Error, MODE, NDIS_HANDLE, NET_BUFFER_LIST, PNET_BUFFER_LIST, ULONG,
};

use crate::{
    ndisbind::ProtocolBindingContext, EthHeader, FileObjectContext, MACAddr, OpenContext,
    OpenContextFlags, OpenState,
};

/// `ProtocolReserved` in sent packets. We save a handle to the io request
/// which generated the send.
#[repr(C)]
pub(crate) struct NprotSendNblRsvd {
    request: WithContext<FileRequest<FileObjectContext>, RequestContext>,
    /// Which open object this is bound to
    _open_object: Ref<GeneralObject<OpenContext>, { wdf_kmdf::tag!(b"Send") }>,
    /// Used to determine when to free the packet back to its pool.
    /// It is used to synchronize between a thread completing a send and a thread attempting
    /// to cancel a send.
    ref_count: AtomicU32,
}

impl NprotSendNblRsvd {
    unsafe fn init_send_nbl(
        nbl: PNET_BUFFER_LIST,
        request: WithContext<FileRequest<FileObjectContext>, RequestContext>,
        open_object: Ref<GeneralObject<OpenContext>, { wdf_kmdf::tag!(b"Send") }>,
    ) {
        let this = NET_BUFFER_LIST::context_data_start(nbl).cast::<Self>();
        this.write(Self {
            request,
            ref_count: AtomicU32::new(1),
            _open_object: open_object,
        })
    }

    unsafe fn from_send_nbl<'a>(nbl: PNET_BUFFER_LIST) -> &'a Self {
        unsafe { &*NET_BUFFER_LIST::context_data_start(nbl).cast::<Self>() }
    }

    unsafe fn request<'a>(
        this: PNET_BUFFER_LIST,
    ) -> &'a WithContext<FileRequest<FileObjectContext>, RequestContext> {
        unsafe { &Self::from_send_nbl(this).request }
    }

    unsafe fn deref_send_nbl(nbl: PNET_BUFFER_LIST) {
        let ref_count = {
            let this = unsafe { Self::from_send_nbl(nbl) };
            this.ref_count.fetch_sub(1, Ordering::Release)
        };

        if ref_count == 1 {
            // this was the only ref, so we can deallocate it
            let this = NET_BUFFER_LIST::context_data_start(nbl).cast::<Self>();
            // drop the open object ref
            unsafe { core::ptr::drop_in_place(this) };
            unsafe { windows_kernel_sys::NdisFreeNetBufferList(nbl) };
        }
    }
}

struct RequestContext {
    //nbl: AtomicCell<PNET_BUFFER_LIST>,
    length: ULONG,
}

wdf_kmdf::impl_context_space!(RequestContext);

pub(crate) unsafe extern "C" fn ndisprot_evt_io_write(
    _Queue: WDFQUEUE,    // in
    Request: WDFREQUEST, // in
    Length: usize,       // in
) {
    // FIXME: Would be nice if we didn't have to wrap the file request in a new context
    // We borrow this handle from the framework
    let Request = unsafe { FileRequest::<FileObjectContext>::wrap_raw(Request.cast()) };

    let file_object = Request.file_object();

    let open_object = file_object.get_context().open_context();
    let mut nt_status;

    // Create a context to track the length of transfer associated with this request
    // The NBL holds the association to the request so we don't need the request to do it,
    // and we don't need it for cancellation either since it just takes in a cancel id
    let Request = {
        let Ok(Length) = ULONG::try_from(Length) else {
            unsafe {
                raw::WdfRequestComplete(Request.raw_handle(), STATUS::INVALID_BUFFER_SIZE.into())
            }
            return;
        };

        match WithContext::<_, RequestContext>::create(Request, |_| {
            Ok(pinned_init::init! {
                RequestContext {
                    //nbl: AtomicCell::new(core::ptr::null_mut()),
                    length: Length,
                }
            })
        }) {
            Ok(context) => context,
            Err((_err, request)) => {
                unsafe {
                    raw::WdfRequestComplete(request.raw_handle(), STATUS::INVALID_HANDLE.to_u32())
                }
                return;
            }
        }
    };

    'out: {
        let Some(open_object) = open_object else {
            log::warn!("write: FileObject {file_object:x?} not yet associated with a device");
            nt_status = Err(STATUS::INVALID_HANDLE.into());
            break 'out;
        };

        let mut mdl = core::ptr::null_mut();
        nt_status = unsafe {
            Error::to_err(raw::WdfRequestRetrieveInputWdmMdl(
                Request.raw_handle(),
                &mut mdl,
            ))
        };
        if let Err(err) = nt_status {
            log::error!("write: WdfRequestRetrieveInputWdmMdl failed {err:x?}");
            break 'out;
        }
        let mdl = unsafe { Mdl::ptr_cast_from_raw(mdl).unwrap_unchecked().as_mut() };

        // Try to get a virtual address for the MDL
        let Some(eth_header) =
            mdl.map_mdl(MdlMappingFlags::NormalPagePriority | MdlMappingFlags::NoExecute)
        else {
            log::error!("write: MmGetSystemAddr failed for Request {Request:x?}, MDL {mdl:x?}");
            nt_status = Err(STATUS::INSUFFICIENT_RESOURCES.into());
            break 'out;
        };

        // Sanity-check the length
        let data_length = mdl.byte_count() as usize;
        if data_length < core::mem::size_of::<EthHeader>() {
            log::warn!("write: too small to be a valid packet ({data_length} bytes)");
            nt_status = Err(STATUS::BUFFER_TOO_SMALL.into());
            break 'out;
        }

        let open_context = open_object.get_context();
        let max_frame_size = open_context.max_frame_size.load();
        let max_eth_frame_size =
            (max_frame_size as usize).saturating_add(core::mem::size_of::<EthHeader>());
        if data_length > max_eth_frame_size {
            log::warn!("write: open {open_object:x?}: data length {data_length} larger than max frame size {max_eth_frame_size}");
            nt_status = Err(STATUS::INVALID_BUFFER_SIZE.into());
            break 'out;
        }

        // To prevent applications from sending packets with spoofed MAC
        // addresses, we'll check to make sure that the address in the packet
        // is the same as the current address of the NIC.
        let src_addr = eth_header
            .byte_add(core::mem::offset_of!(EthHeader, src_addr))
            .cast::<MACAddr>()
            .read_unaligned();

        if unsafe { raw::WdfRequestGetRequestorMode(Request.raw_handle()) }
            == MODE::UserMode.0 as i8
            && src_addr != open_context.current_address
        {
            log::warn!("write: failing with invalid source address");
            nt_status = Err(STATUS::INVALID_PARAMETER.into());
            break 'out;
        }

        let inner = open_context.inner.lock();
        if !inner.flags.contains(OpenContextFlags::BIND_ACTIVE) {
            log::error!("write: open {open_object:x?} is not bound or in low power state");
            nt_status = Err(STATUS::INVALID_HANDLE.into());
            break 'out;
        }

        if matches!(inner.state, OpenState::Pausing | OpenState::Paused) {
            log::info!("write: device is paused");
            nt_status = Err(STATUS::UNSUCCESSFUL.into());
            break 'out;
        }

        assert!(!open_context.send_nbl_pool.0.is_null());
        let Some(mut nbl) = NetBufferList::ptr_cast_from_raw(unsafe {
            windows_kernel_sys::NdisAllocateNetBufferAndNetBufferList(
                open_context.send_nbl_pool.0,
                // Request control offset delta
                core::mem::size_of::<NprotSendNblRsvd>() as u16,
                // backfill size
                0,
                Mdl::as_ptr(mdl),
                // data offset
                0,
                data_length as u64,
            )
        }) else {
            log::error!("write: open {open_object:x?}, failed to alloc send net buffer list");
            nt_status = Err(STATUS::INSUFFICIENT_RESOURCES.into());
            break 'out;
        };
        let nbl = unsafe { nbl.as_mut() };
        open_context
            .pended_send_count
            .fetch_add(1, Ordering::Relaxed);

        let send_open_object = wdf_kmdf::clone!(tag: b"Send", open_object);

        // Clone the request so that we can complete it in the event of an error
        let err_request = wdf_kmdf::clone!(ref &*Request);

        // Initialize the send nbl context, setting the initial ref count to 1
        // Note that currently this sample code does not implement the cancellation
        // logic. An actual driver would likely use `WdfRequestMarkCancelableEx` to
        // do this logic.
        unsafe {
            NprotSendNblRsvd::init_send_nbl(nbl.as_ptr(), Request, send_open_object);
        }

        // We create a cancel id on each send NBL (which gets associated with a
        // write request) and save the NBL pointer in the request context. If the
        // request gets cancelled, we use `NdisCancelSendNetBufferLists()` to cancel
        // the NBL.
        //
        // Note that this code currently does not implement the cancellation logic,
        // but an actual driver might use `WdfRequestMarkCancelableEx` to implement
        // it.
        let cancel_id = open_context.driver.get_context().cancel_id_gen.next();
        nbl.set_cancel_id(cancel_id);

        drop(inner);

        nt_status = Err(STATUS::PENDING.into());

        *nbl.source_handle_mut() = open_context.binding_handle.0.load();

        assert_eq!(
            nbl.nb_chain_mut()
                .first_mut()
                .map(|nb| nb.mdl_chain_mut().len()),
            Some(1),
            "should only be one mdl fragment"
        );

        let send_flags = windows_kernel_sys::NDIS_SEND_FLAGS_CHECK_FOR_LOOPBACK;
        unsafe {
            windows_kernel_sys::NdisSendNetBufferLists(
                open_context.binding_handle.0.load(),
                NetBufferList::as_ptr(nbl),
                windows_kernel_sys::NDIS_DEFAULT_PORT_NUMBER,
                send_flags,
            );
        }

        if let Err(err) = nt_status {
            if err.0 != STATUS::PENDING {
                unsafe { raw::WdfRequestComplete(err_request.raw_handle(), err.0.to_u32()) }
            }
        }

        return;
    }

    if let Err(err) = nt_status {
        if err.0 != STATUS::PENDING {
            unsafe { raw::WdfRequestComplete(Request.raw_handle(), err.0.to_u32()) }
        }
    }
}

pub(crate) unsafe extern "C" fn send_complete(
    protocol_binding_context: NDIS_HANDLE,
    net_buffer_list: PNET_BUFFER_LIST,
    send_complete_flags: ULONG,
) {
    let protocol_binding_context =
        unsafe { &*protocol_binding_context.cast::<ProtocolBindingContext>() };

    let open_object = wdf_kmdf::clone!(tag: b"SFin", protocol_binding_context.open_context);
    let open_context = open_object.get_context();

    let _dispatch_level =
        send_complete_flags & windows_kernel_sys::NDIS_SEND_COMPLETE_FLAGS_DISPATCH_LEVEL != 0;

    // SAFETY: We assume NDIS gives us a valid chain of `NET_BUFFER_LIST`s,
    // `NET_BUFFER_LIST_CONTEXT`s, `NET_BUFFER`s, and `MDL`s.
    let mut chain = unsafe { NblChain::from_raw(net_buffer_list) };

    loop {
        {
            let Some(nbl) = chain.pop_front() else {
                break;
            };
            let nbl = NetBufferList::ptr_cast_to_raw(Some(nbl.into()));
            let request = unsafe { NprotSendNblRsvd::request(nbl) };
            let req_context = request.get_context();
            let completion_status = unsafe { NET_BUFFER_LIST::status(nbl) };
            let length = req_context.length;

            log::info!("send_complete: nbl {nbl:x?}/request {request:x?}/length {length} completed with status {completion_status:x?}");

            // We are done with this packet
            unsafe { NprotSendNblRsvd::deref_send_nbl(nbl) };
            // note: original code null's out `nbl`

            if completion_status == STATUS::SUCCESS.to_u32() {
                unsafe {
                    raw::WdfRequestCompleteWithInformation(
                        request.raw_handle(),
                        STATUS::SUCCESS.to_u32(),
                        length.into(),
                    )
                };
            } else {
                unsafe {
                    raw::WdfRequestCompleteWithInformation(
                        request.raw_handle(),
                        STATUS::UNSUCCESSFUL.to_u32(),
                        0,
                    )
                };
            }
        }

        // let inner = open_context.inner.lock();
        let _pended_send_count = open_context
            .pended_send_count
            .fetch_sub(1, Ordering::Relaxed);
    }
}
