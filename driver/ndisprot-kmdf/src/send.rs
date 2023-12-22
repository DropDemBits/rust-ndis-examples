use core::sync::atomic::{AtomicU32, Ordering};

use wdf_kmdf::{file_object::FileObject, object::GeneralObject, raw};
use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_rs::log;
use windows_kernel_sys::{
    result::STATUS, Error, MdlMappingNoExecute, MM_PAGE_PRIORITY, MODE, NDIS_HANDLE,
    NET_BUFFER_LIST, PNET_BUFFER_LIST, ULONG,
};

use crate::{
    ndisbind::ProtocolBindingContext, EthHeader, FileObjectContext, MACAddr, OpenContext,
    OpenContextFlags, OpenState,
};

/// `ProtocolReserved` in sent packets. We save a handle to the io request
/// which generated the send.
///
pub(crate) struct NprotSendNblRsvd {
    request: WDFREQUEST,
    /// Used to determine when to free the packet back to its pool.
    /// It is used to synchronize between a thread completing a send and a thread attempting
    /// to cancel a send.
    ref_count: AtomicU32,
    /// Which open object this is bound to
    // FIXME: Like a `Ref<...>` in `NprotRecvNblRsvd`
    open_object: GeneralObject<OpenContext>,
}

impl NprotSendNblRsvd {
    unsafe fn init_send_nbl(
        nbl: PNET_BUFFER_LIST,
        request: WDFREQUEST,
        open_object: GeneralObject<OpenContext>,
    ) {
        let this = NET_BUFFER_LIST::context_data_start(nbl).cast::<Self>();
        this.write(Self {
            request,
            ref_count: AtomicU32::new(1),
            open_object,
        })
    }

    unsafe fn from_send_nbl<'a>(nbl: PNET_BUFFER_LIST) -> &'a Self {
        unsafe { &*NET_BUFFER_LIST::context_data_start(nbl).cast::<Self>() }
    }

    unsafe fn request(this: PNET_BUFFER_LIST) -> WDFREQUEST {
        unsafe { Self::from_send_nbl(this).request }
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
    nbl: PNET_BUFFER_LIST,
    length: ULONG,
}

wdf_kmdf::impl_context_space!(RequestContext);

pub(crate) unsafe extern "C" fn ndisprot_evt_io_write(
    Queue: WDFQUEUE,         // in
    mut Request: WDFREQUEST, // in
    Length: usize,           // in
) {
    let file_object = unsafe { raw::WdfRequestGetFileObject(Request) };
    let file_object = FileObject::<FileObjectContext>::wrap(file_object);

    let open_object = file_object.get_context().open_context();
    let mut nt_status;

    'out: {
        // Create a context to track the length of transfer and NDIS nbl associated with this request
        let mut attributes = wdf_kmdf::object::default_object_attributes::<RequestContext>();
        let mut req_context = core::ptr::null_mut();

        nt_status = unsafe {
            Error::to_err(raw::WdfObjectAllocateContext(
                Request.cast(),
                &mut attributes,
                &mut req_context,
            ))
        };
        if let Err(err) = nt_status {
            nt_status = Err(STATUS::INVALID_HANDLE.into());
            break 'out;
        }

        let Ok(Length) = ULONG::try_from(Length) else {
            nt_status = Err(STATUS::INVALID_BUFFER_SIZE.into());
            break 'out;
        };

        let Some(open_object) = open_object else {
            log::warn!("write: FileObject {file_object:x?} not yet associated with a device");
            nt_status = Err(STATUS::INVALID_HANDLE.into());
            break 'out;
        };

        let mut mdl = core::ptr::null_mut();
        nt_status = unsafe { Error::to_err(raw::WdfRequestRetrieveInputWdmMdl(Request, &mut mdl)) };
        if let Err(err) = nt_status {
            log::error!("write: WdfRequestRetrieveInputWdmMdl failed {err:x?}");
            break 'out;
        }

        // Try to get a virtual address for the MDL
        let eth_header = unsafe {
            windows_kernel_sys::MmGetSystemAddressForMdlSafe(
                mdl,
                MM_PAGE_PRIORITY::NormalPagePriority.0 as u32 | MdlMappingNoExecute as u32,
            )
        };
        if eth_header.is_null() {
            log::error!("write: MmGetSystemAddr failed for Request {Request:x?}, MDL {mdl:x?}");
            nt_status = Err(STATUS::INSUFFICIENT_RESOURCES.into());
            break 'out;
        }

        // Sanity-check the length
        let data_length = unsafe { windows_kernel_sys::MmGetMdlByteCount(mdl) } as usize;
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

        if unsafe { raw::WdfRequestGetRequestorMode(Request) } == MODE::UserMode.0 as i8
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

        assert!(!open_context.send_nbl_pool.is_null());
        let nbl = unsafe {
            windows_kernel_sys::NdisAllocateNetBufferAndNetBufferList(
                open_context.send_nbl_pool,
                // Request control offset delta
                core::mem::size_of::<NprotSendNblRsvd>() as u16,
                // backfill size
                0,
                mdl,
                // data offset
                0,
                data_length as u64,
            )
        };
        if nbl.is_null() {
            log::error!("write: open {open_object:x?}, failed to alloc send net buffer list");
            nt_status = Err(STATUS::INSUFFICIENT_RESOURCES.into());
            break 'out;
        }
        open_context
            .pended_send_count
            .fetch_add(1, Ordering::Relaxed);

        let send_open_object = open_object.clone_ref();

        // Initialize the send nbl context, setting the initial ref count to 1
        // Note that currently this sample code does not implement the cancellation
        // logic. An actual driver would likely use `WdfRequestMarkCancelableEx` to
        // do this logic.
        unsafe { NprotSendNblRsvd::init_send_nbl(nbl, Request, send_open_object); }

        // We create a cancel id on each send NBL (which gets associated with a
        // write request) and save the NBL pointer in the request context. If the
        // request gets cancelled, we use `NdisCancelSendNetBufferLists()` to cancel
        // the NBL.
        //
        // Note that this code currently does not implement the cancellation logic,
        // but an actual driver might use `WdfRequestMarkCancelableEx` to implement
        // it.
        let cancel_id = open_context.driver.get_context().cancel_id_gen.next();
        NET_BUFFER_LIST::set_cancel_id(nbl, cancel_id);

        // note: is an irrefutable pattern
        let _ = wdf_kmdf::object::context_pin_init::<RequestContext, _, _, _>(&mut Request, |_| {
            Ok(pinned_init::init! {
                RequestContext {
                    nbl,
                    length: Length,
                }
            })
        });

        drop(inner);

        nt_status = Err(STATUS::PENDING.into());

        unsafe { (*nbl).SourceHandle = open_context.binding_handle.load() };
        assert!((*mdl).Next.is_null(), "should only be one mdl fragment");

        let send_flags = windows_kernel_sys::NDIS_SEND_FLAGS_CHECK_FOR_LOOPBACK;
        unsafe {
            windows_kernel_sys::NdisSendNetBufferLists(
                open_context.binding_handle.load(),
                nbl,
                windows_kernel_sys::NDIS_DEFAULT_PORT_NUMBER,
                send_flags,
            );
        }
    }

    if let Err(err) = nt_status {
        if err.0 != STATUS::PENDING {
            unsafe { raw::WdfRequestComplete(Request, err.0.to_u32()) }
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

    let open_object = protocol_binding_context.open_context.clone_ref();
    let open_context = open_object.get_context();

    let dispatch_level =
        send_complete_flags & windows_kernel_sys::NDIS_SEND_COMPLETE_FLAGS_DISPATCH_LEVEL != 0;

    // note: we assume that we'll always have at least 1 nbl in the nbl_queue
    // note: NblQueue terminology comes from ndis-driver-library
    let mut nbl_queue = core::iter::successors(Some(net_buffer_list), |nbl| {
        match unsafe { NET_BUFFER_LIST::next_nbl(*nbl) } {
            nbl if nbl.is_null() => None,
            nbl => Some(nbl),
        }
    });

    loop {
        {
            let Some(nbl) = nbl_queue.next() else {
                break;
            };
            let request = unsafe { NprotSendNblRsvd::request(nbl) };
            let req_context = wdf_kmdf::object::get_context::<RequestContext>(&request)
                .expect("context space should be initialized");
            let completion_status = unsafe { NET_BUFFER_LIST::status(nbl) };
            let length = req_context.length;

            log::info!("send_complete: nbl {nbl:x?}/request {request:x?}/length {length} completed with status {completion_status:x?}");

            // We are done with this packet
            unsafe { NprotSendNblRsvd::deref_send_nbl(nbl) };
            // note: original code null's out `nbl`

            if completion_status == STATUS::SUCCESS.to_u32() {
                unsafe {
                    raw::WdfRequestCompleteWithInformation(
                        request,
                        STATUS::SUCCESS.to_u32(),
                        length.into(),
                    )
                };
            } else {
                unsafe {
                    raw::WdfRequestCompleteWithInformation(
                        request,
                        STATUS::UNSUCCESSFUL.to_u32(),
                        0,
                    )
                };
            }
        }

        let mut inner = open_context.inner.lock();
        let pended_send_count = open_context
            .pended_send_count
            .fetch_sub(1, Ordering::Relaxed);

        if inner.flags.contains(OpenContextFlags::BIND_CLOSING) && pended_send_count == 1 {
            // only pending send finished, signal the closing event
            assert!(!inner.closing_event.is_null());
            let closing_event = unsafe { &*inner.closing_event };

            closing_event.signal();
            inner.closing_event = core::ptr::null_mut();
        }
    }
}
