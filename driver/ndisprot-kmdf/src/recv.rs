use core::{mem::MaybeUninit, pin::Pin, sync::atomic::Ordering};

use ndis_rs::{NblChain, NetBufferList};
use wdf_kmdf::{
    driver::Driver,
    file_object::FileObject,
    handle::{HandleWrapper, HasContext, Ref},
    object::GeneralObject,
    raw,
    request::FileRequest,
};
use wdf_kmdf_sys::{WDFQUEUE, WDFREQUEST};
use windows_kernel_rs::log;
use windows_kernel_sys::{
    result::STATUS, Error, MdlMappingNoExecute, MM_PAGE_PRIORITY, NDIS_HANDLE, NDIS_PORT_NUMBER,
    NET_DEVICE_POWER_STATE, PMDL, PNET_BUFFER_LIST, PUCHAR, PVOID, ULONG,
};

use crate::{
    ndisbind::ProtocolBindingContext, EthHeader, FileObjectContext, NdisProt, OpenContext,
    OpenContextFlags, OpenState, TaggedEthHeader, NPROT_FLAGS_ALLOCATED_NBL,
};

/// `ProtocolReserved` in received packets: we link these packets up in a queue waiting for read io requests.
/// We stuff this inside of NET_BUFFER_LIST.ProtocolReserved
#[repr(C)]
pub(crate) struct RecvNblRsvd {
    _open_object: Ref<GeneralObject<OpenContext>>,
}

// `ProtocolReserved` is a `[PVOID; 4]`
static_assertions::const_assert!(
    core::mem::size_of::<RecvNblRsvd>() <= core::mem::size_of::<[PVOID; 4]>(),
);

impl RecvNblRsvd {
    fn from_nbl_raw(nbl: &mut NetBufferList) -> &mut MaybeUninit<Self> {
        let reserved_area = core::ptr::addr_of_mut!(*nbl.protocol_reserved_area_mut());
        unsafe { &mut *reserved_area.cast() }
    }

    /// # Safety
    ///
    /// Must not initialize the area more than once.
    unsafe fn init_recv_nbl(
        net_buffer_list: &mut NetBufferList,
        open_object: Ref<GeneralObject<OpenContext>>,
    ) {
        let element = Self::from_nbl_raw(net_buffer_list);
        element.write(RecvNblRsvd {
            _open_object: open_object,
        });
    }

    /// # Safety
    ///
    /// Assumes that the area has been initialized.
    unsafe fn drop_init_recv_nbl(net_buffer_list: &mut NetBufferList) {
        let element = Self::from_nbl_raw(net_buffer_list);
        MaybeUninit::assume_init_drop(element);
    }
}

/// Called when the framework receives IRP_MJ_READ requests from usermode.
pub(crate) unsafe extern "C" fn ndisprot_evt_io_read(
    _Queue: WDFQUEUE,    // in
    Request: WDFREQUEST, // in
    _Length: usize,      // in
) {
    let nt_status;

    let file_object =
        FileObject::<FileObjectContext>::wrap(unsafe { raw::WdfRequestGetFileObject(Request) });

    let open_object = file_object.get_context().open_context();

    'out: {
        // validate!
        let Some(open_object) = open_object else {
            log::error!("Read: No OpenContext on FileObject {file_object:#x?}");
            nt_status = Err(STATUS::INVALID_HANDLE.into());
            break 'out;
        };

        let open_context = open_object.get_context();

        if !open_context
            .inner
            .lock()
            .flags
            .contains(OpenContextFlags::BIND_ACTIVE)
        {
            nt_status = Err(STATUS::INVALID_HANDLE.into());
            break 'out;
        }

        // Translation note: `ndisprot` does increment the pended read count in the IRP_MR_READ handler,
        // but `ndisprot-kmdf` does not (likely as an oversight). It still works since nothing waits on
        // all reads to finish, but just in case we do wait on having all of the reads finish.
        open_context
            .pended_read_count
            .fetch_add(1, Ordering::Relaxed);

        // Forward this request to the pending read queue.
        // The framework manages cancellation of requests which are on the queue, so we don't have to worry about that.
        let request = unsafe { FileRequest::wrap_raw(Request.cast()) };
        nt_status = open_context.read_queue.forward_to_queue(request);
    }

    if let Err(err) = nt_status {
        unsafe { raw::WdfRequestCompleteWithInformation(Request, err.0.to_u32(), 0) };
    }
}

/// Called every time the queue becomes non-empty (i.e. the number of requests goes from 0 to 1)
pub(crate) unsafe extern "C" fn ndisprot_evt_notify_read_queue(
    _Queue: WDFQUEUE,
    Context: wdf_kmdf_sys::WDFCONTEXT,
) {
    let open_object = &GeneralObject::<OpenContext>::wrap(Context);

    service_reads(open_object);
}

/// Copies received data into user buffers and completes read requests
fn service_reads(open_object: &GeneralObject<OpenContext>) {
    let open_context = open_object.get_context();

    log::trace!(
        "ServiceReads: open {:#x?}/{:#x?}",
        open_object.raw_handle(),
        open_context.inner.lock().flags
    );

    let mut recv_queue = open_context.recv_queue.lock();

    while !recv_queue.as_ref().is_empty() {
        // Get the first pended read request
        let request = match open_context.read_queue.retrive_next_request() {
            Some(Ok(request)) => request,
            Some(Err(err)) => {
                debug_assert!(false, "WdfIoQueueRetrieveRequest failed {:x?}", err.0);
                break;
            }
            None => break,
        };

        // Request successfully dequed, now need to take care of completing the request
        // in case of an error since we own it now
        let request_status = 'err: {
            // FIXME: Could probably use WdfMemoryCopyFromBuffer for this instead of using Mdls
            let mut mdl = core::ptr::null_mut();
            let nt_status = Error::to_err(unsafe {
                raw::WdfRequestRetrieveOutputWdmMdl(request.raw_handle(), &mut mdl)
            });
            if let Err(err) = nt_status {
                log::error!("read: WdfRequestRetrieveOutputWdfMdl {err:#x?}");
                break 'err Err::<_, Error>(STATUS::UNSUCCESSFUL.into());
            }

            let mut dst = unsafe {
                windows_kernel_sys::MmGetSystemAddressForMdlSafe(
                    mdl,
                    MM_PAGE_PRIORITY::NormalPagePriority.0 as u32 | MdlMappingNoExecute as u32,
                )
            };
            if dst.is_null() {
                log::error!(
                    "read: MmGetSystemAddress failed for Request {request:#x?}, MDL {mdl:#x?}"
                );
                // nt_status = Err(STATUS::INSUFFICIENT_RESOURCES.into());
                break 'err Err(STATUS::INSUFFICIENT_RESOURCES.into());
            }

            // Get the first queued receive packet
            let Some(recv_nbl_entry) = recv_queue.as_mut().dequeue() else {
                log::error!("service reads: {open_object:#x?} recv queue is empty");
                // nt_status = Err(STATUS::NO_MORE_ENTRIES.into());
                break 'err Err(STATUS::NO_MORE_ENTRIES.into());
            };
            let recv_nbl = recv_nbl_entry;
            let first_nb = recv_nbl
                .nb_chain_mut()
                .first_mut()
                .expect("nbl should have at least one nb");

            // assert!(!recv_nbl.is_null());
            // assert!(!first_nb.is_null());
            let mut bytes_remaining = unsafe { windows_kernel_sys::MmGetMdlByteCount(mdl) };
            let total_length = bytes_remaining;

            mdl = first_nb.mdl_chain();

            // Copy the data in the received packet into the buffer provided by the client.
            // if there's more data in the receive packet than can be written into the buffer,
            // we copy as much as we can, and then discard the remaining data. The request
            // completes even if we did a partial copy.

            while bytes_remaining != 0 && !mdl.is_null() {
                let mut src = core::ptr::null_mut();
                let mut bytes_available = 0;
                unsafe {
                    windows_kernel_sys::NdisQueryMdl(
                        mdl,
                        &mut src,
                        &mut bytes_available,
                        MM_PAGE_PRIORITY::NormalPagePriority.0 as u32 | MdlMappingNoExecute as u32,
                    )
                };
                if src.is_null() {
                    log::error!(
                    "service_reads: open {open_object:#x?}, NdisQueryMdl failed for MDL {mdl:#x?}"
                );
                    break;
                }

                if bytes_available != 0 {
                    let bytes_to_copy = bytes_available.min(bytes_remaining);

                    unsafe {
                        dst.cast::<u8>()
                            .copy_from_nonoverlapping(src.cast(), bytes_to_copy as usize)
                    };
                    bytes_remaining -= bytes_to_copy;
                    unsafe {
                        dst = dst.byte_add(bytes_to_copy as usize);
                    }
                }

                unsafe { windows_kernel_sys::NdisGetNextMdl(mdl, &mut mdl) };
            }

            // Complete the request
            let bytes_copied = total_length - bytes_remaining;

            log::info!("service_reads: open {open_object:#x?}, request {request:#x?} completed with {bytes_copied} bytes");

            unsafe { RecvNblRsvd::drop_init_recv_nbl(recv_nbl) };
            free_receive_net_buffer_list(open_context, recv_nbl, false);
            Ok(bytes_copied)
        };

        match request_status {
            Ok(bytes_copied) => {
                unsafe {
                    raw::WdfRequestCompleteWithInformation(
                        request.raw_handle(),
                        STATUS::SUCCESS.to_u32(),
                        bytes_copied.into(),
                    )
                };
            }
            Err(err) => {
                unsafe {
                    raw::WdfRequestCompleteWithInformation(request.raw_handle(), err.0.to_u32(), 0)
                };
            }
        }

        open_context
            .pended_read_count
            .fetch_sub(1, Ordering::Relaxed);
    }
}

/// Protocol entry point called by NDIS when we receive a net buffer list
///
/// We only copy the nbl if the miniport does not allow us to (i.e. if the miniport does not have enough resources),
/// otherwise we borrow it from the miniport.
pub(crate) unsafe extern "C" fn receive_net_buffer_lists(
    protocol_binding_context: NDIS_HANDLE,
    net_buffer_lists: PNET_BUFFER_LIST,
    _port_number: NDIS_PORT_NUMBER,
    _number_of_net_buffer_lists: ULONG,
    receive_flags: ULONG,
) {
    let protocol_binding_context =
        unsafe { &*protocol_binding_context.cast::<ProtocolBindingContext>() };
    let receive_flags = ReceiveFlags::from_bits_truncate(receive_flags);
    let mut return_flags = ReturnFlags::empty();

    let open_object = protocol_binding_context.open_context.clone_ref();
    let open_context = open_object.get_context();

    let Some(driver) = (unsafe { wdf_kmdf::raw::WdfGetDriver() }) else {
        log::error!(
            "receive_net_buffer_lists: ctx {protocol_binding_context:x?}: driver not loaded"
        );
        // no leak!!
        if receive_flags.can_pend() {
            // NDIS doesn't automatically reclaim ownership when we return,
            // so we need to manually return the nbls.
            unsafe {
                windows_kernel_sys::NdisReturnNetBufferLists(
                    open_context.binding_handle.0.load(),
                    net_buffer_lists,
                    return_flags.bits(),
                )
            };
        }
        return;
    };
    let driver = unsafe { Driver::<NdisProt>::wrap(driver) };
    let globals = driver.get_context();

    if receive_flags.contains(ReceiveFlags::DISPATCH_LEVEL) {
        return_flags |= ReturnFlags::DISPATCH_LEVEL;
    }

    // Can the binding receive net buffer lists?
    if matches!(
        open_context.inner.lock().state,
        OpenState::Pausing | OpenState::Paused
    ) {
        // ???: What exactly does it mean when `can_pend` is true (i.e. ReceiveFlags::RESOURCES is unset)?
        if receive_flags.can_pend() {
            // NDIS doesn't automatically reclaim ownership when we return,
            // so we need to manually return the nbls.
            unsafe {
                windows_kernel_sys::NdisReturnNetBufferLists(
                    open_context.binding_handle.0.load(),
                    net_buffer_lists,
                    return_flags.bits(),
                )
            };
        }

        // Since the binding isn't accepting any nbls, we're done
        return;
    }

    // SAFETY: We assume NDIS gives us a valid chain of `NET_BUFFER_LIST`s,
    // `NET_BUFFER_LIST_CONTEXT`s, `NET_BUFFER`s, and `MDL`s.
    let chain = unsafe { NblChain::from_raw(net_buffer_lists) };

    let [ignored_nbls, accepted_nbls] = chain.partition_filtered(|nbl| {
        // Note: since we're not using NDIS 6.30, the bottom two bits are reserved for NDIS
        *nbl.reserved_flags_mut() &= !(windows_kernel_sys::NBL_FLAGS_PROTOCOL_RESERVED & !0x3);

        // Get the first mdl and data length in the list
        let Some(first_nb) = nbl.nb_chain().first() else {
            return false;
        };

        let (eth_header, buffer_length) = {
            let (mdl, offset) = first_nb.current_mdl_offset();
            let mut buffer_length = 0;
            let mut eth_header = core::ptr::null_mut();

            assert!(!mdl.is_null());

            if !mdl.is_null() {
                let mut length = 0;
                windows_kernel_sys::NdisQueryMdl(
                    mdl,
                    &mut eth_header,
                    &mut length,
                    MM_PAGE_PRIORITY::NormalPagePriority.0 as u32 | MdlMappingNoExecute as u32,
                );
                buffer_length = length as usize;
            }

            if eth_header.is_null() {
                // System is low on resources, ignore this nbl
                return false;
            }

            if buffer_length == 0 {
                // Can't inspect the header if it's 0 sized
                return false;
            }

            assert!(buffer_length > offset);
            buffer_length = buffer_length.saturating_sub(offset);
            eth_header = unsafe { eth_header.add(offset) };
            (eth_header, buffer_length)
        };

        if buffer_length < core::mem::size_of::<EthHeader>() {
            log::warn!("receive_net_buffer_list: open {open_object:#x?}, runt nbl {nbl:#x?}, first buffer length {buffer_length}");
            return false;
        }

        let ether_type = {
            let eth_header = eth_header.cast::<EthHeader>();

            let mut ether_type = unsafe {
                eth_header
                    .byte_add(core::mem::offset_of!(EthHeader, eth_type))
                    .cast::<u16>()
                    .read_unaligned()
            };

            if ether_type == crate::NPROT_8021P_TAG_TYPE {
                // Is a tagged ethernet packet, so need to use tagged version of the ethernet header
                if (buffer_length as usize) < core::mem::size_of::<TaggedEthHeader>() {
                    return false;
                }

                ether_type = eth_header
                    .byte_add(core::mem::offset_of!(TaggedEthHeader, eth_type))
                    .cast::<u16>()
                    .read_unaligned();
            }

            ether_type
        };

        if ether_type != globals.eth_type {
            return false;
        }

        log::debug!(
            "receive_net_buffer_list: open {open_object:#x?}, interesting nbl {nbl:#x?}"
        );
        return true;
    });

    // Take care of returning the ignored nbls first, if we need to
    if receive_flags.can_pend() {
        // The miniport transferred ownership of the nbls to us, so we need to
        // give the nbls back to the miniport
        let (head, _) = ignored_nbls.into_raw_parts();

        unsafe {
            windows_kernel_sys::NdisReturnNetBufferLists(
                open_context.binding_handle.0.load(),
                head,
                return_flags.bits(),
            );
        }
    } else {
        // The miniport is running low on resources and will regain ownership
        // of the nbls after we return.
        //
        // Presumably the miniport has its own way of tracking the nbls it owns,
        // so we can safely forget the ignored nbl queue.
        core::mem::forget(ignored_nbls);
    }

    for nbl in accepted_nbls.into_iter() {
        let dispatch_level = receive_flags.at_dispatch_level();

        // If the miniport is out of resources, we need to make a copy of
        // the nbl so that the miniport can reclaim the nbl
        let nbl_to_queue = if !receive_flags.can_pend() {
            let Some(first_nb) = nbl.nb_chain().first() else {
                // `nbl` should have a `NetBuffer`, but it's okay if it doesn't
                // since we can just relinquish ownership of the nbl.
                continue;
            };
            let mdl_chain = first_nb.mdl_chain();
            let data_offset = first_nb.data_offset();
            let total_length = first_nb.data_length();

            let copy_nbl = match allocate_receive_net_buffer_list(&open_object, total_length) {
                Ok(nbl) => nbl,
                Err(err) => {
                    log::error!("receive_net_buffer_list: open {open_object:x?}, failed to alloc copy of {total_length} bytes ({err:x?})");
                    continue;
                }
            };

            let copy_mdl_chain = {
                let first_nb = nbl.nb_chain().first().unwrap();
                first_nb.mdl_chain()
            };

            // Copy the data to the new nbl
            let nt_status =
                copy_mdl_to_mdl(mdl_chain, data_offset, copy_mdl_chain, 0, total_length);

            let bytes_copied = match nt_status {
                Ok(len) => len,
                Err(_err) => {
                    log::error!("receive_net_buffer_list: open {open_object:#x?}, failed to copy the data, {total_length} bytes");
                    // Free the copy_nbl
                    free_receive_net_buffer_list(open_context, copy_nbl, dispatch_level);
                    continue;
                }
            };

            debug_assert_eq!(bytes_copied, total_length as usize);
            copy_nbl
        } else {
            // Miniport still has some resources and transferred ownership to us
            // so we can queue this nbl directly
            nbl
        };

        // Queue this nbl and service any pending read requests.
        queue_receive_net_buffer_list(&open_object, nbl_to_queue, dispatch_level);
    }
}

/// Queues a received NBL on the open context structure, removing the NBL at the head of the queue if the queue is full.
/// Afterwards, runs the queue service routine.
fn queue_receive_net_buffer_list(
    open_object: &GeneralObject<OpenContext>,
    recv_nbl: &'static mut NetBufferList,
    at_dispatch_level: bool,
) {
    let open_context = open_object.get_context();

    'out: {
        let mut recv_queue = open_context.recv_queue.lock();

        if matches!(
            open_context.inner.lock().state,
            OpenState::Pausing | OpenState::Paused
        ) {
            // Binding cannot receive the nbl right now, so let go of it.
            free_receive_net_buffer_list(open_context, recv_nbl, at_dispatch_level);
            break 'out;
        }

        // Check if the binding can receive this nbl (i.e. is active & in D0)
        if open_context
            .inner
            .lock()
            .flags
            .contains(OpenContextFlags::BIND_ACTIVE)
            && open_context.power_state.load() == NET_DEVICE_POWER_STATE::NetDeviceStateD0
        {
            // Translation Note: `init_recv_nbl` sets `nbl` to itself, since we can't
            // queue and then set `nbl` in case the queue becomes full.
            unsafe { RecvNblRsvd::init_recv_nbl(recv_nbl, open_object.clone_ref()) };

            // Queue the nbl
            // let mut entry = unsafe { RecvNblRsvd::from_nbl(recv_nbl) };

            if let Some(trimmed_entry) = recv_queue.as_mut().enqueue(recv_nbl) {
                // Queue has grown too big

                let nbl = trimmed_entry;
                drop(recv_queue);

                log::info!(
                    "queue_recv_nbl: open {open_object:x?} queue too long, discarded {nbl:x?}",
                );

                unsafe { RecvNblRsvd::drop_init_recv_nbl(nbl) };

                free_receive_net_buffer_list(open_context, nbl, at_dispatch_level);
            } else {
                log::trace!(
                    "queue_recv_nbl: open {open_object:x?}, queued nbl {:x?}, queue len {}",
                    recv_queue.as_ref().front(),
                    recv_queue.as_ref().len()
                );
                drop(recv_queue);
            }
        } else {
            // binding is going away, so the recv'd nbl needs to be freed
            free_receive_net_buffer_list(open_context, recv_nbl, at_dispatch_level);
            break 'out;
        }

        // Run the receive queue service routine now.
        service_reads(open_object);
    }
}

// Translation Note: We don't have `data_buffer` as an out parameter since the only caller (`receive_net_buffer_lists`)
// doesn't end up using the buffer.
fn allocate_receive_net_buffer_list(
    open_object: &GeneralObject<OpenContext>,
    data_length: usize,
) -> Result<&'static mut NetBufferList, Error> {
    let Ok(data_length) = u32::try_from(data_length) else {
        return Err(STATUS::INVALID_BUFFER_SIZE.into());
    };

    let open_context = open_object.get_context();
    let mut mdl = core::ptr::null_mut();
    let data_buffer;
    let layout = core::alloc::Layout::from_size_align(data_length as usize, 1).expect("whoops");

    let result = 'out: {
        data_buffer = unsafe { alloc::alloc::alloc_zeroed(layout) };
        if data_buffer.is_null() {
            log::error!(
                "alloc_recv_nbl: open {open_object:x?}, failed to alloc data buffer {data_length} bytes"
            );
            break 'out Err(STATUS::INSUFFICIENT_RESOURCES.into());
        }

        // Make an NDIS buffer
        // note: ownership of `data_buffer` transfers to the mdl
        mdl = unsafe {
            windows_kernel_sys::NdisAllocateMdl(
                open_context.binding_handle.0.load(),
                data_buffer.cast(),
                data_length as u32,
            )
        };
        if mdl.is_null() {
            log::error!(
                "alloc_recv_nbl: open {open_object:x?}, failed to alloc mdl, {data_length} bytes"
            );
            break 'out Err(STATUS::INSUFFICIENT_RESOURCES.into());
        }

        let nbl = unsafe {
            windows_kernel_sys::NdisAllocateNetBufferAndNetBufferList(
                open_context.recv_nbl_pool.0,
                0,
                0,
                mdl,
                0,
                data_length as u64,
            )
        };
        let Some(mut nbl) = (unsafe { NetBufferList::ptr_cast_from_raw(nbl) }) else {
            log::error!(
                "alloc_recv_nbl: open {open_object:x?}, failed to alloc nbl, {data_length} bytes"
            );
            break 'out Err(STATUS::INSUFFICIENT_RESOURCES.into());
        };
        let nbl = unsafe { nbl.as_mut() };

        // We allocated this NBL, so we don't want to accidentally return it to the miniport
        *nbl.reserved_flags_mut() |= NPROT_FLAGS_ALLOCATED_NBL;
        // Zero out the protocol reserved area
        nbl.protocol_reserved_area_mut().fill(core::ptr::null_mut());

        Ok(nbl)
    };

    match result {
        Ok(nbl) => Ok(nbl),
        Err(err) => {
            if !mdl.is_null() {
                unsafe { windows_kernel_sys::NdisFreeMdl(mdl) };
            }

            if !data_buffer.is_null() {
                unsafe { alloc::alloc::dealloc(data_buffer, layout) }
            }
            Err(err)
        }
    }
}

fn free_receive_net_buffer_list(
    open_context: Pin<&OpenContext>,
    net_buffer_list: &'static mut NetBufferList,
    dispatch_level: bool,
) {
    'out: {
        if net_buffer_list.reserved_flags() & NPROT_FLAGS_ALLOCATED_NBL != 0 {
            // This is a local copy
            let first_nb = net_buffer_list
                .nb_chain_mut()
                .first_mut()
                .expect("locally allocated nbl should have 1 nb");
            let mdl = first_nb.mdl_chain();
            let total_length = first_nb.data_length();
            let layout = core::alloc::Layout::from_size_align(total_length, 1).expect("whoops");

            assert!(!mdl.is_null());

            let mut copy_data = core::ptr::null_mut();
            let buffer_length = unsafe {
                let mut length = 0;
                windows_kernel_sys::NdisQueryMdl(
                    mdl,
                    &mut copy_data,
                    &mut length,
                    MM_PAGE_PRIORITY::NormalPagePriority.0 as u32 | MdlMappingNoExecute as u32,
                );
                length as usize
            };
            let copy_data = copy_data.cast::<PUCHAR>();

            assert_eq!(buffer_length, total_length);
            assert!(!copy_data.is_null()); // we would have allocated non-paged pool

            // dealloc it!
            unsafe {
                windows_kernel_sys::NdisFreeNetBufferList(NetBufferList::as_ptr(net_buffer_list))
            };
            unsafe { windows_kernel_sys::NdisFreeMdl(mdl) };
            unsafe { alloc::alloc::dealloc(copy_data.cast(), layout) };
            break 'out;
        } else {
            // The NetBufferList should be returned back to the miniport
            // Note: a standalone `&'static mut NetBufferList` should not be linked to any other `NetBufferList`

            let mut return_flags = 0;
            if dispatch_level {
                return_flags |= windows_kernel_sys::NDIS_RETURN_FLAGS_DISPATCH_LEVEL;
            }

            unsafe {
                windows_kernel_sys::NdisReturnNetBufferLists(
                    open_context.binding_handle.0.load(),
                    NetBufferList::as_ptr(net_buffer_list),
                    return_flags,
                );
            }
        }
    }
}

pub(crate) fn flush_receive_queue(open_object: &GeneralObject<OpenContext>) {
    let open_context = open_object.get_context();
    let mut recv_queue = open_context.recv_queue.lock();

    while !recv_queue.as_ref().is_empty() {
        let Some(entry) = recv_queue.as_mut().dequeue() else {
            break;
        };

        let nbl = entry;
        log::debug!("flush_receive_queue: open {open_object:x?}, nbl {nbl:x?}",);
        drop(recv_queue);

        unsafe { RecvNblRsvd::drop_init_recv_nbl(nbl) };
        free_receive_net_buffer_list(open_context.as_ref(), nbl, false);

        recv_queue = open_context.recv_queue.lock();
    }
}

/// Copies at most `copy_len` bytes from `source_mdl` to `target_mdl`.
///
/// ## Safety:
///
/// Caller must ensure that the memory backing the source and target MDL do not overlap.
///
/// IRQL: `..=DISPATCH_LEVEL`
#[must_use]
unsafe fn copy_mdl_to_mdl(
    mut source_mdl: PMDL,
    mut source_offset: usize,
    mut target_mdl: PMDL,
    mut target_offset: usize,
    copy_len: usize,
) -> Result<usize, Error> {
    let skip_to_start_mdl = |mdl: &mut PMDL, offset: &mut usize| loop {
        if mdl.is_null() {
            break;
        }

        let mdl_len = unsafe { windows_kernel_sys::MmGetMdlByteCount(*mdl) } as usize;
        let Some(next_offset) = offset.checked_sub(mdl_len) else {
            // offset is inside this mdl, so we don't need to adjust it
            break;
        };

        // Still not at the target mdl, need to go to next one
        *offset = next_offset;
        *mdl = unsafe { **mdl }.Next;
    };

    // Skip by the specified offset. Also skips any leading zero-length MDLs at the
    // front of the chains, simplifiying the below logic.
    skip_to_start_mdl(&mut source_mdl, &mut source_offset);
    skip_to_start_mdl(&mut target_mdl, &mut target_offset);

    if copy_len == 0 || source_mdl.is_null() || target_mdl.is_null() {
        // No transfer will happen
        return Ok(0);
    }

    let mut bytes_remaining = copy_len;

    // Compute the length for the first source mdl, and get a virtual address for it.
    let mut source_byte_count = (unsafe { windows_kernel_sys::MmGetMdlByteCount(source_mdl) }
        as usize)
        .saturating_sub(source_offset)
        .max(bytes_remaining);
    let mut source_va = unsafe {
        windows_kernel_sys::MmGetSystemAddressForMdlSafe(
            source_mdl,
            MM_PAGE_PRIORITY::LowPagePriority.0 as u32 | MdlMappingNoExecute as u32,
        )
    };
    if source_va.is_null() {
        return Err(STATUS::INSUFFICIENT_RESOURCES.into());
    }
    source_va = unsafe { source_va.add(source_offset) };

    // Compute the length for the first target mdl, and get a virutal address for it.
    let mut target_byte_count = (unsafe { windows_kernel_sys::MmGetMdlByteCount(target_mdl) }
        as usize)
        .saturating_sub(target_offset)
        .max(bytes_remaining);
    let mut target_va = unsafe {
        windows_kernel_sys::MmGetSystemAddressForMdlSafe(
            target_mdl,
            MM_PAGE_PRIORITY::LowPagePriority.0 as u32 | MdlMappingNoExecute as u32,
        )
    };
    if target_va.is_null() {
        return Err(STATUS::INSUFFICIENT_RESOURCES.into());
    }
    target_va = unsafe { target_va.add(target_offset) };

    // Copy data between the mdl chains until we reach the copy limit, or the end
    // of one of the mdl chains
    loop {
        // Copy the current block, and update the counts if applicable
        let copy_size = target_byte_count.min(source_byte_count);
        unsafe { target_va.copy_from_nonoverlapping(source_va, copy_size) };

        if bytes_remaining == copy_size {
            return Ok(copy_len);
        }
        bytes_remaining = bytes_remaining.saturating_sub(copy_size);

        // Advance to the next nbl in the *target* chain
        if target_byte_count == copy_size {
            loop {
                target_mdl = unsafe { (*target_mdl).Next };
                if target_mdl.is_null() {
                    return Ok(copy_len.saturating_sub(bytes_remaining));
                }
                target_byte_count =
                    unsafe { windows_kernel_sys::MmGetMdlByteCount(target_mdl) } as usize;
                if target_byte_count != 0 {
                    break;
                }
            }
            target_va = unsafe {
                windows_kernel_sys::MmGetSystemAddressForMdlSafe(
                    target_mdl,
                    MM_PAGE_PRIORITY::LowPagePriority.0 as u32 | MdlMappingNoExecute as u32,
                )
            };
            if target_va.is_null() {
                return Err(STATUS::INSUFFICIENT_RESOURCES.into());
            }
        } else {
            target_va = target_va.add(copy_size);
            target_byte_count = target_byte_count.saturating_sub(copy_size);
        }

        // Advance to the next nbl in the *source* chain
        if source_byte_count == copy_size {
            loop {
                source_mdl = unsafe { (*source_mdl).Next };
                if source_mdl.is_null() {
                    return Ok(copy_len.saturating_sub(bytes_remaining));
                }
                source_byte_count =
                    unsafe { windows_kernel_sys::MmGetMdlByteCount(source_mdl) } as usize;
                if source_byte_count != 0 {
                    break;
                }
            }

            if source_byte_count > bytes_remaining {
                source_byte_count = source_byte_count.saturating_sub(bytes_remaining);
            }

            source_va = unsafe {
                windows_kernel_sys::MmGetSystemAddressForMdlSafe(
                    source_mdl,
                    MM_PAGE_PRIORITY::LowPagePriority.0 as u32 | MdlMappingNoExecute as u32,
                )
            };
            if source_va.is_null() {
                return Err(STATUS::INSUFFICIENT_RESOURCES.into());
            }
        } else {
            source_va = source_va.add(copy_size);
            source_byte_count = source_byte_count.saturating_sub(copy_size);
        }
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    pub struct ReceiveFlags: ULONG {
        const DISPATCH_LEVEL = windows_kernel_sys::NDIS_RECEIVE_FLAGS_DISPATCH_LEVEL;
        /// Indicates that NDIS reclaims ownership of all of the `NET_BUFFER_LIST`s and
        /// any associated `NET_BUFFER`s after `ProtocolReceiveNetBufferLists` returns.
        const RESOURCES = windows_kernel_sys::NDIS_RECEIVE_FLAGS_RESOURCES;
        const SINGLE_ETHER_TYPE = windows_kernel_sys::NDIS_RECEIVE_FLAGS_SINGLE_ETHER_TYPE;
        const SINGLE_VLAN = windows_kernel_sys::NDIS_RECEIVE_FLAGS_SINGLE_VLAN;
        const PERFECT_FILTERED = windows_kernel_sys::NDIS_RECEIVE_FLAGS_PERFECT_FILTERED;
    }
}

impl ReceiveFlags {
    pub fn at_dispatch_level(self) -> bool {
        self.contains(ReceiveFlags::DISPATCH_LEVEL)
    }

    pub fn can_pend(self) -> bool {
        !self.contains(Self::RESOURCES)
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    pub struct ReturnFlags: ULONG {
        const DISPATCH_LEVEL = windows_kernel_sys::NDIS_RETURN_FLAGS_DISPATCH_LEVEL;
    }
}
