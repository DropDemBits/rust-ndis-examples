//! Helpers for working with `NET_BUFFER_LIST`s

use core::ptr::NonNull;

use windows_kernel_sys::{
    NDIS_HANDLE, NDIS_NET_BUFFER_LIST_INFO, NET_BUFFER_LIST, PNET_BUFFER_LIST, PVOID,
};

use crate::NbChain;

pub mod chain;
pub mod counted_queue;
pub mod iter;
pub mod queue;

/// A linked list of [`NetBufferList`]s.
///
/// # Validity Invariant
///
/// A pointer to a [`NetBufferList`] is valid if:
///
/// - All of the following `NET_BUFFER_LIST`s in the chain are valid
// FIXME: Include NblContext as part of the validity invariant (although it's just a blob of bytes)
/// - All of the `NET_BUFFER` that are a part of each `NET_BUFFER_LIST`s are
///   valid, and
/// - All of the `MDL` of each `NET_BUFFER` are valid
///
/// i.e. the validity of a [`NetBufferList`] transitively depends on the validity
/// of all accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s
#[repr(transparent)]
pub struct NetBufferList {
    nbl: NET_BUFFER_LIST,
}

impl NetBufferList {
    /// Casts a `PNET_BUFFER_LIST` into a `NetBufferList` pointer
    ///
    /// # Safety
    ///
    /// All of the `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
    /// `nbl` must be valid.
    pub unsafe fn ptr_cast_from_raw(nbl: PNET_BUFFER_LIST) -> Option<NonNull<NetBufferList>> {
        // This is sound because `NetBufferList` and `NET_BUFFER_LIST` have the same
        // layout due to `NetBufferList` being `repr(transparent)`
        NonNull::new(nbl).map(NonNull::cast)
    }

    /// Casts a `NetBufferList` pointer into a `PNET_BUFFER_LIST`
    pub fn ptr_cast_to_raw(nb: Option<NonNull<NetBufferList>>) -> PNET_BUFFER_LIST {
        // This is sound because `NetBufferList` and `NET_BUFFER_LIST` have the same
        // layout due to `NetBufferList` being `repr(transparent)`
        nb.map_or(core::ptr::null_mut(), |ptr| ptr.cast().as_ptr())
    }

    pub fn as_ptr(&mut self) -> PNET_BUFFER_LIST {
        Self::ptr_cast_to_raw(Some(NonNull::from(self)))
    }

    pub fn nb_chain(&self) -> &NbChain {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let first_nb_field = unsafe {
            core::ptr::addr_of!(self.nbl.__bindgen_anon_1.__bindgen_anon_1.FirstNetBuffer)
        };

        // SAFETY: `first_nb_field` is a pointer to
        // `self.[union projections].FirstNetBuffer` which is a `PNET_BUFFER`,
        // and the `NetBufferList` validity invariant asserted by having a
        // `&self` ensures that all of the accessible `NET_BUFFER`s and `MDL`s
        // are valid.
        unsafe { NbChain::from_raw_field(first_nb_field) }
    }

    pub fn nb_chain_mut(&mut self) -> &mut NbChain {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let first_nb_field = unsafe {
            core::ptr::addr_of_mut!(self.nbl.__bindgen_anon_1.__bindgen_anon_1.FirstNetBuffer)
        };

        // SAFETY: `first_nb_field` is a pointer to
        // `self.[union projections].FirstNetBuffer` which is a `PNET_BUFFER`,
        // and the `NetBufferList` validity invariant asserted by having a `&mut
        // self` ensures that all of the accessible `NET_BUFFER`s and `MDL`s
        // are valid.
        //
        // We also have exclusive access over the `NbChain` as we have a `&mut
        // self`.
        unsafe { NbChain::from_raw_field_mut(first_nb_field) }
    }

    /// Handle which originally created this `NetBufferList`
    pub fn source_handle(&self) -> NDIS_HANDLE {
        self.nbl.SourceHandle
    }

    /// A mutable reference to the handle which originally created this `NetBufferList`
    pub fn source_handle_mut(&mut self) -> &mut NDIS_HANDLE {
        &mut self.nbl.SourceHandle
    }

    // FIXME: reserved flag wrapper structs?
    /// Gets the protocol/miniport/NDIS reserved flags of the nbl
    pub fn reserved_flags(&self) -> u32 {
        self.nbl.Flags
    }

    /// Gets a mutable reference to the protocol/miniport/NDIS reserved flags of the nbl
    pub fn reserved_flags_mut(&mut self) -> &mut u32 {
        &mut self.nbl.Flags
    }

    pub fn protocol_reserved_area(&self) -> &[PVOID; 4] {
        &self.nbl.ProtocolReserved
    }

    pub fn protocol_reserved_area_mut(&mut self) -> &mut [PVOID; 4] {
        &mut self.nbl.ProtocolReserved
    }

    /// Gets the cancellation id from the `NetBufferListCancelId` info field.
    pub fn cancel_id(&self) -> usize {
        self.nbl_info(NDIS_NET_BUFFER_LIST_INFO::NetBufferListCancelId)
    }

    /// Sets the `NetBufferListCancelId` info field.
    pub fn set_cancel_id(&mut self, cancel_id: usize) {
        *self.nbl_info_mut(NDIS_NET_BUFFER_LIST_INFO::NetBufferListCancelId) = cancel_id as *mut _;
    }

    /// Get a `NetBufferListInfo` value.
    pub fn nbl_info(&self, id: NDIS_NET_BUFFER_LIST_INFO) -> usize {
        self.nbl.NetBufferListInfo[id.0 as usize] as usize
    }

    /// Get a mutable reference to a `NetBufferListInfo` value.
    pub fn nbl_info_mut(&mut self, id: NDIS_NET_BUFFER_LIST_INFO) -> &mut PVOID {
        &mut self.nbl.NetBufferListInfo[id.0 as usize]
    }

    /// Gets the next [`NetBufferList`] in a chain
    pub(crate) fn next_nbl(&self) -> Option<NonNull<NetBufferList>> {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let next = unsafe { self.nbl.__bindgen_anon_1.__bindgen_anon_1.Next };

        // SAFETY: Having a `&self` transitively guarantees that all of the
        // accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid by
        // the validity invarint of `NetBufferList`.
        unsafe { Self::ptr_cast_from_raw(next) }
    }

    /// Gets the next [`NetBufferList`] in a chain, and detaches the current
    /// [`NetBufferList`] from the chain.
    pub(crate) fn take_next_nbl(&mut self) -> Option<NonNull<NetBufferList>> {
        let next = self.next_nbl();
        // SAFETY: We're setting the next link to `None`, which means that we aren't
        // linking to any other `NetBufferList`s from this one.
        unsafe { self.set_next_nbl(None) };
        next
    }

    /// Sets the next [`NetBufferList`] link to point to `next`.
    ///
    /// # Safety
    ///
    /// If `next` is not `None`, all `NET_BUFFER_LIST`s, `NET_BUFFER`s, and
    /// `MDL`s accessible from `next` must be valid.
    pub(crate) unsafe fn set_next_nbl(&mut self, next: Option<NonNull<NetBufferList>>) {
        // Transform `next` into a `PNET_BUFFER_LIST`
        let next = Self::ptr_cast_to_raw(next);

        // Set the new next link
        //
        // Caller ensures that all `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s
        // accessible from `next` are valid.
        self.nbl.__bindgen_anon_1.__bindgen_anon_1.Next = next;
    }
}

impl core::fmt::Debug for NetBufferList {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!(
            "NetBufferList {{ .. }} @ {:#x?}",
            self as *const _
        ))
    }
}

// SAFETY: `NetBufferList` effectively owns all of the accessible
// `NET_BUFFER_LIST`s, `NET_BUFFER_LIST_CONTEXT`s, `NET_BUFFER`s, and `MDL`s, so
// there won't be any foreign unsynchronized mutable accesses.
unsafe impl Send for NetBufferList {}
// SAFETY: A `NetBufferList` can only mutate fields behind a `&mut`, so
// `&NetBufferList` can safely be sent between threads.
unsafe impl Sync for NetBufferList {}

#[allow(dead_code)]
fn assert_properties() {
    fn is_send<T: Send>() {}
    fn is_sync<T: Sync>() {}

    is_send::<crate::NetBufferList>();
    is_sync::<crate::NetBufferList>();

    is_send::<crate::NblChain>();
    is_sync::<crate::NblChain>();

    is_send::<crate::NblQueue>();
    is_sync::<crate::NblQueue>();

    is_send::<crate::NblCountedQueue>();
    is_sync::<crate::NblCountedQueue>();

    is_send::<iter::Iter<'_>>();
    is_sync::<iter::Iter<'_>>();

    is_send::<iter::IterMut<'_>>();
    is_sync::<iter::IterMut<'_>>();

    is_send::<iter::IntoIter>();
    is_sync::<iter::IntoIter>();
}
