//! Helpers for working with `NET_BUFFER_LIST`s

use core::{marker::PhantomData, ptr::NonNull};

use windows_kernel_sys::{NET_BUFFER_LIST, PNET_BUFFER_LIST};

use crate::{NbChain, NblChain};

pub mod chain;
pub mod counted_queue;
pub mod queue;

/// A linked list of [`NetBufferList`]s.
///
/// # Validity Invariant
///
/// A pointer to a [`NetBufferList`] is valid if:
///
/// - All of the following `NET_BUFFER_LIST`s in the chain are vaild
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

    pub fn nb_chain(&self) -> &NbChain {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let first_nb_field = unsafe {
            core::ptr::addr_of!(self.nbl.__bindgen_anon_1.__bindgen_anon_1.FirstNetBuffer)
        };

        unsafe { NbChain::from_raw_field(first_nb_field) }
    }

    pub fn nb_chain_mut(&mut self) -> &mut NbChain {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let first_nb_field = unsafe {
            core::ptr::addr_of_mut!(self.nbl.__bindgen_anon_1.__bindgen_anon_1.FirstNetBuffer)
        };

        unsafe { NbChain::from_raw_field_mut(first_nb_field) }
    }

    /// Gets the ??? flags of the nbl
    pub fn flags(&self) -> u32 {
        self.nbl.Flags
    }

    /// Gets a mutable reference to the ??? flags of the nbl
    pub fn flags_mut(&mut self) -> &mut u32 {
        &mut self.nbl.Flags
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
        f.debug_struct("NetBufferList").finish_non_exhaustive()
    }
}

unsafe impl Send for NetBufferList {}
unsafe impl Sync for NetBufferList {}

/// An iterator over [`NetBufferList`]s in the same chain.
pub struct Iter<'chain> {
    next: Option<NonNull<NetBufferList>>,
    chain: PhantomData<&'chain NetBufferList>,
}

impl<'chain> Iter<'chain> {
    /// Creates a new iterator over a chain of [`NetBufferList`]s.
    ///
    /// # Safety
    ///
    /// `head` must be a valid pointer to a [`NetBufferList`], and all
    /// [`NetBufferList`]s in the chain must be valid (see the
    /// "Validity Invariant" section of [`NetBufferList`]).
    pub unsafe fn new(head: Option<NonNull<NetBufferList>>) -> Self {
        Self {
            next: head,
            chain: PhantomData,
        }
    }
}

impl<'chain> Iterator for Iter<'chain> {
    type Item = &'chain NetBufferList;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the next nbl to yield, or bail if it's `None`
        let current = self.next.take()?;

        // SAFETY: `Iter::new` ensures that `current` is a valid pointer
        // to a [`NetBufferList`], and the lifetime of the reference is
        // bound to `'chain` (i.e. no element in the chain can be mutated
        // as long as all references yielded from the iterator are live).
        let current = unsafe { current.as_ref() };

        // Get the next nbl in the chain
        self.next = current.next_nbl();
        Some(current)
    }
}

impl<'a> core::iter::FusedIterator for Iter<'a> {}

unsafe impl<'a> Send for Iter<'a> {}
unsafe impl<'a> Sync for Iter<'a> {}

/// A mutable iterator over [`NetBufferList`]s in the same chain.
pub struct IterMut<'chain> {
    next: Option<NonNull<NetBufferList>>,
    chain: PhantomData<&'chain mut NetBufferList>,
}

impl<'chain> IterMut<'chain> {
    /// Creates a new mutable iterator over a chain of [`NetBufferList`]s.
    ///
    /// # Safety
    ///
    /// `head` must be a valid pointer to a [`NetBufferList`], and all
    /// [`NetBufferList`]s in the chain must be valid (see the
    /// "Validity Invariant" section of [`NetBufferList`]).
    ///
    /// Must also not alias with any other mutable reference to the
    /// [`NetBufferList`] elements.
    pub unsafe fn new(head: Option<NonNull<NetBufferList>>) -> Self {
        Self {
            next: head,
            chain: PhantomData,
        }
    }
}

impl<'chain> Iterator for IterMut<'chain> {
    type Item = &'chain mut NetBufferList;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the next nbl to yield, or bail if it's `None`
        let mut current = self.next.take()?;

        // SAFETY: `IterMut::new` ensures that `current` is a valid pointer
        // to a [`NetBufferList`], and the lifetime of the reference is bound
        // to `'chain` (i.e. while mutable references can exist to different
        // elements in the chain, no other mutable iterator can exist so long as
        // all of those mutable references are live).
        let current = unsafe { current.as_mut() };

        // Get the next nbl in the chain
        self.next = current.next_nbl();
        Some(current)
    }
}

impl<'a> core::iter::FusedIterator for IterMut<'a> {}

unsafe impl<'a> Send for IterMut<'a> {}
unsafe impl<'a> Sync for IterMut<'a> {}

/// An owning iterator over all of the [`NetBufferList`]s in the same chain.
pub struct IntoIter {
    chain: NblChain,
}

impl IntoIter {
    /// Creates an owning iterator over a chain of [`NetBufferList`]s.
    pub fn new(chain: NblChain) -> Self {
        Self { chain }
    }
}

impl Iterator for IntoIter {
    type Item = &'static mut NetBufferList;

    fn next(&mut self) -> Option<Self::Item> {
        self.chain.pop_front()
    }
}

impl core::iter::FusedIterator for IntoIter {}

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

    is_send::<Iter<'_>>();
    is_sync::<Iter<'_>>();

    is_send::<IterMut<'_>>();
    is_sync::<IterMut<'_>>();

    is_send::<IntoIter>();
    is_sync::<IntoIter>();
}
