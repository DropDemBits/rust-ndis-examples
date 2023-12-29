//! Helpers for working with `NET_BUFFER`s
use core::{marker::PhantomData, ptr::NonNull};

use windows_kernel_sys::{NET_BUFFER, PMDL, PNET_BUFFER};

use crate::NbChain;

pub mod chain;

/// Data to send or receive over the network.
///
/// # Validity Invariant
///
/// A pointer to a [`NetBuffer`] is valid if:
///
/// - All of the following `NET_BUFFER`s in the chain are valid
/// - All of the `MDL` of each `NET_BUFFER` are valid
///
/// i.e. the validity of a [`NetBuffer`] transitively depends on the validity of
/// all accessible `NET_BUFFER`s and `MDL`s
#[repr(transparent)]
pub struct NetBuffer {
    nb: NET_BUFFER,
}

// For mdl apis, will likely have:
// mdl_chain: to the whole MdlChain<'_>
// data_mdl_span: to the used data as an `MdlSpan<'_>`
// current_mdl_offset: to the used data as an `MdlOffset<'_>`
impl NetBuffer {
    /// Casts a `PNET_BUFFER` into a `NetBuffer` pointer
    ///
    /// # Safety
    ///
    /// All of the `NET_BUFFER`s and `MDL`s accessible from `nb` must be valid.
    pub unsafe fn ptr_cast_from_raw(nb: PNET_BUFFER) -> Option<NonNull<NetBuffer>> {
        // This is sound because `NetBuffer` and `NET_BUFFER` have the same
        // layout due to `NetBuffer` being `repr(transparent)`
        NonNull::new(nb).map(NonNull::cast)
    }

    /// Casts a `NetBuffer` pointer into a `PNET_BUFFER`
    pub fn ptr_cast_to_raw(nb: Option<NonNull<NetBuffer>>) -> PNET_BUFFER {
        // This is sound because `NetBuffer` and `NET_BUFFER` have the same
        // layout due to `NetBuffer` being `repr(transparent)`
        nb.map_or(core::ptr::null_mut(), |ptr| ptr.cast().as_ptr())
    }

    /// Get the current `MDL` and offset into the `MDL` that the driver is using.
    pub fn current_mdl_offset(&self) -> (PMDL, usize) {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let mdl = unsafe { self.nb.__bindgen_anon_1.__bindgen_anon_1.CurrentMdl };

        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        //
        // Note: Windows only supports `usize >= u32`, and `CurrentMdlOffset` is
        // guaranteed to be <= u32::MAX, so this will never truncate.
        let offset = unsafe { self.nb.__bindgen_anon_1.__bindgen_anon_1.CurrentMdlOffset as usize };

        (mdl, offset)
    }

    /// Pointer to the start of a linked list that maps a data buffer holding
    /// the network data.
    pub fn mdl_chain(&self) -> PMDL {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        unsafe { self.nb.__bindgen_anon_1.__bindgen_anon_1.MdlChain }
    }

    /// The offset (in bytes) from the beginning of the `MDL` chain to
    /// the start of the used network data space in the MDL chain.
    ///
    /// Also the size of the unused data space (in bytes).
    pub fn data_offset(&self) -> usize {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let data_offset = unsafe { self.nb.__bindgen_anon_1.__bindgen_anon_1.DataOffset };

        // Note: Windows only supports `usize >= u32`, and `DataOffset` is
        // guaranteed to be <= u32::MAX, so this will never truncate.
        data_offset as usize
    }

    /// The data length (in bytes) of the used data space in the MDL chain.
    ///
    /// Will never be larget than `u32::MAX`.
    pub fn data_length(&self) -> usize {
        let data_length = unsafe {
            self.nb
                .__bindgen_anon_1
                .__bindgen_anon_1
                .__bindgen_anon_1
                .DataLength
        };

        // Note: Windows only supports `usize >= u32`, and `DataLength` is
        // guaranteed to be <= u32::MAX, so this will never truncate.
        data_length as usize
    }

    /// Gets the next [`NetBuffer`] in a chain
    pub(crate) fn next_nb(&self) -> Option<NonNull<NetBuffer>> {
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized.
        let next = unsafe { self.nb.__bindgen_anon_1.__bindgen_anon_1.Next };

        // SAFETY: Having a `&self` transitively guarantees that all of the
        // accessible `NET_BUFFER`s, and `MDL`s are valid by the validity
        // invarint of `NetBuffer`.
        unsafe { Self::ptr_cast_from_raw(next) }
    }

    /// Gets the next [`NetBuffer`] in a chain, and detaches the current
    /// [`NetBuffer`] from the chain.
    pub(crate) fn take_next_nb(&mut self) -> Option<NonNull<NetBuffer>> {
        let next = self.next_nb();
        // SAFETY: We're setting the next link to `None`, which means that we aren't
        // linking to any other `NetBuffer`s from this one.
        unsafe { self.set_next_nb(None) };
        next
    }

    /// Sets the next [`NetBuffer`] link to point to `next`.
    ///
    /// # Safety
    ///
    /// If `next` is not `None`, all `NET_BUFFER`s and
    /// `MDL`s accessible from `next` must be valid.
    pub(crate) unsafe fn set_next_nb(&mut self, next: Option<NonNull<NetBuffer>>) {
        // Transform `next` into a `PNET_BUFFER`
        let next = Self::ptr_cast_to_raw(next);

        // Set the new next link
        //
        // Caller ensures that all `NET_BUFFER`s, and `MDL`s accessible from
        // `next` are valid.
        self.nb.__bindgen_anon_1.__bindgen_anon_1.Next = next;
    }
}

#[cfg(test)]
impl NetBuffer {
    /// Sets the data length
    ///
    /// For testing purposes only
    pub(crate) fn data_length_mut(&mut self) -> &mut u32 {
        unsafe {
            &mut self
                .nb
                .__bindgen_anon_1
                .__bindgen_anon_1
                .__bindgen_anon_1
                .DataLength
        }
    }
}

// SAFETY: `NetBuffer` effectively owns all of the accessible `NET_BUFFER`s and
// `MDL`s, so there won't be any foreign unsynchronized mutable accesses.
unsafe impl Send for NetBuffer {}
// SAFETY: A `NetBuffer` can only mutate fields behind a `&mut`, so `&NetBuffer`
// can safely be sent between threads.
unsafe impl Sync for NetBuffer {}

/// An iterator over [`NetBuffer`]s in the same chain.
pub struct Iter<'chain> {
    next: Option<NonNull<NetBuffer>>,
    chain: PhantomData<&'chain NetBuffer>,
}

impl<'chain> Iter<'chain> {
    /// Creates a new iterator over a chain of [`NetBuffer`]s.
    ///
    /// # Safety
    ///
    /// `head` must be a valid pointer to a [`NetBuffer`], and all
    /// [`NetBuffer`]s in the chain must be valid (see the
    /// "Validity Invariant" section of [`NetBuffer`]).
    pub unsafe fn new(head: Option<NonNull<NetBuffer>>) -> Self {
        Self {
            next: head,
            chain: PhantomData,
        }
    }
}

impl<'chain> Iterator for Iter<'chain> {
    type Item = &'chain NetBuffer;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the next nbl to yield, or bail if it's `None`
        let current = self.next.take()?;

        // SAFETY: `Iter::new` ensures that `current` is a valid pointer
        // to a [`NetBuffer`], and the lifetime of the reference is
        // bound to `'chain` (i.e. no element in the chain can be mutated
        // as long as all references yielded from the iterator are live).
        let current = unsafe { current.as_ref() };

        // Get the next nb in the chain
        self.next = current.next_nb();
        Some(current)
    }
}

impl<'a> core::iter::FusedIterator for Iter<'a> {}

// SAFETY: Effectively a `&NetBuffer` into the chain.
unsafe impl<'a> Send for Iter<'a> {}
// SAFETY: Effectively a `&NetBuffer` into the chain.
unsafe impl<'a> Sync for Iter<'a> {}

/// A mutable iterator over [`NetBuffer`]s in the same chain.
pub struct IterMut<'chain> {
    next: Option<NonNull<NetBuffer>>,
    chain: PhantomData<&'chain mut NetBuffer>,
}

impl<'chain> IterMut<'chain> {
    /// Creates a new mutable iterator over a chain of [`NetBuffer`]s.
    ///
    /// # Safety
    ///
    /// `head` must be a valid pointer to a [`NetBuffer`], and all
    /// [`NetBuffer`]s in the chain must be valid (see the
    /// "Validity Invariant" section of [`NetBuffer`]).
    ///
    /// Must also not alias with any other mutable reference to the
    /// [`NetBuffer`] elements.
    pub unsafe fn new(head: Option<NonNull<NetBuffer>>) -> Self {
        Self {
            next: head,
            chain: PhantomData,
        }
    }
}

impl<'chain> Iterator for IterMut<'chain> {
    type Item = &'chain mut NetBuffer;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the next nbl to yield, or bail if it's `None`
        let mut current = self.next.take()?;

        // SAFETY: `IterMut::new` ensures that `current` is a valid pointer
        // to a [`NetBuffer`], and the lifetime of the reference is bound
        // to `'chain` (i.e. while mutable references can exist to different
        // elements in the chain, no other mutable iterator can exist so long as
        // all of those mutable references are live).
        let current = unsafe { current.as_mut() };

        // Get the next nb in the chain
        self.next = current.next_nb();
        Some(current)
    }
}

impl<'a> core::iter::FusedIterator for IterMut<'a> {}

// SAFETY: Effectively a `&mut NetBuffer` into the chain.
unsafe impl<'a> Send for IterMut<'a> {}
// SAFETY: Effectively a `&mut NetBuffer` into the chain.
unsafe impl<'a> Sync for IterMut<'a> {}

/// An owning iterator over all of the [`NetBuffer`]s in the same chain.
pub struct IntoIter {
    chain: NbChain,
}

impl IntoIter {
    /// Creates an owning iterator over a chain of [`NetBuffer`]s.
    pub fn new(chain: NbChain) -> Self {
        Self { chain }
    }
}

impl Iterator for IntoIter {
    type Item = &'static mut NetBuffer;

    fn next(&mut self) -> Option<Self::Item> {
        self.chain.pop_front()
    }
}

impl core::iter::FusedIterator for IntoIter {}

#[allow(dead_code)]
fn assert_properties() {
    fn is_send<T: Send>() {}
    fn is_sync<T: Sync>() {}

    is_send::<crate::NetBuffer>();
    is_sync::<crate::NetBuffer>();

    is_send::<crate::NbChain>();
    is_sync::<crate::NbChain>();

    is_send::<Iter<'_>>();
    is_sync::<Iter<'_>>();

    is_send::<IterMut<'_>>();
    is_sync::<IterMut<'_>>();

    is_send::<IntoIter>();
    is_sync::<IntoIter>();
}
