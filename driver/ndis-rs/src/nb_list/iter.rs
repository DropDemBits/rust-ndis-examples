use core::{marker::PhantomData, ptr::NonNull};

use crate::{NblChain, NetBufferList};

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
