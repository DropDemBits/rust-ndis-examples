//! A singly-linked list of [`NetBufferList`]s.

use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER_LIST;

use super::{Iter, IterMut, NetBufferList};

/// A singly-linked list of [`NetBufferList`]s.
///
/// A [`NblChain`] owns all of the [`NetBufferList`]s that it comprises.
#[derive(Debug)]
pub struct NblChain {
    /// Invariant: `NblChain::new` ensures that all of the accessible
    /// `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid.
    head: Option<NonNull<NetBufferList>>,
}

impl NblChain {
    /// Creates a [`NblChain`] from an existing `NET_BUFFER_LIST` chain
    ///
    /// # Safety
    ///
    /// `head` must either be null, or a valid pointer to a `NET_BUFFER_LIST`,
    /// and all of the directly accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s,
    /// and `MDL`s are valid.
    pub unsafe fn new(head: PNET_BUFFER_LIST) -> Self {
        let head = NonNull::new(head).map(|head| head.cast());

        Self { head }
    }

    /// Creates an empty [`NblChain`]
    pub fn empty() -> Self {
        Self { head: None }
    }

    /// Gets the first element of the chain
    ///
    /// Completes in O(1) time
    pub fn first(&self) -> Option<&NetBufferList> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that `head`
        // is a valid pointer to a `NetBufferList` (i.e. all of the accessible
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBufferList` to
        // the chain so that no element in the chain will be mutated.
        self.head.map(|head| unsafe { head.as_ref() })
    }

    /// Gets a mutable reference to the first element of the chain
    ///
    /// Completes in O(1) time
    pub fn first_mut(&mut self) -> Option<&mut NetBufferList> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that `head`
        // is a valid pointer to a `NetBufferList` (i.e. all of the accessible
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the mutable reference to the
        // `NetBufferList` to the chain so that only the yielded element will
        // be mutated.
        self.head.map(|mut head| unsafe { head.as_mut() })
    }

    /// Gets the last element in the chain
    ///
    /// Completes in O(n) time
    pub fn last(&self) -> Option<&NetBufferList> {
        self.iter().last()
    }

    /// Gets a mutable reference to the last element in the chain
    ///
    /// Completes in O(n) time
    pub fn last_mut(&mut self) -> Option<&mut NetBufferList> {
        self.iter_mut().last()
    }

    /// Pushes a [`NetBufferList`] at the front of the chain
    ///
    /// Completes in O(1) time
    pub fn push_front(&mut self, nbl: &'static mut NetBufferList) {
        debug_assert!(
            nbl.next_nbl().is_none(),
            "nbl to add to the queue must be detached"
        );

        // Link the new nbl to the old head
        //
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all the
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from `head`
        // are valid.
        unsafe { nbl.set_next_nbl(self.head) };

        // Replace the head with the new nbl
        //
        // SAFETY: `nbl` comes from a `&mut NetBufferList`, which already asserts
        // that all of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s
        // are valid (the `NetBufferList` validity invariant) by references requiring
        // that the place they're referencing is valid.
        unsafe {
            let nbl = Some(NonNull::from(nbl));
            self.set_head(nbl);
        }
    }

    /// Pops the next [`NetBufferList`] from the front of the chain
    ///
    /// Completes in O(1) time
    pub fn pop_front(&mut self) -> Option<&'static mut NetBufferList> {
        let mut current = self.head.take()?;

        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // `NetBufferList`s in the chain are valid, and since an `NblChain` has
        // ownership over all of the `NetBufferList`s in the chain, there will
        // never be any foreign writes to the yielded `NetBufferList`
        let current = unsafe { current.as_mut() };

        // Take & break the link with the next `NetBufferList` so that we don't
        // accidentally take the rest of the chain with `current`.
        //
        // SAFETY: `next` comes from `current` which comes from `head`, and
        // `NblChain::new` as well as previous calls to `NblChain::set_head`
        // ensures that all accessible `NET_BUFFER_LIST`s are valid.
        unsafe {
            let next = current.take_next_nbl();
            self.set_head(next)
        };

        Some(current)
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the chain
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s from
        // `head` are valid.
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the chain
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s from
        // `head` are valid.
        //
        // We also tie the lifetime of the iterator to the chain so that no
        // other mutable iterators can be constructed.
        unsafe { IterMut::new(self.head) }
    }

    /// Sets the head of the chain
    ///
    /// # Safety
    ///
    /// If `nbl` is not `None`, then all of the accessible `NET_BUFFER_LIST`s,
    /// `NET_BUFFER`s, and `MDL`s must be valid
    #[inline]
    unsafe fn set_head(&mut self, nbl: Option<NonNull<NetBufferList>>) {
        self.head = nbl;
    }
}

#[cfg(test)]
mod test {
    use std::boxed::Box;
    use std::vec;
    use std::vec::Vec;

    use crate::NblChain;

    #[test]
    fn create_empty_chain() {
        let chain = NblChain::empty();

        assert_eq!(chain.iter().count(), 0);
    }

    #[test]
    fn chain_push_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = index;
            Box::leak(nbl)
        });

        let mut chain = NblChain::empty();
        for nbl in nbl_elements {
            chain.push_front(nbl);
        }

        let mut flags = chain.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);

        let mut nbls = vec![];
        while let Some(nbl) = chain.pop_front() {
            nbls.push(nbl.flags());
            let _ = unsafe { Box::from_raw(nbl) };
        }
        nbls.reverse();

        assert_eq!(&nbls, &elements);
    }
}
