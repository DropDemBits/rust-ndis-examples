//! A singly-linked list of [`NetBufferList`]s.

use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER_LIST;

use crate::{NblCountedQueue, NblQueue};

use super::{IntoIter, Iter, IterMut, NetBufferList};

/// A singly-linked list of [`NetBufferList`]s.
///
/// A [`NblChain`] owns all of the [`NetBufferList`]s that it comprises.
#[derive(Debug, Default)]
pub struct NblChain {
    /// Invariant: `NblChain::new` and `NblChain::set_head` ensures that all of
    /// the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid.
    head: Option<NonNull<NetBufferList>>,
}

impl NblChain {
    /// Creates a new empty [`NblChain`]
    pub fn new() -> Self {
        Self { head: None }
    }

    /// Creates a [`NblChain`] from an existing `NET_BUFFER_LIST` chain
    ///
    /// # Safety
    ///
    /// `head` must either be null, or a valid pointer to a `NET_BUFFER_LIST`,
    /// and all of the directly accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s,
    /// and `MDL`s are valid.
    pub unsafe fn from_raw(head: PNET_BUFFER_LIST) -> Self {
        // SAFETY: Caller ensures that `head` must either be null, or a valid
        // pointer to a `NET_BUFFER_LIST`, and all of the directly accessible
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid.
        let head = unsafe { NetBufferList::ptr_cast_from_raw(head) };

        Self { head }
    }

    /// Decomposes a [`NblChain`] into its raw components
    ///
    /// The returned pointer is guaranteed to be valid to pass to [`NblChain::from_raw`]
    pub fn into_raw(self) -> PNET_BUFFER_LIST {
        let Self { head } = self;
        NetBufferList::ptr_cast_to_raw(head)
    }

    /// Returns `true` if the chain has no elements.
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
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

    /// Moves all elements of `other` into `self`, leaving `other` empty.
    ///
    /// Completes in O(n) time.
    pub fn append_slow(&mut self, other: &mut NblChain) {
        let Some(other_head) = other.head.take() else {
            return;
        };

        if let Some(this_tail) = self.last_mut() {
            // Queue is not empty, steal the tail.
            //
            // SAFETY: New next nbl comes from another `NblChain`s' head, which
            // is guaranteed to have all of the accessible `NET_BUFFER_LIST`s,
            // `NET_BUFFER`s, and `MDL`s be valid.
            unsafe { this_tail.set_next_nbl(Some(other_head)) };
        } else {
            debug_assert!(self.is_empty());

            // No last entry, so the queue must be empty so we can replace the
            // head.
            //
            // SAFETY: New head comes from another `NblChain`s' head, which is
            // guaranteed to have all of the accessible `NET_BUFFER_LIST`s,
            // `NET_BUFFER`s, and `MDL`s be valid.
            unsafe { self.set_head(Some(other_head)) };
        }
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the chain.
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s from
        // `head` are valid.
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the chain.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s from
        // `head` are valid.
        //
        // We also tie the lifetime of the iterator to the chain so that no
        // other mutable iterators can be constructed.
        unsafe { IterMut::new(self.head) }
    }

    /// Creates an owning iterator consuming all of the [`NetBufferList`]s in the chain.
    pub fn into_iter(self) -> IntoIter {
        IntoIter::new(self)
    }

    /// Converts the chain into a [`NblQueue`].
    ///
    /// Completes in O(n) time.
    pub fn into_queue(self) -> NblQueue {
        // Get the first element from the head
        let Self { head: Some(head) } = self else {
            return NblQueue::new();
        };

        // Get the last element
        //
        // SAFETY:
        // - `head` came from a `NblChain`, which guarantees that all
        //   `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
        //   `head` are valid.
        //
        // - No other mutable references to any other elements are live at this
        //   point (we don't dereference `head` at all), and the ones that we do
        //   generate are quickly killed off, or in the case of the last element
        //   we immediately turn that into a `NonNull` pointer.
        let iter_mut = unsafe { IterMut::new(Some(head)) };
        let Some(tail) = iter_mut.last().map(NonNull::from) else {
            return NblQueue::new();
        };

        // Convert head & tail into the appropriate pointer type
        let head = NetBufferList::ptr_cast_to_raw(Some(head));
        let tail = NetBufferList::ptr_cast_to_raw(Some(tail));

        // SAFETY:
        // - `head` came from a `NblChain`, which guarantees that all
        //   `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
        //   `head` are valid.
        // - `tail` was derived from an `IterMut` from `head` and is the last
        //   element of the chain
        // - both `head` and `tail` are not null since they both came from
        //   `NonNull` pointers
        unsafe { NblQueue::from_raw_parts(head, tail) }
    }

    /// Converts the chain into a [`NblCountedQueue`].
    ///
    /// Completes in O(n) time.
    ///
    /// # Panics
    ///
    /// Panics if the original chain exceeds `usize::MAX` elements.
    pub fn into_counted_queue(self) -> NblCountedQueue {
        // Get the first element from the head
        let Self { head: Some(head) } = self else {
            return NblCountedQueue::new();
        };

        // Get the last element along with the count
        //
        // SAFETY:
        // - `head` came from a `NblChain`, which guarantees that all
        //   `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
        //   `head` are valid.
        //
        // - No other mutable references to any other elements are live at this
        //   point (we don't dereference `head` at all), and the ones that we do
        //   generate are quickly killed off, or in the case of the last element
        //   we immediately turn that into a `NonNull` pointer.
        let iter_mut = unsafe { IterMut::new(Some(head)) };
        let Some((count, tail)) = iter_mut
            .enumerate()
            .last()
            .map(|(count, tail)| (count, NonNull::from(tail)))
        else {
            return NblCountedQueue::new();
        };
        let count = count.checked_add(1).expect("overflow in length count");

        // Convert head & tail into the appropriate pointer type
        let head = NetBufferList::ptr_cast_to_raw(Some(head));
        let tail = NetBufferList::ptr_cast_to_raw(Some(tail));

        // SAFETY:
        // - `head` came from a `NblChain`, which guarantees that all
        //   `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
        //   `head` are valid.
        //
        // - `tail` and count were derived from an `IterMut` from `head` and
        //   `tail` is the last element of the chain
        //
        // - both `head` and `tail` are not null since they both came from
        //   `NonNull` pointers
        unsafe { NblCountedQueue::from_raw_parts(head, tail, count) }
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

unsafe impl Send for NblChain {}
unsafe impl Sync for NblChain {}

#[cfg(test)]
mod test {
    use std::boxed::Box;
    use std::vec;
    use std::vec::Vec;

    use crate::NblChain;

    #[test]
    fn create_empty_chain() {
        let chain = NblChain::new();

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

        let mut chain = NblChain::new();
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

    #[test]
    fn raw_round_trip() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = index;
            Box::leak(nbl)
        });

        let mut chain = NblChain::new();
        for nbl in nbl_elements {
            chain.push_front(nbl);
        }

        let chain = {
            // SAFETY: from `into_raw`
            unsafe { NblChain::from_raw(chain.into_raw()) }
        };

        // should be pushed in reverse order
        let mut flags = chain.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);
    }

    #[test]
    fn chain_append_slow() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        let nbl_elements_a = elem_a.iter().rev().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = *index;
            Box::leak(nbl)
        });

        let mut chain_a = NblChain::new();
        for nbl in nbl_elements_a {
            chain_a.push_front(nbl);
        }

        let nbl_elements_b = elem_b.iter().rev().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = *index;
            Box::leak(nbl)
        });

        let mut chain_b = NblChain::new();
        for nbl in nbl_elements_b {
            chain_b.push_front(nbl);
        }

        chain_a.append_slow(&mut chain_b);
        assert!(chain_b.is_empty());
        assert_eq!(
            &chain_a.iter().map(|it| it.flags()).collect::<Vec<_>>(),
            &elements
        );
    }
}
