//! A singly-linked list of [`NetBufferList`]s.

use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER_LIST;

use crate::{NblCountedQueue, NblQueue, NetBufferList};

use super::iter::{IntoIter, Iter, IterMut};

/// A singly-linked list of [`NetBufferList`]s.
///
/// A [`NblChain`] owns all of the [`NetBufferList`]s that it comprises.
#[derive(Debug, Default)]
#[repr(transparent)]
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

    /// Gets the first element of the chain.
    ///
    /// Completes in O(1) time.
    pub fn first(&self) -> Option<&NetBufferList> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that `head`
        // is a valid pointer to a `NetBufferList` (i.e. all of the accessible
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBufferList` to
        // the chain so that no element in the chain will be mutated.
        self.head.map(|head| unsafe { head.as_ref() })
    }

    /// Gets a mutable reference to the first element of the chain.
    ///
    /// Completes in O(1) time.
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

    /// Gets the last element in the chain.
    ///
    /// Completes in O(n) time.
    pub fn last(&self) -> Option<&NetBufferList> {
        self.iter().last()
    }

    /// Gets a mutable reference to the last element in the chain.
    ///
    /// Completes in O(n) time.
    pub fn last_mut(&mut self) -> Option<&mut NetBufferList> {
        self.iter_mut().last()
    }

    /// Pushes a [`NetBufferList`] at the front of the chain.
    ///
    /// Completes in O(1) time.
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

    /// Pops the next [`NetBufferList`] from the front of the chain.
    ///
    /// Completes in O(1) time.
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
            // Queue is not empty, steal the head.
            //
            // SAFETY: New next nbl comes from another `NblChain`s' head, which
            // is guaranteed to have all of the accessible `NET_BUFFER_LIST`s,
            // `NET_BUFFER`s, and `MDL`s be valid.
            unsafe { this_tail.set_next_nbl(Some(other_head)) };
        } else {
            debug_assert!(self.is_empty());

            // No last entry, so the chain must be empty so we can replace the
            // head.
            //
            // SAFETY: New head comes from another `NblChain`s' head, which is
            // guaranteed to have all of the accessible `NET_BUFFER_LIST`s,
            // `NET_BUFFER`s, and `MDL`s be valid.
            unsafe { self.set_head(Some(other_head)) };
        }
    }

    /// Splits a [`NblChain`] into `BINS` [`NblQueue`] bins, based on
    /// `classifier`.
    ///
    /// The `usize` returned from `classifier` determines which bin the
    /// [`NetBufferList`] will go to, e.g. `0` means it goes into bin 0, `1`
    /// means it goes into bin 1, et cetera.
    ///
    /// # Panics
    ///
    /// Panics if `classifier` returns a `usize` that's not in the range of `0..BINS`.
    pub fn partition_bins<const BINS: usize>(
        mut self,
        mut classifier: impl FnMut(&NetBufferList) -> usize,
    ) -> [NblQueue; BINS] {
        assert!(BINS != 0, "bins must not be empty");

        let mut bins: [NblQueue; BINS] = core::array::from_fn(|_| NblQueue::new());

        while let Some((mut queue, class)) = self.take_similar(&mut classifier) {
            assert!(
                (0..BINS).contains(&class),
                "classifier returned value outside of 0..{BINS}"
            );

            bins[class].append(&mut queue);
        }

        bins
    }

    /// Splits a [`NblChain`] into `BINS` [`NblCountedQueue`] bins, based on
    /// `classifier`.
    ///
    /// The `usize` returned from `classifier` determines which bin the
    /// [`NetBufferList`] will go to, e.g. `0` means it goes into bin 0, `1`
    /// means it goes into bin 1, et cetera.
    ///
    /// # Panics
    ///
    /// Panics if `classifier` returns a `usize` that's not in the range of `0..BINS`.
    /// May panic if there are more than `usize::MAX` elements in any bin or run
    /// of similar elements.
    pub fn partition_counted_bins<const BINS: usize>(
        mut self,
        mut classifier: impl FnMut(&NetBufferList) -> usize,
    ) -> [NblCountedQueue; BINS] {
        assert!(BINS != 0, "bins must not be empty");

        let mut bins: [NblCountedQueue; BINS] = core::array::from_fn(|_| NblCountedQueue::new());

        while let Some((mut queue, class)) = self.take_counted_similar(&mut classifier) {
            assert!(
                (0..BINS).contains(&class),
                "classifier returned value outside of 0..{BINS}"
            );

            bins[class].append(&mut queue);
        }

        bins
    }

    /// Extracts all similar [`NetBufferList`]s and puts it into a [`NblQueue`].
    /// Also returns the `usize` that all the [`NetBufferList`]s are classfied
    /// as, according to `classifier`.
    ///
    /// Two [`NetBufferList`]s are considered similar if `classifier` returns
    /// the same `usize` for both of them.
    pub fn take_similar<'chain>(
        &'chain mut self,
        mut classifier: impl FnMut(&NetBufferList) -> usize,
    ) -> Option<(NblQueue, usize)> {
        // Take the head so as to guarantee that we won't accidentally
        // any extra mutable iterators.
        //
        // Note that `NblChain` effectively owns `&mut NetBufferList` and not the `NetBufferList`s themselves,
        // so we won't accidentally double-free the `NetBufferList` even in the case of a panic.
        let head = self.head.take()?;

        // Create a mutable iterator over all of the nbls, while also
        // classifying them at the same time.
        //
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s from
        // `head` are valid.
        //
        // We also tie the lifetime of the iterator to the chain so that no
        // other mutable iterators can be constructed, and we also set the head
        // to `None` so that even if we do, the iterator yields no elements.
        let nbl_iter = unsafe { IterMut::<'chain>::new(Some(head)) };
        let mut nbl_iter = nbl_iter.map(|nbl| (classifier(nbl), NonNull::from(nbl)));

        let Some((first_class, first_similar)) = nbl_iter.next() else {
            // Note: Techically unreachable since we assert that `head` is not `None` at the start.
            return None;
        };
        let mut last_similar = first_similar;

        // Find the first nbl that isn't of the same class
        let first_different = loop {
            let Some((current_class, current)) = nbl_iter.next() else {
                // We've exhausted the chain, so all of the nbls we have are of
                // the same class.
                break None;
            };

            if current_class != first_class {
                // First different class
                break Some(current);
            } else {
                // Of the same class, is the new last similar
                last_similar = current;
            }
        };

        // Detach the last similar nbl from the queue
        //
        // SAFETY: `last_similar` came from a `&mut NetBufferList` which
        // guarantees that `last_similar` points to a valid `NetBufferList`.
        //
        // No other references to `last_similar` exist at this point, as the
        // only place we do (inside of `nbl_iter`'s map closure) never escape
        // outside the closure.
        unsafe {
            let first_real_different = last_similar.as_mut().take_next_nbl();
            debug_assert_eq!(first_different, first_real_different);
        }

        // Take the similar bits from the queue
        //
        // SAFETY: `first_different` comes from an `IterMut` derived from this
        // `NblChain`, which guarantees that the pointed-to `NetBufferList`
        // is valid.
        unsafe { self.set_head(first_different) };

        // SAFETY:
        // - `first_similar` and `last_similar` come from an `IterMut`
        //   derived from this `NblChain`, which guarantees that the pointed-to
        //   `NetBufferList` is valid.
        let queue = unsafe {
            NblQueue::from_raw_parts(
                NetBufferList::ptr_cast_to_raw(Some(first_similar)),
                NetBufferList::ptr_cast_to_raw(Some(last_similar)),
            )
        };

        Some((queue, first_class))
    }

    /// Extracts all similar [`NetBufferList`]s and puts it into a [`NblCountedQueue`].
    /// Also returns the `usize` that all the [`NetBufferList`]s are classfied
    /// as, according to `classifier`.
    ///
    /// Two [`NetBufferList`]s are considered similar if `classifier` returns
    /// the same `usize` for both of them.
    ///
    /// # Panics
    ///
    /// Might panic if the similar element count exceeds `usize::MAX`.
    pub fn take_counted_similar<'chain>(
        &'chain mut self,
        mut classifier: impl FnMut(&NetBufferList) -> usize,
    ) -> Option<(NblCountedQueue, usize)> {
        let mut length = 0;
        let (queue, class) = self.take_similar(|nbl| {
            length += 1;
            classifier(nbl)
        })?;

        // Check if we've over-counted the number of nbls
        if self.head.is_some() {
            // There's still nbls remaining, so we accidentally included the
            // first different nbl in the count
            length -= 1;
        }

        // Convert the queue into a counted version
        let (head, tail) = queue.into_raw_parts();
        // SAFETY:
        // - `head` and `tail` both come from a `NblQueue`, which guarantees that they are both valid pointers to a `NetBufferList`.
        // - `length` blah
        let queue = unsafe { NblCountedQueue::from_raw_parts(head, tail, length) };

        Some((queue, class))
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

    /// Sets the head of the chain.
    ///
    /// # Safety
    ///
    /// If `nbl` is not `None`, then all of the accessible `NET_BUFFER_LIST`s,
    /// `NET_BUFFER`s, and `MDL`s must be valid.
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

    use crate::{NblChain, NblQueue};

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
            nbl.set_cancel_id(index);
            Box::leak(nbl)
        });

        let mut chain = NblChain::new();
        for nbl in nbl_elements {
            chain.push_front(nbl);
        }

        let mut flags = chain.iter().map(|nbl| nbl.cancel_id()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);

        let mut nbls = vec![];
        while let Some(nbl) = chain.pop_front() {
            nbls.push(nbl.cancel_id());
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
            nbl.set_cancel_id(index);
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
        let mut flags = chain.iter().map(|nbl| nbl.cancel_id()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);
    }

    #[test]
    fn chain_append_slow() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        let nbl_elements_a = elem_a.iter().rev().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(*index);
            Box::leak(nbl)
        });

        let mut chain_a = NblChain::new();
        for nbl in nbl_elements_a {
            chain_a.push_front(nbl);
        }

        let nbl_elements_b = elem_b.iter().rev().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(*index);
            Box::leak(nbl)
        });

        let mut chain_b = NblChain::new();
        for nbl in nbl_elements_b {
            chain_b.push_front(nbl);
        }

        chain_a.append_slow(&mut chain_b);
        assert!(chain_b.is_empty());
        assert_eq!(
            &chain_a.iter().map(|it| it.cancel_id()).collect::<Vec<_>>(),
            &elements
        );
    }

    // empty
    // all_same
    // tail_same
    // mixed (runs of varying lengths)

    #[test]
    fn chain_take_similar_empty() {
        let mut chain = NblChain::new();
        let similar = chain.take_counted_similar(|nbl| nbl.cancel_id());
        assert!(chain.is_empty());
        assert!(similar.is_none());
    }

    #[test]
    fn chain_take_similar_all_same() {
        let elements = [1, 1, 1, 1, 1];
        let mut queue = NblQueue::new();
        for element in elements {
            let nbl = Box::leak(crate::test::alloc_nbl());
            nbl.set_cancel_id(element);
            queue.push_back(nbl);
        }
        let mut chain = NblChain::from(queue);

        let (similar_queue, class) = chain
            .take_counted_similar(|nbl| nbl.cancel_id())
            .expect("should have found nbl run");
        assert!(chain.is_empty());
        assert_eq!(class, 1);
        assert_eq!(similar_queue.len(), 5);

        // Exhaust it
        let similar = chain.take_counted_similar(|nbl| nbl.cancel_id());
        assert!(chain.is_empty());
        assert!(similar.is_none());
    }

    #[test]
    fn chain_take_similar_different_runs() {
        let elements = [1, 1, 1, 1, 1, 2, 2, 2];
        let mut queue = NblQueue::new();
        for element in elements {
            let nbl = Box::leak(crate::test::alloc_nbl());
            nbl.set_cancel_id(element);
            queue.push_back(nbl);
        }
        let mut chain = NblChain::from(queue);

        // Get the run of 1's
        let (similar_queue, class) = chain
            .take_counted_similar(|nbl| nbl.cancel_id())
            .expect("should have found nbl run");
        assert_eq!(chain.iter().count(), 3);
        assert_eq!(class, 1);
        assert_eq!(similar_queue.len(), 5);

        // Get the run of 2's
        let (similar_queue, class) = chain
            .take_counted_similar(|nbl| nbl.cancel_id())
            .expect("should have found nbl run");
        assert!(chain.is_empty());
        assert_eq!(class, 2);
        assert_eq!(similar_queue.len(), 3);
    }

    #[test]
    fn chain_take_similar_interspered_run() {
        let elements = [1, 1, 1, 2, 2, 2, 1, 1, 1, 1];
        let mut queue = NblQueue::new();
        for element in elements {
            let nbl = Box::leak(crate::test::alloc_nbl());
            nbl.set_cancel_id(element);
            queue.push_back(nbl);
        }
        let mut chain = NblChain::from(queue);

        // Get the first run of 1's
        let (similar_queue, class) = chain
            .take_counted_similar(|nbl| nbl.cancel_id())
            .expect("should have found nbl run");
        assert_eq!(chain.iter().count(), 7);
        assert_eq!(class, 1);
        assert_eq!(similar_queue.len(), 3);

        // Get the run of 2's
        let (similar_queue, class) = chain
            .take_counted_similar(|nbl| nbl.cancel_id())
            .expect("should have found nbl run");
        assert_eq!(chain.iter().count(), 4);
        assert_eq!(class, 2);
        assert_eq!(similar_queue.len(), 3);

        // Get the second run of 1's
        let (similar_queue, class) = chain
            .take_counted_similar(|nbl| nbl.cancel_id())
            .expect("should have found nbl run");
        assert!(chain.is_empty());
        assert_eq!(class, 1);
        assert_eq!(similar_queue.len(), 4);
    }

    #[test]
    fn chain_partition_bins_2_interspered_run() {
        let elements = [1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 3, 4];
        let mut queue = NblQueue::new();
        for element in elements {
            let nbl = Box::leak(crate::test::alloc_nbl());
            nbl.set_cancel_id(element);
            queue.push_back(nbl);
        }
        let chain = NblChain::from(queue);

        let [not_1, is_1] = chain.partition_bins(|nbl| (nbl.cancel_id() == 1) as usize);

        assert_eq!(not_1.iter().count(), 5);
        assert_eq!(is_1.iter().count(), 7)
    }

    #[test]
    fn chain_partition_counted_bins_2_interspered_run() {
        let elements = [1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 3, 4];
        let mut queue = NblQueue::new();
        for element in elements {
            let nbl = Box::leak(crate::test::alloc_nbl());
            nbl.set_cancel_id(element);
            queue.push_back(nbl);
        }
        let chain = NblChain::from(queue);

        let [not_1, is_1] = chain.partition_counted_bins(|nbl| (nbl.cancel_id() == 1) as usize);

        assert_eq!(not_1.len(), 5);
        assert_eq!(is_1.len(), 7)
    }
}

// mapping:
//
// delegates to partition_bins::<const N: usize>()
// - Classify2
// - ClassifyByIndex
//
// delegates to partition_bins_counted::<const N: usize>()
// - Classify2WithCount
// - ClassifyByIndexWithCount
//
// delegates to partition_by{_counted}()
// - ClassifyByValue
// - ClassifyByValueWithCount
//
// delegates to partition_by{_counted}_lookahead::<const LOOKAHEAD_BINS: usize = 4>()
// - ClassifyByValueLookahed
// - ClassifyByValueLookaheadWithCount
//
// delegates to NblChain::take_similar, via NblQueue::from(counted_queue)
// - PartialClassifyByValue
//
// delegates to NblChain::take_similar_counted
// - PartialClassifyByValueWithCount
// delegates to NblChain::take_similar_counted
// - PartialClassifyByValueWithCount
