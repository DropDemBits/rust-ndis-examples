//! A queue of [`NetBuffer`] with O(1) append of single elements.

use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER_LIST;

use crate::{NblChain, NetBufferList};

use super::iter::{IntoIter, Iter, IterMut};

/// A queue of [`NetBufferList`]s with O(1) append of single elements.
#[derive(Debug, Default)]
pub struct NblQueue {
    /// `head` and `tail` pointers of the queue, or `None` if the queue is empty.
    inner: Option<QueueInner>,
}

#[derive(Debug, Clone, Copy)]
struct QueueInner {
    /// Points to the head of the queue (the next element to pop).
    head: NonNull<NetBufferList>,
    /// Points to the tail of the queue (the next element to append after).
    tail: NonNull<NetBufferList>,
}

impl NblQueue {
    /// Creates a new empty [`NblQueue`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a [`NblQueue`] from its constituent components.
    ///
    /// # Safety
    ///
    /// - `head` must either be null, or a valid pointer to a `NET_BUFFER_LIST`,
    ///   and all of the directly accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s,
    ///   and `MDL`s are valid.
    /// - `tail` must be null if `head` is null, or must otherwise point to the
    ///   last `NET_BUFFER_LIST` in the chain starting from `head`.
    pub unsafe fn from_raw_parts(head: PNET_BUFFER_LIST, tail: PNET_BUFFER_LIST) -> Self {
        // SAFETY: Caller ensures that `head` is a valid pointer
        let head = unsafe { NetBufferList::ptr_cast_from_raw(head) };
        // SAFETY: Caller ensures that `tail` is a valid pointer
        let tail = unsafe { NetBufferList::ptr_cast_from_raw(tail) };

        let inner = head.zip(tail).map(|(head, tail)| QueueInner { head, tail });
        let queue = Self { inner };
        queue.assert_valid();
        queue
    }

    /// Decomposes a [`NblQueue`] into its raw parts.
    ///
    /// The tuple matches the argument order of [`NblQueue::from_raw_parts`].
    pub fn into_raw_parts(self) -> (PNET_BUFFER_LIST, PNET_BUFFER_LIST) {
        self.assert_valid();

        let Some(QueueInner { head, tail }) = self.inner else {
            return (core::ptr::null_mut(), core::ptr::null_mut());
        };

        (
            NetBufferList::ptr_cast_to_raw(Some(head)),
            NetBufferList::ptr_cast_to_raw(Some(tail)),
        )
    }

    /// Gets the first element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn first(&self) -> Option<&NetBufferList> {
        // SAFETY: `NblQueue::set_head` ensures that `head` is a valid pointer
        // to a `NetBufferList` (i.e. all of the accessible `NET_BUFFER_LIST`s,
        // `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBufferList` to
        // the queue so that no element in the queue will be mutated.
        Some(unsafe { self.inner?.head.as_ref() })
    }

    /// Gets a mutable reference to the first element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn first_mut(&mut self) -> Option<&mut NetBufferList> {
        // SAFETY: `NblQueue::set_head` ensures that `head` is a valid pointer
        // to a `NetBufferList` (i.e. all of the accessible `NET_BUFFER_LIST`s,
        // `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the mutable reference to the
        // `NetBufferList` to the queue so that only the yielded element will
        // be mutated.
        Some(unsafe { self.inner?.head.as_mut() })
    }

    /// Gets the last element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn last(&self) -> Option<&NetBufferList> {
        // SAFETY: `NblQueue::set_tail` ensures that `tail` is a valid pointer
        // to a `NetBufferList` (i.e. all of the accessible `NET_BUFFER_LIST`s,
        // `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBufferList` to
        // the queue so that no element in the queue will be mutated.
        Some(unsafe { self.inner?.tail.as_ref() })
    }

    /// Gets a mutable reference to the last element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn last_mut(&mut self) -> Option<&mut NetBufferList> {
        // SAFETY: `NblQueue::set_tail` ensures that `tail` is a valid pointer
        // to a `NetBufferList` (i.e. all of the accessible `NET_BUFFER_LIST`s,
        // `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the mutable reference to the
        // `NetBufferList` to the queue so that only the yielded element will
        // be mutated.
        Some(unsafe { self.inner?.tail.as_mut() })
    }

    /// Returns `true` if the queue contains no elements.
    pub fn is_empty(&self) -> bool {
        self.inner.is_none()
    }

    /// Pushes a [`NetBufferList`] at the front of the queue.
    ///
    /// Completes in O(1) time.
    pub fn push_front(&mut self, nbl: &'static mut NetBufferList) {
        self.assert_valid();

        debug_assert!(
            nbl.next_nbl().is_none(),
            "nbl to add to the queue must be detached"
        );

        // Link the new `nbl` to the old head
        //
        // SAFETY: `NblQueue::new` and `NblQueue::set_head` ensures that all the
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from `head`
        // are valid.
        unsafe { nbl.set_next_nbl(self.inner.map(|inner| inner.head)) };

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

        self.assert_valid();
    }

    /// Pushes a [`NetBufferList`] at the back of the queue
    ///
    /// Completes in O(1) time
    pub fn push_back(&mut self, nbl: &'static mut NetBufferList) {
        self.assert_valid();

        debug_assert!(
            nbl.next_nbl().is_none(),
            "nbl to add to the queue must be detached"
        );

        let nbl = Some(NonNull::from(nbl));

        if let Some(last) = self.last_mut() {
            // Link the old tail to the new `nbl`
            //
            // SAFETY: `nbl` comes from a `&mut NetBufferList`, which already asserts
            // that all of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s
            // are valid (the `NetBufferList` validity invariant) by references requiring
            // that the place they're referencing is valid.
            unsafe { last.set_next_nbl(nbl) };
        }

        // Replace the tail with the new `nbl`
        //
        // SAFETY: `nbl` comes from a `&mut NetBufferList`, which already asserts
        // that all of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s
        // are valid (the `NetBufferList` validity invariant) by references requiring
        // that the place they're referencing is valid.
        unsafe { self.set_tail(nbl) };

        self.assert_valid();
    }

    /// Pops the next [`NetBufferList`] from the front of the queue.
    ///
    /// Completes in O(1) time.
    pub fn pop_front(&mut self) -> Option<&'static mut NetBufferList> {
        self.assert_valid();

        let mut current = self.inner?.head;

        // SAFETY: `NblQueue::new` ensures that all `NetBufferList`s in the
        // chain are valid, and since an `NblChain` has ownership over all of
        // the `NetBufferList`s in the chain, there will never be any foreign
        // writes to the yielded `NetBufferList`
        let current = unsafe { current.as_mut() };

        // Take & break the link with the next `NetBufferList` so that we don't
        // accidentally take the rest of the queue with `current`.
        //
        // SAFETY: `next` comes from `current` which comes from `head`, and
        // `NblChain::new` as well as previous calls to `NblChain::set_head`
        // ensures that all accessible `NET_BUFFER_LIST`s are valid.
        unsafe {
            let next = current.take_next_nbl();
            self.set_head(next)
        };

        self.assert_valid();

        Some(current)
    }

    /// Moves all elements of `other` into `self`, leaving `other` empty.
    ///
    /// Completes in O(1) time.
    pub fn append(&mut self, other: &mut NblQueue) {
        self.assert_valid();

        // `other` should be a valid queue
        other.assert_valid();

        let other = core::mem::take(other);

        let Some(QueueInner {
            head: other_head,
            tail: other_tail,
        }) = other.inner
        else {
            return;
        };

        // Check if there's a last element to append to
        if let Some(last) = self.last_mut() {
            // Link the chains together
            //
            // SAFETY: `NblQueue::new` and `NblQueue::set_head` ensures that all
            // the `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
            // `other_head` are valid.
            unsafe { last.set_next_nbl(Some(other_head)) };

            // Update the tail to be from the queue
            //
            // SAFETY: `other_tail` comes from an `NblQueue`, and
            // `NblQueue::set_head` `NblQueue::set_tail` ensures that all the
            // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from
            // `other_tail` are valid and that it actually points to the tail.
            unsafe { self.set_tail(Some(other_tail)) };
        } else {
            // No last element to append to (i.e. the queue was empty),
            // so we can just replace this queue with `other`.
            self.inner = other.inner;
        }

        self.assert_valid();
    }

    /// Moves all elements from `other`'s chain into `self`, leaving `other` empty.
    ///
    /// Completes in O(n) time.
    pub fn append_slow(&mut self, other: &mut NblChain) {
        self.assert_valid();

        let other = core::mem::take(other);
        self.append(&mut other.into_queue());

        self.assert_valid();
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the queue.
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: The validity invariant of a `NetBufferList` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are
        // valid.
        unsafe { Iter::new(self.inner.map(|inner| inner.head)) }
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the queue.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: The validity invariant of a `NetBufferList` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are
        // valid.
        unsafe { IterMut::new(self.inner.map(|inner| inner.head)) }
    }

    /// Sets the head of the queue, and updates the tail pointer as appropriate.
    ///
    /// # Safety
    ///
    /// If `nbl` is not `None`, then all of the accessible `NET_BUFFER_LIST`s,
    /// `NET_BUFFER`s, and `MDL`s must be valid.
    #[inline]
    unsafe fn set_head(&mut self, nbl: Option<NonNull<NetBufferList>>) {
        if let Some(nbl) = nbl {
            if let Some(inner) = &mut self.inner {
                // `nbl` is a valid pointer to a `NetBufferList` so we can update the head.
                inner.head = nbl;
            } else {
                // Queue was empty so set the head and tail
                self.inner = Some(QueueInner {
                    head: nbl,
                    tail: nbl,
                });
            }
        } else {
            // Clearing the queue, can set both to `None`
            self.inner = None;
        }
    }

    /// Sets the tail of the queue, and updates the head pointer as appropriate.
    ///
    /// # Safety
    ///
    /// If `nbl` is not `None`, then all of the accessible `NET_BUFFER_LIST`s,
    /// `NET_BUFFER`s, and `MDL`s must be valid.
    #[inline]
    unsafe fn set_tail(&mut self, nbl: Option<NonNull<NetBufferList>>) {
        if let Some(nbl) = nbl {
            if let Some(inner) = &mut self.inner {
                // `nbl` is a valid pointer to a `NetBufferList` so we can update the tail.
                inner.tail = nbl;
            } else {
                // Queue was empty so set the head and tail
                self.inner = Some(QueueInner {
                    head: nbl,
                    tail: nbl,
                });
            }
        } else {
            // Clearing the queue, can set both to `None`
            self.inner = None;
        }
    }

    /// Ensures that the queue is valid (only up to all of the nbls in the chain)
    pub(super) fn assert_valid(&self) {
        if cfg!(debug_assertions) {
            if let Some(QueueInner { head: _, tail }) = self.inner.as_ref() {
                // `inner.tail` should be equal to the last element in the queue
                let real_tail = self.iter().last().expect("queue is not empty");
                debug_assert_eq!(
                    tail.as_ptr().cast_const(),
                    real_tail as *const _,
                    "tail should point to the actual last element"
                );

                // `inner.tail` shouldn't be linked to any elements
                //
                // SAFETY: Comes from `self.tail`, which is guaranteed by
                // `NblQueue::new` and `NblQueue::set_tail` to point to a valid
                // `NetBufferList`
                let after_tail = unsafe { tail.as_ref().next_nbl() };
                assert_eq!(
                    after_tail, None,
                    "tail should not have elements following it"
                );
            } else {
                // Both the head and tail should be `None` if the queue is empty, which is always true because `inner` is `None` here.
            }
        }
    }
}

impl From<NblQueue> for NblChain {
    fn from(value: NblQueue) -> Self {
        // SAFETY: `NblQueue` ensures that `head` is a valid pointer to an
        // `NblChain` from it's internal validity invariants.x
        unsafe { Self::from_raw(value.into_raw_parts().0) }
    }
}

// SAFETY: `NblQueue` effectively owns all of the accessible `NET_BUFFER_LIST`s,
// `NET_BUFFER_LIST_CONTEXT`s, `NET_BUFFER`s, and `MDL`s, so there won't be any
// foreign unsynchronized mutable accesses.
unsafe impl Send for NblQueue {}
// SAFETY: A `NblQueue` can only mutate fields behind a `&mut`, so
// `&NetBufferList` can safely be sent between threads.
unsafe impl Sync for NblQueue {}

impl<'chain> core::iter::IntoIterator for &'chain NblQueue {
    type Item = &'chain NetBufferList;
    type IntoIter = Iter<'chain>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'chain> core::iter::IntoIterator for &'chain mut NblQueue {
    type Item = &'chain mut NetBufferList;
    type IntoIter = IterMut<'chain>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl core::iter::IntoIterator for NblQueue {
    type Item = &'static mut NetBufferList;
    type IntoIter = IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter::new(self.into())
    }
}

#[cfg(test)]
mod test {
    use std::boxed::Box;
    use std::vec;
    use std::vec::Vec;

    use crate::{NblChain, NblQueue};

    #[test]
    fn create_empty_queue() {
        let queue = NblQueue::new();

        assert_eq!(queue.iter().count(), 0);
    }

    #[test]
    fn queue_first_and_last() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(index);
            Box::leak(nbl)
        });

        let mut queue = NblQueue::new();
        for nbl in nbl_elements {
            queue.push_front(nbl);
        }

        assert_eq!(
            queue.first().map(|it| it as *const _),
            queue.iter().next().map(|it| it as *const _),
        );
        assert_eq!(
            queue.last().map(|it| it as *const _),
            queue.iter().last().map(|it| it as *const _),
        );
        assert_eq!(
            queue.first_mut().map(|it| it as *mut _),
            queue.iter_mut().next().map(|it| it as *mut _),
        );
        assert_eq!(
            queue.last_mut().map(|it| it as *mut _),
            queue.iter_mut().last().map(|it| it as *mut _),
        );

        crate::test::free_nbls(queue.into());
    }

    #[test]
    fn queue_push_front_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(index);
            Box::leak(nbl)
        });

        let mut queue = NblQueue::new();
        for nbl in nbl_elements {
            queue.push_front(nbl);
        }

        // should be pushed in reverse order
        let mut flags = queue.iter().map(|nbl| nbl.cancel_id()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);

        // should be pushed in normal order
        let mut nbls = vec![];
        while let Some(nbl) = queue.pop_front() {
            nbls.push(nbl.cancel_id());
            let _ = unsafe { Box::from_raw(nbl) };
        }
        nbls.reverse();

        assert_eq!(&nbls, &elements);

        crate::test::free_nbls(queue.into());
    }

    #[test]
    fn queue_push_back_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(index);
            Box::leak(nbl)
        });

        let mut queue = NblQueue::new();
        for nbl in nbl_elements {
            queue.push_back(nbl);
        }

        // should be pushed in normal order
        let flags = queue.iter().map(|nbl| nbl.cancel_id()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);

        // should be popped in normal order
        let mut nbls = vec![];
        while let Some(nbl) = queue.pop_front() {
            nbls.push(nbl.cancel_id());
            let _ = unsafe { Box::from_raw(nbl) };
        }

        assert_eq!(&nbls, &elements);

        crate::test::free_nbls(queue.into());
    }

    #[test]
    fn raw_round_trip() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(index);
            Box::leak(nbl)
        });

        let mut queue = NblQueue::new();
        for nbl in nbl_elements {
            queue.push_back(nbl);
        }

        let queue = {
            let (head, tail) = queue.into_raw_parts();
            // SAFETY: from `into_raw_parts`
            unsafe { NblQueue::from_raw_parts(head, tail) }
        };

        // should be pushed in normal order
        let flags = queue.iter().map(|nbl| nbl.cancel_id()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);

        crate::test::free_nbls(queue.into());
    }

    #[test]
    fn queue_append() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        let nbl_elements_a = elem_a.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(*index);
            Box::leak(nbl)
        });

        let mut queue_a = NblQueue::new();
        for nbl in nbl_elements_a {
            queue_a.push_back(nbl);
        }

        let nbl_elements_b = elem_b.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(*index);
            Box::leak(nbl)
        });

        let mut queue_b = NblQueue::new();
        for nbl in nbl_elements_b {
            queue_b.push_back(nbl);
        }

        queue_a.append(&mut queue_b);
        assert!(queue_b.is_empty());
        assert_eq!(
            &queue_a.iter().map(|it| it.cancel_id()).collect::<Vec<_>>(),
            &elements
        );

        crate::test::free_nbls(queue_a.into());
    }

    #[test]
    fn queue_append_slow() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        // don't need to reverse because we can `push_back`
        let nbl_elements_a = elem_a.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            nbl.set_cancel_id(*index);
            Box::leak(nbl)
        });

        let mut queue_a = NblQueue::new();
        for nbl in nbl_elements_a {
            queue_a.push_back(nbl);
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

        queue_a.append_slow(&mut chain_b);
        assert!(chain_b.is_empty());
        assert_eq!(
            &queue_a.iter().map(|it| it.cancel_id()).collect::<Vec<_>>(),
            &elements
        );

        crate::test::free_nbls(queue_a.into());
    }
}
