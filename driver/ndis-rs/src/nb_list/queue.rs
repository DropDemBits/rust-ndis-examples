//! A queue of [`NetBuffer`] with O(1) append of single elements.

use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER_LIST;

use crate::{NblChain, NetBufferList};

use super::{IntoIter, Iter, IterMut};

/// A queue of [`NetBufferList`]s with O(1) append of single elements.
#[derive(Debug, Default)]
pub struct NblQueue {
    /// Points to the head of the queue (the next element to pop), or `None` if
    /// the queue is empty.
    head: Option<NonNull<NetBufferList>>,
    /// Points to the tail of the queue (the next element to append after), or
    /// `None` if the queue is empty;
    tail: Option<NonNull<NetBufferList>>,
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
        let head = NonNull::new(head.cast());
        let tail = NonNull::new(tail.cast());

        let queue = Self { head, tail };
        queue.assert_valid();
        queue
    }

    /// Decomposes a [`NblQueue`] into its raw parts.
    ///
    /// The tuple matches the argument order of [`NblQueue::from_raw_parts`].
    pub fn into_raw_parts(self) -> (PNET_BUFFER_LIST, PNET_BUFFER_LIST) {
        let head = self.head.map_or(core::ptr::null_mut(), |it| it.as_ptr());
        let tail = self.tail.map_or(core::ptr::null_mut(), |it| it.as_ptr());

        (head.cast(), tail.cast())
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
        self.head.map(|head| unsafe { head.as_ref() })
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
        self.head.map(|mut head| unsafe { head.as_mut() })
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
        self.tail.map(|tail| unsafe { tail.as_ref() })
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
        self.tail.map(|mut tail| unsafe { tail.as_mut() })
    }

    /// Returns `true` if the queue contains no elements.
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
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

        let mut current = self.head.take()?;

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

        let Some(other_head) = other.head else {
            return;
        };
        // Note: could be `unwrap_unchecked` if llvm can't optimize out the
        // unwrap
        let other_tail = other.tail.unwrap();

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
            *self = other;
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
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the queue.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: The validity invariant of a `NetBufferList` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are
        // valid.
        unsafe { IterMut::new(self.head) }
    }

    /// Creates an owning iterator consuming all of the [`NetBufferList`]s in
    /// the queue.
    pub fn into_iter(self) -> IntoIter {
        IntoIter::new(self.into())
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
            // `nbl` is a valid pointer to a `NetBufferList` so we can update the head.
            self.head = Some(nbl);

            if self.tail.is_none() {
                // Queue was empty so set the tail too
                self.tail = Some(nbl);
            }
        } else {
            // Clearing the queue, can set both to `None`
            self.head = None;
            self.tail = None;
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
            // `nbl` is a valid pointer to a `NetBufferList` so we can update the tail.
            self.tail = Some(nbl);

            if self.head.is_none() {
                // Queue was empty so set the tail too
                self.head = Some(nbl);
            }
        } else {
            // Clearing the queue, can set both to `None`
            self.head = None;
            self.tail = None;
        }
    }

    /// Ensures that the queue is valid (only up to all of the nbls in the chain)
    pub(super) fn assert_valid(&self) {
        if cfg!(debug_assertions) {
            if let Some(_) = self.head.as_ref() {
                // `self.tail` should be equal to the last element in the queue
                let tail = self.tail.expect("tail should be Some if head is Some");
                let real_tail = self.iter().last().expect("queue is not empty");
                debug_assert_eq!(
                    tail.as_ptr().cast_const(),
                    real_tail as *const _,
                    "tail should point to the actual last element"
                );
            } else {
                // Both the head and tail should be `None` if the queue is empty
                debug_assert!(
                    self.tail.as_ref().is_none(),
                    "head and tail in an empty NblQueue must both be None"
                );
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

unsafe impl Send for NblQueue {}
unsafe impl Sync for NblQueue {}

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
    fn queue_push_front_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = index;
            Box::leak(nbl)
        });

        let mut queue = NblQueue::new();
        for nbl in nbl_elements {
            queue.push_front(nbl);
        }

        // should be pushed in reverse order
        let mut flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);

        // should be pushed in normal order
        let mut nbls = vec![];
        while let Some(nbl) = queue.pop_front() {
            nbls.push(nbl.flags());
            let _ = unsafe { Box::from_raw(nbl) };
        }
        nbls.reverse();

        assert_eq!(&nbls, &elements);
    }

    #[test]
    fn queue_push_back_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = index;
            Box::leak(nbl)
        });

        let mut queue = NblQueue::new();
        for nbl in nbl_elements {
            queue.push_back(nbl);
        }

        // should be pushed in normal order
        let flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);

        // should be popped in normal order
        let mut nbls = vec![];
        while let Some(nbl) = queue.pop_front() {
            nbls.push(nbl.flags());
            let _ = unsafe { Box::from_raw(nbl) };
        }

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
        let flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);
    }

    #[test]
    fn queue_append() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        let nbl_elements_a = elem_a.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = *index;
            Box::leak(nbl)
        });

        let mut queue_a = NblQueue::new();
        for nbl in nbl_elements_a {
            queue_a.push_back(nbl);
        }

        let nbl_elements_b = elem_b.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = *index;
            Box::leak(nbl)
        });

        let mut queue_b = NblQueue::new();
        for nbl in nbl_elements_b {
            queue_b.push_back(nbl);
        }

        queue_a.append(&mut queue_b);
        assert!(queue_b.is_empty());
        assert_eq!(
            &queue_a.iter().map(|it| it.flags()).collect::<Vec<_>>(),
            &elements
        );
    }

    #[test]
    fn queue_append_slow() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        // don't need to reverse because we can `push_back`
        let nbl_elements_a = elem_a.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = *index;
            Box::leak(nbl)
        });

        let mut queue_a = NblQueue::new();
        for nbl in nbl_elements_a {
            queue_a.push_back(nbl);
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

        queue_a.append_slow(&mut chain_b);
        assert!(chain_b.is_empty());
        assert_eq!(
            &queue_a.iter().map(|it| it.flags()).collect::<Vec<_>>(),
            &elements
        );
    }
}
