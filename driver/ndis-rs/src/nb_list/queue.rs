//! A queue of [`NetBuffer`] with O(1) append of single elements.

use core::ptr::NonNull;

use crate::NetBufferList;

use super::{Iter, IterMut};

/// A queue of [`NetBuffer`] with O(1) append of single elements.
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
    /// Creates a new empty [`NblQueue`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Pushes a [`NetBufferList`] at the front of the queue
    ///
    /// Completes in O(1) time
    pub fn push_front(&mut self, nbl: &'static mut NetBufferList) {
        self.assert_valid();

        debug_assert!(
            nbl.next_nbl().is_none(),
            "nbl to add to the queue must be detached"
        );

        // Link the old head to the new `nbl`
        *nbl.next_nbl_mut() = self
            .head
            .map_or(core::ptr::null_mut(), |it| it.as_ptr().cast());

        // Replace the head with the new nbl
        let nbl = NonNull::from(nbl);

        if self.head.replace(nbl).is_none() {
            // The queue was empty, so the new head is now also the tail
            self.tail = Some(nbl);
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

        // Link the old tail to the new `nbl`
        *nbl.next_nbl_mut() = self
            .tail
            .map_or(core::ptr::null_mut(), |it| it.as_ptr().cast());

        // Replace the tail with the new nbl
        let nbl = NonNull::from(nbl);

        if self.tail.replace(nbl).is_none() {
            // The queue was empty, so the new tail is now also the head
            self.head = Some(nbl);
        }

        self.assert_valid();
    }

    /// Pops the next [`NetBufferList`] from the front of the queue
    ///
    /// Completes in O(1) time
    pub fn pop_front(&mut self) -> Option<&'static mut NetBufferList> {
        self.assert_valid();

        let mut current = self.head.take()?;

        // SAFETY: `NblQueue::new` ensures that all `NetBufferList`s in the
        // chain are valid, and since an `NblChain` has ownership over all of
        // the `NetBufferList`s in the chain, there will never be any foreign
        // writes to the yielded `NetBufferList`
        let current = unsafe { current.as_mut() };

        // Take & break the link with the next `NetBufferList` so that we don't
        // accidentally take the rest of the chain with `current`.
        self.head = current.take_next_nbl();

        // Fixup the queue links
        if self.head.is_none() {
            // If the queue is empty, both links should be empty
            self.tail = None;
        }

        self.assert_valid();

        Some(current)
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the queue
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: The validity invariant of a `NetBufferList` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are
        // valid.
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the queue
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: The validity invariant of a `NetBufferList` ensures that all
        // of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are
        // valid.
        unsafe { IterMut::new(self.head) }
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
