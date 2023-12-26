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

    /// Gets the first element of the queue
    ///
    /// Completes in O(1) time
    pub fn first(&self) -> Option<&NetBufferList> {
        // SAFETY: `NblQueue::set_head` ensures that `head` is a valid pointer
        // to a `NetBufferList` (i.e. all of the accessible `NET_BUFFER_LIST`s,
        // `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBufferList` to
        // the queue so that no element in the queue will be mutated.
        self.head.map(|head| unsafe { head.as_ref() })
    }

    /// Gets a mutable reference to the first element of the queue
    ///
    /// Completes in O(1) time
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

    /// Gets the last element of the queue
    ///
    /// Completes in O(1) time
    pub fn last(&self) -> Option<&NetBufferList> {
        // SAFETY: `NblQueue::set_tail` ensures that `tail` is a valid pointer
        // to a `NetBufferList` (i.e. all of the accessible `NET_BUFFER_LIST`s,
        // `NET_BUFFER`s, and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBufferList` to
        // the queue so that no element in the queue will be mutated.
        self.tail.map(|tail| unsafe { tail.as_ref() })
    }

    /// Gets a mutable reference to the last element of the queue
    ///
    /// Completes in O(1) time
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

        // Link the old tail to the new `nbl`
        //
        // SAFETY: `NblQueue::new` and `NblQueue::set_tail` ensures that all the
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from `tail`
        // are valid.
        unsafe { nbl.set_next_nbl(self.tail) };

        // Replace the tail with the new nbl
        //
        // SAFETY: `nbl` comes from a `&mut NetBufferList`, which already asserts
        // that all of the accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s
        // are valid (the `NetBufferList` validity invariant) by references requiring
        // that the place they're referencing is valid.
        unsafe {
            let nbl = Some(NonNull::from(nbl));
            self.set_tail(nbl);
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

    /// Sets the head of the queue, and updates the tail pointer as appropriate
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

    /// Sets the tail of the queue, and updates the head pointer as appropriate
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
