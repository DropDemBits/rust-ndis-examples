//! A [`NblQueue`] that keeps track of the queue length.

use windows_kernel_sys::PNET_BUFFER_LIST;

use crate::NblQueue;

use super::{Iter, IterMut};

/// A [`NblQueue`] that keeps track of the queue length.
#[derive(Debug, Default)]
pub struct NblCountedQueue {
    /// Backing queue
    queue: NblQueue,
    length: usize,
}

impl NblCountedQueue {
    /// Creates a new empty [`NblCountedQueue`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a [`NblCountedQueue`] from its constituent components.
    ///
    /// # Safety
    ///
    /// - `head` must either be null, or a valid pointer to a `NET_BUFFER_LIST`,
    ///   and all of the directly accessible `NET_BUFFER_LIST`s, `NET_BUFFER`s,
    ///   and `MDL`s are valid.
    /// - `tail` must be null if `head` is null, or must otherwise point to the
    ///   last `NET_BUFFER_LIST` in the chain starting from `head`.
    /// - `length` must be the length of the chain
    pub unsafe fn from_raw_parts(
        head: PNET_BUFFER_LIST,
        tail: PNET_BUFFER_LIST,
        length: usize,
    ) -> Self {
        // SAFETY: Caller ensures that `head` and `tail` are valid according to
        // the requirements.
        let queue = unsafe { NblQueue::from_raw_parts(head, tail) };
        let queue = Self { queue, length };
        queue.assert_valid();

        queue
    }

    /// Decomposes a [`NblCountedQueue`] into its raw parts.
    ///
    /// The tuple matches the argument order of [`NblCountedQueue::from_raw_parts`].
    pub fn into_raw_parts(self) -> (PNET_BUFFER_LIST, PNET_BUFFER_LIST, usize) {
        let (head, tail) = self.queue.into_raw_parts();

        (head, tail, self.length)
    }

    /// Gets the length of the counted queue
    pub fn len(&self) -> usize {
        self.length
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the queue
    pub fn iter(&self) -> Iter<'_> {
        self.queue.iter()
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the queue
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        self.queue.iter_mut()
    }

    /// Ensures that the counted queue is valid
    fn assert_valid(&self) {
        if cfg!(debug_assertions) {
            self.queue.assert_valid();

            let real_len = self.iter().count();
            debug_assert_eq!(self.length, real_len, "mismatch in counted queue length");
        }
    }
}
