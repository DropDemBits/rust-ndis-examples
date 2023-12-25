//! A [`NblQueue`] that keeps track of the queue length.

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
