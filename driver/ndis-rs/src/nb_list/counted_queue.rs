//! A [`NblQueue`] that keeps track of the queue length.

use windows_kernel_sys::PNET_BUFFER_LIST;

use crate::{NblChain, NblQueue, NetBufferList};

use super::{IntoIter, Iter, IterMut};

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

    /// Gets the length of the counted queue.
    pub fn len(&self) -> usize {
        self.length
    }

    /// Gets the first element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn first(&self) -> Option<&NetBufferList> {
        self.queue.first()
    }

    /// Gets a mutable reference to the first element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn first_mut(&mut self) -> Option<&mut NetBufferList> {
        self.queue.first_mut()
    }

    /// Gets the last element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn last(&self) -> Option<&NetBufferList> {
        self.queue.last()
    }

    /// Gets a mutable reference to the last element of the queue.
    ///
    /// Completes in O(1) time.
    pub fn last_mut(&mut self) -> Option<&mut NetBufferList> {
        self.queue.last_mut()
    }

    /// Returns `true` if the queue contains no elements.
    pub fn is_empty(&self) -> bool {
        self.queue.is_empty()
    }

    /// Pushes a [`NetBufferList`] at the front of the queue.
    ///
    /// Completes in O(1) time.
    pub fn push_front(&mut self, nbl: &'static mut NetBufferList) {
        self.assert_valid();

        self.queue.push_front(nbl);
        self.length = self.length.saturating_add(1);

        self.assert_valid();
    }

    /// Pushes a [`NetBufferList`] at the back of the queue
    ///
    /// Completes in O(1) time
    pub fn push_back(&mut self, nbl: &'static mut NetBufferList) {
        self.assert_valid();

        self.queue.push_back(nbl);
        self.length = self.length.saturating_add(1);

        self.assert_valid();
    }

    /// Pops the next [`NetBufferList`] from the front of the queue.
    ///
    /// Completes in O(1) time.
    pub fn pop_front(&mut self) -> Option<&'static mut NetBufferList> {
        self.assert_valid();

        let element = self.queue.pop_front();
        self.length = self.length.saturating_sub(1);

        self.assert_valid();

        element
    }

    /// Moves all elements of `other` into `self`, leaving `other` empty.
    ///
    /// Completes in O(1) time.
    ///
    /// # Panics
    ///
    /// Panics if the new length exceeds `usize::MAX` elements.
    pub fn append(&mut self, other: &mut NblCountedQueue) {
        self.assert_valid();

        self.queue.append(&mut other.queue);

        let other_length = core::mem::take(&mut other.length);

        self.length = self
            .length
            .checked_add(other_length)
            .expect("overflow in length count");

        self.assert_valid();
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the queue.
    pub fn iter(&self) -> Iter<'_> {
        self.queue.iter()
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the queue.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        self.queue.iter_mut()
    }

    /// Creates an owning iterator consuming all of the [`NetBufferList`]s in
    /// the queue.
    pub fn into_iter(self) -> IntoIter {
        IntoIter::new(self.into())
    }

    /// Ensures that the counted queue is valid.
    fn assert_valid(&self) {
        if cfg!(debug_assertions) {
            self.queue.assert_valid();

            let real_len = self.iter().count();
            debug_assert_eq!(self.length, real_len, "mismatch in counted queue length");
        }
    }
}

impl From<NblCountedQueue> for NblChain {
    fn from(value: NblCountedQueue) -> Self {
        // SAFETY: `NblCountedQueue` ensures that `head` is a valid pointer to
        // an `NblChain` from it's internal validity invariants.
        unsafe { Self::from_raw(value.into_raw_parts().0) }
    }
}

impl From<NblCountedQueue> for NblQueue {
    fn from(value: NblCountedQueue) -> Self {
        value.queue
    }
}

#[cfg(test)]
mod test {
    use std::boxed::Box;
    use std::vec;
    use std::vec::Vec;

    use crate::NblCountedQueue;

    #[test]
    fn create_empty_queue() {
        let queue = NblCountedQueue::new();

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

        let mut queue = NblCountedQueue::new();
        for nbl in nbl_elements {
            queue.push_front(nbl);
        }

        // should be pushed in reverse order
        let mut flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();
        flags.reverse();

        assert_eq!(&flags, &elements);
        assert_eq!(queue.len(), 5);

        // should be pushed in normal order
        let mut nbls = vec![];
        while let Some(nbl) = queue.pop_front() {
            nbls.push(nbl.flags());
            let _ = unsafe { Box::from_raw(nbl) };
        }
        nbls.reverse();

        assert_eq!(&nbls, &elements);
        assert_eq!(queue.len(), 0);
    }

    #[test]
    fn queue_push_back_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = index;
            Box::leak(nbl)
        });

        let mut queue = NblCountedQueue::new();
        for nbl in nbl_elements {
            queue.push_back(nbl);
        }

        // should be pushed in normal order
        let flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);
        assert_eq!(queue.len(), 5);

        // should be popped in normal order
        let mut nbls = vec![];
        while let Some(nbl) = queue.pop_front() {
            nbls.push(nbl.flags());
            let _ = unsafe { Box::from_raw(nbl) };
        }

        assert_eq!(&nbls, &elements);
        assert_eq!(queue.len(), 0);
    }

    #[test]
    fn raw_round_trip() {
        let elements = [1, 2, 3, 4, 5];
        let nbl_elements = elements.map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = index;
            Box::leak(nbl)
        });

        let mut queue = NblCountedQueue::new();
        for nbl in nbl_elements {
            queue.push_back(nbl);
        }

        // should be pushed in normal order
        let flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);
        assert_eq!(queue.len(), 5);

        let queue = {
            let (head, tail, length) = queue.into_raw_parts();
            // SAFETY: from `into_raw_parts`
            unsafe { NblCountedQueue::from_raw_parts(head, tail, length) }
        };

        // should be pushed in normal order
        let flags = queue.iter().map(|nbl| nbl.flags()).collect::<Vec<_>>();

        assert_eq!(&flags, &elements);
        assert_eq!(queue.len(), 5);
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

        let mut queue_a = NblCountedQueue::new();
        for nbl in nbl_elements_a {
            queue_a.push_back(nbl);
        }

        let nbl_elements_b = elem_b.iter().map(|index| {
            let mut nbl = crate::test::alloc_nbl();
            *nbl.flags_mut() = *index;
            Box::leak(nbl)
        });

        let mut queue_b = NblCountedQueue::new();
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
}
