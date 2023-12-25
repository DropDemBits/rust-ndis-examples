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

    /// Pushes a [`NetBufferList`] at the front of the chain
    ///
    /// Completes in O(1) time
    pub fn push_front(&mut self, nbl: &'static mut NetBufferList) {
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
        self.head = Some(nbl);
    }

    /// Pops the next [`NetBufferList`] from the front of the chain
    ///
    /// Completes in O(1) time
    pub fn pop_front(&mut self) -> Option<&'static mut NetBufferList> {
        let mut current = self.head.take()?;

        // SAFETY: `NblChain::new` ensures that all `NetBufferList`s in the
        // chain are valid, and since an `NblChain` has ownership over all of
        // the `NetBufferList`s in the chain, there will never be any foreign
        // writes to the yielded `NetBufferList`
        let current = unsafe { current.as_mut() };

        // Take & break the link with the next `NetBufferList` so that we don't
        // accidentally take the rest of the chain with `current`.
        self.head = current.take_next_nbl();

        Some(current)
    }

    /// Creates an iterator over all of the [`NetBufferList`]s in the chain
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: `NblChain::new` ensures that all of the accessible
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid.
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`NetBufferList`]s in the chain
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: `NblChain::new` ensures that all of the accessible
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s are valid.
        unsafe { IterMut::new(self.head) }
    }
}

#[cfg(test)]
mod test {
    use crate::NblChain;

    #[test]
    fn create_empty_chain() {
        let chain = NblChain::empty();

        assert_eq!(chain.iter().count(), 0);
    }
}
