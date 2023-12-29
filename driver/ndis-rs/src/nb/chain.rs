/// A singly-linked list of [`NetBuffer`]s.
use core::ptr::NonNull;

use windows_kernel_sys::PNET_BUFFER;

use super::{IntoIter, Iter, IterMut, NetBuffer};

/// A singly-linked list of [`NetBuffer`]s.
///
/// A [`NbChain`] owns all of the [`NetBuffer`]s that it comprises.
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct NbChain {
    /// Invariant: `NbChain::new` and `NbChain::set_head` ensures that all of
    /// the accessible `NET_BUFFER`s and `MDL`s are valid.
    head: Option<NonNull<NetBuffer>>,
}

// Must be the same size & alignment as a pointer to a `NET_BUFFER`
static_assertions::assert_eq_size!(PNET_BUFFER, NbChain);
static_assertions::assert_eq_align!(PNET_BUFFER, NbChain);

impl NbChain {
    /// Creates a new empty [`NbChain`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a [`NbChain`] from an existing `NET_BUFFER` chain, or `None` if
    /// the pointer was null
    ///
    /// # Safety
    ///
    /// `head` must either be null, or a valid pointer to a `NET_BUFFER`, and
    /// all of the directly accessible `NET_BUFFER`s and `MDL`s are valid.
    pub unsafe fn from_raw(head: PNET_BUFFER) -> Self {
        let head = NonNull::new(head).map(NonNull::cast);

        // Safety Invariant: The caller ensures that head is a valid non-null
        // `NET_BUFFER`, and that all of the accessible `NET_BUFFER`s and `MDL`s
        // are valid.
        Self { head }
    }

    /// Decomposes a [`NbChain`] into its raw components
    ///
    /// The returned pointer is guaranteed to be valid to pass to [`NbChain::from_raw`]
    pub fn into_raw(self) -> PNET_BUFFER {
        let Self { head } = self;
        NetBuffer::ptr_cast_to_raw(head)
    }

    /// Gets a reference to a [`NbChain`] from a field
    ///
    /// # Safety
    ///
    /// `field` must be a valid pointer to a `PNET_BUFFER`, and the pointer to
    /// `NET_BUFFER` must have all accessible `NET_BUFFER` and `MDL`s be valid.
    pub(crate) unsafe fn from_raw_field<'a>(field: *const PNET_BUFFER) -> &'a Self {
        // SAFETY: Caller ensures that
        // - `field` is a valid pointer to a `PNET_BUFFER`.
        // - the pointed to `NET_BUFFER` satisfies the `NetBuffer` validity
        //   invariant.
        //
        // The cast is sound because `Option<NonNull<NetBuffer>>` is guaranteed
        // to have the same layout as a `*const NET_BUFFER` because:
        //
        // - `Option<NonNull<_>>` and `*const _` are guaranteed to have the same
        //   layout due to `NonNull`s null niche optimization.
        // - `NET_BUFFER` and `NetBuffer` are guaranteed to have the same layout
        //   as `NetBuffer` is `repr(transparent)`.
        unsafe { &*field.cast() }
    }

    /// Gets a mutable reference to a [`NbChain`] from a field
    ///
    /// # Safety
    ///
    /// `field` must be a valid pointer to a `PNET_BUFFER`, and the pointer to
    /// `NET_BUFFER` must have all accessible `NET_BUFFER` and `MDL`s be valid.
    ///
    /// Also, `field` must not alias with any other live mutable pointers.
    pub(crate) unsafe fn from_raw_field_mut<'a>(field: *mut PNET_BUFFER) -> &'a mut Self {
        // SAFETY: Caller ensures that
        // - `field` is a valid pointer to a `PNET_BUFFER`.
        // - the pointed to `NET_BUFFER` satisfies the `NetBuffer` validity
        //   invariant.
        // - the reference will not alias any other memory.
        //
        // The cast is sound because `Option<NonNull<NetBuffer>>` is guaranteed
        // to have the same layout as a `*mut NET_BUFFER` because:
        //
        // - `Option<NonNull<_>>` and `*mut _` are guaranteed to have the same
        //   layout due to `NonNull`s null niche optimization.
        // - `NET_BUFFER` and `NetBuffer` are guaranteed to have the same layout
        //   as `NetBuffer` is `repr(transparent)`.
        unsafe { &mut *field.cast() }
    }

    /// Returns `true` if the chain has no elements.
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    /// Gets the first element of the chain.
    ///
    /// Completes in O(1) time.
    pub fn first(&self) -> Option<&NetBuffer> {
        // SAFETY: `NbChain::new` and `NbChain::set_head` ensures that `head`
        // is a valid pointer to a `NetBuffer` (i.e. all of the accessible
        // `NET_BUFFER`s and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the reference to the `NetBuffer` to
        // the chain so that no element in the chain will be mutated.
        self.head.map(|head| unsafe { head.as_ref() })
    }

    /// Gets a mutable reference to the first element of the chain.
    ///
    /// Completes in O(1) time.
    pub fn first_mut(&mut self) -> Option<&mut NetBuffer> {
        // SAFETY: `NbChain::new` and `NbChain::set_head` ensures that `head`
        // is a valid pointer to a `NetBuffer` (i.e. all of the accessible
        // `NET_BUFFER`s and `MDL`s are valid).
        //
        // Also, we tie the lifetime of the mutable reference to the
        // `NetBuffer` to the chain so that only the yielded element will
        // be mutated.
        self.head.map(|mut head| unsafe { head.as_mut() })
    }

    /// Gets the last element in the chain.
    ///
    /// Completes in O(n) time.
    pub fn last(&self) -> Option<&NetBuffer> {
        self.iter().last()
    }

    /// Gets a mutable reference to the last element in the chain.
    ///
    /// Completes in O(n) time.
    pub fn last_mut(&mut self) -> Option<&mut NetBuffer> {
        self.iter_mut().last()
    }

    /// Pushes a [`NetBuffer`] at the front of the chain.
    ///
    /// Completes in O(1) time.
    pub fn push_front(&mut self, nb: &'static mut NetBuffer) {
        debug_assert!(
            nb.next_nb().is_none(),
            "nb to add to the queue must be detached"
        );

        // Link the new nb to the old head
        //
        // SAFETY: `NbChain::new` and `NbChain::set_head` ensures that all the
        // `NET_BUFFER_LIST`s, `NET_BUFFER`s, and `MDL`s accessible from `head`
        // are valid.
        unsafe { nb.set_next_nb(self.head) };

        // Replace the head with the new nb
        //
        // SAFETY: `nb` comes from a `&mut NetBuffer`, which already asserts
        // that the `NetBuffer` satisfies the `NetBuffer` validity invariant by
        // references requiring that the place they're referencing is valid.
        unsafe {
            let nb = Some(NonNull::from(nb));
            self.set_head(nb);
        }
    }

    /// Pops the next [`NetBuffer`] from the front of the chain.
    ///
    /// Completes in O(1) time.
    pub fn pop_front(&mut self) -> Option<&'static mut NetBuffer> {
        let mut current = self.head.take()?;

        // SAFETY: `NbChain::new` and `NbChain::set_head` ensures that all
        // `NetBuffer`s in the chain are valid, and since an `NbChain` has
        // ownership over all of the `NetBuffer`s in the chain, there will
        // never be any foreign writes to the yielded `NetBuffer`
        let current = unsafe { current.as_mut() };

        // Take & break the link with the next `NetBuffer` so that we don't
        // accidentally take the rest of the chain with `current`.
        //
        // SAFETY: `next` comes from `current` which comes from `head`, and
        // `NbChain::new` as well as previous calls to `NbChain::set_head`
        // ensures that all accessible `NET_BUFFER_LIST`s are valid.
        unsafe {
            let next = current.take_next_nb();
            self.set_head(next)
        };

        Some(current)
    }

    /// Moves all elements of `other` into `self`, leaving `other` empty.
    ///
    /// Completes in O(n) time.
    pub fn append_slow(&mut self, other: &mut NbChain) {
        let Some(other_head) = other.head.take() else {
            return;
        };

        if let Some(this_tail) = self.last_mut() {
            // Queue is not empty, steal the head.
            //
            // SAFETY: New next nb comes from another `NbChain`s' head, which
            // is guaranteed to satisfy the `NetBuffer` validity invariant.
            unsafe { this_tail.set_next_nb(Some(other_head)) };
        } else {
            debug_assert!(self.is_empty());

            // No last entry, so the chain must be empty so we can replace the
            // head.
            //
            // SAFETY: New head comes from another `NbChain`s' head, which is
            // guaranteed to satisfy the `NetBuffer` validity invariant.
            unsafe { self.set_head(Some(other_head)) };
        }
    }

    /// Creates an iterator over all of the [`NetBuffer`]s in the chain.
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: `NblChain::new` and `NblChain::set_head` ensures that all
        // of the accessible `NET_BUFFER`s and `MDL`s from `head` are valid.
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`NetBuffer`]s in the chain.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: `NbChain::new` and `NbChain::set_head` ensures that all
        // of the accessible `NET_BUFFER`s and `MDL`s from `head` are valid.
        //
        // We also tie the lifetime of the iterator to the chain so that no
        // other mutable iterators can be constructed.
        unsafe { IterMut::new(self.head) }
    }

    /// Creates an owning iterator consuming all of the [`NetBuffer`]s in the chain.
    pub fn into_iter(self) -> IntoIter {
        IntoIter::new(self)
    }

    /// Sets the head of the chain.
    ///
    /// # Safety
    ///
    /// If `nb` is not `None`, then the pointed to `NetBufferList` must satisfy
    /// the `NetBuffer` validity invariant.
    #[inline]
    unsafe fn set_head(&mut self, nb: Option<NonNull<NetBuffer>>) {
        self.head = nb;
    }
}

// SAFETY: `NbChain` effectively owns all of the accessible `NET_BUFFER`s and
// `MDL`s, so there won't be any foreign unsynchronized mutable accesses.
unsafe impl Send for NbChain {}
// SAFETY: A `NbChain` can only mutate fields behind a `&mut`, so `&NbChain`
// can safely be sent between threads.
unsafe impl Sync for NbChain {}

#[cfg(test)]
mod test {
    use std::boxed::Box;
    use std::vec;
    use std::vec::Vec;

    use crate::NbChain;

    #[test]
    fn create_empty_chain() {
        let chain = NbChain::new();

        assert_eq!(chain.iter().count(), 0);
    }

    #[test]
    fn chain_push_pop_front() {
        let elements = [1, 2, 3, 4, 5];
        let nb_elements = elements.map(|index| {
            let mut nb = crate::test::alloc_nb();
            *nb.data_length_mut() = index;
            Box::leak(nb)
        });

        let mut chain = NbChain::new();
        for nb in nb_elements {
            chain.push_front(nb);
        }

        let mut data_length = chain
            .iter()
            .map(|nb| nb.data_length() as u32)
            .collect::<Vec<_>>();
        data_length.reverse();

        assert_eq!(&data_length, &elements);

        let mut nbs = vec![];
        while let Some(nb) = chain.pop_front() {
            nbs.push(nb.data_length() as u32);
            let _ = unsafe { Box::from_raw(nb) };
        }
        nbs.reverse();

        assert_eq!(&nbs, &elements);

        crate::test::free_nbs(chain);
    }

    #[test]
    fn raw_round_trip() {
        let elements = [1, 2, 3, 4, 5];
        let nb_elements = elements.map(|index| {
            let mut nb = crate::test::alloc_nb();
            *nb.data_length_mut() = index;
            Box::leak(nb)
        });

        let mut chain = NbChain::new();
        for nb in nb_elements {
            chain.push_front(nb);
        }

        let chain = {
            // SAFETY: from `into_raw`
            unsafe { NbChain::from_raw(chain.into_raw()) }
        };

        // should be pushed in reverse order
        let mut data_length = chain
            .iter()
            .map(|nb| nb.data_length() as u32)
            .collect::<Vec<_>>();
        data_length.reverse();

        assert_eq!(&data_length, &elements);
        crate::test::free_nbs(chain);
    }

    #[test]
    fn chain_append_slow() {
        let elements = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let (elem_a, elem_b) = elements.split_at(elements.len() / 2);

        let nb_elements_a = elem_a.iter().rev().map(|index| {
            let mut nb = crate::test::alloc_nb();
            *nb.data_length_mut() = *index;
            Box::leak(nb)
        });

        let mut chain_a = NbChain::new();
        for nb in nb_elements_a {
            chain_a.push_front(nb);
        }

        let nb_elements_b = elem_b.iter().rev().map(|index| {
            let mut nb = crate::test::alloc_nb();
            *nb.data_length_mut() = *index;
            Box::leak(nb)
        });

        let mut chain_b = NbChain::new();
        for nb in nb_elements_b {
            chain_b.push_front(nb);
        }

        chain_a.append_slow(&mut chain_b);
        assert!(chain_b.is_empty());
        assert_eq!(
            &chain_a
                .iter()
                .map(|it| it.data_length() as u32)
                .collect::<Vec<_>>(),
            &elements
        );
        crate::test::free_nbs(chain_a);
    }

    #[test]
    fn chain_from_nbl() {
        let mut nbl = crate::test::alloc_nbl();
        let chain = nbl.nb_chain_mut();

        assert!(chain.is_empty());

        for i in 1..=4 {
            let mut nb = crate::test::alloc_nb();
            *nb.data_length_mut() = i;
            chain.push_front(Box::leak(nb));
        }

        crate::test::free_nbs(core::mem::take(chain));
    }
}
