// MdlChain: Start of an MdlChain, has no offset
// MdlOffset: Offset within an MdlChain, points to a specific MDL and offset within the buffer
// MdlSpan: Offset + Length, can be split into two MdlOffsets

use core::{marker::PhantomData, mem::MaybeUninit, ptr::NonNull};

use windows_kernel_sys::{MDL, PMDL, ULONG};

/// A single memory descriptor as part of an [`MdlChain`].
///
/// # Validity Invariant
///
/// A pointer to a [`Mdl`] is valid if:
///
/// - All of the following `Mdl`s in the chain are valid
///
/// i.e. the validity of a [`Mdl`] transitively depends on the validity
/// of all accessible `MDL`s.
#[repr(transparent)]
pub struct Mdl {
    mdl: MDL,
}

impl Mdl {
    /// Casts a `PMDL` into a `Mdl` pointer
    ///
    /// # Safety
    ///
    /// All of the `MDL`s accessible from `mdl` must be valid.
    pub unsafe fn ptr_cast_from_raw(mdl: PMDL) -> Option<NonNull<Mdl>> {
        // This is sound because `Mdl` and `MDL` have the same
        // layout due to `Mdl` being `repr(transparent)`
        NonNull::new(mdl).map(NonNull::cast)
    }

    /// Casts a `Mdl` pointer into a `PMDL`
    pub fn ptr_cast_to_raw(mdl: Option<NonNull<Mdl>>) -> PMDL {
        // This is sound because `Mdl` and `MDL` have the same
        // layout due to `Mdl` being `repr(transparent)`
        mdl.map_or(core::ptr::null_mut(), |ptr| ptr.cast().as_ptr())
    }

    pub fn as_ptr(&mut self) -> PMDL {
        Self::ptr_cast_to_raw(Some(NonNull::from(self)))
    }

    pub fn into_raw(mut self) -> PMDL {
        Self::ptr_cast_to_raw(Some(NonNull::from(&mut self)))
    }

    /// Gets the byte length of the MDL's buffer
    pub fn byte_count(&self) -> ULONG {
        let mdl = &self.mdl as *const MDL;
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized
        unsafe { windows_kernel_sys::MmGetMdlByteCount(mdl) }
    }

    /// Gets the offset within the initial page of the MDL's buffer
    pub fn byte_offset(&self) -> ULONG {
        let mdl = &self.mdl as *const MDL;
        // SAFETY: Having a `&self` transitively guarantees that all fields are
        // properly initialized
        unsafe { windows_kernel_sys::MmGetMdlByteOffset(mdl) }
    }

    /// Maps the MDL's buffer into system address space.
    ///
    /// This always sets the no-execute flag (if supported). If mapping the
    /// pages as executable memory is desired, use [`Mdl::map_mdl_exec`].
    ///
    /// # Returns
    ///
    /// If mapping succeeds, a pointer to the base virtual address in the system
    /// address space is returned. Adding [`Mdl::byte_offset`] will point to the
    /// start of the actual buffer.
    ///
    /// If mapping fails, a null pointer is returned.
    pub fn map_mdl(&mut self, flags: MdlMappingFlags) -> Option<NonNull<core::ffi::c_void>> {
        let mdl = &mut self.mdl as PMDL;
        // SAFETY: Having a `&mut self` transitively guarantees that all fields
        // are properly initialized, and exclusive access to the `MDL` is
        // guaranteed by having a  `&mut self`.
        let ptr = unsafe {
            windows_kernel_sys::MmGetSystemAddressForMdlSafe(
                mdl,
                (flags | MdlMappingFlags::NoExecute).bits(),
            )
        };

        NonNull::new(ptr)
    }

    /// Maps the MDL's buffer as executable memory into system address space.
    ///
    /// # Returns
    ///
    /// If mapping succeeds, a pointer to the base virtual address in the system
    /// address space is returned. Adding [`Mdl::byte_offset`] will point to the
    /// start of the actual buffer.
    ///
    /// If mapping fails, a null pointer is returned.
    ///
    /// # Safety
    ///
    /// The backing buffer must have a valid instruction stream.
    pub unsafe fn map_mdl_exec(
        &mut self,
        flags: MdlMappingFlags,
    ) -> Option<NonNull<core::ffi::c_void>> {
        let mdl = &mut self.mdl as PMDL;
        // SAFETY: Having a `&mut self` transitively guarantees that all fields
        // are properly initialized, and exclusive access to the `MDL` is
        // guaranteed by having a  `&mut self`.
        let ptr = unsafe { windows_kernel_sys::MmGetSystemAddressForMdlSafe(mdl, flags.bits()) };

        NonNull::new(ptr)
    }

    /// Get the next [`Mdl`] in a chain
    pub(crate) fn next_mdl(&self) -> Option<NonNull<Mdl>> {
        // SAFETY: Having a `&self` transitively guarantees that all of the
        // accessible `MDL`s are valid by the validity invarint of `Mdl`.
        unsafe { Self::ptr_cast_from_raw(self.mdl.Next) }
    }

    /// Gets the next [`Mdl`] in a chain, and detaches the current
    /// [`Mdl`] from the chain.
    pub(crate) fn take_next_mdl(&mut self) -> Option<NonNull<Mdl>> {
        let next = self.next_mdl();
        // SAFETY: We're setting the next link to `None`, which means that we aren't
        // linking to any other `Mdl`s from this one.
        unsafe { self.set_next_mdl(None) };
        next
    }

    /// Sets the next [`Mdl`] link to point to `next`.
    ///
    /// # Safety
    ///
    /// If `next` is not `None`, all `MDL`s and accessible from `next` must
    /// be valid.
    pub(crate) unsafe fn set_next_mdl(&mut self, next: Option<NonNull<Mdl>>) {
        // Transform `next` into a `PMDL`
        let next = Self::ptr_cast_to_raw(next);

        // Set the new next link
        //
        // Caller ensures that all `MDL`s, and `MDL`s accessible from
        // `next` are valid.
        self.mdl.Next = next;
    }
}

bitflags::bitflags! {
    pub struct MdlMappingFlags: ULONG {
        /// Mapping can fail if the system is farily low on resources.
        const LowPagePriority = windows_kernel_sys::MM_PAGE_PRIORITY::LowPagePriority.0 as u32;
        /// Mapping can fail if the system is very low on resources.
        const NormalPagePriority = windows_kernel_sys::MM_PAGE_PRIORITY::NormalPagePriority.0 as u32;
        /// Mapping will only fail if the system has no resources.
        const HighPagePriority = windows_kernel_sys::MM_PAGE_PRIORITY::HighPagePriority.0 as u32;
        /// [Since Windows 8] Maps the pages as read-only.
        const ReadOnly = windows_kernel_sys::MdlMappingNoWrite;
        /// [Since Windows 8] Maps the pages as no-execute. This is the default unless you're using [`Mdl::map_mdl_exec`]
        const NoExecute = windows_kernel_sys::MdlMappingNoExecute;
    }
}

impl core::fmt::Debug for Mdl {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!("Mdl {{ .. }} @ {:#x?}", self as *const _))
    }
}

/// A singly-linked list of [`Mdl`]s.
///
/// A [`MdlChain`] owns all of the [`Mdl`]s that it comprises.
#[derive(Debug, Default)]
#[repr(transparent)]
pub struct MdlChain {
    /// Invariant: `MdlChain::new` and `MdlChain::set_head` ensures that all of
    /// the accessible `MDL`s are valid.
    head: Option<NonNull<Mdl>>,
}

// Must be the same size & alignment as a pointer to a `MDL`
static_assertions::assert_eq_size!(PMDL, MdlChain);
static_assertions::assert_eq_align!(PMDL, MdlChain);

impl MdlChain {
    /// Creates a new empty [`MdlChain`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a [`MdlChain`] from an existing `MDL` chain, or `None` if
    /// the pointer was null
    ///
    /// # Safety
    ///
    /// `head` must either be null, or a valid pointer to a `MDL`, and
    /// all of the directly accessible `MDL`s are valid.
    pub unsafe fn from_raw(head: PMDL) -> Self {
        let head = NonNull::new(head).map(NonNull::cast);

        // Safety Invariant: The caller ensures that head is a valid non-null
        // `MDL`, and that all of the accessible `MDL`s are valid.
        Self { head }
    }

    /// Decomposes a [`MdlChain`] into its raw components
    ///
    /// The returned pointer is guaranteed to be valid to pass to [`MdlChain::from_raw`]
    pub fn into_raw(self) -> PMDL {
        let Self { head } = self;
        Mdl::ptr_cast_to_raw(head)
    }
    /// Gets a reference to a [`MdlChain`] from a field
    ///
    /// # Safety
    ///
    /// `field` must be a valid pointer to a `PMDL`, and the pointer to
    /// `MDL` must have all accessible `MDL` and `MDL`s be valid.
    pub(crate) unsafe fn from_raw_field<'a>(field: *const PMDL) -> &'a Self {
        // SAFETY: Caller ensures that
        // - `field` is a valid pointer to a `PMDL`.
        // - the pointed to `MDL` satisfies the `Mdl` validity invariant.
        //
        // The cast is sound because `Option<NonNull<Mdl>>` is guaranteed
        // to have the same layout as a `*const MDL` because:
        //
        // - `Option<NonNull<_>>` and `*const _` are guaranteed to have the same
        //   layout due to `NonNull`s null niche optimization.
        // - `MDL` and `Mdl` are guaranteed to have the same layout
        //   as `Mdl` is `repr(transparent)`.
        unsafe { &*field.cast() }
    }

    /// Gets a mutable reference to a [`MdlChain`] from a field
    ///
    /// # Safety
    ///
    /// `field` must be a valid pointer to a `PMDL`, and the pointer to
    /// `MDL` must have all accessible `MDL` and `MDL`s be valid.
    ///
    /// Also, `field` must not alias with any other live mutable pointers.
    pub(crate) unsafe fn from_raw_field_mut<'a>(field: *mut PMDL) -> &'a mut Self {
        // SAFETY: Caller ensures that
        // - `field` is a valid pointer to a `PMDL`.
        // - the pointed to `MDL` satisfies the `Mdl` validity invariant.
        // - the reference will not alias any other memory.
        //
        // The cast is sound because `Option<NonNull<Mdl>>` is guaranteed
        // to have the same layout as a `*mut MDL` because:
        //
        // - `Option<NonNull<_>>` and `*mut _` are guaranteed to have the same
        //   layout due to `NonNull`s null niche optimization.
        // - `MDL` and `Mdl` are guaranteed to have the same layout
        //   as `Mdl` is `repr(transparent)`.
        unsafe { &mut *field.cast() }
    }

    /// Returns `true` if the chain has no elements.
    pub fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    /// Gets the first element of the chain.
    ///
    /// Completes in O(1) time.
    pub fn first(&self) -> Option<&Mdl> {
        // SAFETY: `MdlChain::new` and `MdlChain::set_head` ensures that `head`
        // is a valid pointer to a `Mdl` (i.e. all of the accessible `MDL`s
        // are valid).
        //
        // Also, we tie the lifetime of the reference to the `Mdl` to
        // the chain so that no element in the chain will be mutated.
        self.head.map(|head| unsafe { head.as_ref() })
    }

    /// Gets a mutable reference to the first element of the chain.
    ///
    /// Completes in O(1) time.
    pub fn first_mut(&mut self) -> Option<&mut Mdl> {
        // SAFETY: `MdlChain::new` and `MdlChain::set_head` ensures that `head`
        // is a valid pointer to a `Mdl` (i.e. all of the accessible `MDL`s
        // are valid).
        //
        // Also, we tie the lifetime of the mutable reference to the
        // `Mdl` to the chain so that only the yielded element will
        // be mutated.
        self.head.map(|mut head| unsafe { head.as_mut() })
    }

    /// Gets the last element in the chain.
    ///
    /// Completes in O(n) time.
    pub fn last(&self) -> Option<&Mdl> {
        self.iter().last()
    }

    /// Gets a mutable reference to the last element in the chain.
    ///
    /// Completes in O(n) time.
    pub fn last_mut(&mut self) -> Option<&mut Mdl> {
        self.iter_mut().last()
    }

    /// Computes the length of the chain.
    ///
    /// Completes in O(n) time.
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Pushes a [`Mdl`] at the front of the chain.
    ///
    /// Completes in O(1) time.
    pub fn push_front(&mut self, mdl: &'static mut Mdl) {
        debug_assert!(
            mdl.next_mdl().is_none(),
            "mdl to add to the chain must be detached"
        );

        // Link the new nb to the old head
        //
        // SAFETY: `MdlChain::new` and `MdlChain::set_head` ensures that all the
        // `MDL`s accessible from `head` are valid.
        unsafe { mdl.set_next_mdl(self.head) };

        // Replace the head with the new mdl
        //
        // SAFETY: `mdl` comes from a `&mut Mdl`, which already asserts that the
        // `Mdl` satisfies the `Mdl` validity invariant by references requiring
        // that the place they're referencing is valid.
        unsafe {
            let nb = Some(NonNull::from(mdl));
            self.set_head(nb);
        }
    }

    /// Pops the next [`Mdl`] from the front of the chain.
    ///
    /// Completes in O(1) time.
    pub fn pop_front(&mut self) -> Option<&'static mut Mdl> {
        let mut current = self.head.take()?;

        // SAFETY: `MdlChain::new` and `MdlChain::set_head` ensures that all
        // `Mdl`s in the chain are valid, and since an `MdlChain` has
        // ownership over all of the `Mdl`s in the chain, there will
        // never be any foreign writes to the yielded `Mdl`
        let current = unsafe { current.as_mut() };

        // Take & break the link with the next `Mdl` so that we don't
        // accidentally take the rest of the chain with `current`.
        //
        // SAFETY: `next` comes from `current` which comes from `head`, and
        // `MdlChain::new` as well as previous calls to `MdlChain::set_head`
        // ensures that all accessible `NET_BUFFER_LIST`s are valid.
        unsafe {
            let next = current.take_next_mdl();
            self.set_head(next)
        };

        Some(current)
    }

    /// Creates an iterator over all of the [`Mdl`]s in the chain.
    pub fn iter(&self) -> Iter<'_> {
        // SAFETY: `MdlChain::new` and `MdlChain::set_head` ensures that all
        // of the accessible `MDL`s from `head` are valid.
        unsafe { Iter::new(self.head) }
    }

    /// Creates a mutable iterator over all of the [`Mdl`]s in the chain.
    pub fn iter_mut(&mut self) -> IterMut<'_> {
        // SAFETY: `MdlChain::new` and `MdlChain::set_head` ensures that all
        // of the accessible `MDL`s from `head` are valid.
        //
        // We also tie the lifetime of the iterator to the chain so that no
        // other mutable iterators can be constructed.
        unsafe { IterMut::new(self.head) }
    }

    /// Sets the head of the chain.
    ///
    /// # Safety
    ///
    /// If `mdl` is not `None`, then the pointed to `Mdl` must satisfy
    /// the `Mdl` validity invariant.
    #[inline]
    unsafe fn set_head(&mut self, nb: Option<NonNull<Mdl>>) {
        self.head = nb;
    }
}

// SAFETY: `MdlChain` effectively owns all of the accessible `NET_BUFFER`s and
// `MDL`s, so there won't be any foreign unsynchronized mutable accesses.
unsafe impl Send for MdlChain {}
// SAFETY: A `MdlChain` can only mutate fields behind a `&mut`, so `&MdlChain`
// can safely be sent between threads.
unsafe impl Sync for MdlChain {}

impl<'chain> core::iter::IntoIterator for &'chain MdlChain {
    type Item = &'chain Mdl;
    type IntoIter = Iter<'chain>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'chain> core::iter::IntoIterator for &'chain mut MdlChain {
    type Item = &'chain mut Mdl;
    type IntoIter = IterMut<'chain>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// An iterator over [`Mdl`]s in the same chain.
pub struct Iter<'chain> {
    next: Option<NonNull<Mdl>>,
    chain: PhantomData<&'chain Mdl>,
}

impl<'chain> Iter<'chain> {
    /// Creates a new iterator over a chain of [`Mdl`]s.
    ///
    /// # Safety
    ///
    /// `head` must be a valid pointer to a [`Mdl`], and all
    /// [`Mdl`]s in the chain must be valid (see the
    /// "Validity Invariant" section of [`Mdl`]).
    pub unsafe fn new(head: Option<NonNull<Mdl>>) -> Self {
        Self {
            next: head,
            chain: PhantomData,
        }
    }
}

impl<'chain> Iterator for Iter<'chain> {
    type Item = &'chain Mdl;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the next mdl to yield, or bail if it's `None`
        let current = self.next.take()?;

        // SAFETY: `Iter::new` ensures that `current` is a valid pointer
        // to a [`Mdl`], and the lifetime of the reference is
        // bound to `'chain` (i.e. no element in the chain can be mutated
        // as long as all references yielded from the iterator are live).
        let current = unsafe { current.as_ref() };

        // Get the next mdl in the chain
        self.next = current.next_mdl();
        Some(current)
    }
}

impl<'a> core::iter::FusedIterator for Iter<'a> {}

// SAFETY: Effectively a `&Mdl` into the chain.
unsafe impl<'a> Send for Iter<'a> {}
// SAFETY: Effectively a `&Mdl` into the chain.
unsafe impl<'a> Sync for Iter<'a> {}

/// A mutable iterator over [`Mdl`]s in the same chain.
pub struct IterMut<'chain> {
    next: Option<NonNull<Mdl>>,
    chain: PhantomData<&'chain mut Mdl>,
}

impl<'chain> IterMut<'chain> {
    /// Creates a new mutable iterator over a chain of [`Mdl`]s.
    ///
    /// # Safety
    ///
    /// `head` must be a valid pointer to a [`Mdl`], and all
    /// [`Mdl`]s in the chain must be valid (see the
    /// "Validity Invariant" section of [`Mdl`]).
    ///
    /// Must also not alias with any other mutable reference to the
    /// [`Mdl`] elements.
    pub unsafe fn new(head: Option<NonNull<Mdl>>) -> Self {
        Self {
            next: head,
            chain: PhantomData,
        }
    }
}

impl<'chain> Iterator for IterMut<'chain> {
    type Item = &'chain mut Mdl;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the next mdl to yield, or bail if it's `None`
        let mut current = self.next.take()?;

        // SAFETY: `IterMut::new` ensures that `current` is a valid pointer
        // to a [`Mdl`], and the lifetime of the reference is bound
        // to `'chain` (i.e. while mutable references can exist to different
        // elements in the chain, no other mutable iterator can exist so long as
        // all of those mutable references are live).
        let current = unsafe { current.as_mut() };

        // Get the next mdl in the chain
        self.next = current.next_mdl();
        Some(current)
    }
}

impl<'a> core::iter::FusedIterator for IterMut<'a> {}

// SAFETY: Effectively a `&mut Mdl` into the chain.
unsafe impl<'a> Send for IterMut<'a> {}
// SAFETY: Effectively a `&mut Mdl` into the chain.
unsafe impl<'a> Sync for IterMut<'a> {}

impl MdlChain {
    pub fn at_offset_mut(&mut self, offset: usize) -> CursorMut<'_> {
        // SAFETY: `MdlChain::from_raw` and `MdlChain::set_head` ensures that
        // all of the accessible `MDL`s are valid.
        let mut cursor = unsafe { CursorMut::from_raw(self.head, offset) };
        // Need to normalize to get to the correct place (and past any initial 0 length MDLs)
        cursor.normalize();
        cursor
    }
}

pub struct CursorMut<'chain> {
    /// Invariant: `CursorMut::new` and `CursorMut::normalize` ensures that all
    /// of the accessible `MDL`s are valid.
    inner: Option<CursorInner>,
    _chain: PhantomData<&'chain mut MdlChain>,
}

impl<'chain> CursorMut<'chain> {
    /// Creates a [`CursorMut`] from an `MdlChain` and byte offset in the chain.
    ///
    /// # Safety
    ///
    /// `current` must be a valid pointer to a `MDL`, and all of the directly
    /// accessible `MDL`s are valid. `current` must also have exclusive access
    /// to all of the accessible `MDL`s.
    unsafe fn from_raw(current: Option<NonNull<Mdl>>, offset: usize) -> Self {
        CursorMut {
            inner: CursorInner::new(current, offset),
            _chain: PhantomData,
        }
    }

    /// Advances the cursor by `offset` bytes.
    ///
    /// # Panics
    ///
    /// Panics if the new offset is past the end of the `Mdl` chain.
    pub fn advance(&mut self, offset: usize) {
        let Some(inner) = &mut self.inner else {
            panic!("byte offset is out of bounds of the MDL chain")
        };

        let Some(new_offset) = inner.offset.checked_add(offset) else {
            panic!("byte offset is out of bounds of the MDL chain")
        };

        inner.offset = new_offset;
        self.normalize();
    }

    /// Ensures that the cursor points to the correct `Mdl` for the current
    /// offset. This also skips any zero length `Mdl`s.
    ///
    /// # Panics
    ///
    /// Panics if the new offset is past the end of the `Mdl` chain.
    pub(crate) fn normalize(&mut self) {
        let Some(inner) = &mut self.inner else {
            // At the end of the chain already
            return;
        };

        // SAFETY: `CursorMut::from_raw` and previous `CursorMut::normalize`
        // calls ensures that all of the accessible `MDL`s from `mdl` are valid.
        //
        // `cursor` is a mutable reference which ensures that we have exckusive
        // access, and this is the only iterator we make, so we don't create
        // aliasing mutable references.
        let mut iter = unsafe { IterMut::new(Some(inner.current)) };
        let mut offset = inner.offset;

        // Find which mdl the offset lands in.
        while let Some(mdl) = iter.next() {
            if let Some(remainder) = offset.checked_sub(mdl.byte_count() as usize) {
                // Offset goes past this mdl, continue to the next one
                offset = remainder;
            } else {
                // Offset is within this mdl, it's normalized
                self.inner = Some(CursorInner {
                    // Invariant: Having a mutable reference to an `Mdl`
                    // guarantees that the validity requirements of `Mdl` are
                    // met.
                    current: NonNull::from(mdl),
                    // Invariant: `offset` is guranteed to be less than or equal
                    // to the byte count since we don't get a remainder from the
                    // subtraction above.
                    offset,
                });
                return;
            }
        }

        if offset == 0 {
            // Cursor has reached the end of the chain and the offset is one past the end
            self.inner = None;
        } else {
            panic!("byte offset is out of bounds of the MDL chain")
        }
    }

    pub fn iter_spans_mut(&mut self) -> IterSpansMut<'_, 'chain> {
        IterSpansMut::new(self)
    }

    // Gets the currently pointed to `Mdl`, offset, and length of the span
    pub(crate) fn current_mdl_span(&mut self) -> Option<(NonNull<Mdl>, usize, usize)> {
        let inner = &mut self.inner?;
        // SAFETY: By the validity of `current`, all of the accessible
        // `Mdl`s from `current` are valid.
        let mdl = unsafe { inner.current.as_mut() };
        let length = (mdl.byte_count() as usize) - inner.offset;

        Some((NonNull::from(mdl), inner.offset, length))
    }

    /// Derives another cursor from a given cursor.
    ///
    /// # Safety
    ///
    /// The derived cursor must not be alive as the same time as the parent
    /// cursor.
    pub(crate) unsafe fn derived_from(other: &mut CursorMut<'chain>) -> CursorMut<'chain> {
        CursorMut {
            inner: other.inner,
            _chain: other._chain,
        }
    }
}

#[derive(Clone, Copy)]
struct CursorInner {
    current: NonNull<Mdl>,
    offset: usize,
}

impl CursorInner {
    fn new(current: Option<NonNull<Mdl>>, offset: usize) -> Option<CursorInner> {
        if let Some(current) = current {
            Some(CursorInner { current, offset })
        } else {
            assert_eq!(offset, 0, "byte offset is out of bounds of the MDL chain");
            None
        }
    }
}

/// A mutable cursor derived from another mutable cursor. The original cursor
/// is unaffected.
pub struct DerivedCursorMut<'cursor, 'chain> {
    cursor: CursorMut<'chain>,
    _parent_cursor: PhantomData<&'cursor mut CursorMut<'chain>>,
}

impl<'cursor, 'chain> From<&'cursor mut CursorMut<'chain>> for DerivedCursorMut<'cursor, 'chain> {
    fn from(value: &'cursor mut CursorMut<'chain>) -> Self {
        Self {
            // SAFETY: We tie the derived cursor to the lifetime of the parent
            // cursor so that the parent cursor cannot be used while the derived
            // pointer is still alive.
            cursor: unsafe { CursorMut::derived_from(value) },
            _parent_cursor: PhantomData,
        }
    }
}

pub struct IterSpansMut<'cursor, 'chain> {
    cursor: DerivedCursorMut<'cursor, 'chain>,
}

impl<'cursor, 'chain> IterSpansMut<'cursor, 'chain> {
    fn new(cursor: &'cursor mut CursorMut<'chain>) -> Self {
        Self {
            cursor: DerivedCursorMut::from(cursor),
        }
    }
}

impl<'cursor, 'chain> Iterator for IterSpansMut<'cursor, 'chain> {
    type Item = MdlSpanMut<'chain>;

    fn next(&mut self) -> Option<Self::Item> {
        let (mdl, offset, length) = self.cursor.cursor.current_mdl_span()?;
        // Move on to the next span
        self.cursor.cursor.advance(length);

        // SAFETY: `mdl`, `offset`, and `length` comes from `CursorMut`, which
        // guarantees that all of the safety requirements are met
        Some(unsafe { MdlSpanMut::from_raw_parts(mdl, offset, length) })
    }
}

impl<'cursor, 'chain> core::iter::FusedIterator for IterSpansMut<'cursor, 'chain> {}

pub struct MdlSpanMut<'chain> {
    mdl: NonNull<Mdl>,
    offset: usize,
    length: usize,
    _chain: PhantomData<&'chain mut MdlChain>,
}

impl<'chain> MdlSpanMut<'chain> {
    /// Creates an `MdlSpanMut` from its component parts.
    ///
    /// # Safety
    ///
    /// - `mdl` must point to a valid `Mdl`, and that we have exclusive access
    ///   to it.
    /// - `offset` and `length` must be within the bounds of the pointed to
    ///   `Mdl` bufffer.
    pub unsafe fn from_raw_parts(mdl: NonNull<Mdl>, offset: usize, length: usize) -> Self {
        Self {
            mdl,
            offset,
            length,
            _chain: PhantomData,
        }
    }

    /// Gets the byte length of the MDL span's buffer
    pub fn byte_count(&self) -> usize {
        self.length
    }

    /// Gets the offset within the MDL span's buffer
    pub fn byte_offset(&self) -> usize {
        self.offset
    }

    fn mdl(&mut self) -> &mut Mdl {
        // SAFETY: `Self::new` guarantees that `mdl` points to a valid `Mdl`,
        // and that we have exclusive access to it.
        unsafe { self.mdl.as_mut() }
    }

    /// Maps the MDL's buffer into system address space.
    ///
    /// This always sets the no-execute flag (if supported). If mapping the
    /// pages as executable memory is desired, use [`Mdl::map_mdl_exec`].
    ///
    /// # Returns
    ///
    /// If mapping succeeds, a slice to the used part of the span.
    ///
    /// If mapping fails, a null pointer is returned.
    pub fn map_mdl(&mut self, flags: MdlMappingFlags) -> Option<&mut [MaybeUninit<u8>]> {
        let buffer = self.mdl().map_mdl(flags)?.cast::<MaybeUninit<u8>>();

        // Get a pointer to the start of the span.
        //
        // SAFETY: `Self::new` guarantees that `offset` is within the bounds of
        // the mapped buffer space, or one past the end of the space.
        let buffer = unsafe { buffer.as_ptr().byte_add(self.offset) };
        // SAFETY: `Self::new` guarantees that `length` is within the bounds of
        // the mapped buffer space.
        Some(unsafe { core::slice::from_raw_parts_mut(buffer, self.length) })
    }

    /// Maps the MDL's buffer as executable memory into system address space.
    ///
    /// # Returns
    ///
    /// If mapping succeeds, a pointer to the base virtual address in the system
    /// address space is returned. Adding [`Mdl::byte_offset`] will point to the
    /// start of the actual buffer.
    ///
    /// If mapping fails, a null pointer is returned.
    ///
    /// # Safety
    ///
    /// The backing buffer must have a valid instruction stream.
    pub unsafe fn map_mdl_exec(
        &mut self,
        flags: MdlMappingFlags,
    ) -> Option<&mut [MaybeUninit<u8>]> {
        // SAFETY: Caller ensures that the backing buffer has a valid instruction stream.
        let buffer = unsafe { self.mdl().map_mdl_exec(flags)? };
        let buffer = buffer.cast::<MaybeUninit<u8>>();

        // Get a pointer to the start of the span.
        //
        // SAFETY: `Self::new` guarantees that `offset` is within the bounds of
        // the mapped buffer space, or one past the end of the space.
        let buffer = unsafe { buffer.as_ptr().byte_add(self.offset) };
        // SAFETY: `Self::new` guarantees that `length` is within the bounds of
        // the mapped buffer space.
        Some(unsafe { core::slice::from_raw_parts_mut(buffer, self.length) })
    }
}

/// Iterates over spans of the same length from both cursors.
/// Stops at the end of the shorter chain
pub fn pairwise_spans<'cursor, 'chain1, 'chain2>(
    cursor1: &'cursor mut CursorMut<'chain1>,
    cursor2: &'cursor mut CursorMut<'chain2>,
) -> PairwiseSpans<'cursor, 'chain1, 'chain2> {
    PairwiseSpans::new(cursor1, cursor2)
}

pub struct PairwiseSpans<'cursor, 'chain1, 'chain2> {
    cursor1: DerivedCursorMut<'cursor, 'chain1>,
    cursor2: DerivedCursorMut<'cursor, 'chain2>,
}

impl<'cursor, 'chain1, 'chain2> PairwiseSpans<'cursor, 'chain1, 'chain2> {
    pub fn new(
        cursor1: &'cursor mut CursorMut<'chain1>,
        cursor2: &'cursor mut CursorMut<'chain2>,
    ) -> Self {
        Self {
            cursor1: DerivedCursorMut::from(cursor1),
            cursor2: DerivedCursorMut::from(cursor2),
        }
    }
}

impl<'cursor, 'chain1, 'chain2> Iterator for PairwiseSpans<'cursor, 'chain1, 'chain2> {
    type Item = (MdlSpanMut<'chain1>, MdlSpanMut<'chain2>);

    fn next(&mut self) -> Option<Self::Item> {
        let ((span1_mdl, span1_offset, span1_length), (span2_mdl, span2_offset, span2_length)) =
            Option::zip(
                self.cursor1.cursor.current_mdl_span(),
                self.cursor2.cursor.current_mdl_span(),
            )?;

        // Get the common length between the spans
        let common_length = span1_length.min(span2_length);

        // Advance the cursors by the same amount
        self.cursor1.cursor.advance(common_length);
        self.cursor2.cursor.advance(common_length);

        // SAFETY: `span1_mdl`, `span1_offset`, and `span1_length` comes from
        // `CursorMut`, which guarantees that all of the safety requirements
        // are met.
        let span1 = unsafe { MdlSpanMut::from_raw_parts(span1_mdl, span1_offset, span1_length) };
        // SAFETY: `span2_mdl`, `span2_offset`, and `span2_length` comes from
        // `CursorMut`, which guarantees that all of the safety requirements
        // are met.
        let span2 = unsafe { MdlSpanMut::from_raw_parts(span2_mdl, span2_offset, span2_length) };

        Some((span1, span2))
    }
}

impl<'cursor, 'chain1, 'chain2> core::iter::FusedIterator
    for PairwiseSpans<'cursor, 'chain1, 'chain2>
{
}
