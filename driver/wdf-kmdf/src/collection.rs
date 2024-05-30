//! Intrusively linked lists built on the `nt-list` crate.
//!
//! This does not use WDF's Collection type, as it's equivalent to a
//! dynamically allocated array of handles.

// Need:
// - RefBorrow so that we can
//   - get some backing storage to ref to (since we get usize and we want &usize)
//   - access handles without any indirection?
//   - can also just be Entry so that we can add getting the containing context space
//
// - (inspired by RfL) Have some way of going
//   - Unique -> CollectionEntry (or something like that, basically nothing that's a ref)
//   - Essentially `Handle` which is implemented for all handles except the wrappers
//     (`Ref`, `Unique`, `Wrapped`)
//     - Always safe to implement, since it's used to indicate if it's an owning reference or not???
//     - not really though
//   - Any -> Result<CollectionEntry, CollectionEntryError>
//     - Main way to use it
//
// An entry is in a given context which is associated with a given handle
// Essentially need a way of going from Handle -> Context -> Entry
//
// Unique<H> -> SingleCollectionEntry<H, L> where Unique<H>: CollectionElement<L>
// - That way, we know that we own that refbump
//
// H -> Result<SingleCollectionEntry<H, L>, H> where H: CollectionElement<L>
//
// SingleCollectionEntry<H, L> -> &H is easy since that's getting the backing handle
// SingleCollectionEntry<H, L> -> &ContextSpace is harder since that'll require an
// additional trait to get the container of the entry handle

use core::{marker::PhantomData, mem::ManuallyDrop, pin::Pin, sync::atomic::Ordering};

use nt_list::NtTypedList;

use crate::{
    context_space::IntoContextSpace,
    handle::{HasContext, Unique},
};

use self::link::{CollectionInnerLinkImpl, CollectionLinkImpl, LinkImpl};

pub mod single_list {
    //! Intrusive singly-linked collection
    use core::marker::PhantomData;

    use nt_list::{
        single_list::{NtSingleList, NtSingleListHead},
        NtTypedList,
    };

    use crate::{context_space::IntoContextSpace, handle::HasContext};

    use super::{
        link::{CollectionInnerLinkImpl, CollectionLinkImpl},
        list_impl::Iter as CollectionIter,
        CollectionElement, CollectionEntry, CollectionEntryRef,
    };

    /// A link in a [`SingleCollectionHead`] collection.
    pub type SingleCollectionLink<E, H, L> = CollectionLinkImpl<E, H, L, NtSingleList>;

    /// Iterator over a collection of elements.
    pub type Iter<'list, E, H, L> = CollectionIter<'list, E, H, L, NtSingleList>;

    /// The inner link between collection elements
    type SingleCollectionInnerLink<E, H, L> = CollectionInnerLinkImpl<E, H, L, NtSingleList>;

    /// A singly-linked collection of handles.
    pub struct SingleCollectionHead<E, H, L>
    where
        E: CollectionElement<L, LinkImpl = NtSingleList> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = NtSingleList>,
    {
        head: NtSingleListHead<SingleCollectionInnerLink<E, H, L>, L>,
        _entries: PhantomData<(E, H, L)>,
    }

    impl<E, H, L> SingleCollectionHead<E, H, L>
    where
        E: CollectionElement<L, LinkImpl = NtSingleList> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = NtSingleList>,
    {
        pub fn new() -> Self {
            Self {
                head: NtSingleListHead::new(),
                _entries: PhantomData,
            }
        }

        /// Drops every entry in the list.
        pub fn clear(&mut self) {
            while let Some(_) = self.pop_front() {}
        }

        /// Provides a reference to the first element, or `None` if the list is empty.
        pub fn front(&self) -> Option<CollectionEntryRef<'_, E, H, L>> {
            // SAFETY: `SingleCollectionEntry` can only be constructed
            // if the associated handle's refcount is greater than 0,
            // so the backing context space will always be valid.
            let link = unsafe { self.head.front() };

            link.map(CollectionLinkImpl::as_entry)
        }

        /// Returns `true` if the list is empty.
        pub fn is_empty(&self) -> bool {
            self.head.is_empty()
        }

        pub fn iter(&self) -> Iter<'_, E, H, L> {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            Iter::new(unsafe { self.head.iter() })
        }

        /// Counts all of the elements and returns the length of the list.
        pub fn len(&self) -> usize {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            unsafe { self.head.len() }
        }

        /// Pops the first entry from the front of the collection, or `None`
        /// if the collection is empty.
        pub fn pop_front(&mut self) -> Option<CollectionEntry<E, H, L>> {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            let link = unsafe { self.head.pop_front()? };

            // SAFETY: This link comes from the `CollectionEntry` we destructured
            // in `push_front`.
            Some(unsafe { CollectionLinkImpl::into_entry(link) })
        }

        /// Pushes an entry to the start of the collection.
        pub fn push_front<'this>(&'this mut self, entry: CollectionEntry<E, H, L>) {
            let link = entry.into_link::<'this>();
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            unsafe { self.head.push_front(link) };
        }

        /// Filters out elements satisfying the predicate oh no
        pub fn retain<F>(&mut self, mut f: F)
        where
            F: FnMut(CollectionEntryRef<'_, E, H, L>) -> bool,
        {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            //
            // FIXME: this will leak handles as we can't drop them without
            // invoking undefined behaviour (since dropping an entry can lead to
            // dropping the context space the link is stored in).
            unsafe {
                self.head
                    .retain(|link| f(SingleCollectionLink::as_entry(link)))
            };
        }
    }

    impl<E, H, L> Default for SingleCollectionHead<E, H, L>
    where
        E: CollectionElement<L, LinkImpl = NtSingleList> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = NtSingleList>,
    {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<E, H, L> Drop for SingleCollectionHead<E, H, L>
    where
        E: CollectionElement<L, LinkImpl = NtSingleList> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = NtSingleList>,
    {
        fn drop(&mut self) {
            self.clear();
        }
    }
}

pub mod list {
    //! Intrusive doubly-linked collection
    use core::{marker::PhantomData, mem::MaybeUninit, pin::Pin};

    use nt_list::{
        list::{NtList, NtListHead},
        NtTypedList,
    };
    use pinned_init::{pin_init, PinInit};

    use crate::{context_space::IntoContextSpace, handle::HasContext};

    use super::{
        link::{CollectionInnerLinkImpl, CollectionLinkImpl},
        list_impl::Iter as CollectionIter,
        CollectionElement, CollectionEntry, CollectionEntryRef,
    };

    /// A link in a [`CollectionHead`] collection.
    pub type CollectionLink<E, H, L> = CollectionLinkImpl<E, H, L, NtList>;

    /// Iterator over a collection of elements.
    pub type Iter<'list, E, H, L> = CollectionIter<'list, E, H, L, NtList>;

    /// The inner link between collection elements
    type CollectionInnerLink<E, H, L> = CollectionInnerLinkImpl<E, H, L, NtList>;

    /// A doubly-linked collection of handles.
    #[pinned_init::pin_data(PinnedDrop)]
    pub struct CollectionHead<El, H, L>
    where
        El: CollectionElement<L, LinkImpl = NtList> + IntoContextSpace,
        H: HasContext<El>,
        L: NtTypedList<T = NtList>,
    {
        #[pin]
        head: NtListHead<CollectionInnerLink<El, H, L>, L>,
        _entries: PhantomData<(El, H, L)>,
    }

    impl<E, H, L> CollectionHead<E, H, L>
    where
        E: CollectionElement<L, LinkImpl = NtList> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = NtList>,
    {
        pub fn new() -> impl PinInit<Self> {
            let init_head = unsafe {
                pinned_init::pin_init_from_closure(
                    |head: *mut NtListHead<CollectionInnerLink<E, H, L>, L>| {
                        use moveit::New;

                        let head =
                            head.cast::<MaybeUninit<NtListHead<CollectionInnerLink<E, H, L>, L>>>();

                        // SAFETY: Is a `MaybeUninit` so we don't need to care about the validity invariants,
                        // and we trust `head` to be a valid pointer otherwise.
                        let head = &mut *head;
                        // SAFETY: The created reference never escapes the closure.
                        let head = Pin::new_unchecked(head);

                        // SAFETY: `head` is freshly created, and `head` never escapes the closure.
                        NtListHead::new().new(head);

                        Ok::<_, core::convert::Infallible>(())
                    },
                )
            };

            pin_init! {
                Self {
                    head <- init_head,
                    _entries: PhantomData
                }
            }
        }

        fn head(self: Pin<&Self>) -> Pin<&NtListHead<CollectionInnerLink<E, H, L>, L>> {
            // SAFETY: Pin projection
            unsafe { self.map_unchecked(|this| &this.head) }
        }

        fn head_mut(self: Pin<&mut Self>) -> Pin<&mut NtListHead<CollectionInnerLink<E, H, L>, L>> {
            // SAFETY: Pin projection
            unsafe { self.map_unchecked_mut(|this| &mut this.head) }
        }

        /// Drops every entry in the list.
        pub fn clear(mut self: Pin<&mut Self>) {
            while let Some(_) = self.as_mut().pop_front() {}
        }

        /// Provides a reference to the first element, or `None` if the list is empty.
        pub fn front(self: Pin<&Self>) -> Option<CollectionEntryRef<'_, E, H, L>> {
            // SAFETY: `CollectionEntry` can only be constructed
            // if the associated handle's refcount is greater than 0,
            // so the backing context space will always be valid.
            let link = unsafe { self.head().front() };

            link.map(CollectionLink::as_entry)
        }

        /// Provides a reference to the last element, or `None` if the list is empty.
        pub fn back(self: Pin<&Self>) -> Option<CollectionEntryRef<'_, E, H, L>> {
            // SAFETY: `CollectionEntry` can only be constructed
            // if the associated handle's refcount is greater than 0,
            // so the backing context space will always be valid.
            let link = unsafe { self.head().back() };

            link.map(CollectionLink::as_entry)
        }

        /// Returns `true` if the list is empty.
        pub fn is_empty(self: Pin<&Self>) -> bool {
            self.head().is_empty()
        }

        pub fn iter(self: Pin<&Self>) -> Iter<'_, E, H, L> {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            Iter::new(unsafe { self.head().iter() })
        }

        /// Counts all of the elements and returns the length of the list.
        pub fn len(self: Pin<&Self>) -> usize {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            unsafe { self.head().len() }
        }

        /// Pops the first entry from the front of the collection, or `None`
        /// if the collection is empty.
        pub fn pop_front(self: Pin<&mut Self>) -> Option<CollectionEntry<E, H, L>> {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            let link = unsafe { self.head_mut().pop_front()? };

            // SAFETY: This link comes from the `CollectionEntry` we destructured
            // in `push_front` and `push_back`.
            Some(unsafe { CollectionLink::into_entry(link) })
        }

        /// Pops the first entry from the back of the collection, or `None`
        /// if the collection is empty.
        pub fn pop_back(self: Pin<&mut Self>) -> Option<CollectionEntry<E, H, L>> {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            let link = unsafe { self.head_mut().pop_back()? };

            // SAFETY: This link comes from the `CollectionEntry` we destructured
            // in `push_front` and `push_back`.
            Some(unsafe { CollectionLink::into_entry(link) })
        }

        /// Pushes an entry at the start of the collection.
        pub fn push_front<'this>(self: Pin<&'this mut Self>, entry: CollectionEntry<E, H, L>) {
            let link = entry.into_link::<'this>();
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            unsafe { self.head_mut().push_front(link) };
        }

        /// Pushes an entry at the back of the collection.
        pub fn push_back<'this>(self: Pin<&'this mut Self>, entry: CollectionEntry<E, H, L>) {
            let link = entry.into_link::<'this>();
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            unsafe { self.head_mut().push_back(link) };
        }

        /// Filters out elements satisfying the predicate, and drops them.
        pub fn retain<F>(self: Pin<&mut Self>, mut f: F)
        where
            F: FnMut(CollectionEntryRef<'_, E, H, L>) -> bool,
        {
            // SAFETY: `CollectionEntry` can only be constructed if the
            // associated handle's refcount is greater than 0, so the backing
            // context space will always be valid.
            //
            // FIXME: this will leak handles as we can't drop them without
            // invoking undefined behaviour (since dropping an entry can lead to
            // dropping the context space the link is stored in).
            unsafe {
                self.head_mut()
                    .retain(|link| f(CollectionLink::as_entry(link)))
            };
        }
    }

    #[pinned_init::pinned_drop]
    impl<El, H, L> PinnedDrop for CollectionHead<El, H, L>
    where
        El: CollectionElement<L, LinkImpl = NtList> + IntoContextSpace,
        H: HasContext<El>,
        L: NtTypedList<T = NtList>,
    {
        fn drop(self: Pin<&mut Self>) {
            self.clear();
        }
    }
}

mod link {
    use core::{
        cell::UnsafeCell,
        marker::PhantomData,
        mem::ManuallyDrop,
        sync::atomic::{AtomicPtr, Ordering},
    };

    use nt_list::{
        list::{NtList, NtListEntry},
        single_list::{NtSingleList, NtSingleListEntry},
        NtListElement, NtTypedList,
    };

    use crate::{
        context_space::IntoContextSpace,
        handle::{HasContext, RawObject},
    };

    use super::{CollectionElement, CollectionEntry, CollectionEntryRef};

    /// A general linked-list link in a collection.
    #[repr(C)]
    pub struct CollectionLinkImpl<E, H, L, Impl>
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = Impl>,
        Impl: LinkImpl,
    {
        pub(super) link: UnsafeCell<CollectionInnerLinkImpl<E, H, L, Impl>>,
        // Invariant: must only be accessed with `Acquire`/`Release` for loads
        // and stores respectively, as this guards exclusive access to link
        pub(super) handle: AtomicPtr<RawObject>,
        _phantom_data: PhantomData<(E, H, L)>,
    }

    impl<E, H, L, Impl> CollectionLinkImpl<E, H, L, Impl>
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace,
        H: HasContext<E>,
        L: NtTypedList<T = Impl>,
        Impl: LinkImpl,
    {
        pub(crate) fn as_entry<'a>(
            inner: &'a CollectionInnerLinkImpl<E, H, L, Impl>,
        ) -> CollectionEntryRef<'a, E, H, L> {
            // SAFETY: A `CollectionInnerLinkImpl` can only come from a `CollectionLinkImpl`,
            // so we'll always go to the start of `CollectionLinkImpl`.
            let this = unsafe {
                core::ptr::from_ref(inner)
                    .byte_sub(core::mem::offset_of!(Self, link))
                    .cast::<Self>()
            };
            // SAFETY: A `CollectionInnerLinkImpl` can only come from a `CollectionLinkImpl`,
            // and the link will have the correct handle and element type.
            let this = unsafe { &*this };

            let handle = this.handle.load(Ordering::Acquire);
            debug_assert!(!handle.is_null(), "link is not attached to a list");

            // SAFETY: We don't invoke the original handle destructor by
            // wrapping it in a `ManuallyDrop`.
            let handle = unsafe { ManuallyDrop::new(H::wrap_raw(handle.cast::<H::Handle>())) };

            CollectionEntryRef(handle, PhantomData)
        }

        /// ## Safety
        ///
        /// Must come from the original [`CollectionEntry`] that this (inner)
        /// link came from.
        pub(crate) unsafe fn into_entry<'a>(
            inner: &'a mut CollectionInnerLinkImpl<E, H, L, Impl>,
        ) -> CollectionEntry<E, H, L> {
            // SAFETY: A `CollectionInnerLinkImpl` can only come from a `CollectionLinkImpl`,
            // so we'll always go to the start of `CollectionLinkImpl`.
            let this = unsafe {
                core::ptr::from_ref(inner)
                    .byte_sub(core::mem::offset_of!(Self, link))
                    .cast::<Self>()
            };
            // SAFETY: A `CollectionInnerLinkImpl` can only come from a `CollectionLinkImpl`,
            // and the link will have the correct handle and element type.
            let this = unsafe { &*this };

            let handle = this.handle.load(Ordering::Acquire);
            debug_assert!(!handle.is_null(), "link is not attached to a list");

            // SAFETY: Caller guarantees that this is the orignal handle that
            // the associated `CollectionEntry` was constructed from.
            let handle = unsafe { H::wrap_raw(handle.cast::<H::Handle>()) };

            // SAFETY: Caller guarantees that this is the orignal handle that
            // the associated `CollectionEntry` was constructed from.
            unsafe { CollectionEntry::new(handle) }
        }
    }

    // SAFETY:
    // - `E` must be `Sync` as we need access to `&E` on other threads,
    // - `H` must be `Sync` as we also need to access `&H` on other threads,
    // - Modifications to `entry` are guarded by `handle` (since `AtomicCell`
    //   guarantees that all loads are `Acquire` and all stores are `Release`).
    unsafe impl<E, H, L, Impl> Sync for CollectionLinkImpl<E, H, L, Impl>
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace + Sync,
        H: HasContext<E> + Sync,
        L: NtTypedList<T = Impl>,
        Impl: LinkImpl,
    {
    }

    /// Wrapper around the link entry so that we can wrap it in an [`UnsafeCell`]
    #[repr(C)]
    pub(crate) struct CollectionInnerLinkImpl<E, H, L, Impl>(
        Impl::Entry<Self, L>,
        PhantomData<(E, H)>,
    )
    where
        L: NtTypedList<T = Impl>,
        Impl: LinkImpl;

    // SAFETY: `CollectionInnerLinkImpl` is `repr(C)`, and is implemented using
    // `offset_of` which always gets the correct offset.
    unsafe impl<E, H, L, Impl> nt_list::NtListElement<L> for CollectionInnerLinkImpl<E, H, L, Impl>
    where
        L: NtTypedList<T = Impl>,
        Impl: LinkImpl,
    {
        fn offset() -> usize {
            core::mem::offset_of!(Self, 0)
        }
    }

    /// Link backends for a [`CollectionLinkImpl`]. Can be a [`NtSingleList`] for a
    /// singly-linked list link, or [`NtList`] for a doubly-linked list link.
    // sealed!
    pub trait LinkImpl: sealed::Sealed {
        type Entry<E, L>
        where
            E: NtListElement<L>,
            L: NtTypedList<T = Self>;

        type Iter<'a, E, L>: Iterator<Item = &'a E>
        where
            E: NtListElement<L> + 'a,
            L: NtTypedList<T = Self> + 'static;
    }

    impl LinkImpl for NtSingleList {
        type Entry<E, L> = NtSingleListEntry<E, L>
        where
            E: NtListElement<L>,
            L: NtTypedList<T = Self>;

        type Iter<'a, E, L> = nt_list::single_list::Iter<'a, E, L>
        where
            E: NtListElement<L> + 'a,
            L: NtTypedList<T = Self> + 'static;
    }

    impl LinkImpl for NtList {
        type Entry<E, L> = NtListEntry<E, L>
        where
            E: NtListElement<L>,
            L: NtTypedList<T = Self>;

        type Iter<'a, E, L> = nt_list::list::Iter<'a, E, L>
        where
            E: NtListElement<L> + 'a,
            L: NtTypedList<T = Self> + 'static;
    }

    mod sealed {
        use nt_list::{list::NtList, single_list::NtSingleList};

        pub trait Sealed {}

        impl Sealed for NtSingleList {}
        impl Sealed for NtList {}
    }
}

pub(crate) mod list_impl {
    //! List implementation details
    use core::{iter::FusedIterator, marker::PhantomData};

    use nt_list::{list::NtList, NtTypedList};

    use crate::{context_space::IntoContextSpace, handle::HasContext};

    use super::{
        link::{CollectionInnerLinkImpl, CollectionLinkImpl, LinkImpl},
        CollectionElement, CollectionEntryRef,
    };

    /// Iterator over a collection of elements.
    pub struct Iter<'a, E, H, L, Impl>(
        pub(crate) Impl::Iter<'a, CollectionInnerLinkImpl<E, H, L, Impl>, L>,
        PhantomData<(E, H)>,
    )
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace + 'a,
        H: HasContext<E> + 'a,
        L: NtTypedList<T = Impl> + 'static,
        Impl: LinkImpl + 'static;

    impl<'a, E, H, L, Impl> Iter<'a, E, H, L, Impl>
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace + 'a,
        H: HasContext<E> + 'a,
        L: NtTypedList<T = Impl> + 'static,
        Impl: LinkImpl + 'static,
    {
        pub(super) fn new(iter: Impl::Iter<'a, CollectionInnerLinkImpl<E, H, L, Impl>, L>) -> Self {
            Self(iter, PhantomData)
        }
    }

    impl<'a, E, H, L, Impl> Iterator for Iter<'a, E, H, L, Impl>
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace + 'a,
        H: HasContext<E> + 'a,
        L: NtTypedList<T = Impl> + 'static,
        Impl: LinkImpl + 'static,
    {
        type Item = CollectionEntryRef<'a, E, H, L>;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next().map(CollectionLinkImpl::as_entry)
        }
    }

    impl<'a, E, H, L, Impl> FusedIterator for Iter<'a, E, H, L, Impl>
    where
        E: CollectionElement<L, LinkImpl = Impl> + IntoContextSpace + 'a,
        H: HasContext<E> + 'a,
        L: NtTypedList<T = Impl> + 'static,
        Impl: LinkImpl + 'static,
    {
    }

    impl<'a, E, H, L> DoubleEndedIterator for Iter<'a, E, H, L, NtList>
    where
        E: CollectionElement<L, LinkImpl = NtList> + IntoContextSpace + 'a,
        H: HasContext<E> + 'a,
        L: NtTypedList<T = NtList> + 'static,
    {
        fn next_back(&mut self) -> Option<Self::Item> {
            self.0.next_back().map(CollectionLinkImpl::as_entry)
        }
    }
}

pub trait CollectionElement<L>: IntoContextSpace + Sized
where
    L: NtTypedList<T = Self::LinkImpl>,
{
    type LinkImpl: LinkImpl;

    fn link<H: HasContext<Self>>(
        self: Pin<&Self>,
    ) -> &CollectionLinkImpl<Self, H, L, Self::LinkImpl>;

    fn link_mut<H: HasContext<Self>>(
        self: Pin<&mut Self>,
    ) -> &mut CollectionLinkImpl<Self, H, L, Self::LinkImpl>;
}

// Invariant: Having a `CollectionEntry` implies that the associated
// `CollectionLinkImpl` can be accessed exclusively.
#[repr(transparent)]
pub struct CollectionEntry<E, H, L>(H, PhantomData<(L, E)>)
where
    E: CollectionElement<L> + IntoContextSpace,
    H: HasContext<E>,
    L: NtTypedList<T = E::LinkImpl>;

impl<E, H, L> CollectionEntry<E, H, L>
where
    E: CollectionElement<L> + IntoContextSpace,
    H: HasContext<E>,
    L: NtTypedList<T = E::LinkImpl>,
{
    /// Creates a [`CollectionEntry`] in order to be able to insert the handle
    /// into a collection.
    pub fn into_entry(mut handle: Unique<H, E>) -> CollectionEntry<E, H, L> {
        // Mark this entry as having exclusive access over the link
        {
            let raw_handle = handle.raw_handle().cast();
            let link = handle.get_context_mut().link_mut::<H>();
            *link.handle.get_mut() = raw_handle;
        }

        // SAFETY: `Unique` guarantees that we have exclusive access to the context area
        // with the collection link, and we've set the raw handle
        unsafe { Self::new(handle.into_shared()) }
    }

    /// Tries to create a [`CollectionEntry`] in order to be able to insert
    /// the handle into a collection. If the handle is already a part of a
    /// collection of this type, the original handle is returned.
    pub fn try_into_entry(handle: H) -> Result<CollectionEntry<E, H, L>, H> {
        // Try to mark this entry as having exclusive access over the link
        {
            let raw_handle = handle.as_object_handle().cast();
            let link = handle.get_context().link::<H>();
            let Ok(_) = link.handle.compare_exchange(
                core::ptr::null_mut(),
                raw_handle,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) else {
                // This collection link is already owned by another `CollectionEntry`
                return Err(handle);
            };
        }

        // SAFETY: Successfully setting `CollectionLinkImpl.handle` means we have
        // exclusive access to the associated `CollectionLinkImpl`.
        Ok(unsafe { Self::new(handle) })
    }

    /// Unwraps the inner handle
    pub fn into_inner(self) -> H {
        // Release ownership of the collection link
        self.0
            .get_context()
            .link::<H>()
            .handle
            .store(core::ptr::null_mut(), Ordering::Release);

        self.0
    }

    /// ## Safety
    ///
    /// Must ensure that there is exclusive access to the associated [`ContextLink`]
    /// (i.e. by successfully setting [`CollectionLinkImpl::handle`]).
    unsafe fn new(handle: H) -> Self {
        Self(handle, PhantomData)
    }

    fn into_link<'a>(self) -> &'a mut CollectionInnerLinkImpl<E, H, L, E::LinkImpl> {
        let entry = ManuallyDrop::new(self);
        let inner = entry.0.get_context().link::<H>().link.get();

        // SAFETY: Having a `CollectionLinkImpl` means we have exclusive access to the
        // associated `CollectionInnerLinkImpl`.
        unsafe { &mut *inner }
    }
}

/// A reference to an entry in a collection. Equivalent to a `&CollectionEntry<E, H, L>`.
#[repr(transparent)]
pub struct CollectionEntryRef<'a, E, H, L>(ManuallyDrop<H>, PhantomData<(&'a (), H, L, E)>)
where
    E: CollectionElement<L> + IntoContextSpace,
    H: HasContext<E>,
    L: NtTypedList<T = E::LinkImpl>;
