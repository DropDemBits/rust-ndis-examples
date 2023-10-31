//! Rust KMDF Abstractions
#![no_std]
#![deny(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::undocumented_unsafe_blocks
)]

#[macro_export]
#[doc(hidden)]
macro_rules! cstr {
    ($str:expr) => {{
        match ::core::ffi::CStr::from_bytes_with_nul(concat!($str, "\0").as_bytes()) {
            Ok(it) => it,
            Err(_) => panic!("unreachable code: always concat a nul terminator at the end"),
        }
    }};
}

pub mod raw;

pub mod file_object {
    use core::marker::PhantomData;

    use pinned_init::PinInit;
    use wdf_kmdf_sys::WDFFILEOBJECT;
    use windows_kernel_sys::Error;

    use crate::object::{self, IntoContextSpace};

    pub struct FileObject<T>(WDFFILEOBJECT, PhantomData<T>);

    impl<T> core::fmt::Debug for FileObject<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_tuple("FileObject").field(&self.0).finish()
        }
    }

    impl<T> FileObject<T> {
        /// Wraps the raw handle in a file object wrapper
        ///
        /// ## Safety
        ///
        /// Respect aliasing rules, since this can be used to
        /// generate aliasing mutable references to the context space.
        /// Also, the context space must be initialized.
        // FIXME: Make a proper wrapper eventually
        pub unsafe fn wrap(handle: WDFFILEOBJECT) -> Self {
            Self(handle, PhantomData)
        }

        /// Gets the file object handle for use with WDF functions that don't have clean wrappers yet
        pub fn raw_handle(&mut self) -> WDFFILEOBJECT {
            self.0
        }
    }

    impl<T> FileObject<T>
    where
        T: IntoContextSpace,
    {
        pub fn get_context(&self) -> object::ContextSpaceGuard<'_, T> {
            object::get_context(self).expect("context space was not initialized")
        }

        /// ## Safety
        ///
        /// - Must only be initializing the context space once
        pub unsafe fn init_context_space(
            &mut self,
            init_context: impl PinInit<T, Error>,
        ) -> Result<(), Error> {
            // SAFETY: By construction of `Self`, this guarantees that we have the context space
            // Caller guarantees that this is only called once
            unsafe { object::context_pin_init(self, |_| Ok(init_context)) }
        }
    }

    impl<T> object::AsObjectHandle for FileObject<T> {
        fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }

        fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }
    }
}

pub mod object {

    use core::{
        marker::PhantomData,
        mem::MaybeUninit,
        pin::Pin,
        sync::atomic::{AtomicU8, AtomicUsize, Ordering},
    };

    use pinned_init::PinInit;
    use wdf_kmdf_sys::{
        WDFOBJECT, WDF_EXECUTION_LEVEL, WDF_OBJECT_ATTRIBUTES, WDF_OBJECT_CONTEXT_TYPE_INFO,
        WDF_SYNCHRONIZATION_SCOPE,
    };
    use windows_kernel_sys::{result::STATUS, Error};

    use crate::raw;

    #[doc(hidden)]
    pub mod __macro_internals {
        pub use crate::cstr;
        pub use static_assertions::const_assert;
        pub use windows_kernel_sys::MEMORY_ALLOCATION_ALIGNMENT;
    }

    pub type ContextInfo = WDF_OBJECT_CONTEXT_TYPE_INFO;

    #[macro_export]
    macro_rules! impl_context_space {
        ($ty:ty) => {
            // Safety: We statically assert that $ty has the correct alignment,
            // and we use `core::mem::size_of` on $ty to get the right size.
            unsafe impl $crate::object::IntoContextSpace for $ty {
                const CONTEXT_INFO: &'static $crate::object::ContextInfo =
                    &$crate::object::ContextInfo {
                        // Size of `ContextInfo` is known to be small
                        Size: ::core::mem::size_of::<$crate::object::ContextInfo>() as u32,
                        // Safety: We always concatenate a nul at the end
                        ContextName: $crate::object::__macro_internals::cstr!(stringify!($ty))
                            .as_ptr(),
                        ContextSize: ::core::mem::size_of::<$ty>(),
                        // Set to null because this appears to only be used to
                        // work around having multiple definitions of the same
                        // context type info in the same translation unit.
                        //
                        // We also don't use `UniqueType` as as consts can't
                        // refer to statics, which would be required for
                        // `UniqueType` to point to the same context info.
                        UniqueType: ::core::ptr::null(),
                        EvtDriverGetUniqueContextType: None,
                    };
            }

            // Alignment of context type should be smaller than or equal to the arch's defined alignment
            $crate::object::__macro_internals::const_assert!(
                ::core::mem::align_of::<$ty>()
                    <= $crate::object::__macro_internals::MEMORY_ALLOCATION_ALIGNMENT as usize
            );
        };
    }

    /// Converting a data struct into a context space
    ///
    /// ## Safety
    ///
    /// - Type sizing must be accurate.
    /// - Alignment must be smaller than or equal to the architecture defined alignment
    ///   (16 on 64-bit systems, 8 on 32-bit systems)
    ///
    /// Use the [`impl_context_space`] macro to do it safely.
    pub unsafe trait IntoContextSpace {
        const CONTEXT_INFO: &'static ContextInfo;
    }

    /// The maximum IRQL the object's event callback functions will be called at
    ///
    /// Execution levels can only be specified for:
    ///
    ///
    /// - Driver objects
    /// - Device objects
    /// - File objects
    /// - General objects
    ///
    /// And since WDF 1.9 and later:
    ///
    /// - Queue objects
    /// - Timer objects
    #[derive(Copy, Clone, PartialEq, Eq, Default)]
    #[repr(i32)]
    pub enum ExecutionLevel {
        /// Use the execution level from the parent device object.
        ///
        /// This is also used as the default execution level for [`Driver`](crate::driver::Driver),
        /// where:
        /// - In KMDF, the default execution level is [`ExecutionLevel::Dispatch`]
        /// - In UMDF, the default execution level is [`ExecutionLevel::Passive`]
        #[default]
        InheritFromParent = WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent.0,
        /// Always execute event callbacks at IRQL == PASSIVE_LEVEL
        Passive = WDF_EXECUTION_LEVEL::WdfExecutionLevelPassive.0,
        /// Execute event callbacks at IRQL <= DISPATCH_LEVEL (KMDF only)
        Dispatch = WDF_EXECUTION_LEVEL::WdfExecutionLevelDispatch.0,
    }

    /// What scope to sequentialize event callbacks at
    ///
    /// Synchronization scopes can only be specified for
    ///
    /// - Driver objects
    /// - Device objects
    /// - Queue objects
    #[derive(Copy, Clone, PartialEq, Eq, Default)]
    #[repr(i32)]
    pub enum SynchronizationScope {
        /// Use the synchronization scope from the parent device object.
        ///
        /// This is also used as the default synchronization level for [`Driver`](crate::driver::Driver),
        /// where it is [`SynchronizationScope::None`].
        #[default]
        InheritFromParent = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent.0,
        /// Event callbacks for all of the device's descendant objects will be executed sequentially (i.e.
        /// before executing an event callback, the device's synchronization lock is acquired).
        ///
        /// This applies to:
        ///
        /// - Queue objects
        /// - File objects
        ///
        /// If the `AutomaticSerialization` flag is true in the object's config struct,
        /// this also applies to the event callbacks of:
        ///
        /// - Interrupt objects
        /// - DPC objects
        /// - Work Item objects
        /// - Timer objects
        ///
        /// Note that this is sequentialization does not happen between device object hierarchies,
        /// which are free to independently execute event callbacks concurrently.
        Device = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeDevice.0,
        /// Event callbacks for a queue will be executed sequentially (i.e. before executing a queue
        /// event callback, the queue's synchronization lock is acquired).
        ///
        /// If the `AutomaticSerialization` flag is true in the object's config struct,
        /// this also applies to the event callbacks of:
        ///
        /// - Interrupt objects
        /// - DPC objects
        /// - Work Item objects
        /// - Timer objects
        ///
        /// Note that this is sequentialization does not happen between queue object hierarchies,
        /// which are free to independently execute event callbacks concurrently.
        ///
        /// Since WDF 1.9, `SynchronizationScope::Queue` should be specified on the queue objects
        /// themselves.
        /// For older WDF versions, the queue object should specify `SynchronizationScope::InheritFromParent`,
        /// and the parent object should specify `SynchronizationScope::Queue`
        Queue = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeQueue.0,
        /// No synchronization is performed, so event callbacks may execute concurrently
        None = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeNone.0,
    }

    pub fn default_object_attributes<T: IntoContextSpace>() -> WDF_OBJECT_ATTRIBUTES {
        let mut object_attrs = WDF_OBJECT_ATTRIBUTES::init();

        // Always set the context info
        object_attrs.ContextTypeInfo = T::CONTEXT_INFO as *const _;
        // Make space for the drop lock at the end
        object_attrs.ContextSizeOverride = core::mem::size_of::<ContextSpaceWithDropLock<T>>();

        object_attrs
    }

    /// Converts the typed handle into the generic version
    pub trait AsObjectHandle {
        fn as_handle(&self) -> WDFOBJECT;
        fn as_handle_mut(&mut self) -> WDFOBJECT;
    }

    /// Wraps a raw object pointer in a handle
    pub struct RawObjectHandle(pub WDFOBJECT);

    impl AsObjectHandle for RawObjectHandle {
        fn as_handle(&self) -> WDFOBJECT {
            self.0
        }

        fn as_handle_mut(&mut self) -> WDFOBJECT {
            self.0
        }
    }

    // Modes:
    // - Wrapped (nothing to do in drop)
    // - Owned (need drop for delete)
    // - Parented (need drop for delete so that it can be deleted sooner)
    // - Ref (need to deref)

    #[derive(Debug)]
    pub(crate) enum HandleKind {
        Wrapped,
        Owned,
        Parented,
        Ref,
    }

    pub struct GeneralObject<T> {
        handle: WDFOBJECT,
        kind: HandleKind,
        _context: PhantomData<T>,
    }

    impl<T> core::fmt::Debug for GeneralObject<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_struct("GeneralObject")
                .field("handle", &self.handle)
                .field("kind", &self.kind)
                .finish()
        }
    }

    impl<T> GeneralObject<T>
    where
        T: IntoContextSpace,
    {
        /// Creates a new WDF General Object
        ///
        /// ## IRQL: <= `DISPATCH_LEVEL`
        ///
        /// ## Errors
        ///
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn create<I>(
            init_context: impl FnOnce(&mut Self) -> Result<I, Error>,
        ) -> Result<Self, Error>
        where
            I: PinInit<T, Error>,
        {
            Self::_create(default_object_attributes::<T>(), init_context)
        }

        /// Creates a new WDF General Object attached to an object
        ///
        /// ## IRQL: <= `DISPATCH_LEVEL`
        ///
        /// ## Errors
        ///
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn with_parent<I>(
            parent: &impl AsObjectHandle,
            init_context: impl FnOnce(&mut Self) -> Result<I, Error>,
        ) -> Result<Self, Error>
        where
            I: PinInit<T, Error>,
        {
            let mut object_attrs = default_object_attributes::<T>();
            object_attrs.ParentObject = parent.as_handle();
            Self::_create(object_attrs, init_context)
        }

        fn _create<I>(
            mut object_attrs: wdf_kmdf_sys::WDF_OBJECT_ATTRIBUTES,
            init_context: impl FnOnce(&mut Self) -> Result<I, Error>,
        ) -> Result<Self, Error>
        where
            I: PinInit<T, Error>,
        {
            let kind = if object_attrs.ParentObject.is_null() {
                HandleKind::Owned
            } else {
                HandleKind::Parented
            };

            // Add the drop callback
            object_attrs.EvtDestroyCallback = Some(Self::__dispatch_evt_destroy);

            // Make the object!
            let mut handle = {
                let mut handle = core::ptr::null_mut();

                // SAFETY:
                // - Caller ensures that we're at the right IRQL
                unsafe {
                    Error::to_err(raw::WdfObjectCreate(Some(&mut object_attrs), &mut handle))
                }?;

                Self {
                    handle,
                    kind,
                    _context: PhantomData,
                }
            };

            // SAFETY:
            // - It's WDF's responsibility to insert the context area, since we create
            //   the default object attributes with T's context area
            // - The object was just created, and the context space has not been initialized yet
            let status = unsafe { crate::object::context_pin_init(&mut handle, init_context) };

            if let Err(err) = status {
                // Drop the handle so that we clean up any resources allocated by `init_context`
                drop(handle);

                Err(err)
            } else {
                Ok(handle)
            }
        }

        /// Wraps the raw handle in a general object wrapper
        ///
        /// ## Safety
        ///
        /// Respect aliasing rules, since this can be used to
        /// generate aliasing mutable references to the context space.
        /// Also, the context space must be initialized.
        pub unsafe fn wrap(handle: WDFOBJECT) -> Self {
            Self {
                handle,
                kind: HandleKind::Wrapped,
                _context: PhantomData,
            }
        }

        /// Gets the object handle for use with WDF functions that don't have clean wrappers yet
        pub fn raw_handle(&self) -> WDFOBJECT {
            self.handle
        }

        pub fn get_context(&self) -> Result<ContextSpaceGuard<'_, T>, GetContextSpaceError> {
            crate::object::get_context(self)
        }

        /// Makes another shared reference to the general object
        ///
        /// ## IRQL: <= Dispatch
        pub fn clone_ref(&self) -> GeneralObject<T> {
            // Safety: Caller ensures that we're at the right IRQL
            unsafe {
                raw::WdfObjectReferenceActual(
                    self.handle,
                    None,
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                )
            }

            GeneralObject {
                kind: HandleKind::Ref,
                ..*self
            }
        }

        unsafe extern "C" fn __dispatch_evt_destroy(object: wdf_kmdf_sys::WDFOBJECT) {
            // SAFETY: EvtDestroy only gets called once, and once all other references are gone
            let handle = unsafe { Self::wrap(object) };

            // Drop the context area
            let status = crate::object::drop_context_space::<T>(&handle, |_| ());

            if let Err(err) = status {
                // No (valid) context space to drop, nothing to do
                windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
            }
        }
    }

    impl<T> Drop for GeneralObject<T> {
        fn drop(&mut self) {
            // FIXME: Assert that this is at <= DISPATCH_LEVEL
            match self.kind {
                HandleKind::Wrapped => {}
                // Safety: assertion that we're at the correct IRQL
                HandleKind::Owned | HandleKind::Parented => unsafe {
                    raw::WdfObjectDelete(self.handle)
                },
                // Safety: assertion that we're at the correct IRQL
                HandleKind::Ref => unsafe {
                    raw::WdfObjectDereferenceActual(
                        self.handle,
                        None,
                        line!() as i32,
                        Some(cstr!(file!()).as_ptr()),
                    )
                },
            }
        }
    }

    impl<T> AsObjectHandle for GeneralObject<T> {
        fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
            self.handle.cast()
        }

        fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
            self.handle.cast()
        }
    }

    impl<T> PartialEq for GeneralObject<T> {
        fn eq(&self, other: &Self) -> bool {
            self.handle == other.handle
        }
    }

    impl<T> Eq for GeneralObject<T> {}

    #[repr(C)]
    struct ContextSpaceWithDropLock<T> {
        /// Original context space
        data: MaybeUninit<T>,
        /// Drop lock ensuring that `EvtDestroyCallback` / `EvtDriverUnload` have
        /// exclusive access to the context area.
        ///
        /// While WDF ensures that all WDF objects that may access a context area
        /// and are appropriatedly parented or refcounted (e.g. timers, work items,
        /// and DPCs) get completed before driver unload / object destruction.
        ///
        /// However, in the situation of improperly parented objects, bad refcounts,
        /// or other threads (created using `PsCreateSystemThread`) potentially
        /// accessing the context area at the wrong time, they may cause a data
        /// race as other live shared references breaks the exclusivity invariant
        /// required for dropping an object.
        ///
        /// `DropLock` (in combination with [`ContextSpaceGuard`]) ensures that
        /// dropping waits for all existing readers to drop their access, and
        /// prevents new readers from getting access to the about-to-be-dropped
        /// context space.
        //
        // Note: WDF Guarantees that the entire context space is zero-initialized,
        // so `readers` and `init_flag` of `DropLock` are both initialized to 0
        drop_lock: DropLock,
    }

    impl<T> ContextSpaceWithDropLock<T> {
        /// ## Safety
        ///
        /// `this` must be a pointer to an allocated context space, and must
        /// be valid for the whole area
        unsafe fn drop_lock<'a>(this: *mut Self) -> &'a DropLock
        where
            'static: 'a,
        {
            // SAFETY: Caller guarantees that the pointer is to an allocated context space
            // and that it is to the full context space area
            let drop_lock = unsafe { core::ptr::addr_of!((*this).drop_lock) };

            // SAFETY: Context space is guaranteed to be aligned to at most 16-byte alignment,
            // and all possible bit patterns of both `u8` and `usize` (which `AtomicU8` and `AtomicUsize` respectively has the same repr as)
            // are valid values
            unsafe { &*drop_lock }
        }
    }

    /// Marker for when the context space is initialized
    const CONTEXT_SPACE_INIT_MARKER: u8 = 0xAA;
    /// Marker for when the context space has been dropped
    const CONTEXT_SPACE_DROPPED_MARKER: u8 = 0xDD;

    struct DropLock {
        /// How many active read referencences are there before it can be dropped
        readers: AtomicUsize,
        /// Current initialization state
        ///
        /// Representing the initialization flag as a `AtomicU8` because we need to synchronize
        /// access to readers, and also because multiple threads may be trying accessing the
        /// context area
        init_flag: AtomicU8,
    }

    impl DropLock {
        fn acquire(&self) -> Result<DropGuard<'_>, GetContextSpaceError> {
            // Pre-acquire a reader reference so that we don't accidentally get the context space
            // dropped before we can acquire it
            //
            // Is Relaxed since this is a counter, and `init_flag` is responsible
            // for indicating if the context space is actually initialized or not
            self.readers
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |readers| {
                    readers.checked_add(1)
                })
                .expect("too many context space readers");

            let guard = DropGuard { drop_lock: self };

            // Check if the context space has been initialized
            if !self.is_init() {
                // Not initialized
                drop(guard);
                return Err(GetContextSpaceError::NotInitialized);
            }

            Ok(guard)
        }

        fn is_init(&self) -> bool {
            // `Acquire` is paired with the `Release `store in `mark_initialized`
            // and `acquire_exclusive`
            self.init_flag.load(Ordering::Acquire) == CONTEXT_SPACE_INIT_MARKER
        }

        /// ## Safety
        ///
        /// The associated context space must be initialized
        unsafe fn mark_initialized(&self) {
            // Needs to be `Release` ordering so as to guarantee that the initialization
            // writes are visible to other threads
            self.init_flag
                .store(CONTEXT_SPACE_INIT_MARKER, Ordering::Release)
        }

        fn acquire_exclusive(&self) -> Result<(), GetContextSpaceError> {
            // Check if the context space is still initialized and we're the only one dropping it
            //
            // Success ordering is `Acquire` ordering to pair with the `Release` store in here and in `mark_initialized`
            // Failure ordering is `Relaxed` because we don't care about the value
            if self
                .init_flag
                .compare_exchange(
                    CONTEXT_SPACE_INIT_MARKER,
                    CONTEXT_SPACE_DROPPED_MARKER,
                    Ordering::Acquire,
                    Ordering::Relaxed,
                )
                .is_err()
            {
                // Context space has either already been dropped, in the process of being dropped, or was never initialized
                return Err(GetContextSpaceError::NotInitialized);
            }

            // Wait for any remaining readers to drop their context space guards.
            // There won't be any additional readers added because acquiring a
            // context space guard requires that the `init_flag` is `CONTEXT_SPACE_INIT_MARKER`,
            // but we've set `init_flag` to `CONTEXT_SPACE_DROP_MARKER`
            //
            // Is `Relaxed` ordering because it is not related to any other access
            // (just a counter for how many readers there are).
            while self.readers.load(Ordering::Relaxed) != 0 {
                core::hint::spin_loop();
            }

            Ok(())
        }
    }

    /// Guard that prevents the context area from being dropped
    struct DropGuard<'a> {
        drop_lock: &'a DropLock,
    }

    impl<'a> Drop for DropGuard<'a> {
        fn drop(&mut self) {
            self.drop_lock.readers.fetch_sub(1, Ordering::Relaxed);
        }
    }

    pub struct ContextSpaceGuard<'a, T> {
        context_space: &'a ContextSpaceWithDropLock<T>,
        _guard: DropGuard<'a>,
    }

    impl<'a, T> ContextSpaceGuard<'a, T> {
        fn new(
            context_space: &'a ContextSpaceWithDropLock<T>,
        ) -> Result<Self, GetContextSpaceError> {
            // Try to acquire the drop lock
            // Getting a guard ensures that the context space has been initialized
            let guard = context_space.drop_lock.acquire()?;

            // Make the context space guard!
            Ok(ContextSpaceGuard {
                context_space,
                _guard: guard,
            })
        }
    }

    impl<'a, T> core::ops::Deref for ContextSpaceGuard<'a, T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            // SAFETY: By having an instance of `ContextSpace`, we know the originating context space is
            // valid and initialized
            unsafe { &*self.context_space.data.as_ptr() }
        }
    }

    /// ## Safety:
    ///
    /// - `context_space` must be a pointer to the original context space
    /// - The context space must actually be initialized
    unsafe fn mark_context_space_init<T: IntoContextSpace>(
        context_space: *mut ContextSpaceWithDropLock<T>,
    ) {
        // SAFETY: Caller guarantees that the pointer is to an allocated context space
        // and that it is to the full context space area
        let drop_lock = unsafe { ContextSpaceWithDropLock::drop_lock(context_space) };

        // SAFETY: Caller guarantees that the context space is initialized
        unsafe { drop_lock.mark_initialized() };
    }

    fn get_context_space_ptr<T: IntoContextSpace>(
        handle: &impl AsObjectHandle,
    ) -> Result<*mut ContextSpaceWithDropLock<T>, GetContextSpaceError> {
        let handle = handle.as_handle();

        // SAFETY: `handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space = unsafe { raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO) };

        if context_space.is_null() {
            // `context_space` is only NULL if `T` doesnt't have a context on the object
            return Err(GetContextSpaceError::NotFound);
        }

        // Note: `context_space` comes from `WdfGetTypedContextWorker`, which
        // returns a pointer to the base of the context space so we can get the additional metadata
        let context_space = context_space.cast::<ContextSpaceWithDropLock<T>>();

        Ok(context_space)
    }

    /// Gets a ref to the associated context space, or the appropriate [`GetContextSpaceError`] code
    pub(crate) fn get_context<T: IntoContextSpace>(
        handle: &impl AsObjectHandle,
    ) -> Result<ContextSpaceGuard<'_, T>, GetContextSpaceError> {
        let context_space = get_context_space_ptr::<T>(handle)?;

        // SAFETY:
        // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        // - `IntoContextSpace` requires alignment to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - We've checked `context_space` to not be NULL
        //
        // Note that the actual context space data isn't required to be initialized since it's behind a `MaybeUninit`
        let context_space = unsafe { &*context_space };

        // Make the guard to the context space
        ContextSpaceGuard::new(context_space)
    }

    /// Drops the associated context space
    ///
    /// Allows performing extra work before the context space is dropped.
    ///
    /// This waits for any additional readers to stop accessing the context space before dropping,
    /// and this wait may take a bit. Care must be taken for other readers to not hold their [`ContextSpaceGuard`]
    /// for too long in order to drop in a timely manner.
    ///
    /// Returns the appropriate [`GetContextError::NotInitialized`] code if the context space was never initialized or already dropped,
    /// or [`GetContextError::NotFound`] if this was not a context space of the object
    pub(crate) fn drop_context_space<T: IntoContextSpace>(
        handle: &impl AsObjectHandle,
        additional_work: impl FnOnce(Pin<&mut T>),
    ) -> Result<(), GetContextSpaceError> {
        let context_space = get_context_space_ptr::<T>(handle)?;

        // SAFETY: `get_context_space_ptr` returns a pointer to the base of the context space
        let drop_lock = unsafe { ContextSpaceWithDropLock::drop_lock(context_space) };

        // Ensure that we are the only user of the context space,
        // and that the context space is still initialized
        drop_lock.acquire_exclusive()?;

        // Do any additonal work before dropping the context area,
        // which requires a pinned mut ref
        //
        // No other threads have access to the context space,
        // so we can soundly make a pinned mutable ref to the context area
        //
        // SAFETY:
        // - `context_space` comes from `WdfGetTypedContextWorker`, which
        //   returns a pointer to the base of the context space
        // - WDF aligns memory allocations to MEMORY_ALLOCATION_ALIGNMENT
        //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        // - `IntoContextSpace` requires alignment to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - `DropLock::acquire_exlusive` ensures that the context space is initialized,
        //   and we have exclusive access to the context space
        let context_space = unsafe { &mut *context_space };
        // SAFETY: Ditto
        let context_space = unsafe { &mut *context_space.data.as_mut_ptr() };

        // SAFETY: An object context space is heap-allocated and has a stable address
        let mut context_space = unsafe { Pin::new_unchecked(context_space) };

        // Do any additional work before we drop the context space
        additional_work(context_space.as_mut());

        // And then drop it!
        // SAFETY: We're only dropping it below
        let context_space = unsafe { context_space.get_unchecked_mut() };

        // SAFETY:
        // Guaranteed to be unused by this point, since the destroy callback is the last one
        // called.
        // Object initialization guarantees that this was valid initialized memory
        unsafe { core::ptr::drop_in_place(context_space) };

        Ok(())
    }

    /// Error encountered while trying to get an object context space
    #[derive(Debug)]
    pub enum GetContextSpaceError {
        /// The context space was not found
        NotFound,
        /// The context space has not been initialized yet,
        /// or initialization of the context space has failed
        NotInitialized,
    }

    impl From<GetContextSpaceError> for Error {
        fn from(value: GetContextSpaceError) -> Self {
            match value {
                GetContextSpaceError::NotFound => Error(STATUS::NOT_FOUND),
                GetContextSpaceError::NotInitialized => Error(STATUS::INVALID_DEVICE_STATE),
            }
        }
    }

    /// Initializes the object's context area, using the closure provided
    ///
    /// ## Safety
    ///
    /// - Must not reinitialize an object's context space
    /// - Object must actually have the context space
    pub(crate) unsafe fn context_pin_init<T, Handle, I, Err>(
        handle: &mut Handle,
        init_context: impl FnOnce(&mut Handle) -> Result<I, Err>,
    ) -> Result<(), Err>
    where
        T: IntoContextSpace,
        Handle: AsObjectHandle,
        I: PinInit<T, Err>,
    {
        let raw_handle = handle.as_handle_mut();

        // SAFETY: `raw_handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            raw::WdfObjectGetTypedContextWorker(raw_handle, T::CONTEXT_INFO)
        };
        let context_space = context_space.cast::<T>();

        // Get closure to initialize context with
        //
        // Note: while this can panic, since we're assuming we're in a
        // `panic=abort` context, we won't ever have access to an uninitialized
        // context area as a result of panicking.
        //
        // However, if the handle were to ever lead to getting the context
        // space during initialization of the same context space, then we could
        // technically access uninitialized memory. This can happen if we were
        // to access the driver context area via having a handle to a device
        // (which allows getting the driver).
        //
        // We'd also have to do this anyway if we were to be in a `panic=unwind`
        // context (which might supported if we link ucrt in)
        let pin_init = init_context(handle)?;

        // SAFETY:
        // - The following ensures that the context space is valid pinned uninitialized memory:
        //   - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //     (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        //   - WDF does not move the allocation for the original object context
        //   - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - We directly `?` the produced error
        unsafe { pin_init.__pinned_init(context_space)? }

        // SAFETY: By this point we've successfully initialized the context space
        unsafe { mark_context_space_init::<T>(context_space.cast()) };

        Ok(())
    }
}

pub mod sync {
    //! WDF-Based synchronization primatives

    use core::{
        cell::UnsafeCell,
        marker::PhantomPinned,
        ops::{Deref, DerefMut},
        pin::Pin,
    };

    use pinned_init::{pin_data, PinInit};
    use wdf_kmdf_sys::WDFSPINLOCK;
    use windows_kernel_sys::Error;

    use crate::raw;

    /// A spin-lock based mutex protecting some data.
    ///
    /// The data is guaranteed to be pinned.
    ///
    /// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
    #[pin_data]
    pub struct SpinMutex<T> {
        /// The backing spin lock
        spin_lock: SpinLock,
        /// Data we are protecting
        #[pin]
        data: UnsafeCell<T>,
    }

    impl<T> SpinMutex<T> {
        /// Creates a new mutex
        ///
        /// ## IRQL: <= Dispatch
        ///
        /// ## Errors
        ///
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn new<E>(value: impl PinInit<T, E>) -> impl PinInit<Self, Error>
        where
            E: Into<Error>,
        {
            pinned_init::try_pin_init!(SpinMutex {
                spin_lock: SpinLock::new()?,
                data <- {
                    let init = move |slot: *mut UnsafeCell<T>| {
                        // SAFETY: by guarantees of `pin_init_from_closure`
                        unsafe { value.__pinned_init(slot.cast()).map_err(|err| err.into()) }
                    };

                    // SAFETY: `data` is pinned too, and initialization requirements are guaranteed
                    // by nature of delegating to `value`'s pinned init
                    unsafe { pinned_init::pin_init_from_closure(init)}
                }
            }? Error)
        }

        /// Tries to acquire the mutex, returning a guard if successful
        ///
        /// ## IRQL: <= Dispatch
        pub fn try_lock(&self) -> Option<Pin<SpinMutexGuard<'_, T>>> {
            let guard = self.spin_lock.acquire();

            // SAFETY: Uhhhh
            Some(unsafe {
                Pin::new_unchecked(SpinMutexGuard {
                    mutex: self,
                    _guard: guard,
                    _pin: PhantomPinned,
                })
            })
        }

        /// Locks the mutex, busy-looping until the lock is acquired
        ///
        /// ## IRQL: <= Dispatch
        pub fn lock(&self) -> Pin<SpinMutexGuard<'_, T>> {
            loop {
                let Some(guard) = self.try_lock() else {
                    core::hint::spin_loop();
                    continue;
                };
                break guard;
            }
        }
    }

    // Can't send `SpinMutex` to another thread because we've guaranteed that the storage will never move
    // impl<T> !Send for SpinMutex<T> {}

    // SAFETY: Can send &SpinMutex<T> to other threads as we can only observe `T` changing
    // when we hold the lock, and only one thread can hold the lock.
    //
    // `T` also needs to be `Send` as we need to be able to manifest a `&mut T` on any thread.
    // This is so that `SpinMutex<Rc<_>>` is invalid, as otherwise we could have any number of `Rc`'s on different threads.
    unsafe impl<T> Sync for SpinMutex<T> where T: Send {}

    /// Lock guard for a [`SpinMutex`]
    pub struct SpinMutexGuard<'a, T> {
        mutex: &'a SpinMutex<T>,
        _guard: SpinLockGuard<'a>,
        _pin: PhantomPinned,
    }

    impl<'a, T> Deref for SpinMutexGuard<'a, T> {
        type Target = T;

        #[inline]
        fn deref(&self) -> &Self::Target {
            // SAFETY: We have exclusive access to the data
            unsafe { &*self.mutex.data.get() }
        }
    }

    impl<'a, T> DerefMut for SpinMutexGuard<'a, T> {
        #[inline]
        fn deref_mut(&mut self) -> &mut Self::Target {
            // SAFETY: We have exclusive access to the data
            unsafe { &mut *self.mutex.data.get() }
        }
    }

    /// Wrapper around a framework-based spin lock.
    ///
    /// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the lock is acquired
    pub struct SpinLock(WDFSPINLOCK);

    impl SpinLock {
        /// Creates a new spin lock
        ///
        /// ## IRQL: <= Dispatch
        ///
        /// ## Errors
        ///
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn new() -> Result<Self, Error> {
            let mut lock = core::ptr::null_mut();

            // SAFETY:
            // - Caller ensures we're calling this at the right IRQL
            // - We're manually managing the lifetime of the spinlock (easier that way)
            Error::to_err(unsafe { raw::WdfSpinLockCreate(None, &mut lock) })?;

            Ok(Self(lock))
        }

        /// Acquires the spin lock, returning a guard that releases the lock once dropped
        ///
        /// ## IRQL: <= Dispatch
        pub fn acquire(&self) -> SpinLockGuard<'_> {
            // SAFETY:
            // - We created the spin lock ourselves, so it's not part of an interrupt config struct
            // - Caller ensures we're calling this at the right IRQL
            unsafe {
                raw::WdfSpinLockAcquire(self.0);
            }

            SpinLockGuard { lock: self }
        }
    }

    impl Drop for SpinLock {
        fn drop(&mut self) {
            // SAFETY:
            // - Cleanup callbacks are always called at either `DISPATCH_LEVEL` or `PASSIVE_LEVEL`
            // - We're always deleting a spin lock, which has no special deletion requirements
            unsafe { raw::WdfObjectDelete(self.0.cast()) }
        }
    }

    // SAFETY: Can manifest SpinLock to another thread (as-if we'd moved a pointer)
    unsafe impl Send for SpinLock {}
    // SAFETY: Can send &SpinLock to other threads (no interior mutability observable)
    unsafe impl Sync for SpinLock {}

    /// A guard for a [`SpinLock`]
    pub struct SpinLockGuard<'a> {
        lock: &'a SpinLock,
    }

    impl<'a> Drop for SpinLockGuard<'a> {
        fn drop(&mut self) {
            // SAFETY:
            // Having a guard guarantees we've called `WdfSpinLockAcquire`,
            // and also implies we're at IRQL `DISPATCH_LEVEL`
            unsafe { raw::WdfSpinLockRelease(self.lock.0) }
        }
    }
}

pub mod driver {
    use core::{marker::PhantomData, ops::Deref, pin::Pin};

    use pinned_init::PinInit;
    use vtable::vtable;
    use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDRIVER, WDF_DRIVER_INIT_FLAGS};
    use windows_kernel_rs::{string::unicode_string::NtUnicodeStr, DriverObject};
    use windows_kernel_sys::{result::STATUS, Error, NTSTATUS};

    use crate::{
        object::{
            self, default_object_attributes, GetContextSpaceError, HandleKind, IntoContextSpace,
        },
        raw,
    };

    pub struct Driver<T> {
        handle: WDFDRIVER,
        kind: HandleKind,
        _context: PhantomData<T>,
    }

    impl<T> Driver<T>
    where
        T: DriverCallbacks,
    {
        /// Wraps the handle in a raw driver object
        ///
        /// ## Safety
        ///
        /// Respect aliasing rules, since this can be used to
        /// generate aliasing mutable references to the context space
        pub unsafe fn wrap(handle: WDFDRIVER) -> Self {
            Self {
                handle,
                kind: HandleKind::Wrapped,
                _context: PhantomData,
            }
        }

        /// Gets the driver handle for use with WDF functions that don't have clean wrappers yet
        pub fn raw_handle(&mut self) -> WDFDRIVER {
            self.handle
        }
    }

    impl<T> object::AsObjectHandle for Driver<T> {
        fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
            self.handle.cast()
        }

        fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
            self.handle.cast()
        }
    }

    impl<T> Driver<T>
    where
        T: IntoContextSpace + DriverCallbacks,
    {
        pub fn get_context(
            &self,
        ) -> Result<object::ContextSpaceGuard<'_, T>, GetContextSpaceError> {
            object::get_context(self)
        }
    }

    /// Opaque driver handle used during the initialization of the driver's context space
    pub struct DriverHandle(WDFDRIVER);

    impl DriverHandle {
        /// Wraps the handle in a raw driver object
        ///
        /// ## Safety
        ///
        /// Respect aliasing rules, since this can be used to
        /// generate aliasing mutable references to the context space
        unsafe fn wrap(handle: WDFDRIVER) -> Self {
            Self(handle)
        }

        /// Gets the driver handle for use with WDF functions that don't have clean wrappers yet
        pub fn raw_handle(&mut self) -> WDFDRIVER {
            self.0
        }
    }

    impl object::AsObjectHandle for DriverHandle {
        fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }

        fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }
    }

    #[derive(Default, Clone, Copy)]
    pub struct DriverConfig {
        /// If the driver is a PnP driver, and thus requires an `EvtDriverDeviceAdd` callback.
        pub pnp_mode: PnpMode,
        /// Tag to mark allocations made by WDF
        pub pool_tag: Option<u32>,
    }

    #[derive(Default, Clone, Copy, PartialEq, Eq)]
    pub enum PnpMode {
        /// This is a pnp driver, and if there isn't already an `device_add` callback,
        /// the default one is used.
        #[default]
        Pnp,
        /// This is a non-pnp driver, and the `device_add` callback shouldn't be present
        NonPnp,
    }

    /// Event callbacks for a driver
    #[vtable]
    pub trait DriverCallbacks: IntoContextSpace {
        // FIXME: Just a stub, not really the real thing
        #[allow(unused)]
        fn device_add(&self, device_init: PWDFDEVICE_INIT) -> Result<(), Error> {
            Ok(())
        }

        /// Performs operations that must take place before unloading the driver.
        ///
        /// Must deallocate any non-device-specific system resources allocated during
        /// the driver entry function.
        ///
        /// ## IRQL: Passive
        ///
        /// ## Note
        ///
        /// Most ownership-related unload tasks should instead be done via the fields
        /// implementing `Drop`, rather than being manually deallocated here.
        fn unload(self: Pin<&Self>) {}
    }

    impl<T: DriverCallbacks> Driver<T> {
        /// Creates a new WDF Driver Object
        ///
        /// ## IRQL: Passive
        ///
        /// ## Errors
        ///
        /// - `STATUS_INVALID_PARAMETER` if non-pnp mode is specified, but `device_add` is also specified
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn create<I>(
            driver_object: DriverObject,
            registry_path: NtUnicodeStr<'_>,
            config: DriverConfig,
            init_context: impl FnOnce(&mut DriverHandle) -> Result<I, Error>,
        ) -> Result<(), Error>
        where
            I: PinInit<T, Error>,
        {
            if matches!(config.pnp_mode, PnpMode::NonPnp) && T::HAS_DEVICE_ADD {
                // Non-pnp drivers shouldn't specify the `device_add` callback
                return Err(Error(STATUS::INVALID_PARAMETER));
            }

            // NOTE: Since we can't `WdfObjectDelete` a driver, the framework handles
            // uninitializing the driver via EvtDriverUnload, so we don't need to set
            // EvtCleanupCallback and EvtDestroyCallback.
            //
            // In fact, the framework destroys itself after the EvtUnloadCallback,
            // so it's best to always drop the driver context area in our unload
            // trampoline.
            let mut object_attrs = default_object_attributes::<T>();
            object_attrs.EvtCleanupCallback = Some(Self::__dispatch_evt_cleanup);
            object_attrs.EvtDestroyCallback = Some(Self::__dispatch_evt_destroy);

            let mut driver_config = wdf_kmdf_sys::WDF_DRIVER_CONFIG::init(
                T::HAS_DEVICE_ADD.then_some(Self::__dispatch_driver_device_add),
            );
            driver_config.EvtDriverUnload = Some(Self::__dispatch_driver_unload);

            if matches!(config.pnp_mode, PnpMode::NonPnp) {
                driver_config.DriverInitFlags |=
                    WDF_DRIVER_INIT_FLAGS::WdfDriverInitNonPnpDriver.0 as u32;
            }

            // NOTE: This is always available since we're targeting KMDF versions after 1.5
            if let Some(tag) = config.pool_tag {
                driver_config.DriverPoolTag = tag;
            }

            // Make it!
            let mut handle = {
                let driver_object = driver_object.into_raw();

                // SAFETY: Replaced with the real driver pointer next
                let mut handle = unsafe { DriverHandle::wrap(core::ptr::null_mut()) };

                // SAFETY: Owned `DriverObject` means that it only gets called once
                unsafe {
                    Error::to_err(raw::WdfDriverCreate(
                        driver_object,
                        registry_path.as_ptr().cast(),
                        Some(&mut object_attrs),
                        &mut driver_config,
                        Some(&mut handle.0),
                    ))?;
                }

                handle
            };

            // SAFETY:
            // - It's WDF's responsibility to insert the context area, since we create
            //   the default object attributes with T's context area
            // - The driver object was just created
            unsafe { object::context_pin_init(&mut handle, init_context) }
        }

        /// Makes a shared reference to the driver
        ///
        /// ## IRQL: <= Dispatch
        pub fn clone_ref(&self) -> Driver<T> {
            // Safety: Caller ensures that we're at the right IRQL
            unsafe {
                raw::WdfObjectReferenceActual(
                    self.handle.cast(),
                    None,
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                )
            }

            Driver {
                kind: HandleKind::Ref,
                ..*self
            }
        }

        unsafe extern "C" fn __dispatch_driver_device_add(
            driver: WDFDRIVER,
            device_init: PWDFDEVICE_INIT,
        ) -> NTSTATUS {
            // NOTE: Unsure if this can be called concurrently, so for safety
            // we only use an immutable handle.
            // SAFETY: Only used behind an immutable reference, so we prevent
            // concurrent immutable mutations
            let handle = unsafe { DriverHandle::wrap(driver) };

            let context_space = match object::get_context::<T>(&handle) {
                Ok(it) => it,
                Err(err) => return Error::from(err).0.to_u32(),
            };

            match T::device_add(context_space.deref(), device_init) {
                Ok(()) => STATUS::SUCCESS.to_u32(),
                Err(err) => err.0.to_u32(),
            }
        }

        unsafe extern "C" fn __dispatch_driver_unload(driver: WDFDRIVER) {
            // SAFETY: Driver unload only gets called once, and drop waits for exclusive access
            let handle = unsafe { Driver::<T>::wrap(driver) };

            if T::HAS_UNLOAD {
                if let Ok(context_space) = handle.get_context() {
                    // SAFETY: `get_context` should be returning pinned guards
                    let context_space = unsafe { Pin::new_unchecked(&*context_space) };

                    T::unload(context_space);
                } else {
                    // No (valid) context space to unload with, nothing to do
                    windows_kernel_rs::log::warn!("No driver context space to unload");
                    return;
                }
            }

            // Drop the context area
            //
            // Needs to happen after calling unload but not in EvtDestroy
            // because unload could call functions which depend on the driver
            // context area being initialized, while also allowing manually
            // deleting owned WDF objects without accessing freed memory
            let status = object::drop_context_space::<T>(&handle, |_| ());

            if let Err(err) = status {
                // No (valid) context space to drop, nothing to do
                windows_kernel_rs::log::warn!("No driver context space to drop ({:x?})", err);
            }
        }

        unsafe extern "C" fn __dispatch_evt_cleanup(_driver: wdf_kmdf_sys::WDFOBJECT) {
            windows_kernel_rs::log::debug!("EvtCleanupCallback");
        }

        unsafe extern "C" fn __dispatch_evt_destroy(_driver: wdf_kmdf_sys::WDFOBJECT) {
            windows_kernel_rs::log::debug!("EvtDestroyCallback");
        }
    }

    impl<T> Drop for Driver<T> {
        fn drop(&mut self) {
            // FIXME: Assert that this is at <= DISPATCH_LEVEL

            // Only need to decrement ref counts, WDF takes care of deleting the driver object
            if let HandleKind::Ref = self.kind {
                // Safety: assertion that we're at the correct IRQL
                unsafe {
                    raw::WdfObjectDereferenceActual(
                        self.handle.cast(),
                        None,
                        line!() as i32,
                        Some(cstr!(file!()).as_ptr()),
                    )
                }
            }
        }
    }
}
