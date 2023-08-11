//! Rust KMDF Abstractions
#![no_std]
#![deny(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::undocumented_unsafe_blocks
)]

pub mod raw;

pub mod file_object {
    use core::marker::PhantomData;

    use wdf_kmdf_sys::WDFFILEOBJECT;

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
        pub fn get_context(&self) -> &T {
            // SAFETY: Contruction guarantees that the space is initialized
            unsafe { object::get_context(self).unwrap() }
        }

        pub fn get_context_mut(&mut self) -> &mut T {
            // SAFETY: Contruction guarantees that the space is initialized
            unsafe { object::get_context_mut(self).unwrap() }
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

    use wdf_kmdf_sys::{
        WDFOBJECT, WDF_EXECUTION_LEVEL, WDF_OBJECT_ATTRIBUTES, WDF_OBJECT_CONTEXT_TYPE_INFO,
        WDF_SYNCHRONIZATION_SCOPE,
    };

    #[doc(hidden)]
    pub mod __macro_internals {
        pub use static_assertions::const_assert;
        pub use windows_kernel_sys::MEMORY_ALLOCATION_ALIGNMENT;
    }

    pub type ContextInfo = WDF_OBJECT_CONTEXT_TYPE_INFO;

    #[macro_export]
    macro_rules! impl_context_space {
        ($ty:ty) => {
            unsafe impl $crate::object::IntoContextSpace for $ty {
                const CONTEXT_INFO: &'static $crate::object::ContextInfo =
                    &$crate::object::ContextInfo {
                        // Size is known to be small
                        Size: ::core::mem::size_of::<$crate::object::ContextInfo>() as u32,
                        ContextName: match ::core::ffi::CStr::from_bytes_until_nul(
                            concat!(stringify!($ty), "\0").as_bytes(),
                        ) {
                            Ok(v) => v.as_ptr(),
                            Err(_) => panic!("forgor nul byte 💀"),
                        },
                        ContextSize: ::core::mem::size_of::<$ty>(),
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

        object_attrs
    }

    /// Converts the typed handle into the generic version
    pub trait AsObjectHandle {
        fn as_handle(&self) -> WDFOBJECT;
        fn as_handle_mut(&mut self) -> WDFOBJECT;
    }

    /// Gets a mut ref to the associated context space, or `None` if not found
    ///
    /// ## Safety
    ///
    /// The object's context space must be initialized
    pub(crate) unsafe fn get_context<T: IntoContextSpace>(
        handle: &impl AsObjectHandle,
    ) -> Option<&T> {
        let handle = handle.as_handle();

        // SAFETY: `handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO)
        };
        let context_space = context_space.cast::<T>();

        // SAFETY:
        // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        // - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - It's the caller's responsibility to ensure that the context space is initialized
        unsafe { context_space.as_ref() }
    }

    /// Gets a mut ref to the associated context space, or `None` if not found
    ///
    /// ## Safety
    ///
    /// The object's context space must be initialized
    pub(crate) unsafe fn get_context_mut<T: IntoContextSpace>(
        handle: &mut impl AsObjectHandle,
    ) -> Option<&mut T> {
        let handle = handle.as_handle_mut();

        // SAFETY: `handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO)
        };
        let context_space = context_space.cast::<T>();

        // SAFETY:
        // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        // - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - It's the caller's responsibility to ensure that the context space is initialized
        // - &mut on the original handle guarantees exclusivity
        unsafe { context_space.as_mut() }
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
        I: pinned_init::PinInit<T, Err>,
    {
        let raw_handle = handle.as_handle_mut();

        // SAFETY: `raw_handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(raw_handle, T::CONTEXT_INFO)
        };
        let context_space = context_space.cast::<T>();

        // Get closure to initialize context with
        // Note: while this can panic, since we're assuming we're
        // in a `panic=abort` context, we won't ever have access
        // to an uninitialized context area.
        //
        // However, if we were to be in a `panic=unwind` context
        // (which might supported if we link ucrt in), then we'd
        // need to set an initialization flag stored after the
        // real context area so that we don't drop uninitialized
        // memory.
        let pin_init = init_context(handle)?;

        // SAFETY:
        // - The following ensures that the context space is valid pinned uninitialized memory:
        //   - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //     (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        //   - WDF does not move the allocation for the original object context
        //   - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - We directly return the produced error
        unsafe { pin_init.__pinned_init(context_space) }
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
        pub fn new(value: impl PinInit<T, Error>) -> impl PinInit<Self, Error> {
            pinned_init::try_pin_init!(SpinMutex {
                spin_lock: SpinLock::new()?,
                data <- {
                    let init = move |slot: *mut UnsafeCell<T>| {
                        // SAFETY: by guarantees of `pin_init_from_closure`
                        unsafe { value.__pinned_init(slot.cast()) }
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
    use core::marker::PhantomData;

    use pinned_init::PinInit;
    use vtable::vtable;
    use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDRIVER, WDF_DRIVER_INIT_FLAGS};
    use windows_kernel_rs::{string::unicode_string::NtUnicodeStr, DriverObject};
    use windows_kernel_sys::{result::STATUS, Error, NTSTATUS};

    use crate::{
        object::{self, default_object_attributes, IntoContextSpace},
        raw,
    };

    pub struct Driver<T: DriverCallbacks> {
        _context: PhantomData<T>,
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
        fn unload(&mut self) {}
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

        unsafe extern "C" fn __dispatch_driver_device_add(
            driver: WDFDRIVER,
            device_init: PWDFDEVICE_INIT,
        ) -> NTSTATUS {
            // NOTE: Unsure if this can be called concurrently, so for safety
            // we only use an immutable handle.
            // SAFETY: Only used behind an immutable reference, so we prevent
            // concurrent immutable mutations
            let handle = unsafe { DriverHandle::wrap(driver) };

            // SAFETY: Initialized by this point
            let context_space = unsafe { object::get_context(&handle) };
            let Some(context_space) = context_space else {
                return STATUS::SUCCESS.to_u32();
            };

            match T::device_add(context_space, device_init) {
                Ok(()) => STATUS::SUCCESS.to_u32(),
                Err(err) => err.0.to_u32(),
            }
        }

        unsafe extern "C" fn __dispatch_driver_unload(driver: WDFDRIVER) {
            // SAFETY: Driver unload only gets called once, and after everything?
            let mut handle = unsafe { DriverHandle::wrap(driver) };

            // SAFETY: Initialized by this point
            let context_space = unsafe { object::get_context_mut(&mut handle) };
            let Some(context_space) = context_space else {
                // Nothing to do
                return;
            };

            if T::HAS_UNLOAD {
                // Do the unload callback...
                T::unload(context_space);
            }

            // And then drop it!
            // SAFETY:
            // Guaranteed to be unused by this point, since the destroy callback is the last one
            // called.
            // Object initialization guarantees that this was valid initialized memory
            unsafe { core::ptr::drop_in_place(context_space) };
        }
    }
}
