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
                    ContextName: $crate::object::__macro_internals::cstr!(stringify!($ty)).as_ptr(),
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

pub(crate) const OWN_TAG: usize = u32::from_le_bytes(*b"Own ") as usize;
pub(crate) const REF_TAG: usize = u32::from_le_bytes(*b"Ref ") as usize;

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
    // FIXME: Make pointer-sized
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
            unsafe { Error::to_err(raw::WdfObjectCreate(Some(&mut object_attrs), &mut handle)) }?;

            // SAFETY:
            // - Caller ensures that we're at the right IRQL
            unsafe {
                raw::WdfObjectReferenceActual(
                    handle,
                    Some(OWN_TAG as *mut _),
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                )
            }

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
                Some(REF_TAG as *mut _),
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
        // SAFETY: EvtDestroy is only called once, and when there are no other references to the object
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
        // FIXME: Assert that this is at ..=DISPATCH_LEVEL
        match self.kind {
            HandleKind::Wrapped => {}
            HandleKind::Owned | HandleKind::Parented => {
                if matches!(self.kind, HandleKind::Owned) {
                    // SAFETY:
                    // - assertion that we're at the correct IRQL (..=DISPATCH_LEVEL)
                    // - is a valid WDFOBJECT
                    unsafe { raw::WdfObjectDelete(self.handle) };
                }

                // SAFETY: assertion that we're at the correct IRQL (..=DISPATCH_LEVEL)
                unsafe {
                    raw::WdfObjectDereferenceActual(
                        self.handle,
                        Some(OWN_TAG as *mut _),
                        line!() as i32,
                        Some(cstr!(file!()).as_ptr()),
                    )
                };
            }
            HandleKind::Ref => {
                // SAFETY: assertion that we're at the correct IRQL (..=DISPATCH_LEVEL)
                unsafe {
                    raw::WdfObjectDereferenceActual(
                        self.handle,
                        Some(REF_TAG as *mut _),
                        line!() as i32,
                        Some(cstr!(file!()).as_ptr()),
                    )
                }
            }
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
    fn new(context_space: &'a ContextSpaceWithDropLock<T>) -> Result<Self, GetContextSpaceError> {
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
