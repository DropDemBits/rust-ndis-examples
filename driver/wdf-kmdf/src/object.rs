use core::{
    mem::MaybeUninit,
    pin::Pin,
    sync::atomic::{AtomicU8, Ordering},
};

use pinned_init::PinInit;
use wdf_kmdf_sys::{
    WDFOBJECT, WDF_EXECUTION_LEVEL, WDF_OBJECT_ATTRIBUTES, WDF_OBJECT_CONTEXT_TYPE_INFO,
    WDF_SYNCHRONIZATION_SCOPE,
};
use windows_kernel_sys::{result::STATUS, Error};

use crate::{
    handle::{DriverOwned, HandleWrapper, RawHandle, Ref, Wrapped},
    raw,
};

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
// FIXME: require `Self: Sync` since references to object context spaces can be manifested on different threads.
pub unsafe trait IntoContextSpace {
    const CONTEXT_INFO: &'static ContextInfo;
}

// Impl for `()` so that it's easy to create empty context spaces
impl_context_space!(());

pub(crate) const OWN_TAG: usize = u32::from_le_bytes(*b"Own ") as usize;
pub(crate) const REF_TAG: usize = u32::from_le_bytes(*b"Ref ") as usize;
pub(crate) const FRAMEWORK_TAG: usize = u32::from_le_bytes(*b"Fwrk") as usize;

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

pub struct GeneralObject<T: IntoContextSpace> {
    handle: RawHandle<WDFOBJECT, T, DriverOwned>,
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
    pub fn create<I>(init_context: impl FnOnce(&Self) -> Result<I, Error>) -> Result<Self, Error>
    where
        I: PinInit<T, Error>,
    {
        Self::_create(default_object_attributes::<T>(), init_context, |handle| {
            let handle = unsafe { RawHandle::create(handle) };
            Self { handle }
        })
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
        parent: &impl HandleWrapper,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
    ) -> Result<Ref<Self>, Error>
    where
        I: PinInit<T, Error>,
    {
        let mut object_attrs = default_object_attributes::<T>();
        object_attrs.ParentObject = parent.as_object_handle();

        Self::_create(object_attrs, init_context, |handle| {
            let handle = unsafe { RawHandle::create_parented(handle) };
            let handle = Self { handle };
            unsafe { Ref::into_parented(handle) }
        })
    }

    fn _create<Handle, I>(
        mut object_attrs: WDF_OBJECT_ATTRIBUTES,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
        create_handle: impl FnOnce(WDFOBJECT) -> Handle,
    ) -> Result<Handle, Error>
    where
        Handle: HandleWrapper + AsRef<Self>,
        I: PinInit<T, Error>,
    {
        // Add the drop callback
        object_attrs.EvtDestroyCallback = Some(Self::__dispatch_evt_destroy);

        let handle = {
            let mut handle = core::ptr::null_mut();
            // SAFETY:
            // - Caller ensures that we're at the right IRQL
            unsafe { Error::to_err(raw::WdfObjectCreate(Some(&mut object_attrs), &mut handle)) }?;
            // Wrap it in the right raw handle
            create_handle(handle)
        };

        // SAFETY:
        // - It's WDF's responsibility to insert the context area, since we create
        //   the default object attributes with T's context area
        // - The object was just created, and the context space has not been initialized yet
        unsafe { crate::object::context_pin_init(handle.as_ref(), init_context)? };

        Ok(handle)
    }

    /// Wraps the raw handle in a general object wrapper
    ///
    /// ## Safety
    ///
    /// Respect aliasing rules, since this can be used to
    /// generate aliasing mutable references to the context space.
    /// Also, the context space must be initialized.
    pub unsafe fn wrap(handle: WDFOBJECT) -> Wrapped<Self> {
        // SAFETY: Caller ensures that this is a valid handle
        unsafe { Wrapped::wrap_raw(handle) }
    }

    /// Gets the object handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFOBJECT {
        self.handle.as_handle()
    }

    pub fn get_context(&self) -> Pin<&T> {
        crate::object::get_context(self).expect("context space must be initialized")
    }

    /// Makes a shared reference to the object
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Ref<Self> {
        Ref::clone_from_handle(self)
    }

    unsafe extern "C" fn __dispatch_evt_destroy(object: wdf_kmdf_sys::WDFOBJECT) {
        // SAFETY: EvtDestroy is only called once, and there are no other references to the object by the time we're here
        let handle = unsafe { Self::wrap(object) };

        // Drop the context area
        // SAFETY: `EvtDestroy` guarantees that we have exclusive access to the context space
        let status = unsafe { crate::object::drop_context_space::<T, _>(&handle, |_| ()) };

        if let Err(err) = status {
            // No (valid) context space to drop, nothing to do
            windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
        }
    }
}

impl<T: IntoContextSpace> HandleWrapper for GeneralObject<T> {
    type Handle = WDFOBJECT;

    #[inline]
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Self {
        Self {
            // SAFETY: Caller has the responsibility to ensure that this is valid
            handle: unsafe { RawHandle::wrap_raw(raw) },
        }
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<T: IntoContextSpace> AsRef<GeneralObject<T>> for GeneralObject<T> {
    fn as_ref(&self) -> &GeneralObject<T> {
        self
    }
}

impl<T: IntoContextSpace> core::fmt::Debug for GeneralObject<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("GeneralObject")
            .field("handle", &self.handle)
            .finish()
    }
}

impl<T: IntoContextSpace> PartialEq for GeneralObject<T> {
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl<T: IntoContextSpace> Eq for GeneralObject<T> {}

#[repr(C)]
struct ContextSpaceWithDropLock<T> {
    /// Original context space
    data: MaybeUninit<T>,
    /// Drop flag ensuring that the context space isn't dropped more than once,
    /// and that the context space is initialized when required.
    ///
    /// Most of the time it's not a concern whether the context space is initialized
    /// outside of initializing other context spaces as a context space is guaranteed
    /// to be initialized so long as the associated handle is live.
    ///
    /// However, during context space initialization, a handle is given to allow any
    /// child objects that are created to be parented to the object in construction.
    /// This presents the situation of accessing a context space while it is being
    /// initialized, which results in accessing uninitialized memory. This is also
    /// applicable for accessing the associated driver of a control device while
    /// the driver context space is being initialized.
    ///
    /// The drop flag is also useful in the event of a panic happening inside of a
    /// context space initialization so that unwinding (if done) won't try to access
    /// uninitialized memory when dropping the context space.
    //
    // Note: WDF Guarantees that the entire context space is zero-initialized,
    // so `init_flag` of `DropLock` are both initialized to 0
    drop_flag: DropFlag,
}

impl<T> ContextSpaceWithDropLock<T> {
    /// ## Safety
    ///
    /// `this` must be a pointer to an allocated context space, and must
    /// be valid for the whole area
    unsafe fn drop_flag<'a>(this: *mut Self) -> &'a DropFlag
    where
        'static: 'a,
    {
        // SAFETY: Caller guarantees that the pointer is to an allocated context space
        // and that it is to the full context space area
        let drop_flag = unsafe { core::ptr::addr_of!((*this).drop_flag) };

        // SAFETY: Context space is guaranteed to be aligned to at most 16-byte alignment,
        // and all possible bit patterns of `u8` (which `AtomicU8` has the same repr as)
        // are valid values
        unsafe { &*drop_flag }
    }

    /// ## Safety
    ///
    /// `this` must be a pointer to an allocated context space, must
    /// be valid for the whole area, and must have exclusive access to the context space
    unsafe fn drop_flag_mut<'a>(this: *mut Self) -> &'a mut DropFlag
    where
        'static: 'a,
    {
        // SAFETY: Caller guarantees that the pointer is to an allocated context space,
        // that we have exclusive access to the context space, and that it is a pointer
        // to the full context space area
        let drop_flag = unsafe { core::ptr::addr_of_mut!((*this).drop_flag) };

        // SAFETY: Context space is guaranteed to be aligned to at most 16-byte alignment,
        // and all possible bit patterns of `u8` (which `AtomicU8` has the same repr as)
        // are valid values
        //
        // Caller also ensures that we have exclusive access to the context space
        unsafe { &mut *drop_flag }
    }
}

/// Marker for when the context space is initialized
const CONTEXT_SPACE_INIT_MARKER: u8 = 0xAA;
/// Marker for when the context space has been dropped
const CONTEXT_SPACE_DROPPED_MARKER: u8 = 0xDD;

struct DropFlag {
    /// Current initialization state
    ///
    /// Representing the initialization flag as a `AtomicU8` because we need to synchronize
    /// access to readers, and also because multiple threads may be trying accessing the
    /// context area
    init_flag: AtomicU8,
}

impl DropFlag {
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

    /// Returns true if the context space was previously initialized
    fn mark_uninitialized(&mut self) -> bool {
        let flag = self.init_flag.get_mut();
        let is_init = *flag == CONTEXT_SPACE_INIT_MARKER;
        *flag = CONTEXT_SPACE_DROPPED_MARKER;

        is_init
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
    let drop_lock = unsafe { ContextSpaceWithDropLock::drop_flag(context_space) };

    // SAFETY: Caller guarantees that the context space is initialized
    unsafe { drop_lock.mark_initialized() };
}

fn get_context_space_ptr<T: IntoContextSpace, H: AsObjectHandle>(
    handle: &H,
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
// FIXME: Temporarily `pub` until we have something like `WithContextSpace`
pub fn get_context<T: IntoContextSpace, H: AsObjectHandle>(
    handle: &H,
) -> Result<Pin<&T>, GetContextSpaceError> {
    let context_space = get_context_space_ptr::<T, H>(handle)?;

    // SAFETY:
    // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
    //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
    // - `IntoContextSpace` requires alignment to MEMORY_ALLOCATION_ALIGNMENT or smaller
    // - We've checked `context_space` to not be NULL
    //
    // Note that the actual context space data isn't required to be initialized since it's behind a `MaybeUninit`
    let context_space = unsafe { &*context_space };

    // Ensure the context space is initialized
    if context_space.drop_flag.is_init() {
        // SAFETY: `is_init` returning true ensures that the context space is initialized
        let context_space = unsafe { &*context_space.data.as_ptr() };

        // SAFETY: `context_space` has a stable address as it is heap allocated, and will never be moved
        // as it is an immutable reference.
        Ok(unsafe { Pin::new_unchecked(context_space) })
    } else {
        Err(GetContextSpaceError::NotInitialized)
    }
}

/// Drops the associated context space
///
/// Allows performing extra work before the context space is dropped.
///
/// Returns the appropriate [`GetContextError::NotInitialized`] code if the context space was never initialized or already dropped,
/// or [`GetContextError::NotFound`] if this was not a context space of the object
///
/// ## Safety
///
/// Must have exclusive access to the context space, which is guaranteed to happen inside of `EvtDestroy`.
// FIXME: Temporarily `pub` until we have something like `WithContextSpace`
pub unsafe fn drop_context_space<T: IntoContextSpace, H: AsObjectHandle>(
    handle: &H,
    additional_work: impl FnOnce(Pin<&mut T>),
) -> Result<(), GetContextSpaceError> {
    let context_space = get_context_space_ptr::<T, H>(handle)?;

    // SAFETY: `get_context_space_ptr` returns a pointer to the base of the context space
    let drop_lock = unsafe { ContextSpaceWithDropLock::drop_flag_mut(context_space) };

    // Ensure that the context space is actually initialized.
    // If it isn't then there isn't anything we can do.
    if !drop_lock.mark_uninitialized() {
        return Err(GetContextSpaceError::NotInitialized);
    }

    // Do any additonal work before dropping the context area,
    // which requires a pinned mut ref
    //
    // Caller guarantees that there are no other threads have access to the context space,
    // so we can soundly make a pinned mutable ref to the context area
    //
    // SAFETY:
    // - `context_space` comes from `WdfGetTypedContextWorker`, which
    //   returns a pointer to the base of the context space
    // - WDF aligns memory allocations to MEMORY_ALLOCATION_ALIGNMENT
    //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
    // - `IntoContextSpace` requires alignment to MEMORY_ALLOCATION_ALIGNMENT or smaller
    // - We checked above that the context space is initialized
    // - Caller ensures we have exclusive access to the context space
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
// FIXME: Temporarily `pub` until we have something like `WithContextSpace`
pub unsafe fn context_pin_init<T, Handle, I, Err>(
    handle: &Handle,
    init_context: impl FnOnce(&Handle) -> Result<I, Err>,
) -> Result<(), Err>
where
    T: IntoContextSpace,
    Handle: HandleWrapper,
    I: PinInit<T, Err>,
{
    let context_space =
        get_context_space_ptr::<T, Handle>(handle).expect("object must have the context space");

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
    // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
    //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
    // - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
    // - `data` is located at the start of the context space (guaranteed because it's repr(C),
    //   guaranteeing that it's in-bounds and aligned
    // - `addr_of_mut!` doesn't load from `context_space` at all, so it doesn't access uninit memory
    let context_data = unsafe { core::ptr::addr_of_mut!((*context_space).data).cast::<T>() };

    // SAFETY:
    // The following ensures that the context space is valid pinned uninitialized memory:
    // - `context_data` is aligned & in-bounds as per above
    // - WDF does not move the allocation for the original object context
    // - We directly `?` the produced error
    unsafe { pin_init.__pinned_init(context_data)? }

    // SAFETY: By this point we've successfully initialized the context space
    unsafe { mark_context_space_init::<T>(context_space) };

    Ok(())
}
