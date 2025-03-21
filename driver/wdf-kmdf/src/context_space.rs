use core::{
    mem::MaybeUninit,
    pin::Pin,
    sync::atomic::{AtomicU8, Ordering},
};

use pinned_init::PinInit;
use wdf_kmdf_sys::{WDFOBJECT, WDF_OBJECT_ATTRIBUTES, WDF_OBJECT_CONTEXT_TYPE_INFO};
use windows_kernel_sys::{result::STATUS, Error};

use crate::{
    handle::{FrameworkOwned, HandleWrapper, RawHandleWithContext, RawObject},
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
        unsafe impl $crate::context_space::IntoContextSpace for $ty {
            const CONTEXT_INFO: &'static $crate::context_space::ContextInfo = {
                static INFO: $crate::context_space::ContextInfo =
                    $crate::context_space::ContextInfo {
                        // Size of `ContextInfo` is known to be small
                        Size: ::core::mem::size_of::<$crate::context_space::ContextInfo>() as u32,
                        // Safety: We always concatenate a nul at the end
                        ContextName: $crate::context_space::__macro_internals::cstr!(stringify!(
                            $ty
                        ))
                        .as_ptr(),
                        ContextSize: ::core::mem::size_of::<$ty>(),
                        // Set to null because this appears to only be used to
                        // work around having multiple definitions of the same
                        // context type info in the same translation unit.
                        //
                        // We also don't use `UniqueType` as as consts can't
                        // refer to statics, which would be required for
                        // `UniqueType` to point to the same context info.
                        UniqueType: &INFO,
                        EvtDriverGetUniqueContextType: None,
                    };

                &INFO
            };
        }

        // Alignment of context type should be smaller than or equal to the arch's defined alignment
        $crate::context_space::__macro_internals::const_assert!(
            ::core::mem::align_of::<$ty>()
                <= $crate::context_space::__macro_internals::MEMORY_ALLOCATION_ALIGNMENT as usize
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
pub unsafe trait IntoContextSpace: Sync {
    const CONTEXT_INFO: &'static ContextInfo;
}

// Impl for `()` so that it's easy to create empty context spaces
impl_context_space!(());

pub(crate) const OWN_TAG: usize = u32::from_le_bytes(*b"Own ") as usize;
pub(crate) const REF_TAG: usize = u32::from_le_bytes(*b"Ref ") as usize;
pub(crate) const FRAMEWORK_TAG: usize = u32::from_le_bytes(*b"Fwrk") as usize;

pub fn default_object_attributes<T: IntoContextSpace>() -> WDF_OBJECT_ATTRIBUTES {
    let mut object_attrs = WDF_OBJECT_ATTRIBUTES::init();

    // Always set the context info
    object_attrs.ContextTypeInfo = T::CONTEXT_INFO as *const _;
    // Make space for the drop lock at the end
    object_attrs.ContextSizeOverride = core::mem::size_of::<ContextSpaceWithDropLock<T>>();

    object_attrs.EvtDestroyCallback = Some(default_dispatch_evt_destroy::<T>);

    object_attrs
}

unsafe extern "C" fn default_dispatch_evt_destroy<T: IntoContextSpace>(object: WDFOBJECT) {
    // SAFETY: EvtDestroy is only called once, and there are no other references to the object by the time we're here
    let handle = unsafe { RawHandleWithContext::<RawObject, T, FrameworkOwned>::wrap_raw(object) };

    // Drop the context area
    // SAFETY: `EvtDestroy` guarantees that we have exclusive access to the context space
    let status = unsafe { drop_context_space::<T, _>(&handle, |_| ()) };

    if let Err(err) = status {
        // No (valid) context space to drop, nothing to do
        windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
    }
}

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

    /// ## Safety
    ///
    /// `this` must be a pointer to an allocated context space, and must
    /// be valid for the whole area.
    unsafe fn data<'a>(this: *mut Self) -> &'a MaybeUninit<T> {
        // SAFETY: Caller guarantees that the pointer is to an allocated context space
        // and that it is to the full context space area.
        let data = unsafe { core::ptr::addr_of!((*this).data) };

        // SAFETY: Context space is guaranteed to be aligned to at most 16-byte alignment,
        // and is a pointer to a `MaybeUninit` which allows the backing memory to be
        // uninitialized.
        unsafe { &*data }
    }

    /// ## Safety
    ///
    /// `this` must be a pointer to an allocated context space, must
    /// be valid for the whole area, and must have exclusive access to the context space
    unsafe fn data_mut<'a>(this: *mut Self) -> &'a mut MaybeUninit<T> {
        // SAFETY: Caller guarantees that the pointer is to an allocated context space
        // and that it is to the full context space area.
        let data = unsafe { core::ptr::addr_of_mut!((*this).data) };

        // SAFETY: Context space is guaranteed to be aligned to at most 16-byte alignment,
        // and is a pointer to a `MaybeUninit` which allows the backing memory to be
        // uninitialized.
        //
        // Caller also ensures that we have exclusive access to the context space.
        unsafe { &mut *data }
    }
}

/// Marker for when the context space is initialized with exclusive access
const CONTEXT_SPACE_INIT_EXCLUSIVE_MARKER: u8 = 0xEE;
/// Marker for when the context space is initialized with shared access
const CONTEXT_SPACE_INIT_SHARED_MARKER: u8 = 0xAA;
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
    fn is_init_exclusive(&self) -> bool {
        // `Acquire` is paired with the `Release `store in `mark_init_shared`
        // and `mark_init_exclusive`
        self.init_flag.load(Ordering::Acquire) == CONTEXT_SPACE_INIT_EXCLUSIVE_MARKER
    }

    fn is_init_shared(&self) -> bool {
        // `Acquire` is paired with the `Release `store in `mark_init_shared`
        // and `mark_init_exclusive`
        self.init_flag.load(Ordering::Acquire) == CONTEXT_SPACE_INIT_SHARED_MARKER
    }

    /// ## Safety
    ///
    /// The associated context space must be initialized, and there must be exclusive
    /// access to the context space
    unsafe fn mark_init_exclusive(&self) {
        // Needs to be `Release` ordering so as to guarantee that the initialization
        // writes are visible to other threads
        self.init_flag
            .store(CONTEXT_SPACE_INIT_EXCLUSIVE_MARKER, Ordering::Release)
    }

    /// ## Safety
    ///
    /// The associated context space must be initialized
    unsafe fn mark_init_shared(&self) {
        // Needs to be `Release` ordering so as to guarantee that the initialization
        // writes are visible to other threads
        self.init_flag
            .store(CONTEXT_SPACE_INIT_SHARED_MARKER, Ordering::Release)
    }

    /// ## Safety
    ///
    /// The associated context space must be initialized, and have been initialized
    /// with exclusive access.
    unsafe fn into_shared(&self) {
        // Needs to be `AcqRel` ordering so as to guarantee that the initialization
        // writes are visible to other threads
        let old_flag = self.init_flag.compare_exchange(
            CONTEXT_SPACE_INIT_EXCLUSIVE_MARKER,
            CONTEXT_SPACE_INIT_SHARED_MARKER,
            Ordering::AcqRel,
            Ordering::Acquire,
        );

        debug_assert_eq!(
            old_flag,
            Ok(CONTEXT_SPACE_INIT_EXCLUSIVE_MARKER),
            "trying to make a non-exclusive context space into a shared one"
        );
    }

    /// Returns true if the context space was previously initialized
    fn mark_uninitialized(&mut self) -> bool {
        let flag = self.init_flag.get_mut();
        let is_init = matches!(
            *flag,
            CONTEXT_SPACE_INIT_EXCLUSIVE_MARKER | CONTEXT_SPACE_INIT_SHARED_MARKER
        );
        *flag = CONTEXT_SPACE_DROPPED_MARKER;

        is_init
    }
}

/// ## Safety:
///
/// - `context_space` must be a pointer to the original context space
/// - The context space must actually be initialized
unsafe fn mark_context_space_init_shared<T: IntoContextSpace>(
    context_space: *mut ContextSpaceWithDropLock<T>,
) {
    // SAFETY: Caller guarantees that the pointer is to an allocated context space
    // and that it is to the full context space area
    let drop_lock = unsafe { ContextSpaceWithDropLock::drop_flag(context_space) };

    // SAFETY: Caller guarantees that the context space is initialized
    unsafe { drop_lock.mark_init_shared() };
}

/// ## Safety:
///
/// - `context_space` must be a pointer to the original context space
/// - The context space must actually be initialized
/// - There must be exclusive access to the context space
unsafe fn mark_context_space_init_exclusive<T: IntoContextSpace>(
    context_space: *mut ContextSpaceWithDropLock<T>,
) {
    // SAFETY: Caller guarantees that the pointer is to an allocated context space
    // and that it is to the full context space area
    let drop_lock = unsafe { ContextSpaceWithDropLock::drop_flag(context_space) };

    // SAFETY: Caller guarantees that the context space is initialized and has exclusive access
    unsafe { drop_lock.mark_init_exclusive() };
}

fn get_context_space_ptr<T: IntoContextSpace, H: HandleWrapper>(
    handle: &H,
) -> Result<*mut ContextSpaceWithDropLock<T>, GetContextSpaceError> {
    let handle = handle.as_object_handle();

    // SAFETY: `handle` validity assured by `HandleWrapper`, and context info validity assured by `IntoContextSpace`
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
pub(crate) fn get_context<T: IntoContextSpace, H: HandleWrapper>(
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
    let drop_flag = unsafe { ContextSpaceWithDropLock::drop_flag(context_space) };

    // Ensure the context space is initialized and is shared
    if drop_flag.is_init_shared() {
        // SAFETY: This is also okay per the same requirements as getting the drop flag field above.
        let context_space = unsafe { ContextSpaceWithDropLock::data(context_space) };

        // SAFETY: `is_init_shared` returning true ensures that the context space data is initialized
        let context_space = unsafe { context_space.assume_init_ref() };

        // SAFETY: `context_space` has a stable address as it is heap allocated, and will never be moved
        // as it is an immutable reference.
        Ok(unsafe { Pin::new_unchecked(context_space) })
    } else {
        Err(GetContextSpaceError::NotInitialized)
    }
}

/// Gets a mutable ref to the associated context space, or the appropriate [`GetContextSpaceError`] code
///
/// ## Safety
///
/// Must have exclusive access to the context space
pub(crate) unsafe fn get_context_mut<T: IntoContextSpace, H: HandleWrapper>(
    handle: &H,
) -> Result<Pin<&mut T>, GetContextSpaceError> {
    let context_space = get_context_space_ptr::<T, H>(handle)?;

    // SAFETY:
    // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
    //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
    // - `IntoContextSpace` requires alignment to MEMORY_ALLOCATION_ALIGNMENT or smaller
    // - We've checked `context_space` to not be NULL
    //
    // Note that the actual context space data isn't required to be initialized since it's behind a `MaybeUninit`
    let drop_flag = unsafe { ContextSpaceWithDropLock::drop_flag(context_space) };

    // Ensure the context space is initialized and is exclusive
    if drop_flag.is_init_exclusive() {
        // SAFETY: This is also okay per the same requirements as getting the drop flag field above.
        //
        // `is_init_exclusive` also ensures that we have exclusive access to the context space.
        let context_space = unsafe { ContextSpaceWithDropLock::data_mut(context_space) };

        // SAFETY: `is_init_exclusive` returning true ensures that the context space data is initialized
        let context_space = unsafe { context_space.assume_init_mut() };

        // SAFETY: `data` has a stable address as it is heap allocated, and we never let the
        // intermediate unpinned `context_space` mutable ref outside of here.
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
pub(crate) unsafe fn drop_context_space<T: IntoContextSpace, H: HandleWrapper>(
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
pub(crate) unsafe fn context_pin_init<T, Handle, I, Err>(
    handle: &Handle,
    init_context: impl FnOnce(&Handle) -> Result<I, Err>,
) -> Result<(), Err>
where
    T: IntoContextSpace,
    Handle: HandleWrapper,
    I: PinInit<T, Err>,
{
    // Initialize the context space
    let context_space = context_pin_init_inner(handle, init_context)?;

    // SAFETY: By this point we've successfully initialized the context space
    unsafe { mark_context_space_init_shared::<T>(context_space) };

    Ok(())
}

/// Initializes the object's context area, using the closure provided
///
/// ## Safety
///
/// - Must not reinitialize an object's context space
/// - Object must actually have the context space
pub(crate) unsafe fn context_pin_init_mut<T, Handle, I, Err>(
    handle: &Handle,
    init_context: impl FnOnce(&Handle) -> Result<I, Err>,
) -> Result<(), Err>
where
    T: IntoContextSpace,
    Handle: HandleWrapper,
    I: PinInit<T, Err>,
{
    // Initialize the context space
    let context_space = context_pin_init_inner(handle, init_context)?;

    // SAFETY: By this point we've successfully initialized the context space,
    // and the caller guarantees that we have exclusive access
    unsafe { mark_context_space_init_exclusive::<T>(context_space) };

    Ok(())
}

fn context_pin_init_inner<T, Handle, I, Err>(
    handle: &Handle,
    init_context: impl FnOnce(&Handle) -> Result<I, Err>,
) -> Result<*mut ContextSpaceWithDropLock<T>, Err>
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

    Ok(context_space)
}

/// Marks a previously exclusive context space as being shared, allowing multiple shared refs into the context space.
///
/// ## Safety
///
/// The context space must be initialized and be currently marked as exclusive.
pub(crate) unsafe fn make_context_shared<T, Handle>(handle: &Handle)
where
    T: IntoContextSpace,
    Handle: HandleWrapper,
{
    let context_space =
        get_context_space_ptr::<T, Handle>(handle).expect("object must have the context space");

    // SAFETY:
    // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
    //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
    // - `IntoContextSpace` requires alignment to MEMORY_ALLOCATION_ALIGNMENT or smaller
    // - We've checked `context_space` to not be NULL
    let drop_flag = unsafe { ContextSpaceWithDropLock::drop_flag(context_space) };

    // SAFETY: Caller ensures the context space is initialized with exclusive access
    unsafe { drop_flag.into_shared() }
}
