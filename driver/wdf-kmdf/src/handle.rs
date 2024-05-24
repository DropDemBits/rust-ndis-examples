//! Helpers for working with object handles.

// okay so there is owning handles, parented/ref handles, and wrapping handles
// raw handles do not handle ownership of the handle, as that's passed to the
// responsibilty of the `Owned` and `Ref` wrapper types

// Want to look like
// - Handle<RawSpinlock, (), Owned>
// - Ref<WdfObject<OpenContext>> -> Ref<Handle<RawObject, OpenContext, Owned>>
// - WdfDevice<()> -> Ref<Handle<RawDevice, ()>>
//
// let hhhh = WithContext<RequestContext, WdfRequest>::wrap(object_context)
//
// can't transit Ref<T> through FFI and back, though that's okay

// so we want clone_ref on some of them, wrap on all of them, and as_object handle on all of them?
// - clone_ref on all since we want T -> Ref<T>
//   - could be a separate thing?
// - wrap on all since we want T -> Ref<T>

use core::{marker::PhantomData, mem::ManuallyDrop, ops::Deref, pin::Pin};

use pinned_init::PinInit;
use wdf_kmdf_sys::WDFOBJECT;
use windows_kernel_sys::Error;

use crate::{
    context_space::{self, IntoContextSpace},
    raw,
};

#[macro_export]
macro_rules! impl_clone_ref {
    ($ty:ident$(<$($ty_arg:ident $(: $clause:tt)?),+>)?) => {
        impl $(<$($ty_arg)+>)? $crate::handle::CloneRef for $ty$(<$($ty_arg)+>)?
        $(where
            $($ty_arg: $($clause)?,)+
        )?
        {
            type Ref<const TAG: usize> = $crate::handle::Ref<Self>;

            fn clone_ref<const TAG: usize>(
                &self,
                file: *const i8,
                line: u32,
            ) -> Self::Ref<{ TAG }> {
                $crate::handle::Ref::clone_from_handle_location_tag(self, file, line)
            }
        }
    };
    ($ty:ident) => {
        impl $crate::handle::CloneRef for $ty {
            type Ref<const TAG: usize> = $crate::handle::Ref<Self>;

            fn clone_ref<const TAG: usize>(
                &self,
                file: *const i8,
                line: u32,
            ) -> Self::Ref<{ TAG }> {
                $crate::handle::Ref::clone_from_handle_location_tag(self, file, line)
            }
        }
    };
}

/// A wrapper around a raw handle
///
/// ## Drop IRQL: `..=DISPATCH_LEVEL`
pub struct RawHandle<H: WdfHandle, Mode: OwningMode> {
    handle: *mut H,
    _mode: PhantomData<Mode>,
}

impl<H: WdfHandle, Mode: OwningMode> RawHandle<H, Mode> {
    /// Creates a new raw handle from a foreign handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Safety
    ///
    /// Must have the correct ownership over the raw handle
    pub unsafe fn create(handle: *mut H) -> Self {
        // We only adjust refcount for framework objects
        if Mode::TAG != context_space::FRAMEWORK_TAG {
            // SAFETY: Caller ensures that the handle is valid and that we're
            // called at the right IRQL
            unsafe {
                raw::WdfObjectReferenceActual(
                    handle.cast(),
                    Some(Mode::TAG as *mut _),
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                );
            }
        }

        Self {
            handle,
            _mode: PhantomData,
        }
    }

    /// Creates a new raw handle from a foreign handle,
    /// where the handle is parented to another object.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Safety
    ///
    /// Must have the correct ownership over the raw handle,
    /// and the handle must be parented.
    pub unsafe fn create_parented(handle: *mut H) -> Self {
        // SAFETY: Caller ensures that the handle is valid and that we're
        // called at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                handle.cast(),
                Some(context_space::REF_TAG as *mut _),
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            );
        }

        Self {
            handle,
            _mode: PhantomData,
        }
    }

    /// Gets the backing raw handle
    #[inline]
    pub fn as_handle(&self) -> *mut H {
        self.handle
    }
}

impl<H: WdfHandle, Mode: OwningMode> Drop for RawHandle<H, Mode> {
    fn drop(&mut self) {
        // FIXME: Assert that this is at `..=DISPATCH_LEVEL`

        // We only adjust refcount for framework objects since we aren't responsible
        // for deleting them
        if Mode::TAG != context_space::FRAMEWORK_TAG {
            if Mode::TAG == context_space::OWN_TAG {
                // Is a driver-owned object, so we must clean it up ourselves
                //
                // SAFETY: Caller ensures that the handle is valid and that we're
                // called at the right IRQL
                unsafe {
                    raw::WdfObjectDelete(self.as_object_handle());
                }
            }

            // SAFETY: Caller ensures that the handle is valid and that we're
            // called at the right IRQL
            unsafe {
                raw::WdfObjectDereferenceActual(
                    self.as_object_handle(),
                    Some(Mode::TAG as *mut _),
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                );
            }
        }
    }
}

impl<H: WdfHandle, Mode: OwningMode> HandleWrapper for RawHandle<H, Mode> {
    type Handle = H;

    #[inline]
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: raw,
            _mode: PhantomData,
        }
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.handle.cast()
    }
}

impl<H, Mode> core::fmt::Debug for RawHandle<H, Mode>
where
    H: WdfHandle,
    Mode: OwningMode,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("RawHandle").field(&self.handle).finish()
    }
}

impl<H, Mode> PartialEq for RawHandle<H, Mode>
where
    H: WdfHandle,
    Mode: OwningMode,
{
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl<H, Mode> Eq for RawHandle<H, Mode>
where
    H: WdfHandle,
    Mode: OwningMode,
{
}

// SAFETY: Equivalent to passing a `usize` around, and all WDF functions are thread-safe
unsafe impl<H: WdfHandle, Mode: OwningMode> Send for RawHandle<H, Mode> {}
// SAFETY: Equivalent to passing a `usize` around, and all WDF functions are thread-safe
unsafe impl<H: WdfHandle, Mode: OwningMode> Sync for RawHandle<H, Mode> {}

/// A wrapper around a raw handle, with a context space allocated at creation
///
/// ## Drop IRQL: `..=DISPATCH_LEVEL`
pub struct RawHandleWithContext<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> {
    handle: RawHandle<H, Mode>,
    _context_space: PhantomData<fn() -> T>,
}

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> RawHandleWithContext<H, T, Mode> {
    /// Creates a new raw handle from a foreign handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Safety
    ///
    /// Must have the correct ownership over the raw handle
    pub unsafe fn create(handle: *mut H) -> Self {
        // SAFETY: Caller handles the responsibility of this function
        let handle = unsafe { RawHandle::create(handle) };
        Self {
            handle,
            _context_space: PhantomData,
        }
    }

    /// Creates a new raw handle from a foreign handle,
    /// where the handle is parented to another object.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Safety
    ///
    /// Must have the correct ownership over the raw handle,
    /// and the handle must be parented.
    pub unsafe fn create_parented(handle: *mut H) -> Self {
        // SAFETY: Caller handles the responsibility of this function
        let handle = unsafe { RawHandle::create_parented(handle) };
        Self {
            handle,
            _context_space: PhantomData,
        }
    }

    /// Gets the backing raw handle
    #[inline]
    pub fn as_handle(&self) -> *mut H {
        self.handle.as_handle()
    }
}

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> HandleWrapper
    for RawHandleWithContext<H, T, Mode>
{
    type Handle = H;

    #[inline]
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: unsafe { RawHandle::wrap_raw(raw) },
            _context_space: PhantomData,
        }
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<H, T, Mode> core::fmt::Debug for RawHandleWithContext<H, T, Mode>
where
    H: WdfHandle,
    T: IntoContextSpace,
    Mode: OwningMode,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("RawHandleWithContext")
            .field(&self.handle)
            .finish()
    }
}

impl<H, T, Mode> PartialEq for RawHandleWithContext<H, T, Mode>
where
    H: WdfHandle,
    T: IntoContextSpace,
    Mode: OwningMode,
{
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl<H, T, Mode> Eq for RawHandleWithContext<H, T, Mode>
where
    H: WdfHandle,
    T: IntoContextSpace,
    Mode: OwningMode,
{
}

/// A handle with an extra context space attached to it.
pub struct WithContext<H: HandleWrapper, T: IntoContextSpace> {
    handle: H,
    _context_space: PhantomData<fn() -> T>,
}

impl<H: HandleWrapper, T: IntoContextSpace> WithContext<H, T> {
    /// Creates an extra context space for the handle, initializing it immediately.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn create<I, Err>(
        handle: H,
        init: impl FnOnce(&H) -> Result<I, Err>,
    ) -> Result<Self, (Error, H)>
    where
        I: PinInit<T, Err>,
        Err: Into<Error>,
    {
        UninitContextSpace::create(handle)?
            .init(init)
            .map_err(|(err, handle)| (err.into(), handle))
    }

    /// Creates an extra context space for the handle, but deferring initialization for later.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Errors
    ///
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    pub fn create_uninit(handle: H) -> Result<UninitContextSpace<H, T>, (Error, H)> {
        UninitContextSpace::create(handle)
    }

    /// Gets the context space associated with this handle
    pub fn get_context(&self) -> Pin<&T> {
        context_space::get_context(&self.handle)
            .expect("object context space should be initialized")
    }
}

impl<H: HandleWrapper, T: IntoContextSpace> HandleWrapper for WithContext<H, T> {
    type Handle = <H as HandleWrapper>::Handle;

    #[inline]
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: unsafe { H::wrap_raw(raw) },
            _context_space: PhantomData,
        }
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<H: HandleWrapper, T: IntoContextSpace> Deref for WithContext<H, T> {
    type Target = H;

    fn deref(&self) -> &Self::Target {
        &self.handle
    }
}

impl<H, T> core::fmt::Debug for WithContext<H, T>
where
    H: HandleWrapper + core::fmt::Debug,
    T: IntoContextSpace,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("WithContext").field(&self.handle).finish()
    }
}

impl<H, T> PartialEq for WithContext<H, T>
where
    H: HandleWrapper + PartialEq,
    T: IntoContextSpace,
{
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl<H, T> Eq for WithContext<H, T>
where
    H: HandleWrapper + Eq,
    T: IntoContextSpace,
{
}

/// An additional context space for a handle that has not been initialized yet.
pub struct UninitContextSpace<H: HandleWrapper, T: IntoContextSpace> {
    handle: H,
    drop_bomb: UninitDropBomb,
    _context_space: PhantomData<fn() -> T>,
}

impl<H: HandleWrapper, T: IntoContextSpace> UninitContextSpace<H, T> {
    fn create(handle: H) -> Result<Self, (Error, H)> {
        let mut attributes = context_space::default_object_attributes::<T>();

        // Note: we never use this context space handle as we get it through
        // `HasContext::get_context`.
        let mut _context_space = core::ptr::null_mut();

        // SAFETY:
        // `HandleWrapper` guarantees that the resultant object handle
        // is a valid handle to a WDF object.
        //
        // As a bonus, `WdfObjectAllocateContext` will not try to create a
        // duplicate context space and instead return an error, thus making
        // it guaranteed that the context space of interest will only be
        // initialized once (see
        // https://github.com/microsoft/Windows-Driver-Frameworks/blob/a94b8c30dad524352fab90872aefc83920b98e56/src/framework/shared/object/fxobject.cpp#L649-L657
        // ).
        let status = unsafe {
            Error::to_err(raw::WdfObjectAllocateContext(
                handle.as_object_handle(),
                &mut attributes,
                &mut _context_space,
            ))
        };
        if let Err(err) = status {
            return Err((err, handle));
        }

        Ok(Self {
            handle,
            drop_bomb: UninitDropBomb::new(),
            _context_space: PhantomData,
        })
    }

    pub fn init<I, E>(
        mut self,
        init: impl FnOnce(&H) -> Result<I, E>,
    ) -> Result<WithContext<H, T>, (E, H)>
    where
        I: PinInit<T, E>,
    {
        // `init` has been called, so we can defuse the drop bomb
        self.drop_bomb.defuse();

        let handle = self.handle;

        // SAFETY:
        // Object has the context space since `self` existing means that
        // the object does have T's context space, and that the context
        // space hasn't been initialized yet.
        let status = unsafe { context_space::context_pin_init::<T, _, _, _>(&handle, init) };
        if let Err(err) = status {
            return Err((err, handle));
        }

        Ok(WithContext {
            handle,
            _context_space: PhantomData,
        })
    }
}

impl<H: HandleWrapper, T: IntoContextSpace> Deref for UninitContextSpace<H, T> {
    type Target = H;

    fn deref(&self) -> &Self::Target {
        &self.handle
    }
}

impl<H, T> core::fmt::Debug for UninitContextSpace<H, T>
where
    H: HandleWrapper + core::fmt::Debug,
    T: IntoContextSpace,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("UninitContextSpace")
            .field(&self.handle)
            .finish()
    }
}

/// A ref-counted reference to a handle
///
/// ## Drop IRQL: `..=DISPATCH_LEVEL`
///
// Note: Is a `ManuallyDrop` around a handle because technically
// we make this by using `wrap_raw`, which requires that we don't
// accidentally cause aliasing accesses on drop.
#[derive(PartialEq, Eq)]
pub struct Ref<H: HandleWrapper, const TAG: usize = { context_space::REF_TAG }>(ManuallyDrop<H>);

// For any handle, must be pointer-sized
static_assertions::assert_eq_size!(Ref<RawHandle<RawDriver, DriverOwned>>, *const ());

impl<H: HandleWrapper> Ref<H, { context_space::REF_TAG }> {
    /// Transforms an owning handle into a parented ref handle
    ///
    /// ## Safety
    ///
    /// Handle must be from a [`RawHandle::create_parented`] call
    pub unsafe fn into_parented(handle: H) -> Self {
        // Note: We don't bump up the refcount because [`RawHandle::create_parented`]
        // already bumps the refcount
        Self(ManuallyDrop::new(handle))
    }
}

impl<H: HandleWrapper, const TAG: usize> Ref<H, TAG> {
    /// Creates a new reference to a handle from an existing handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_from_handle(handle: &H) -> Self {
        // SAFETY: Manually drop ensures that we only adjust the refcount
        // and not delete the object
        let handle = ManuallyDrop::new(unsafe { H::wrap_raw(handle.as_object_handle().cast()) });

        // SAFETY: Caller ensures that we're called at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                handle.as_object_handle(),
                Some(TAG as *mut _),
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            );
        }

        Self(handle)
    }

    /// Creates a new reference to a handle from an existing handle, including
    /// location info.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_from_handle_location(handle: &H, file: *const i8, line: u32) -> Self {
        // SAFETY: Manually drop ensures that we only adjust the refcount
        // and not delete the object
        let handle = ManuallyDrop::new(unsafe { H::wrap_raw(handle.as_object_handle().cast()) });

        // SAFETY: Caller ensures that we're called at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                handle.as_object_handle(),
                Some(TAG as *mut _),
                line as i32,
                Some(file),
            );
        }

        Self(handle)
    }

    /// Creates a new reference to a handle from an existing handle, including
    /// location info and a custom tag.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_from_handle_location_tag(handle: &H, file: *const i8, line: u32) -> Self {
        // SAFETY: Manually drop ensures that we only adjust the refcount
        // and not delete the object
        let handle = ManuallyDrop::new(unsafe { H::wrap_raw(handle.as_object_handle().cast()) });

        // SAFETY: Caller ensures that we're called at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                handle.as_object_handle(),
                Some(TAG as *mut _),
                line as i32,
                Some(file),
            );
        }

        Self(handle)
    }

    /// Makes a new [`Ref`] to this handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Self {
        Self::clone_from_handle(&self.0)
    }

    /// Makes a new [`Ref`] to this handle, including location info.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref_location(&self, file: *const i8, line: u32) -> Self {
        Self::clone_from_handle_location(&self.0, file, line)
    }

    /// Makes a new [`Ref`] to this handle, including location info and tag.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref_location_tag(&self, file: *const i8, line: u32) -> Self {
        Self::clone_from_handle_location_tag(&self.0, file, line)
    }

    pub fn handle_eq<const OTHER_TAG: usize>(&self, other: &Ref<H, OTHER_TAG>) -> bool
    where
        H: Eq,
    {
        &self.0 == &other.0
    }
}

impl<H, const PARENT_TAG: usize> CloneRef for Ref<H, PARENT_TAG>
where
    H: HandleWrapper,
{
    type Ref<const TAG: usize> = Ref<H, TAG>;

    fn clone_ref<const TAG: usize>(&self, file: *const i8, line: u32) -> Self::Ref<{ TAG }> {
        Ref::<H, TAG>::clone_from_handle_location_tag(&self.0, file, line)
    }
}

impl<H: HandleWrapper, const TAG: usize> Drop for Ref<H, TAG> {
    fn drop(&mut self) {
        // FIXME: Assert that this is at `..=DISPATCH_LEVEL`

        // SAFETY: Caller ensures that the handle is valid and that we're
        // called at the right IRQL
        unsafe {
            raw::WdfObjectDereferenceActual(
                self.0.as_object_handle(),
                Some(TAG as *mut _),
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            );
        }
    }
}

impl<H: HandleWrapper, const TAG: usize> HandleWrapper for Ref<H, TAG> {
    type Handle = <H as HandleWrapper>::Handle;

    #[inline]
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        // SAFETY: Caller ensures that we don't alias on drop
        Self(ManuallyDrop::new(unsafe { H::wrap_raw(raw) }))
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.0.as_object_handle()
    }
}

impl<H: HandleWrapper, const TAG: usize> AsRef<H> for Ref<H, TAG> {
    fn as_ref(&self) -> &H {
        &self.0
    }
}

impl<H: HandleWrapper, const TAG: usize> Deref for Ref<H, TAG> {
    type Target = H;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<H, const TAG: usize> core::fmt::Debug for Ref<H, TAG>
where
    H: HandleWrapper + core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Ref").field(&*self.0).finish()
    }
}

impl<H: HandleWrapper, const TAG: usize> PartialEq<H> for Ref<H, TAG>
where
    H: PartialEq,
{
    fn eq(&self, other: &H) -> bool {
        &*self.0 == other
    }
}

/// Wraps a foreign handle, ensuring that it won't be dropped
#[derive(Debug)]
pub struct Wrapped<H: HandleWrapper>(ManuallyDrop<H>);

impl<H> CloneRef for Wrapped<H>
where
    H: HandleWrapper,
{
    type Ref<const TAG: usize> = Ref<H>;

    fn clone_ref<const TAG: usize>(&self, file: *const i8, line: u32) -> Self::Ref<{ TAG }> {
        Ref::clone_from_handle_location_tag(&self.0, file, line)
    }
}

impl<H: HandleWrapper> HandleWrapper for Wrapped<H> {
    type Handle = H::Handle;

    #[inline]
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Wrapped<H> {
        // SAFETY: Caller ensures that we don't alias on drop
        Wrapped(ManuallyDrop::new(unsafe { H::wrap_raw(raw) }))
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.0.as_object_handle()
    }
}

impl<H: HandleWrapper> AsRef<H> for Wrapped<H> {
    fn as_ref(&self) -> &H {
        &self.0
    }
}

impl<H: HandleWrapper> Deref for Wrapped<H> {
    type Target = H;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<H: HandleWrapper> core::ops::DerefMut for Wrapped<H> {
    fn deref_mut(&mut self) -> &mut H {
        &mut self.0
    }
}

/// A handle with unique access to a context space
///
/// ## Drop IRQL: `..=DISPATCH_LEVEL`
#[derive(PartialEq, Eq)]
pub struct Unique<H: HandleWrapper, T: IntoContextSpace>(H, PhantomData<T>);

impl<H, T> Unique<H, T>
where
    T: IntoContextSpace,
    H: HandleWrapper + HasContext<T>,
{
    /// Creates a new `Unique` wrapper for a context space
    ///
    /// ## Safety
    ///
    /// Must have exclusive access to the context space
    pub unsafe fn into_unique(handle: H) -> Self {
        Self(handle, PhantomData)
    }

    /// Gets the context space of a specific type on a handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn get_context(&self) -> Pin<&T> {
        // SAFETY: Having a `Unique` guarantee that we have exclusive access to the context space
        unsafe {
            context_space::get_context_mut::<T, _>(&self.0)
                .expect("object context space should be initialized")
                .into_ref()
        }
    }

    /// Gets a mutable reference to the context space of a specific type on a handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn get_context_mut(&mut self) -> Pin<&mut T> {
        // SAFETY: Having a `Unique` guarantee that we have exclusive access to the context space
        unsafe {
            context_space::get_context_mut(&self.0)
                .expect("object context space should be initialized")
        }
    }

    /// Unwraps the `Unique` wrapper, allowing the handle to be cloned.
    pub fn into_shared(self) -> H {
        // SAFETY: Having a `Unique` guarantee that we have exclusive access to the context space
        unsafe {
            context_space::make_context_shared::<T, _>(&self.0);
        }

        self.0
    }

    pub fn raw_handle(&self) -> *mut H::Handle {
        self.0.as_object_handle().cast()
    }
}

pub trait HasContext<T: IntoContextSpace>: HandleWrapper {
    /// Gets the context space of a specific type on a handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    fn get_context(&self) -> Pin<&T> {
        context_space::get_context(self).expect("object context space should be initialized")
    }
}

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> HasContext<T>
    for RawHandleWithContext<H, T, Mode>
{
}

impl<H: HandleWrapper, T: IntoContextSpace> HasContext<T> for WithContext<H, T> {}

impl<T: IntoContextSpace, H: HandleWrapper + HasContext<T>, const TAG: usize> HasContext<T>
    for Ref<H, TAG>
{
}

// /// ## Safety
// ///
// /// Must have exclusive access to the context space
// pub unsafe trait HasContextMut<T: IntoContextSpace>: HasContext<T> {
//     /// Gets the context space of a specific type on a handle
//     ///
//     /// ## IRQL: `..=DISPATCH_LEVEL`
//     fn get_context_mut(&self) -> Pin<&mut T> {
//         // SAFETY: Trait implementors guarantee that we have exclusive access to the context space
//         unsafe {
//             context_space::get_context_mut(self)
//                 .expect("object context space should be initialized")
//         }
//     }
// }

/// Represents a wrapper around a framework handle
pub trait HandleWrapper: Sized {
    type Handle;

    /// ## Safety
    ///
    /// Must not alias exclusive accesses on drop.
    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self;

    /// Yield the raw handle backing this wrapper
    fn as_object_handle(&self) -> WDFOBJECT;
}

/// Any handle that can be cloned into a [`Ref`] wrapper.
pub trait CloneRef: Sized {
    type Ref<const TAG: usize>: HandleWrapper;

    /// Makes a shared reference to the handle, with location and tagging info.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    fn clone_ref<const TAG: usize>(&self, file: *const i8, line: u32) -> Self::Ref<{ TAG }>;

    /// Helper method to clone without caring about the tag
    fn clone_ref_untagged(
        &self,
        file: *const i8,
        line: u32,
    ) -> Self::Ref<{ context_space::REF_TAG }> {
        Self::clone_ref(self, file, line)
    }
}

impl<T> CloneRef for &T
where
    T: CloneRef,
{
    type Ref<const TAG: usize> = <T as CloneRef>::Ref<TAG>;

    fn clone_ref<const TAG: usize>(&self, file: *const i8, line: u32) -> Self::Ref<{ TAG }> {
        <T as CloneRef>::clone_ref(self, file, line)
    }
}

/// A raw framework handle type
pub trait WdfHandle {}

macro_rules! impl_wdf_handle {
    ($($handle:ident = $handle_ty:ident),+ $(,)?) => {
        $(
            pub(crate) type $handle = wdf_kmdf_sys::$handle_ty;
            impl WdfHandle for $handle {}
        )+
    };
}

impl_wdf_handle! {
    RawChildList = WDFCHILDLIST__,
    RawCMResList = WDFCMRESLIST__,
    RawCollection = WDFCOLLECTION__,
    RawCommonBuffer = WDFCOMMONBUFFER__,
    RawCompanionTarget = WDFCOMPANIONTARGET__,
    RawDevice = WDFDEVICE__,
    RawDmaEnabler = WDFDMAENABLER__,
    RawDmaTransaction = WDFDMATRANSACTION__,
    RawDpc = WDFDPC__,
    RawDriver = WDFDRIVER__,
    RawFileObject = WDFFILEOBJECT__,
    RawInterrupt = WDFINTERRUPT__,
    RawIoResList = WDFIORESLIST__,
    RawIoResReqList = WDFIORESREQLIST__,
    RawIoTarget = WDFIOTARGET__,
    RawKey = WDFKEY__,
    RawLookaside = WDFLOOKASIDE__,
    RawMemory = WDFMEMORY__,
    RawQueue = WDFQUEUE__,
    RawRequest = WDFREQUEST__,
    RawSpinlock = WDFSPINLOCK__,
    RawString = WDFSTRING__,
    RawTimer = WDFTIMER__,
    RawUsbDevice = WDFUSBDEVICE__,
    RawUsbInterface = WDFUSBINTERFACE__,
    RawUsbPipe = WDFUSBPIPE__,
    RawWaitlock = WDFWAITLOCK__,
    RawWmiInstance = WDFWMIINSTANCE__,
    RawWmiProvider = WDFWMIPROVIDER__,
    RawWorkItem = WDFWORKITEM__,
}

pub(crate) type RawObject = core::ffi::c_void;
impl WdfHandle for RawObject {}

/// How the handle is to be owned
pub trait OwningMode: private::Sealed {
    const TAG: usize;
}

/// A raw handle is owned by the driver, and is responsible for deleting it.
pub struct DriverOwned;

/// A raw handle is owned by the framework, and is never manually deleted.
pub struct FrameworkOwned;

impl OwningMode for DriverOwned {
    const TAG: usize = context_space::OWN_TAG;
}
impl OwningMode for FrameworkOwned {
    const TAG: usize = context_space::FRAMEWORK_TAG;
}

mod private {
    pub trait Sealed {}
}

impl private::Sealed for DriverOwned {}
impl private::Sealed for FrameworkOwned {}

struct UninitDropBomb(bool);

impl UninitDropBomb {
    fn new() -> Self {
        Self(false)
    }

    fn defuse(&mut self) {
        self.0 = true;
    }

    fn is_defused(&self) -> bool {
        self.0
    }
}

impl Drop for UninitDropBomb {
    fn drop(&mut self) {
        assert!(
            self.is_defused(),
            "`UninitContextSpace::init` was not called"
        );
    }
}
