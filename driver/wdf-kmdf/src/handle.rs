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
use wdf_kmdf_sys::{WDFOBJECT, WDF_OBJECT_ATTRIBUTES};
use windows_kernel_sys::Error;

use crate::{
    object::{self, IntoContextSpace},
    raw,
};

/// A wrapper around a raw handle
///
/// ## Drop IRQL: `..=DISPATCH_LEVEL`
pub struct RawHandle<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> {
    handle: H,
    _context_space: PhantomData<fn() -> T>,
    _mode: PhantomData<Mode>,
}

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> RawHandle<H, T, Mode> {
    /// Creates a new raw handle from a foreign handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    ///
    /// ## Safety
    ///
    /// Must have the correct ownership over the raw handle
    pub unsafe fn create(handle: H) -> Self {
        // We only adjust refcount for framework objects
        if Mode::TAG != object::FRAMEWORK_TAG {
            // SAFETY: Caller ensures that the handle is valid and that we're
            // called at the right IRQL
            unsafe {
                raw::WdfObjectReferenceActual(
                    handle.as_object_handle(),
                    Some(Mode::TAG as *mut _),
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                );
            }
        }

        Self {
            handle,
            _context_space: PhantomData,
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
    pub unsafe fn create_parented(handle: H) -> Self {
        // SAFETY: Caller ensures that the handle is valid and that we're
        // called at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                handle.as_object_handle(),
                Some(object::REF_TAG as *mut _),
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            );
        }

        Self {
            handle,
            _context_space: PhantomData,
            _mode: PhantomData,
        }
    }

    /// Gets the backing raw handle
    #[inline]
    pub fn as_handle(&self) -> H {
        self.handle
    }

    /// Default attributes to properly contstruct the object context space
    pub fn default_object_attributes() -> WDF_OBJECT_ATTRIBUTES {
        let mut attributes = object::default_object_attributes::<T>();
        attributes.EvtDestroyCallback = Some(default_dispatch_evt_destroy::<T>);
        attributes
    }
}

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> Drop for RawHandle<H, T, Mode> {
    fn drop(&mut self) {
        // FIXME: Assert that this is at `..=DISPATCH_LEVEL`

        // We only adjust refcount for framework objects since we aren't responsible
        // for deleting them
        if Mode::TAG != object::FRAMEWORK_TAG {
            if Mode::TAG == object::OWN_TAG {
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

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> HandleWrapper for RawHandle<H, T, Mode> {
    type Handle = H;

    #[inline]
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Self {
        Self {
            // SAFETY: Caller ensures that it is a handle of the correct type
            handle: unsafe { H::wrap_raw(raw) },
            _context_space: PhantomData,
            _mode: PhantomData,
        }
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<H, T, Mode> core::fmt::Debug for RawHandle<H, T, Mode>
where
    H: WdfHandle + core::fmt::Debug,
    T: IntoContextSpace,
    Mode: OwningMode,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("RawHandle").field(&self.handle).finish()
    }
}

impl<H, T, Mode> PartialEq for RawHandle<H, T, Mode>
where
    H: WdfHandle + PartialEq,
    T: IntoContextSpace,
    Mode: OwningMode,
{
    fn eq(&self, other: &Self) -> bool {
        self.handle == other.handle
    }
}

impl<H, T, Mode> Eq for RawHandle<H, T, Mode>
where
    H: WdfHandle + Eq,
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
        object::get_context(&self.handle).expect("object context space should be initialized")
    }
}

impl<H: HandleWrapper, T: IntoContextSpace> HandleWrapper for WithContext<H, T> {
    type Handle = <H as HandleWrapper>::Handle;

    #[inline]
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Self {
        Self {
            // SAFETY: Caller has the responsibility of this being a valid handle
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
        let mut attributes = object::default_object_attributes::<T>();
        attributes.EvtDestroyCallback = Some(default_dispatch_evt_destroy::<T>);

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
        let mut context_space = core::ptr::null_mut();
        let status = unsafe {
            Error::to_err(raw::WdfObjectAllocateContext(
                handle.as_object_handle(),
                &mut attributes,
                &mut context_space,
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
        let status = unsafe { object::context_pin_init::<T, H, _, _>(&handle, init) };
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
pub struct Ref<H: HandleWrapper>(ManuallyDrop<H>);


impl<H: HandleWrapper> Ref<H> {
    /// Transforms an owning handle into a parented ref handle
    ///
    /// ## Safety
    ///
    /// Handle must be from a [`RawHandle::create_parented`] call
    pub unsafe fn into_parented(handle: H) -> Self {
        // Note: We don't bump up the refcount becausae
        Self(ManuallyDrop::new(handle))
    }

    /// Creates a new reference to a handle from an existing handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_from_handle(handle: &H) -> Self {
        // SAFETY: Manually drop ensures that we only adjust the refcount
        // and not delete the object
        let handle = ManuallyDrop::new(unsafe { H::wrap_raw(handle.as_object_handle()) });

        // SAFETY: Caller ensures that we're called at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                handle.as_object_handle(),
                Some(object::REF_TAG as *mut _),
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            );
        }

        Self(handle)
    }

    /// Makes a new [`Ref`] to this handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Ref<H> {
        Self::clone_from_handle(&self.0)
    }
}

impl<H: HandleWrapper> Drop for Ref<H> {
    fn drop(&mut self) {
        // FIXME: Assert that this is at `..=DISPATCH_LEVEL`

        // SAFETY: Caller ensures that the handle is valid and that we're
        // called at the right IRQL
        unsafe {
            raw::WdfObjectDereferenceActual(
                self.0.as_object_handle(),
                Some(object::REF_TAG as *mut _),
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            );
        }
    }
}

impl<H: HandleWrapper> HandleWrapper for Ref<H> {
    type Handle = <H as HandleWrapper>::Handle;

    #[inline]
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Self {
        // SAFETY: Caller has the responsibility of this being a valid handle
        Self(ManuallyDrop::new(unsafe { H::wrap_raw(raw) }))
    }

    #[inline]
    fn as_object_handle(&self) -> WDFOBJECT {
        self.0.as_object_handle()
    }
}

impl<H: HandleWrapper> AsRef<H> for Ref<H> {
    fn as_ref(&self) -> &H {
        &self.0
    }
}

impl<H: HandleWrapper> Deref for Ref<H> {
    type Target = H;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<H> core::fmt::Debug for Ref<H>
where
    H: HandleWrapper + core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Ref").field(&*self.0).finish()
    }
}

impl<H: HandleWrapper> PartialEq<H> for Ref<H>
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

impl<H: HandleWrapper> Wrapped<H> {
    /// Makes a new [`Ref`] to this handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Ref<H> {
        Ref::clone_from_handle(&self.0)
    }
}

impl<H: HandleWrapper> HandleWrapper for Wrapped<H> {
    type Handle = H::Handle;

    #[inline]
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Wrapped<H> {
        // SAFETY: It is the caller's responsibility that `raw` is a valid handle
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

pub trait HasContext<T: IntoContextSpace>: HandleWrapper {
    /// Gets the context space of a specific type on a handle
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    fn get_context(&self) -> Pin<&T> {
        object::get_context(self).expect("object context space should be initialized")
    }
}

impl<H: WdfHandle, T: IntoContextSpace, Mode: OwningMode> HasContext<T> for RawHandle<H, T, Mode> {}

impl<H: HandleWrapper, T: IntoContextSpace> HasContext<T> for WithContext<H, T> {}

/// Represents a wrapper around a framework handle
pub trait HandleWrapper: Sized {
    type Handle;
    // type RefHandle;

    /// ## Safety:
    ///
    /// Must be a valid handle of the correct type, and must not
    /// alias exclusive accesses on drop.
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Self;

    /// Yield the raw handle backing this wrapper
    fn as_object_handle(&self) -> WDFOBJECT;
}

// FIXME: Just to bridge the gap between `AsObjectHandle` and `HandleWrapper`
impl<T: HandleWrapper> object::AsObjectHandle for T {
    #[inline]
    fn as_handle(&self) -> WDFOBJECT {
        self.as_object_handle()
    }

    #[inline]
    fn as_handle_mut(&mut self) -> WDFOBJECT {
        self.as_object_handle()
    }
}

/// A raw framework handle
pub trait WdfHandle: Copy {
    /// Gets a typed raw handle from an object handle
    ///
    /// ## Safety
    ///
    /// `raw` must be the correct type of handle, and it must be valid.
    unsafe fn wrap_raw(raw: WDFOBJECT) -> Self;

    /// Gets the WDF handle as an object handle
    fn as_object_handle(&self) -> WDFOBJECT;
}

macro_rules! impl_wdf_handle {
    ($($handle:ident),+ $(,)?) => {
        $(
            impl WdfHandle for wdf_kmdf_sys::$handle {
                #[inline]
                unsafe fn wrap_raw(raw: WDFOBJECT) -> Self {
                    raw.cast()
                }

                #[inline]
                fn as_object_handle(&self) -> WDFOBJECT {
                    self.cast()
                }
            }
        )+
    };
}

impl_wdf_handle! {
    WDFCHILDLIST,
    WDFCMRESLIST,
    WDFCOLLECTION,
    WDFCOMMONBUFFER,
    WDFCOMPANIONTARGET,
    WDFDEVICE,
    WDFDMAENABLER,
    WDFDMATRANSACTION,
    WDFDPC,
    WDFDRIVER,
    WDFFILEOBJECT,
    WDFINTERRUPT,
    WDFIORESLIST,
    WDFIORESREQLIST,
    WDFIOTARGET,
    WDFKEY,
    WDFLOOKASIDE,
    WDFMEMORY,
    WDFOBJECT,
    WDFQUEUE,
    WDFREQUEST,
    WDFSPINLOCK,
    WDFSTRING,
    WDFTIMER,
    WDFUSBDEVICE,
    WDFUSBINTERFACE,
    WDFUSBPIPE,
    WDFWAITLOCK,
    WDFWMIINSTANCE,
    WDFWMIPROVIDER,
    WDFWORKITEM,
}

/// How the handle is to be owned
pub trait OwningMode: private::Sealed {
    const TAG: usize;
}

/// A raw handle is owned by the driver, and is responsible for deleting it.
pub struct DriverOwned;

/// A raw handle is owned by the framework, and is never manually deleted.
pub struct FrameworkOwned;

impl OwningMode for DriverOwned {
    const TAG: usize = object::OWN_TAG;
}
impl OwningMode for FrameworkOwned {
    const TAG: usize = object::FRAMEWORK_TAG;
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

// FIXME: I guess this should go into `object`?
unsafe extern "C" fn default_dispatch_evt_destroy<T: IntoContextSpace>(object: WDFOBJECT) {
    // SAFETY: EvtDestroy is only called once, and there are no other references to the object by the time we're here
    let handle = unsafe { RawHandle::<WDFOBJECT, T, FrameworkOwned>::wrap_raw(object) };

    // Drop the context area
    // SAFETY: `EvtDestroy` guarantees that we have exclusive access to the context space
    let status = unsafe { crate::object::drop_context_space::<T, _>(&handle, |_| ()) };

    if let Err(err) = status {
        // No (valid) context space to drop, nothing to do
        windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
    }
}
