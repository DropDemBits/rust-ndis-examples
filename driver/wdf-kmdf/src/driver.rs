use core::{marker::PhantomData, ops::Deref, pin::Pin};

use pinned_init::PinInit;
use vtable::vtable;
use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDRIVER, WDF_DRIVER_INIT_FLAGS};
use windows_kernel_rs::{string::unicode_string::NtUnicodeStr, DriverObject};
use windows_kernel_sys::{result::STATUS, Error, NTSTATUS};

use crate::{
    object::{self, default_object_attributes, GetContextSpaceError, HandleKind, IntoContextSpace},
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
    pub fn get_context(&self) -> Result<object::ContextSpaceGuard<'_, T>, GetContextSpaceError> {
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
                Some(object::REF_TAG as *mut _),
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
        // SAFETY: Driver unload only gets called once
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
    }

    unsafe extern "C" fn __dispatch_evt_cleanup(_driver: wdf_kmdf_sys::WDFOBJECT) {
        windows_kernel_rs::log::debug!("EvtCleanupCallback");
    }

    unsafe extern "C" fn __dispatch_evt_destroy(_driver: wdf_kmdf_sys::WDFOBJECT) {
        windows_kernel_rs::log::debug!("EvtDestroyCallback");

        // SAFETY: EvtDestroy is only called once, and when there are no other references to the object
        let handle = unsafe { Driver::<T>::wrap(_driver.cast()) };

        // Drop the context area
        //
        // Dropping the driver object context area is special because all objects
        // without an explicitly set parent get parented to the driver, and by the
        // time we get to `EvtDestroy` any child objects with a refcount of 1 are
        // deleted. This is an issue as some of those objects may be in the driver
        // object context area, and trying to delete an already fully deleted object
        // results in a use-after-free.
        //
        // Thankfully, by adding an initial refcount bump when creating unparented
        // objects, they stay alive until it's time to delete them.
        let status = object::drop_context_space::<T>(&handle, |_| ());

        if let Err(err) = status {
            // No (valid) context space to drop, nothing to do
            windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
        }
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
                    Some(object::REF_TAG as *mut _),
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                )
            }
        }
    }
}
