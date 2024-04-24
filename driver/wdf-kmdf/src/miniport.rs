//! Support structures for creating KMDF Miniport drivers
use core::pin::Pin;

use pinned_init::PinInit;
use vtable::vtable;
use wdf_kmdf_sys::{WDFDEVICE, WDFDRIVER, WDF_DRIVER_INIT_FLAGS};
use windows_kernel_rs::{string::unicode_string::NtUnicodeStr, DriverObject};
use windows_kernel_sys::{Error, PDEVICE_OBJECT, PDRIVER_OBJECT};

use crate::{
    context_space::{self, default_object_attributes, IntoContextSpace},
    handle::{
        DriverOwned, FrameworkOwned, HandleWrapper, HasContext, RawDevice, RawDriver,
        RawHandleWithContext, Ref, Wrapped,
    },
    raw,
};

/// Event callbacks for a miniport driver
#[vtable]
pub trait MiniportDriverCallbacks: IntoContextSpace {
    /// Performs operations that must take place before unloading the driver.
    ///
    /// Must deallocate any non-device-specific system resources allocated during
    /// the driver entry function.
    ///
    /// ## IRQL: `..=PASSIVE_LEVEL`
    ///
    /// ## Note
    ///
    /// Most ownership-related unload tasks should instead be done via the fields
    /// implementing `Drop`, rather than being manually deallocated here.
    fn unload(self: Pin<&Self>) {}
}

/// A KMDF Miniport Driver.
pub struct MiniportDriver<T: IntoContextSpace> {
    handle: RawHandleWithContext<RawDriver, T, FrameworkOwned>,
}

impl<T: IntoContextSpace> core::fmt::Debug for MiniportDriver<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("MiniportDriver")
            .field("handle", &self.handle)
            .finish()
    }
}

impl<T> MiniportDriver<T>
where
    T: IntoContextSpace + MiniportDriverCallbacks,
{
    /// Wraps the handle in a raw driver object
    ///
    /// ## Safety
    ///
    /// Respect aliasing rules, since this can be used to
    /// generate aliasing mutable references to the context space
    pub unsafe fn wrap(handle: WDFDRIVER) -> Wrapped<Self> {
        // SAFETY: It's the caller's responsibility to ensure that this doesn't
        // generate aliasing references on drop.
        unsafe { Wrapped::wrap_raw(handle) }
    }

    /// Gets the driver handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFDRIVER {
        self.handle.as_handle()
    }
}

#[derive(Default, Clone, Copy)]
pub struct MiniportDriverConfig {
    /// Tag to mark allocations made by WDF
    pub pool_tag: Option<u32>,
}

impl<T> MiniportDriver<T>
where
    T: IntoContextSpace + MiniportDriverCallbacks,
{
    /// Creates a new WDF Miniport Driver Object.
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
        config: MiniportDriverConfig,
        init_context: impl FnOnce(&Self) -> Result<I, Error>,
    ) -> Result<Self, Error>
    where
        I: PinInit<T, Error>,
    {
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

        let mut driver_config = wdf_kmdf_sys::WDF_DRIVER_CONFIG::init(None);
        driver_config.EvtDriverUnload = Some(Self::__dispatch_driver_unload);

        // WDF does not control the pnp callbacks
        driver_config.DriverInitFlags |= WDF_DRIVER_INIT_FLAGS::WdfDriverInitNonPnpDriver.0 as u32;
        // WDF should not inject itself into the driver unload path
        driver_config.DriverInitFlags |=
            WDF_DRIVER_INIT_FLAGS::WdfDriverInitNoDispatchOverride.0 as u32;

        // NOTE: This is always available since we're targeting KMDF versions after 1.5
        if let Some(tag) = config.pool_tag {
            driver_config.DriverPoolTag = tag;
        }

        // Make it!
        let handle = {
            let driver_object = driver_object.into_raw();

            let mut handle = core::ptr::null_mut();

            // SAFETY: Owned `DriverObject` means that it only gets called once
            unsafe {
                Error::to_err(raw::WdfDriverCreate(
                    driver_object,
                    registry_path.as_ptr().cast(),
                    Some(&mut object_attrs),
                    &mut driver_config,
                    Some(&mut handle),
                ))?;
            }

            // SAFETY: We just created this handle and are immediately wrapping it
            let handle = unsafe { RawHandleWithContext::create(handle) };
            Self { handle }
        };

        // SAFETY:
        // - It's WDF's responsibility to insert the context area, since we create
        //   the default object attributes with T's context area
        // - The driver object was just created
        unsafe { context_space::context_pin_init(&handle, init_context)? };

        // Return the wrapped handle since WDF owns the driver object
        Ok(handle)
    }

    /// Makes a shared reference to the driver
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Ref<Self> {
        Ref::clone_from_handle(self)
    }

    /// Gets the global miniport driver object.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn get() -> Wrapped<Self> {
        // SAFETY: We can only ever call this before or after `WdfDriverCreate`
        // executes.
        let handle = unsafe {
            raw::WdfGetDriver().expect("`MiniportDriver::create` should have returned already")
        };

        // SAFETY: Context spaces can only be accessed through shared refs
        // after creation, and the drop flag ensures that the context space is
        // initialized once we access it.
        unsafe { Self::wrap(handle) }
    }

    /// Gets the WDM driver object that this miniport driver backs
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn wdm_driver_object(&self) -> PDRIVER_OBJECT {
        // SAFETY: Handle guarantees that we pass in a valid driver object.
        unsafe { raw::WdfDriverWdmGetDriverObject(self.handle.as_handle()) }
    }

    /// Unloads the miniport driver.
    ///
    /// This directs the framework to call the
    /// [`MiniportDriverCallbacks::unload`] function and destroy the framework.
    ///
    /// ## Safety
    ///
    /// This must only be called once, and from the port-defined unload callback.
    /// No framework functions should be called after this point.
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub unsafe fn unload() {
        // SAFETY: Caller ensures that this is only called once and won't call
        // any framework functions again.
        unsafe { raw::WdfDriverMiniportUnload(Self::get().handle.as_handle()) }
    }

    unsafe extern "C" fn __dispatch_driver_unload(driver: WDFDRIVER) {
        // SAFETY: Driver unload only gets called once
        let handle = unsafe { MiniportDriver::<T>::wrap(driver) };

        if T::HAS_UNLOAD {
            let context_space = handle.get_context();

            T::unload(context_space);
        }
    }

    unsafe extern "C" fn __dispatch_evt_cleanup(_driver: wdf_kmdf_sys::WDFOBJECT) {
        windows_kernel_rs::log::debug!("EvtCleanupCallback");
    }

    unsafe extern "C" fn __dispatch_evt_destroy(_driver: wdf_kmdf_sys::WDFOBJECT) {
        windows_kernel_rs::log::debug!("EvtDestroyCallback");

        // SAFETY: EvtDestroy is only called once, and when there are no other references to the object
        let handle = unsafe { MiniportDriver::<T>::wrap(_driver.cast()) };

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
        // SAFETY: `EvtDestroy` guarantees that we have exclusive access to the context space
        let status = unsafe { context_space::drop_context_space::<T, _>(&handle, |_| ()) };

        if let Err(err) = status {
            // No (valid) context space to drop, nothing to do
            windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
        }
    }
}

impl<T: IntoContextSpace> HandleWrapper for MiniportDriver<T> {
    type Handle = RawDriver;

    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: unsafe { RawHandleWithContext::wrap_raw(raw) },
        }
    }

    fn as_object_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<T: IntoContextSpace> HasContext<T> for MiniportDriver<T> {}

impl<T: IntoContextSpace> AsRef<MiniportDriver<T>> for MiniportDriver<T> {
    fn as_ref(&self) -> &MiniportDriver<T> {
        self
    }
}

pub struct MiniportDevice<T>
where
    T: IntoContextSpace,
{
    handle: RawHandleWithContext<RawDevice, T, DriverOwned>,
}

impl<T> MiniportDevice<T>
where
    T: IntoContextSpace,
{
    /// Creates a miniport device object
    ///
    /// ## IRQL: `..=PASSIVE_LEVEL`
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn create<D, I, Err>(
        driver: &MiniportDriver<D>,
        device_object: PDEVICE_OBJECT,
        attached_device_object: Option<PDEVICE_OBJECT>,
        pdo: Option<PDEVICE_OBJECT>,
        init_context: impl FnOnce(&Self) -> Result<I, Err>,
    ) -> Result<Self, Error>
    where
        D: IntoContextSpace,
        I: PinInit<T, Err>,
        Error: From<Err>,
    {
        let mut object_attrs = context_space::default_object_attributes::<T>();

        let handle = {
            let mut handle = core::ptr::null_mut();
            // SAFETY:
            // - Caller ensures that we're at the right IRQL
            unsafe {
                Error::to_err(raw::WdfDeviceMiniportCreate(
                    driver.handle.as_handle(),
                    Some(&mut object_attrs),
                    device_object,
                    attached_device_object,
                    pdo,
                    &mut handle,
                ))
            }?;

            // SAFETY: We just created this handle and are immediately wrapping it
            let handle = unsafe { RawHandleWithContext::create(handle) };
            Self { handle }
        };

        // SAFETY:
        // - It's WDF's responsibility to insert the context area, since we create
        //   the default object attributes with T's context area
        // - The object was just created, and the context space has not been initialized yet
        unsafe { context_space::context_pin_init(handle.as_ref(), init_context)? };

        Ok(handle)
    }

    /// Wraps the handle in a raw device object
    ///
    /// ## Safety
    ///
    /// Respect aliasing rules, since this can be used to
    /// generate aliasing mutable references to the context space
    pub unsafe fn wrap(handle: WDFDEVICE) -> Wrapped<Self> {
        // SAFETY: It's the caller's responsibility to ensure that this doesn't
        // generate aliasing references on drop.
        unsafe { Wrapped::wrap_raw(handle) }
    }

    /// Gets the handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFDEVICE {
        self.handle.as_handle()
    }

    /// Makes a shared reference to the device
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Ref<Self> {
        Ref::clone_from_handle(self)
    }
}

impl<T: IntoContextSpace> HandleWrapper for MiniportDevice<T> {
    type Handle = RawDevice;

    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: unsafe { RawHandleWithContext::wrap_raw(raw) },
        }
    }

    fn as_object_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<T: IntoContextSpace> HasContext<T> for MiniportDevice<T> {}

impl<T: IntoContextSpace> AsRef<MiniportDevice<T>> for MiniportDevice<T> {
    fn as_ref(&self) -> &MiniportDevice<T> {
        self
    }
}
