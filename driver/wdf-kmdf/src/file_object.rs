use core::pin::Pin;

use pinned_init::PinInit;
use wdf_kmdf_sys::WDFFILEOBJECT;
use windows_kernel_sys::Error;

use crate::{
    handle::{FrameworkOwned, HandleWrapper, HasContext, RawHandleWithContext, Ref, Wrapped},
    object::{self, IntoContextSpace},
};

pub struct FileObject<T: IntoContextSpace> {
    handle: RawHandleWithContext<WDFFILEOBJECT, T, FrameworkOwned>,
}

impl<T: IntoContextSpace> core::fmt::Debug for FileObject<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FileObject")
            .field("handle", &self.handle)
            .finish()
    }
}

impl<T> FileObject<T>
where
    T: IntoContextSpace,
{
    /// Wraps the raw handle in a file object wrapper
    ///
    /// ## Safety
    ///
    /// Respect aliasing rules, since this can be used to
    /// generate aliasing mutable references to the context space.
    /// Also, the context space must be initialized.
    // FIXME: Make a proper wrapper eventually
    pub unsafe fn wrap(handle: WDFFILEOBJECT) -> Wrapped<Self> {
        // SAFETY: uhhhh
        unsafe { Wrapped::wrap_raw(handle.cast()) }
    }

    /// Gets the file object handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&mut self) -> WDFFILEOBJECT {
        self.handle.as_handle()
    }

    /// FIXME: Temporarily pub, figure out a proper way of hiding this
    #[doc(hidden)]
    pub unsafe extern "C" fn __dispatch_evt_destroy(object: wdf_kmdf_sys::WDFOBJECT) {
        // SAFETY: EvtDestroy is only called once, and when there are no other references to the object
        let handle = unsafe { Self::wrap(object.cast()) };

        // Drop the context area
        // SAFETY: `EvtDestroy` guarantees that we have exclusive access to the context space
        let status = unsafe { crate::object::drop_context_space::<T, _>(&handle, |_| ()) };

        if let Err(err) = status {
            // No (valid) context space to drop, nothing to do
            windows_kernel_rs::log::warn!("No object context space to drop ({:x?})", err);
        }
    }
}

impl<T> FileObject<T>
where
    T: IntoContextSpace,
{
    pub fn get_context(&self) -> Pin<&T> {
        crate::object::get_context(self).expect("context space must be initialized")
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

    /// Makes a shared reference to the file object
    ///
    /// ## IRQL: <= Dispatch
    pub fn clone_ref(&self) -> Ref<FileObject<T>> {
        Ref::clone_from_handle(self)
    }
}

impl<T: IntoContextSpace> HandleWrapper for FileObject<T> {
    type Handle = WDFFILEOBJECT;

    unsafe fn wrap_raw(raw: wdf_kmdf_sys::WDFOBJECT) -> Self {
        Self {
            // SAFETY: Caller ensures that the handle is valid
            handle: unsafe { RawHandleWithContext::wrap_raw(raw) },
        }
    }

    fn as_object_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<T: IntoContextSpace> HasContext<T> for FileObject<T> {}

impl<T: IntoContextSpace> AsRef<FileObject<T>> for FileObject<T> {
    fn as_ref(&self) -> &FileObject<T> {
        self
    }
}
