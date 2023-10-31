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
