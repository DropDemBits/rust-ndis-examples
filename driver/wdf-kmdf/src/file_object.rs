use core::{marker::PhantomData, pin::Pin};

use pinned_init::PinInit;
use wdf_kmdf_sys::WDFFILEOBJECT;
use windows_kernel_sys::Error;

use crate::{
    object::{self, HandleKind, IntoContextSpace},
    raw,
};

pub struct FileObject<T> {
    handle: WDFFILEOBJECT,
    kind: HandleKind,
    _context: PhantomData<fn() -> T>,
}

impl<T> core::fmt::Debug for FileObject<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("FileObject")
            .field("handle", &self.handle)
            .field("kind", &self.kind)
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
    pub unsafe fn wrap(handle: WDFFILEOBJECT) -> Self {
        Self {
            handle,
            kind: HandleKind::Wrapped,
            _context: PhantomData,
        }
    }

    /// Gets the file object handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&mut self) -> WDFFILEOBJECT {
        self.handle
    }

    /// FIXME: Temporarily pub, figure out a proper way of hiding this
    #[doc(hidden)]
    pub unsafe extern "C" fn __dispatch_evt_destroy(object: wdf_kmdf_sys::WDFOBJECT) {
        // SAFETY: EvtDestroy is only called once, and when there are no other references to the object
        let handle = unsafe { Self::wrap(object.cast()) };

        // Drop the context area
        // SAFETY: `EvtDestroy` guarantees that we have exclusive access to the context space
        let status = unsafe { crate::object::drop_context_space::<T>(&handle, |_| ()) };

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
    pub fn clone_ref(&self) -> FileObject<T> {
        // Safety: Caller ensures that we're at the right IRQL
        unsafe {
            raw::WdfObjectReferenceActual(
                self.handle.cast(),
                None,
                line!() as i32,
                Some(cstr!(file!()).as_ptr()),
            )
        }

        FileObject {
            kind: HandleKind::Ref,
            ..*self
        }
    }
}

impl<T> Drop for FileObject<T> {
    fn drop(&mut self) {
        // FIXME: Assert that this is at <= DISPATCH_LEVEL
        match self.kind {
            HandleKind::Wrapped => {}
            // Safety: assertion that we're at the correct IRQL
            HandleKind::Owned | HandleKind::Parented => unsafe {
                raw::WdfObjectDelete(self.handle.cast())
            },
            // Safety: assertion that we're at the correct IRQL
            HandleKind::Ref => unsafe {
                raw::WdfObjectDereferenceActual(
                    self.handle.cast(),
                    None,
                    line!() as i32,
                    Some(cstr!(file!()).as_ptr()),
                )
            },
        }
    }
}

impl<T> object::AsObjectHandle for FileObject<T> {
    fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.cast()
    }

    fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.cast()
    }
}
