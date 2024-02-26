use core::marker::PhantomData;

use wdf_kmdf_sys::WDFREQUEST;

use crate::{
    context_space::IntoContextSpace,
    file_object::FileObject,
    handle::{FrameworkOwned, HandleWrapper, RawHandle, RawRequest, Ref, Wrapped},
    raw,
};

/// Trait implemented by all request types
pub trait Request: HandleWrapper<Handle = RawRequest> {}

pub struct FileRequest<F> {
    handle: RawHandle<RawRequest, FrameworkOwned>,
    // FIXME: Bound with file callbacks?
    _file: PhantomData<F>,
}

impl<F> FileRequest<F> {
    /// Gets the request handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFREQUEST {
        self.handle.as_handle()
    }
}

impl<F> FileRequest<F>
where
    F: IntoContextSpace,
{
    /// Gets the file object associated with this request
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn file_object(&self) -> Wrapped<FileObject<F>> {
        // SAFETY: `self.handle.as_handle()` always points to a valid request handle by construction.
        let file_object = unsafe { raw::WdfRequestGetFileObject(self.handle.as_handle()) };
        assert!(
            !file_object.is_null(),
            "request {:x?} does not have a file object",
            self.handle.as_handle()
        );

        // SAFETY: We ensure that `file_object` is never null, and we trust WDF
        // that the `file_object` we get is a vaild `WDFFILEOBJECT`
        unsafe { FileObject::wrap(file_object) }
    }

    /// Makes a shared reference to the object
    ///
    /// ## IRQL: `..=DISPATCH_LEVEL`
    pub fn clone_ref(&self) -> Ref<Self> {
        Ref::clone_from_handle(self)
    }
}

impl<F> HandleWrapper for FileRequest<F> {
    type Handle = RawRequest;

    unsafe fn wrap_raw(raw: *mut Self::Handle) -> Self {
        Self {
            // SAFETY: Caller ensures that we don't alias on drop
            handle: unsafe { RawHandle::wrap_raw(raw) },
            _file: PhantomData,
        }
    }

    fn as_object_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.as_object_handle()
    }
}

impl<F> core::fmt::Debug for FileRequest<F> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("FileRequest").field(&self.handle).finish()
    }
}

impl<F> Request for FileRequest<F> {}
