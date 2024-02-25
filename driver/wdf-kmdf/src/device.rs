use wdf_kmdf_sys::WDFDEVICE;

use crate::{
    context_space::IntoContextSpace,
    handle::{FrameworkOwned, HandleWrapper, HasContext, RawHandleWithContext},
};

pub struct ControlDevice<T: IntoContextSpace> {
    handle: RawHandleWithContext<WDFDEVICE, T, FrameworkOwned>,
}

impl<T: IntoContextSpace> ControlDevice<T> {
    /// Gets the control handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFDEVICE {
        self.handle.as_handle()
    }
}

impl<T: IntoContextSpace> HandleWrapper for ControlDevice<T> {
    type Handle = WDFDEVICE;

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

impl<T: IntoContextSpace> HasContext<T> for ControlDevice<T> {}
