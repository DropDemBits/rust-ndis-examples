use wdf_kmdf_sys::WDFDEVICE;

use crate::{
    context_space::IntoContextSpace,
    handle::{FrameworkOwned, HandleWrapper, HasContext, RawDevice, RawHandleWithContext},
};

pub struct ControlDevice<T: IntoContextSpace> {
    handle: RawHandleWithContext<RawDevice, T, FrameworkOwned>,
}

impl<T: IntoContextSpace> ControlDevice<T> {
    /// Gets the control handle for use with WDF functions that don't have clean wrappers yet
    pub fn raw_handle(&self) -> WDFDEVICE {
        self.handle.as_handle()
    }
}

impl<T: IntoContextSpace> HandleWrapper for ControlDevice<T> {
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

impl<T: IntoContextSpace> HasContext<T> for ControlDevice<T> {}
