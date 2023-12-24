use wdf_kmdf_sys::WDFDEVICE;

use crate::{
    handle::{FrameworkOwned, HandleWrapper, RawHandleWithContext},
    object::IntoContextSpace,
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
        // SAFETY: uhhhhh caller's problem
        Self {
            handle: unsafe { RawHandleWithContext::wrap_raw(raw) },
        }
    }

    fn as_object_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
        self.handle.as_object_handle()
    }
}
