#![no_std]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    // it's all unsafe
    clippy::missing_safety_doc,
)]

use windows_kernel_sys::*;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[macro_export]
macro_rules! WDF_NO_OBJECT_ATTRIBUTES {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_EVENT_CALLBACK {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_HANDLE {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_CONTEXT {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_SEND_OPTIONS {
    () => {
        ::core::ptr::null_mut()
    };
}

// Right now, we don't handle struct versioning, so it's just the struct's size.
// Should probably be a `Result<u32, Error>`
fn WDF_STRUCTURE_SIZE<T: Sized>() -> u32 {
    core::mem::size_of::<T> as u32
}

impl WDF_FILEOBJECT_CONFIG {
    /// Sets:
    /// - Size
    /// - Specified callback function pointers
    /// - FileObjectClass to [`_WDF_FILEOBJECT_CLASS::WdfFileObjectWdfCannotUseFsContexts`]
    /// - AutoForwardCleanupClose to [`_WDF_TRI_STATE::WdfUseDefault`]
    pub fn init(
        EvtDeviceFileCreate: PFN_WDF_DEVICE_FILE_CREATE, // in, optional
        EvtFileClose: PFN_WDF_FILE_CLOSE,                // in, optional
        EvtFileCleanup: PFN_WDF_FILE_CLEANUP,            // in, optional
    ) -> Self {
        // SAFETY: All are zero-able
        let mut config: Self = unsafe { core::mem::zeroed() };

        config.Size = WDF_STRUCTURE_SIZE::<Self>();

        config.EvtDeviceFileCreate = EvtDeviceFileCreate;
        config.EvtFileClose = EvtFileClose;
        config.EvtFileCleanup = EvtFileCleanup;

        config.FileObjectClass = _WDF_FILEOBJECT_CLASS::WdfFileObjectWdfCannotUseFsContexts;
        config.AutoForwardCleanupClose = _WDF_TRI_STATE::WdfUseDefault;

        config
    }
}
