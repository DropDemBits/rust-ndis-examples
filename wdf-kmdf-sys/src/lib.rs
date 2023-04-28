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

#[link(name = "wrapper_bindings")]
extern "C" {
    pub fn wdf_driver_config_init(
        Config: PWDF_DRIVER_CONFIG,
        EvtDriverDeviceAdd: PFN_WDF_DRIVER_DEVICE_ADD,
    );
}
