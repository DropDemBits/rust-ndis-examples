#![no_std]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    // it's all unsafe
    clippy::missing_safety_doc,
)]

pub use core::ffi::*;

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

    /// ## IRQL
    /// PASSIVE_LEVEL
    #[must_use]
    pub fn wdf_driver_create(
        DriverObject: PDRIVER_OBJECT,
        RegistryPath: PCUNICODE_STRING,
        DriverAttributes: PWDF_OBJECT_ATTRIBUTES,
        DriverConfig: PWDF_DRIVER_CONFIG,
        Driver: *mut WDFDRIVER,
    ) -> NTSTATUS;

    /// ## IRQL
    /// PASSIVE_LEVEL
    #[must_use]
    pub fn wdf_device_create(
        DeviceInit: *mut PWDFDEVICE_INIT,
        DeviceAttributes: PWDF_OBJECT_ATTRIBUTES,
        Device: *mut WDFDEVICE,
    ) -> NTSTATUS;
}
