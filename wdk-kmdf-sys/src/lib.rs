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

#[link(name = "wrapper_bindings")]
extern "C" {
    pub fn wdf_driver_config_init(
        Config: PWDF_DRIVER_CONFIG,
        EvtDriverDeviceAdd: PFN_WDF_DRIVER_DEVICE_ADD,
    );

    pub fn wdf_driver_create(
        DriverObject: PDRIVER_OBJECT,
        RegistryPath: PCUNICODE_STRING,
        DriverAttributes: PWDF_OBJECT_ATTRIBUTES,
        DriverConfig: PWDF_DRIVER_CONFIG,
        Driver: *mut WDFDRIVER,
    ) -> NTSTATUS;
}
