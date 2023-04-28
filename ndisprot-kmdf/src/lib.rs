#![no_std]

use core::mem::MaybeUninit;

use wdf_kmdf_sys::{
    wdf_driver_config_init, PWDFDEVICE_INIT, WDFDEVICE, WDFDRIVER, WDF_DRIVER_CONFIG,
};
use windows_kernel_sys::{DbgPrint, NTSTATUS, PDRIVER_OBJECT, PUNICODE_STRING};

#[allow(non_snake_case)]
#[no_mangle]
unsafe extern "system" fn DriverEntry(
    driver_object: PDRIVER_OBJECT,
    registry_path: PUNICODE_STRING,
) -> NTSTATUS {
    // Allocate the driver configuration object
    let mut config: MaybeUninit<WDF_DRIVER_CONFIG> = MaybeUninit::uninit();

    // Print "Hello World" for DriverEntry
    unsafe { DbgPrint(b"KmdfHewwoWowwd: DriverEntry\n\0".as_ptr().cast()) };

    // Initiaze the driver configuration object to register the
    // entry point for the EvtDeviceAdd callback, evt_device_add
    unsafe { wdf_driver_config_init(config.as_mut_ptr(), Some(evt_device_add)) };
    // SAFETY: `wdf_driver_config_init` initializes it
    let mut config = unsafe { config.assume_init() };

    // Finally, create the driver object
    unsafe { wdf_kmdf::raw::WdfDriverCreate(driver_object, registry_path, None, &mut config, None) }
}

unsafe extern "C" fn evt_device_add(
    _driver: WDFDRIVER,
    mut device_init: PWDFDEVICE_INIT,
) -> NTSTATUS {
    // Allocate the device object
    let mut h_device: WDFDEVICE = core::ptr::null_mut();

    // Print "Hello World"
    unsafe { DbgPrint(b"KmdfHewwoWowwd: evt_device_add\n\0".as_ptr().cast()) };

    // Create the device object
    unsafe { wdf_kmdf::raw::WdfDeviceCreate(&mut device_init, None, &mut h_device) }
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
