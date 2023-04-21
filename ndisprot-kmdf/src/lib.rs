#![no_std]

use core::mem::MaybeUninit;

use wdk_kmdf_sys::{
    wdf_device_create, wdf_driver_config_init, wdf_driver_create, DbgPrint, NTSTATUS,
    PDRIVER_OBJECT, PUNICODE_STRING, PWDFDEVICE_INIT, WDFDEVICE, WDFDRIVER, WDF_DRIVER_CONFIG,
    WDF_NO_HANDLE, WDF_NO_OBJECT_ATTRIBUTES,
};

#[allow(non_snake_case)]
#[no_mangle]
unsafe extern "system" fn DriverEntry(
    driver_object: PDRIVER_OBJECT,
    registry_path: PUNICODE_STRING,
) -> NTSTATUS {
    let status;

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
    status = unsafe {
        wdf_driver_create(
            driver_object,
            registry_path,
            WDF_NO_OBJECT_ATTRIBUTES!(),
            &mut config,
            WDF_NO_HANDLE!(),
        )
    };

    status
}

unsafe extern "C" fn evt_device_add(
    _driver: WDFDRIVER,
    mut device_init: PWDFDEVICE_INIT,
) -> NTSTATUS {
    let status;

    // Allocate the device object
    let mut h_device: WDFDEVICE = core::ptr::null_mut();

    // Print "Hello World"
    unsafe { DbgPrint(b"KmdfHewwoWowwd: evt_device_add\n\0".as_ptr().cast()) };

    // Create the device object
    status =
        unsafe { wdf_device_create(&mut device_init, WDF_NO_OBJECT_ATTRIBUTES!(), &mut h_device) };

    status
}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
