#![no_std]
#![feature(allocator_api)]

use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDEVICE, _DPFLTR_TYPE::DPFLTR_IHVDRIVER_ID};
use windows_kernel_sys::{
    DbgPrintEx, Error, DPFLTR_INFO_LEVEL, NTSTATUS, PDRIVER_OBJECT, PUNICODE_STRING,
};

#[allow(non_snake_case)]
#[no_mangle]
unsafe extern "system" fn DriverEntry(
    driver_object: PDRIVER_OBJECT,
    registry_path: PUNICODE_STRING,
) -> NTSTATUS {
    #[global_allocator]
    static ALLOCATOR: windows_kernel_rs::alloc::KernelAlloc = windows_kernel_rs::alloc::KernelAlloc;

    // Print "Hello World" for DriverEntry
    unsafe {
        DbgPrintEx(
            DPFLTR_IHVDRIVER_ID as u32,
            DPFLTR_INFO_LEVEL,
            b"KmdfHewwoWowwd: DriverEntry\n\0".as_ptr().cast(),
        )
    };

    match wdf_kmdf::driver::Driver::<KernelModule>::create(
        driver_object,
        registry_path,
        wdf_kmdf::driver::DriverConfig::default(),
        |_| pinned_init::try_init!(KernelModule {}? Error),
    ) {
        Ok(()) => windows_kernel_sys::sys::Win32::Foundation::STATUS_SUCCESS,
        Err(err) => err.0,
    }
}

struct KernelModule {}

wdf_kmdf::impl_context_space!(KernelModule);

#[vtable::vtable]
impl wdf_kmdf::driver::DriverCallbacks for KernelModule {
    fn device_add(
        &self,
        mut device_init: PWDFDEVICE_INIT,
    ) -> Result<(), windows_kernel_sys::Error> {
        // Allocate the device object
        let mut h_device: WDFDEVICE = core::ptr::null_mut();

        // Print "Hello World"
        unsafe {
            DbgPrintEx(
                DPFLTR_IHVDRIVER_ID as u32,
                DPFLTR_INFO_LEVEL,
                b"KmdfHewwoWowwd: evt_device_add\n\0".as_ptr().cast(),
            )
        };

        // Create the device object
        let status =
            unsafe { wdf_kmdf::raw::WdfDeviceCreate(&mut device_init, None, &mut h_device) };

        windows_kernel_sys::Error::to_err(status)
    }

    fn unload(&mut self) {
        unsafe {
            DbgPrintEx(
                DPFLTR_IHVDRIVER_ID as u32,
                DPFLTR_INFO_LEVEL,
                b"KmdfHewwoWowwd: EvtUnload\n\0".as_ptr().cast(),
            )
        };
    }
}

// unfortunately we need to declare these two (__CxxFrameHandler3 & _fltused)
//
// `_fltused` is particularly problematic, since it implies that we have
// floating point operations somewhere, and on x86 kernel mode drivers
// should wrap floating point operations with state saving.
// (see https://github.com/Trantect/win_driver_example/issues/4)
//
// Since we don't plan to support the x86 arch, this isn't *too* bad,
// but still sorta bad.
//
// This maybe comes from llvm being unable to determine that panicking
// never happens, but might require having to use a crate using linker
// tricks to guarantee it
#[used]
#[no_mangle]
pub static _fltused: i32 = 0;
#[no_mangle]
pub extern "system" fn __CxxFrameHandler3() -> i32 {
    0
}

#[cfg(not(test))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}
