#![no_std]

use windows_kernel_rs::{log, string::unicode_string::NtUnicodeStr, DriverObject};
use windows_kernel_sys::{result::STATUS, Error, NTSTATUS, PDRIVER_OBJECT, PUNICODE_STRING};

fn driver_entry(
    _driver_object: DriverObject,
    _registry_path: NtUnicodeStr<'_>,
) -> Result<(), Error> {
    Ok(())
}

#[no_mangle]
unsafe extern "system" fn DriverEntry(
    driver_object: PDRIVER_OBJECT,
    registry_path: PUNICODE_STRING,
) -> NTSTATUS {
    #[global_allocator]
    static ALLOCATOR: windows_kernel_rs::allocator::KernelAlloc =
        windows_kernel_rs::allocator::KernelAlloc;

    windows_kernel_rs::init_kernel_logger!(log::COMPONENT_IHVDRIVER, log::LevelFilter::Trace);

    // SAFETY: This is the driver entry point
    let driver_object = unsafe { DriverObject::new(driver_object) };
    // SAFETY: Is a copy of the original PCUNICODE_STRING, but never modified,
    // and lifetime is tied to this variable (which is bound to `DriverEntry`)
    let registry_path = unsafe {
        NtUnicodeStr::from_raw_parts(
            (*registry_path).Buffer,
            (*registry_path).Length,
            (*registry_path).MaximumLength,
        )
    };

    match driver_entry(driver_object, registry_path) {
        Ok(()) => STATUS::SUCCESS,
        Err(err) => err.0,
    }
    .to_u32()
}

// unfortunately we need to declare _fltused
//
// `_fltused` is problematic since it implies that we have floating point
// operations somewhere, and on x86 kernel mode drivers should wrap floating
// point operations with state saving.
// (see https://github.com/Trantect/win_driver_example/issues/4)
//
// Since we don't plan to support the x86 arch, this isn't *too* bad,
// but still sorta bad.
#[used]
#[no_mangle]
pub static _fltused: i32 = 0;

#[cfg(not(any(test, std)))]
#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    windows_kernel_rs::__handle_panic(info);
}

#[no_mangle]
pub extern "system" fn __CxxFrameHandler3() -> i32 {
    0
}
