#![no_std]

extern crate alloc;

pub mod allocator {
    //! Global kernel allocators
    //! Modified from <https://github.com/not-matthias/kernel-alloc-rs/blob/19b2b992c0f0dacf60ba60e758929809f85b5790/src/lib.rs>
    use core::alloc::{GlobalAlloc, Layout};

    use windows_kernel_sys::{ExAllocatePool2, ExFreePool, POOL_FLAG_NON_PAGED};

    const POOL_TAG: u32 = u32::from_ne_bytes(*b"tsuR");

    /// The global kernel allocator structure.
    pub struct KernelAlloc;

    unsafe impl GlobalAlloc for KernelAlloc {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            // Defer error handling to clients
            ExAllocatePool2(POOL_FLAG_NON_PAGED, layout.size() as u64, POOL_TAG).cast()
        }

        unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
            ExFreePool(ptr.cast());
        }
    }
}

pub mod log {
    //! Logging helpers

    // Reexport for convenience
    pub use log::*;

    use windows_kernel_sys::{
        DbgPrintEx, DPFLTR_ERROR_LEVEL, DPFLTR_INFO_LEVEL, DPFLTR_TRACE_LEVEL, DPFLTR_TYPE,
        DPFLTR_WARNING_LEVEL,
    };

    pub const COMPONENT_IHVBUS: u32 = DPFLTR_TYPE::DPFLTR_IHVBUS_ID.0 as u32;
    pub const COMPONENT_IHVAUDIO: u32 = DPFLTR_TYPE::DPFLTR_IHVAUDIO_ID.0 as u32;
    pub const COMPONENT_IHVVIDEO: u32 = DPFLTR_TYPE::DPFLTR_IHVVIDEO_ID.0 as u32;
    pub const COMPONENT_IHVDRIVER: u32 = DPFLTR_TYPE::DPFLTR_IHVDRIVER_ID.0 as u32;
    pub const COMPONENT_IHVNETWORK: u32 = DPFLTR_TYPE::DPFLTR_IHVNETWORK_ID.0 as u32;
    pub const COMPONENT_IHVSTREAMING: u32 = DPFLTR_TYPE::DPFLTR_IHVSTREAMING_ID.0 as u32;

    // Borrowed from the kernel_log crate, made to be more resilient to
    // allocation failures, as well as specifying component id
    pub struct KernelLogger<const COMPONENT_ID: u32>;

    // FIXME: Investigate using IoAllocateErrorLogEntry too
    // on log init failure
    #[doc(hidden)]
    pub fn report_init_fail<const COMPONENT_ID: u32>() {
        unsafe {
            DbgPrintEx(
                COMPONENT_ID,
                DPFLTR_ERROR_LEVEL,
                b"Failed to initialize kernel logger\0".as_ptr().cast(),
            )
        };
    }

    /// Initializes the kernel logging infrastructure, based off of `DbgPrintEx`
    #[macro_export(local_inner_macros)]
    macro_rules! init_kernel_logger {
        ($component_id:expr, $level:expr) => {{
            static LOGGER: $crate::log::KernelLogger<{ $component_id }> = $crate::log::KernelLogger;

            match log::set_logger(&LOGGER) {
                Ok(()) => $crate::log::set_max_level($level),
                Err(_) => $crate::log::report_init_fail::<{ $component_id }>(),
            }
        };};
    }

    impl<const COMPONENT_ID: u32> log::Log for KernelLogger<COMPONENT_ID> {
        fn enabled(&self, _metadata: &Metadata) -> bool {
            true
        }

        fn log(&self, record: &Record) {
            use core::fmt::Write;

            if self.enabled(record.metadata()) {
                // DbgPrint(Ex) buffer is specified to be at least 512 bytes long
                // per invocation
                let mut message_buf = heapless::String::<512>::new();

                let status = write!(
                    &mut message_buf,
                    "{:<5} [{}] {}\n\0",
                    record.level(),
                    record.target(),
                    record.args()
                );

                let real_level = match record.level() {
                    Level::Error => DPFLTR_ERROR_LEVEL,
                    Level::Warn => DPFLTR_WARNING_LEVEL,
                    Level::Info => DPFLTR_INFO_LEVEL,
                    // There isn't a debug level, so these get to share
                    Level::Debug | Level::Trace => DPFLTR_TRACE_LEVEL,
                };

                if status.is_ok() {
                    unsafe { DbgPrintEx(COMPONENT_ID, real_level, message_buf.as_ptr().cast()) };
                } else {
                    unsafe {
                        DbgPrintEx(
                            COMPONENT_ID,
                            DPFLTR_ERROR_LEVEL,
                            b"ERROR [windows_kernel_rs] overflow while formatting message buffer\n\0".as_ptr().cast(),
                        )
                    };

                    // Do a breakpoint so that we can still log some information
                    unsafe { windows_kernel_sys::DbgBreakPointWithStatus(0) };
                }
            }
        }

        fn flush(&self) {}
    }
}

pub use nt_string as string;

pub struct DriverObject(windows_kernel_sys::PDRIVER_OBJECT);

impl DriverObject {
    /// Creates a new `DriverObject` wrapper
    ///
    /// ## Safety
    ///
    /// Must only be called from the `DriverEntry` method
    #[must_use]
    pub unsafe fn new(driver_object: windows_kernel_sys::PDRIVER_OBJECT) -> Self {
        Self(driver_object)
    }

    /// Unwraps the driver object, yielding the original raw pointer
    #[must_use]
    pub fn into_raw(self) -> windows_kernel_sys::PDRIVER_OBJECT {
        self.0
    }
}

pub fn __handle_panic(info: &core::panic::PanicInfo) -> ! {
    // Closest bugcheck we can get that doesn't have any parameters
    const FATAL_UNHANDLED_HARD_ERROR: u32 = 0x4C;

    // Show panic message
    log::error!("{info}");

    // Try entering the debugger first...
    unsafe { windows_kernel_sys::DbgBreakPointWithStatus(0) };
    // Before causing a bugcheck

    // SAFETY: Just a matching FFI signature
    unsafe { windows_kernel_sys::KeBugCheck(FATAL_UNHANDLED_HARD_ERROR) }
}
