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
            ExAllocatePool2(POOL_FLAG_NON_PAGED, layout.size() as u64, POOL_TAG) as _
        }

        unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
            ExFreePool(ptr as _);
        }
    }
}

pub mod log {
    //! Logging helpers

    // Reexport for convenience
    pub use log::*;

    use windows_kernel_sys::{
        DbgPrintEx, DPFLTR_ERROR_LEVEL, DPFLTR_INFO_LEVEL, DPFLTR_TRACE_LEVEL,
        DPFLTR_WARNING_LEVEL, _DPFLTR_TYPE,
    };

    pub const COMPONENT_IHVBUS: u32 = _DPFLTR_TYPE::DPFLTR_IHVBUS_ID as u32;
    pub const COMPONENT_IHVAUDIO: u32 = _DPFLTR_TYPE::DPFLTR_IHVAUDIO_ID as u32;
    pub const COMPONENT_IHVVIDEO: u32 = _DPFLTR_TYPE::DPFLTR_IHVVIDEO_ID as u32;
    pub const COMPONENT_IHVDRIVER: u32 = _DPFLTR_TYPE::DPFLTR_IHVDRIVER_ID as u32;
    pub const COMPONENT_IHVNETWORK: u32 = _DPFLTR_TYPE::DPFLTR_IHVNETWORK_ID as u32;
    pub const COMPONENT_IHVSTREAMING: u32 = _DPFLTR_TYPE::DPFLTR_IHVSTREAMING_ID as u32;

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
                b"Failed to initialize kernel logger\0".as_ptr() as _,
            )
        };
    }

    /// Initializes the kernel logging infrastructure, based off of DbgPrintEx
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
                    Level::Debug => DPFLTR_TRACE_LEVEL,
                    Level::Trace => DPFLTR_TRACE_LEVEL,
                };

                match status {
                    Ok(_) => {
                        unsafe { DbgPrintEx(COMPONENT_ID, real_level, message_buf.as_ptr() as _) };
                    }
                    Err(_) => {
                        unsafe {
                            DbgPrintEx(
                                COMPONENT_ID,
                                DPFLTR_ERROR_LEVEL,
                                b"ERROR [windows_kernel_rs] overflow while formatting message buffer\n\0".as_ptr() as _,
                            )
                        };

                        // Do a breakpoint so that we can still log some information
                        unsafe { windows_kernel_sys::DbgBreakPointWithStatus(0) };
                    }
                }
            }
        }

        fn flush(&self) {}
    }
}

pub mod string {
    //! Helpers for working with Unicode Strings

    use core::marker::PhantomData;

    use windows_kernel_sys::{PCUNICODE_STRING, UNICODE_STRING};

    pub use widestring::*;

    /// An **immutable** Win32-compatible unicode string, with a lifetime attached
    #[derive(Clone, Copy)]
    pub struct UnicodeString<'a>(UNICODE_STRING, PhantomData<&'a [u16]>);

    unsafe impl<'a> Send for UnicodeString<'a> {}
    unsafe impl<'a> Sync for UnicodeString<'a> {}

    impl UnicodeString<'_> {
        /// Yields the underlying raw [`UNICODE_STRING`]
        ///
        /// ## Safety
        ///
        /// - The yielded [`UNICODE_STRING`] must not outlive the original
        ///   `UnicodeString` that it was yielded from.
        /// - The original backing memory in should not be modified
        pub unsafe fn into_raw(&self) -> UNICODE_STRING {
            self.0
        }

        /// Creates a wrapper around a raw [`UNICODE_STRING`]
        ///
        /// ## Safety
        ///
        /// - The yielded `UnicodeString` must not outlive the original
        ///   [`UNICODE_STRING`] that it was yielded from.
        /// - The original backing memory in should not be modified
        pub unsafe fn from_raw(raw: UNICODE_STRING) -> Self {
            Self(raw, PhantomData)
        }

        /// Yields the underlying raw [`UNICODE_STRING`] as a pointer
        ///
        /// ## Safety
        ///
        /// - The yielded [`PCUNICODE_STRING`] must not outlive the original
        ///   `UnicodeString` that it was yielded from.
        /// - The original backing memory in should not be modified
        pub unsafe fn as_raw_ptr(&self) -> PCUNICODE_STRING {
            &self.0
        }
    }

    /// A **mutable** Win32-compatible unicode string, with a lifetime attached
    #[derive(Clone, Copy)]
    pub struct UnicodeStringMut<'a>(UNICODE_STRING, PhantomData<&'a mut [u16]>);

    unsafe impl<'a> Send for UnicodeStringMut<'a> {}

    impl UnicodeStringMut<'_> {
        /// Creates an immutable version of a unicode string
        pub fn as_ref(&self) -> UnicodeString<'_> {
            UnicodeString(self.0, PhantomData)
        }

        /// Yields the underlying raw [`UNICODE_STRING`]
        ///
        /// ## Safety
        ///
        /// - The yielded [`UNICODE_STRING`] must not outlive the original
        ///   `UnicodeString` that it was yielded from.
        pub unsafe fn into_raw(&mut self) -> UNICODE_STRING {
            self.0
        }
    }

    /// Helper extension trait for converting `U16CStr{ing}` and `U16Str{ing}`
    /// into the Win32-compatible [`UnicodeString`]
    pub trait IntoUnicodeString {
        /// Tries to convert into a [`UnicodeString`], or None if it wasn't successful
        fn into_unicode_string(&self) -> Option<UnicodeString<'_>>;
    }

    impl IntoUnicodeString for U16CStr {
        fn into_unicode_string(&self) -> Option<UnicodeString<'_>> {
            // Length is expected to be in bytes
            let len = self
                .len()
                .checked_mul(2)
                .and_then(|it| u16::try_from(it).ok())?;
            // Note: Lengths do not include the trailing nul terminator
            let len = len.checked_sub(2)?;

            Some(UnicodeString(
                UNICODE_STRING {
                    Length: len,
                    MaximumLength: len,
                    Buffer: self.as_ptr().cast_mut(),
                },
                PhantomData,
            ))
        }
    }

    impl IntoUnicodeString for U16CString {
        fn into_unicode_string(&self) -> Option<UnicodeString<'_>> {
            self.as_ucstr().into_unicode_string()
        }
    }

    impl IntoUnicodeString for Utf16Str {
        fn into_unicode_string(&self) -> Option<UnicodeString<'_>> {
            // Length is expected to be in bytes
            let len = self
                .len()
                .checked_mul(2)
                .and_then(|it| u16::try_from(it).ok())?;
            // Note: There is no trailing nul terminator to exclude

            Some(UnicodeString(
                UNICODE_STRING {
                    Length: len,
                    MaximumLength: len,
                    Buffer: self.as_ptr().cast_mut(),
                },
                PhantomData,
            ))
        }
    }

    impl IntoUnicodeString for Utf16String {
        fn into_unicode_string(&self) -> Option<UnicodeString<'_>> {
            self.as_utfstr().into_unicode_string()
        }
    }

    /// Helper extension trait for converting `U16CString` and `U16String`
    /// into the Win32-compatible [`UnicodeStringMut`]
    pub trait IntoUnicodeStringMut {
        /// Tries to convert into a [`UnicodeStringMut`], or None if it wasn't successful
        fn into_unicode_string_mut(&mut self) -> Option<UnicodeStringMut<'_>>;
    }

    impl IntoUnicodeStringMut for U16CString {
        fn into_unicode_string_mut(&mut self) -> Option<UnicodeStringMut<'_>> {
            // Length is expected to be in bytes
            let len = self
                .len()
                .checked_mul(2)
                .and_then(|it| u16::try_from(it).ok())?;
            // Note: Lengths do not include the trailing nul terminator
            let len = len.checked_sub(2)?;

            Some(UnicodeStringMut(
                UNICODE_STRING {
                    Length: len,
                    MaximumLength: len,
                    Buffer: self.as_mut_ptr(),
                },
                PhantomData,
            ))
        }
    }

    impl IntoUnicodeStringMut for Utf16String {
        fn into_unicode_string_mut(&mut self) -> Option<UnicodeStringMut<'_>> {
            // Length is expected to be in bytes
            let len = self
                .len()
                .checked_mul(2)
                .and_then(|it| u16::try_from(it).ok())?;
            // Note: There is no trailing nul terminator to exclude

            Some(UnicodeStringMut(
                UNICODE_STRING {
                    Length: len,
                    MaximumLength: len,
                    Buffer: self.as_mut_ptr(),
                },
                PhantomData,
            ))
        }
    }
}

pub struct DriverObject(windows_kernel_sys::PDRIVER_OBJECT);

impl DriverObject {
    /// Creates a new `DriverObject` wrapper
    ///
    /// ## Safety
    ///
    /// Must only be called from the `DriverEntry` method
    pub unsafe fn new(driver_object: windows_kernel_sys::PDRIVER_OBJECT) -> Self {
        Self(driver_object)
    }

    /// Unwraps the driver object, yielding the original raw pointer
    pub fn into_raw(self) -> windows_kernel_sys::PDRIVER_OBJECT {
        self.0
    }
}

pub fn __handle_panic(info: &core::panic::PanicInfo) -> ! {
    // Show panic message
    log::error!("{info}");

    // Try entering the debugger first...
    unsafe { windows_kernel_sys::DbgBreakPointWithStatus(0) };
    // Before causing a bugcheck

    // Closest bugcheck we can get that doesn't have any parameters
    const FATAL_UNHANDLED_HARD_ERROR: u32 = 0x4C;
    // SAFETY: Just a matching FFI signature
    unsafe { windows_kernel_sys::KeBugCheck(FATAL_UNHANDLED_HARD_ERROR) }
}
