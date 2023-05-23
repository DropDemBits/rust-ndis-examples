#![no_std]

pub mod alloc {
    //! Global kernel allocators
    //! Modified from <https://github.com/not-matthias/kernel-alloc-rs/blob/19b2b992c0f0dacf60ba60e758929809f85b5790/src/lib.rs>
    use core::alloc::{GlobalAlloc, Layout};

    use windows_kernel_sys::{ExAllocatePool2, ExFreePool, POOL_FLAG_NON_PAGED};

    extern crate alloc;

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

pub mod string {
    //! Helpers for working with Unicode Strings

    use core::marker::PhantomData;

    use windows_kernel_sys::UNICODE_STRING;

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
        /// - Also, the original backing memory in should not be modified
        pub unsafe fn into_raw(&self) -> UNICODE_STRING {
            self.0
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
