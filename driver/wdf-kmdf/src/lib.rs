//! Rust KMDF Abstractions
#![no_std]
#![deny(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::undocumented_unsafe_blocks
)]

#[macro_export]
#[doc(hidden)]
macro_rules! cstr {
    ($str:expr) => {{
        match ::core::ffi::CStr::from_bytes_with_nul(concat!($str, "\0").as_bytes()) {
            Ok(it) => it,
            Err(_) => panic!("unreachable code: always concat a nul terminator at the end"),
        }
    }};
}

/// Clones a WDF handle, providing location and optional tagging information
#[macro_export]
macro_rules! clone {
    ($handle:expr) => {
        $crate::handle::CloneRef::clone_ref(&$handle, $crate::cstr!(file!()).as_ptr(), line!())
    };

    (ref $handle:expr) => {
        $crate::handle::CloneRef::clone_ref_untagged(
            &$handle,
            $crate::cstr!(file!()).as_ptr(),
            line!(),
        )
    };

    (tag:$tag:literal, $handle:expr) => {
        $crate::handle::CloneRef::clone_ref::<{ u32::from_le_bytes(*$tag) as usize }>(
            &$handle,
            $crate::cstr!(file!()).as_ptr(),
            line!(),
        )
    };
}

#[macro_export]
macro_rules! tag {
    ($tag:literal) => {
        u32::from_le_bytes(*$tag) as usize
    };
}

pub mod context_space;
pub mod raw;

pub mod collection;
pub mod device;
pub mod driver;
pub mod file_object;
pub mod handle;
pub mod io_queue;
pub mod miniport;
pub mod object;
pub mod request;
pub mod sync;
