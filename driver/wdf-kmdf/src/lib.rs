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

// FIXME: Use a `CloneRef` trait to handle both `Ref` and non-`Ref` handles
#[macro_export]
macro_rules! clone {
    ($handle:expr) => {
        $crate::handle::Ref::clone_from_handle_location(
            &$handle,
            $crate::cstr!(file!()).as_ptr(),
            line!(),
        )
    };

    (ref $handle:expr) => {
        $crate::handle::Ref::clone_ref_location(&$handle, $crate::cstr!(file!()).as_ptr(), line!())
    };

    (tag:$tag:literal, $handle:expr) => {
        $crate::handle::Ref::clone_from_handle_location_tag(
            &$handle,
            $crate::cstr!(file!()).as_ptr(),
            line!(),
            u32::from_le_bytes(*$tag) as usize,
        )
    };

    (tag:$tag:literal, ref $handle:expr) => {
        $crate::handle::Ref::clone_ref_location_tag(
            &$handle,
            $crate::cstr!(file!()).as_ptr(),
            line!(),
            u32::from_le_bytes(*$tag) as usize,
        )
    };
}

pub mod context_space;
pub mod raw;

pub mod device;
pub mod driver;
pub mod file_object;
pub mod handle;
pub mod io_queue;
pub mod miniport;
pub mod object;
pub mod request;
pub mod sync;
