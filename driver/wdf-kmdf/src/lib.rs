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

pub mod raw;

pub mod device;
pub mod driver;
pub mod file_object;
pub mod handle;
pub mod object;
pub mod request;
pub mod sync;
