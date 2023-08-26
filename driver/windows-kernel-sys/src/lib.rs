#![no_std]
#![feature(allocator_api)]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    // it's all unsafe
    clippy::missing_safety_doc,
)]

#[allow(
    clippy::useless_transmute,
    clippy::too_many_arguments,
    clippy::unnecessary_cast,
    clippy::pedantic,
    // fixed by rust-lang/rust-bindgen#2583 but it's not part of a release yet
    clippy::incorrect_clone_impl_on_copy_type
)]
mod bindings;

pub use bindings::*;
pub use winresult as result;

use winresult::NtStatus;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Error(pub NtStatus);

impl Error {
    /// Converts an [`NTSTATUS`] into an `Error`.
    ///
    /// Note that this treats anything that isn't `STATUS_SUCCESSFUL` as an error
    pub fn to_err(status: NTSTATUS) -> Result<(), Error> {
        if status == 0 {
            Ok(())
        } else {
            Err(Error(NtStatus::from(status)))
        }
    }
}

impl From<core::alloc::AllocError> for Error {
    fn from(_: core::alloc::AllocError) -> Self {
        Self(result::STATUS::INSUFFICIENT_RESOURCES)
    }
}

pub const POOL_FLAG_REQUIRED_START: POOL_FLAGS = 0x0000000000000001_u64;
/// Charge quota
pub const POOL_FLAG_USE_QUOTA: POOL_FLAGS = 0x0000000000000001_u64;
/// Don't zero-initialize allocation
pub const POOL_FLAG_UNINITIALIZED: POOL_FLAGS = 0x0000000000000002_u64;
/// Use session specific pool
pub const POOL_FLAG_SESSION: POOL_FLAGS = 0x0000000000000004_u64;
/// Cache aligned allocation
pub const POOL_FLAG_CACHE_ALIGNED: POOL_FLAGS = 0x0000000000000008_u64;
/// Reserved for system use
pub const POOL_FLAG_RESERVED1: POOL_FLAGS = 0x0000000000000010_u64;
// Raise exception on failure
pub const POOL_FLAG_RAISE_ON_FAILURE: POOL_FLAGS = 0x0000000000000020_u64;
/// Non paged pool NX
pub const POOL_FLAG_NON_PAGED: POOL_FLAGS = 0x0000000000000040_u64;
/// Non paged pool executable
pub const POOL_FLAG_NON_PAGED_EXECUTE: POOL_FLAGS = 0x0000000000000080_u64;
/// Paged pool
pub const POOL_FLAG_PAGED: POOL_FLAGS = 0x0000000000000100_u64;
/// Reserved for system use
pub const POOL_FLAG_RESERVED2: POOL_FLAGS = 0x0000000000000200_u64;
/// Reserved for system use
pub const POOL_FLAG_RESERVED3: POOL_FLAGS = 0x0000000000000400_u64;
pub const POOL_FLAG_REQUIRED_END: POOL_FLAGS = 0x0000000080000000_u64;

pub const POOL_FLAG_OPTIONAL_START: POOL_FLAGS = 0x0000000100000000_u64;
/// Make special pool allocation
pub const POOL_FLAG_SPECIAL_POOL: POOL_FLAGS = 0x0000000100000000_u64;
pub const POOL_FLAG_OPTIONAL_END: POOL_FLAGS = 0x8000000000000000_u64;
