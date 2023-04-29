#![no_std]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    // it's all unsafe
    clippy::missing_safety_doc,
)]

mod bindings;

pub use bindings::*;
pub use windows_sys as sys;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Error(pub NTSTATUS);

impl Error {
    /// Converts an [`NTSTATUS`] into an `Error`.
    ///
    /// Note that this treats anything that isn't `STATUS_SUCCESSFUL` as an error
    pub fn to_err(status: NTSTATUS) -> Result<(), Error> {
        if status == 0 {
            Ok(())
        } else {
            Err(Error(status))
        }
    }
}
