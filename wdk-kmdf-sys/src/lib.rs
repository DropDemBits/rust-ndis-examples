#![no_std]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    // it's all unsafe
    clippy::missing_safety_doc,
)]

pub use core::ffi::*;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
