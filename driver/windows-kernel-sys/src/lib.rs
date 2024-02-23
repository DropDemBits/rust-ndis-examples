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
    clippy::pedantic
)]
mod bindings;

pub use bindings::*;
pub use winresult as result;

use winresult::NtStatus;

#[link(name = "ntoskrnl")]
extern "C" {}
// for __security_cookie
#[link(name = "bufferoverflowfastfailk")]
extern "C" {}
#[link(name = "wdmsec")]
extern "C" {}
#[link(name = "ndis")]
extern "C" {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Error(pub NtStatus);

impl Error {
    /// Converts an [`NTSTATUS`] into an `Error`.
    ///
    /// Note that this treats anything that isn't `STATUS_SUCCESS` as an error
    pub fn to_err(status: NTSTATUS) -> Result<(), Error> {
        if status == winresult::STATUS::SUCCESS.to_u32() {
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

impl From<NtStatus> for Error {
    fn from(err: NtStatus) -> Self {
        Self(err)
    }
}

impl From<core::convert::Infallible> for Error {
    fn from(value: core::convert::Infallible) -> Self {
        match value {}
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

pub const NDIS_DEFAULT_PORT_NUMBER: NDIS_PORT_NUMBER = 0;

impl NET_BUFFER_LIST {
    pub unsafe fn first_nb(Nbl: PNET_BUFFER_LIST) -> PNET_BUFFER {
        unsafe { NET_BUFFER_LIST_FirstNb(Nbl) }
    }

    pub unsafe fn next_nbl(Nbl: PNET_BUFFER_LIST) -> PNET_BUFFER_LIST {
        unsafe { NET_BUFFER_LIST_NextNbl(Nbl) }
    }

    pub unsafe fn next_nbl_mut<'a>(Nbl: PNET_BUFFER_LIST) -> &'a mut PNET_BUFFER_LIST {
        // Have to directly access the field this time, unless we generate
        // another wrapper function returning the address to the field
        unsafe { &mut (*Nbl).__bindgen_anon_1.__bindgen_anon_1.Next }
    }

    pub unsafe fn info(Nbl: PNET_BUFFER_LIST, Id: NDIS_NET_BUFFER_LIST_INFO) -> *mut PVOID {
        unsafe { core::ptr::addr_of_mut!((*Nbl).NetBufferListInfo[Id.0 as usize]) }
    }

    pub unsafe fn set_cancel_id(Nbl: PNET_BUFFER_LIST, CancelId: usize) {
        unsafe {
            *Self::info(Nbl, NDIS_NET_BUFFER_LIST_INFO::NetBufferListCancelId) =
                core::mem::transmute(CancelId);
        }
    }

    pub unsafe fn context_data_start(Nbl: PNET_BUFFER_LIST) -> PUCHAR {
        let nbl_context = unsafe { (*Nbl).Context };
        let context_data = unsafe {
            nbl_context
                .byte_add(core::mem::offset_of!(NET_BUFFER_LIST_CONTEXT, ContextData))
                .cast::<UCHAR>()
        };
        let offset = usize::from(unsafe { (*nbl_context).Offset });
        unsafe { context_data.add(offset) }
    }

    pub unsafe fn status(Nbl: PNET_BUFFER_LIST) -> NTSTATUS {
        unsafe { *(*Nbl).__bindgen_anon_2.Status.as_ref() }
    }

    pub unsafe fn test_flag(Nbl: PNET_BUFFER_LIST, Flag: u32) -> bool {
        unsafe { (*Nbl).Flags & Flag != 0 }
    }

    pub unsafe fn set_flag(Nbl: PNET_BUFFER_LIST, Flag: u32) {
        unsafe { (*Nbl).Flags |= Flag }
    }

    pub unsafe fn clear_flag(Nbl: PNET_BUFFER_LIST, Flag: u32) {
        unsafe { (*Nbl).Flags &= !Flag }
    }
}

#[link(name = "wrapper_bindings")]
extern "C" {
    pub fn wrapper_MmGetSystemAddressForMdlSafe(Mdl: PMDL, Priority: ULONG) -> PVOID;

    pub fn wrapper_MmGetMdlByteCount(Mdl: PMDL) -> ULONG;

    pub fn wrapper_NET_BUFFER_LIST_FirstNb(Nbl: PNET_BUFFER_LIST) -> PNET_BUFFER;

    pub fn wrapper_NET_BUFFER_LIST_NextNbl(Nbl: PNET_BUFFER_LIST) -> PNET_BUFFER_LIST;

    pub fn wrapper_NdisGetNextMdl(CurrentMdl: PMDL, NextMdl: *mut PMDL) -> PMDL;

    pub fn wrapper_NdisQueryMdl(
        Mdl: PMDL,
        VirtualAddress: *mut PVOID,
        Length: *mut ULONG,
        Priority: ULONG,
    );
}

pub use self::wrapper_MmGetMdlByteCount as MmGetMdlByteCount;
pub use self::wrapper_MmGetSystemAddressForMdlSafe as MmGetSystemAddressForMdlSafe;
pub use self::wrapper_NET_BUFFER_LIST_FirstNb as NET_BUFFER_LIST_FirstNb;
pub use self::wrapper_NET_BUFFER_LIST_NextNbl as NET_BUFFER_LIST_NextNbl;
pub use self::wrapper_NdisGetNextMdl as NdisGetNextMdl;
pub use self::wrapper_NdisQueryMdl as NdisQueryMdl;
