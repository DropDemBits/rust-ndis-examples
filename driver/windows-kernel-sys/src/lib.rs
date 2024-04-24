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

const fn size_of_pointee<T>(_: *const T) -> usize {
    core::mem::size_of::<T>()
}

/// Gets the size of a struct up to (and including) a given field
macro_rules! sizeof_through_field {
    ($ty:path, $field:ident) => {{
        let size_up_to = core::mem::offset_of!($ty, $field);

        // Project to the pointer type
        // No UB here, and the pointer does not dangle, either.
        // But we have to make sure that `uninit` lives long enough,
        // so it has to be in the same scope as `$name`. That's why
        // `let_base_ptr` declares a variable (several, actually)
        // instead of returning one.
        let uninit = core::mem::MaybeUninit::<$ty>::uninit();
        let base = uninit.as_ptr();
        // SAFETY: "$field" is in-bounds as we've already determined it to be
        // part of the struct.
        let ptr = unsafe { core::ptr::addr_of!((*(base)).$field) };
        let field_size = $crate::size_of_pointee(ptr);

        size_up_to + field_size
    }};
}

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

#[link(name = "wrapper_bindings", kind = "static")]
extern "C" {
    #[link_name = "wrapper_IoGetCurrentIrpStackLocation"]
    pub fn IoGetCurrentIrpStackLocation(Irp: PIRP) -> PIO_STACK_LOCATION;

    /// ## Safety
    ///
    /// This assumes that the current thread owns `Mdl`, and thus is not thread-safe.  
    #[link_name = "wrapper_MmGetSystemAddressForMdlSafe"]
    pub fn MmGetSystemAddressForMdlSafe(Mdl: PMDL, Priority: ULONG) -> PVOID;

    #[link_name = "wrapper_MmGetMdlByteCount"]
    pub fn MmGetMdlByteCount(Mdl: *const MDL) -> ULONG;

    #[link_name = "wrapper_MmGetMdlByteOffset"]
    pub fn MmGetMdlByteOffset(Mdl: *const MDL) -> ULONG;

    #[link_name = "wrapper_NdisGetNextMdl"]
    pub fn NdisGetNextMdl(CurrentMdl: PMDL, NextMdl: *mut PMDL) -> PMDL;

    #[link_name = "wrapper_NdisQueryMdl"]
    pub fn NdisQueryMdl(Mdl: PMDL, VirtualAddress: *mut PVOID, Length: *mut ULONG, Priority: ULONG);
}

#[inline]
pub unsafe fn IoCompleteRequest(Irp: PIRP, PriorityBoost: CCHAR) {
    unsafe { IofCompleteRequest(Irp, PriorityBoost) }
}

pub const NDIS_SIZEOF_DEVICE_OBJECT_ATTRIBUTES_REVISION_1: u16 =
    sizeof_through_field!(NDIS_DEVICE_OBJECT_ATTRIBUTES, DeviceClassGuid) as u16;
pub const NDIS_SIZEOF_LINK_STATE_REVISION_1: u16 =
    sizeof_through_field!(NDIS_LINK_STATE, AutoNegotiationFlags) as u16;
pub const NDIS_SIZEOF_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1: u16 = sizeof_through_field!(
    NDIS_MINIPORT_DRIVER_CHARACTERISTICS,
    CancelOidRequestHandler
) as u16;
pub const NDIS_SIZEOF_NET_BUFFER_LIST_POOL_PARAMETERS_REVISION_1: u16 =
    sizeof_through_field!(NET_BUFFER_LIST_POOL_PARAMETERS, DataSize) as u16;
pub const NDIS_SIZEOF_NDIS_OPEN_PARAMETERS_REVISION_1: u16 =
    sizeof_through_field!(NDIS_OPEN_PARAMETERS, FrameTypeArraySize) as u16;
pub const NDIS_SIZEOF_OID_REQUEST_REVISION_1: u16 =
    sizeof_through_field!(NDIS_OID_REQUEST, Reserved2) as u16;
pub const NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1: u16 = sizeof_through_field!(
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS,
    SendNetBufferListsCompleteHandler
) as u16;
pub const NDIS_SIZEOF_STATUS_INDICATION_REVISION_1: u16 =
    sizeof_through_field!(NDIS_STATUS_INDICATION, NdisReserved) as u16;
