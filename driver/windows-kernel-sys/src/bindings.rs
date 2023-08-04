include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub type NTSTATUS = u32;
pub type PNTSTATUS = *mut NTSTATUS;
pub type PCNTSTATUS = *const NTSTATUS;

pub type NDIS_STATUS = u32;
pub type PNDIS_STATUS = *mut NDIS_STATUS;
