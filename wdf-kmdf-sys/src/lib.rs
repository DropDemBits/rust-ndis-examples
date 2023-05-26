#![no_std]
#![allow(
    non_upper_case_globals,
    non_camel_case_types,
    non_snake_case,
    // it's all unsafe
    clippy::missing_safety_doc,
)]

#[allow(
    clippy::fn_to_numeric_cast_with_truncation,
    clippy::useless_transmute,
    clippy::upper_case_acronyms,
    clippy::too_many_arguments,
    clippy::pedantic
)]
mod bindings {
    use windows_kernel_sys::{
        ACCESS_MASK, BOOLEAN, CCHAR, CHAR, DEVICE_POWER_STATE, DEVICE_REGISTRY_PROPERTY,
        DEVICE_RELATION_TYPE, DMA_COMPLETION_STATUS, DMA_WIDTH, GROUP_AFFINITY, HANDLE,
        INTERFACE_TYPE, IO_STATUS_BLOCK, KINTERRUPT_MODE, KIRQL, KPROCESSOR_MODE, LARGE_INTEGER,
        LCID, LONG, LONGLONG, LPCSTR, NTSTATUS, PCCH, PCM_PARTIAL_RESOURCE_DESCRIPTOR,
        PCUNICODE_STRING, PCWSTR, PDEVICE_OBJECT, PDMA_ADAPTER, PDRIVER_OBJECT, PEPROCESS,
        PFILE_OBJECT, PHYSICAL_ADDRESS, PINTERFACE, PIO_RESOURCE_DESCRIPTOR, PIO_SECURITY_CONTEXT,
        PIO_STACK_LOCATION, PIRP, PKDPC, PKINTERRUPT, PKTHREAD, PLONGLONG, PMDL, POHANDLE,
        POOL_TYPE, POWER_ACTION, PPNP_BUS_INFORMATION, PPO_FX_COMPONENT,
        PPO_FX_COMPONENT_ACTIVE_CONDITION_CALLBACK, PPO_FX_COMPONENT_IDLE_CONDITION_CALLBACK,
        PPO_FX_COMPONENT_IDLE_STATE_CALLBACK, PPO_FX_POWER_CONTROL_CALLBACK, PSCATTER_GATHER_LIST,
        PSTR, PUCHAR, PULONG, PUNICODE_STRING, PUSHORT, PVOID, PWSTR, SYSTEM_POWER_STATE, UCHAR,
        ULONG, ULONGLONG, UNICODE_STRING, USHORT, WCHAR, _EXCEPTION_RECORD,
    };

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub use bindings::*;

#[macro_export]
macro_rules! WDF_NO_OBJECT_ATTRIBUTES {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_EVENT_CALLBACK {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_HANDLE {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_CONTEXT {
    () => {
        ::core::ptr::null_mut()
    };
}
#[macro_export]
macro_rules! WDF_NO_SEND_OPTIONS {
    () => {
        ::core::ptr::null_mut()
    };
}

// Right now, we don't handle struct versioning, so it's just the struct's size.
// Should probably be a `Result<u32, Error>`
#[allow(clippy::cast_possible_truncation)] // all sizes are less than u32::MAX
fn WDF_STRUCTURE_SIZE<T: Sized>() -> u32 {
    core::mem::size_of::<T>() as u32
}

impl WDF_OBJECT_ATTRIBUTES {
    /// Initializes the [`WDF_OBJECT_ATTRIBUTES`] structure
    ///
    /// Sets
    /// - `ExecutionLevel` to `WdfSynchronizationScopeInheritFromParent`
    /// - `SynchronizationScope` to `WdfSynchronizationScopeInheritFromParent`
    #[must_use]
    pub fn init() -> Self {
        // SAFETY: All fields are zero-able
        let mut attributes: Self = unsafe { core::mem::zeroed() };

        attributes.Size = WDF_STRUCTURE_SIZE::<WDF_OBJECT_ATTRIBUTES>();
        attributes.SynchronizationScope =
            _WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;
        attributes.ExecutionLevel = _WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;

        attributes
    }
}

impl WDF_DRIVER_CONFIG {
    /// Initializes the [`WDF_DRIVER_CONFIG`] structure
    #[must_use]
    pub fn init(EvtDriverDeviceAdd: PFN_WDF_DRIVER_DEVICE_ADD) -> Self {
        // SAFETY: All fields are zero-able
        let mut config: Self = unsafe { core::mem::zeroed() };

        config.Size = WDF_STRUCTURE_SIZE::<Self>();

        config.EvtDriverDeviceAdd = EvtDriverDeviceAdd;

        config
    }
}

impl WDF_FILEOBJECT_CONFIG {
    /// Initializes the [`WDF_FILEOBJECT_CONFIG`] structure
    ///
    /// Sets:
    /// - Size
    /// - Specified callback function pointers
    /// - `FileObjectClass` to [`_WDF_FILEOBJECT_CLASS::WdfFileObjectWdfCannotUseFsContexts`]
    /// - `AutoForwardCleanupClose` to [`_WDF_TRI_STATE::WdfUseDefault`]
    #[must_use]
    pub fn init(
        EvtDeviceFileCreate: PFN_WDF_DEVICE_FILE_CREATE, // in, optional
        EvtFileClose: PFN_WDF_FILE_CLOSE,                // in, optional
        EvtFileCleanup: PFN_WDF_FILE_CLEANUP,            // in, optional
    ) -> Self {
        // SAFETY: All fields are zero-able
        let mut config: Self = unsafe { core::mem::zeroed() };

        config.Size = WDF_STRUCTURE_SIZE::<Self>();

        config.EvtDeviceFileCreate = EvtDeviceFileCreate;
        config.EvtFileClose = EvtFileClose;
        config.EvtFileCleanup = EvtFileCleanup;

        config.FileObjectClass = _WDF_FILEOBJECT_CLASS::WdfFileObjectWdfCannotUseFsContexts;
        config.AutoForwardCleanupClose = _WDF_TRI_STATE::WdfUseDefault;

        config
    }
}
