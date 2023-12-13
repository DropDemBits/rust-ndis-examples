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
        DEVICE_RELATION_TYPE, DEVPROPKEY, DEVPROPTYPE, DMA_COMPLETION_STATUS, DMA_WIDTH,
        GROUP_AFFINITY, GUID, HANDLE, INTERFACE_TYPE, IO_STATUS_BLOCK, KAFFINITY, KINTERRUPT_MODE,
        KIRQL, KPROCESSOR_MODE, LCID, LONG, LONGLONG, LPCGUID, LPCSTR, LPGUID, NTSTATUS, PCCH,
        PCM_PARTIAL_RESOURCE_DESCRIPTOR, PCUNICODE_STRING, PCWSTR, PDEVICE_OBJECT, PDEVPROPTYPE,
        PDMA_ADAPTER, PDRIVER_OBJECT, PEPROCESS, PFILE_OBJECT, PHYSICAL_ADDRESS, PINTERFACE,
        PIO_RESOURCE_DESCRIPTOR, PIO_SECURITY_CONTEXT, PIO_STACK_LOCATION, PIRP, PKDPC,
        PKINTERRUPT, PKTHREAD, PLONGLONG, PMDL, POHANDLE, POOL_TYPE, POWER_ACTION,
        PPNP_BUS_INFORMATION, PPO_FX_COMPONENT, PPO_FX_COMPONENT_ACTIVE_CONDITION_CALLBACK,
        PPO_FX_COMPONENT_IDLE_CONDITION_CALLBACK, PPO_FX_COMPONENT_IDLE_STATE_CALLBACK,
        PPO_FX_POWER_CONTROL_CALLBACK, PSCATTER_GATHER_LIST, PSTR, PUCHAR, PULONG, PULONG_PTR,
        PUNICODE_STRING, PUSHORT, PVOID, PWSTR, SYSTEM_POWER_STATE, UCHAR, ULONG, ULONG64,
        ULONGLONG, ULONG_PTR, UNICODE_STRING, USHORT,
    };

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub use bindings::*;
use windows_kernel_sys::ULONG;

// We also define `WdfMinimumVersionRequired` as a static since bindgen
// interprets the original `WdfMinimumVersionRequired` as a constant.
//
// We also don't explicitly set the minimum version and instead use whatever
// we got from bindgen so that we only have to change the minimum minor version
// from one place (useful for if we allow changing the minimum version), as well
// as not having to remake the logic for choosing the minimum version.
#[allow(hidden_glob_reexports)] // only used during WDF loading
#[no_mangle]
static WdfMinimumVersionRequired: ULONG = bindings::WdfMinimumVersionRequired;

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
macro_rules! WDF_STRUCTURE_SIZE {
    ($name:ty) => {
        ::core::mem::size_of::<$name>() as u32
    };
}

impl WDF_OBJECT_ATTRIBUTES {
    /// Initializes the [`WDF_OBJECT_ATTRIBUTES`] structure
    ///
    /// Sets
    /// - `ExecutionLevel` to [`WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent`]
    /// - `SynchronizationScope` to [`WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent`]
    #[must_use]
    pub fn init() -> Self {
        // SAFETY: All fields are zero-able
        let mut attributes: Self = unsafe { core::mem::zeroed() };

        attributes.Size = WDF_STRUCTURE_SIZE!(WDF_OBJECT_ATTRIBUTES);
        attributes.SynchronizationScope =
            WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent;
        attributes.ExecutionLevel = WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent;

        attributes
    }
}

impl WDF_DRIVER_CONFIG {
    /// Initializes the [`WDF_DRIVER_CONFIG`] structure
    #[must_use]
    pub fn init(EvtDriverDeviceAdd: PFN_WDF_DRIVER_DEVICE_ADD) -> Self {
        // SAFETY: All fields are zero-able
        let mut config: Self = unsafe { core::mem::zeroed() };

        config.Size = WDF_STRUCTURE_SIZE!(WDF_DRIVER_CONFIG);

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
    /// - `FileObjectClass` to [`WDF_FILEOBJECT_CLASS::WdfFileObjectWdfCannotUseFsContexts`]
    /// - `AutoForwardCleanupClose` to [`WDF_TRI_STATE::WdfUseDefault`]
    #[must_use]
    pub fn init(
        EvtDeviceFileCreate: PFN_WDF_DEVICE_FILE_CREATE, // in, optional
        EvtFileClose: PFN_WDF_FILE_CLOSE,                // in, optional
        EvtFileCleanup: PFN_WDF_FILE_CLEANUP,            // in, optional
    ) -> Self {
        // SAFETY: All fields are zero-able
        let mut config: Self = unsafe { core::mem::zeroed() };

        config.Size = WDF_STRUCTURE_SIZE!(WDF_FILEOBJECT_CONFIG);

        config.EvtDeviceFileCreate = EvtDeviceFileCreate;
        config.EvtFileClose = EvtFileClose;
        config.EvtFileCleanup = EvtFileCleanup;

        config.FileObjectClass = WDF_FILEOBJECT_CLASS::WdfFileObjectWdfCannotUseFsContexts;
        config.AutoForwardCleanupClose = WDF_TRI_STATE::WdfUseDefault;

        config
    }
}

impl WDF_IO_QUEUE_CONFIG {
    /// Initializes the [`WDF_IO_QUEUE_CONFIG`] structure
    ///
    /// Sets:
    /// - Size
    /// - PowerManaged to [`WDF_TRI_STATE::WdfUseDefault`]
    ///
    /// If `DispatchType` is [`WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchParallel`],
    /// `NumberOfPresentedRequests` is set to [`ULONG::MAX`] to indicate that unlimited
    /// IO requests can be sent
    #[must_use]
    pub fn init(DispatchType: WDF_IO_QUEUE_DISPATCH_TYPE) -> Self {
        // SAFETY: All fields are zero-able
        let mut config: Self = unsafe { core::mem::zeroed() };

        config.Size = WDF_STRUCTURE_SIZE!(WDF_IO_QUEUE_CONFIG);
        config.PowerManaged = WDF_TRI_STATE::WdfUseDefault;
        config.DispatchType = DispatchType;

        if config.DispatchType == WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchParallel {
            // SAFETY: Is aligned, and the only union structure member
            let settings = unsafe { config.Settings.Parallel.as_mut() };

            settings.NumberOfPresentedRequests = ULONG::MAX;
        }

        config
    }

    /// Initializes the [`WDF_IO_QUEUE_CONFIG`] structure for the default queue
    ///
    /// Built on [`WDF_IO_QUEUE_CONFIG::init`]
    #[must_use]
    pub fn init_default_queue(DispatchType: WDF_IO_QUEUE_DISPATCH_TYPE) -> Self {
        let mut config = Self::init(DispatchType);

        config.DefaultQueue = true as u8;

        config
    }
}
