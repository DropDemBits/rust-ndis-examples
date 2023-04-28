//! Rust KMDF Abstractions
#![no_std]
#![feature(const_cstr_methods, const_trait_impl)]

pub mod raw {
    //! Raw bindings to KMDF functions
    #![allow(non_snake_case)] // Preserving the names of the original WDF functions

    use wdf_kmdf_sys::{
        PWDFDEVICE_INIT, PWDF_DRIVER_CONFIG, PWDF_OBJECT_ATTRIBUTES, WDFDEVICE, WDFDRIVER,
        WDF_NO_HANDLE, WDF_NO_OBJECT_ATTRIBUTES,
    };
    use windows_kernel_sys::{NTSTATUS, PCUNICODE_STRING, PDRIVER_OBJECT};

    /// Which function table to use
    macro_rules! function_table {
        () => {
            wdf_kmdf_sys::WdfFunctions_01031
        };
    }

    /// Dispatches to an always-available WDF Function
    macro_rules! dispatch {
        ($name:ident ( $($args:expr),* $(,)? )) => {{
            let fn_handle = {paste::paste! {
                const FN_INDEX: usize = wdf_kmdf_sys::_WDFFUNCENUM::[<$name TableIndex>] as usize;

                // Must be in the always-available function category
                static_assertions::const_assert!(FN_INDEX < wdf_kmdf_sys::WDF_ALWAYS_AVAILABLE_FUNCTION_COUNT as usize);

                let fn_handle = function_table!()
                    .add(FN_INDEX)
                    .cast::<wdf_kmdf_sys::[<PFN_ $name:upper>]>();

                // SAFETY: Ensured that this is present by the static assert
                unsafe { fn_handle.read().unwrap_unchecked() }
            }};

            // SAFETY: It is up to the caller to pass unsafety
            fn_handle(wdf_kmdf_sys::WdfDriverGlobals, $($args),*)
        }};
    }

    // region: wdfdevice

    /// Creates a framework device object
    ///
    /// The device object created could be either:
    /// - a function device object (FDO) if the [`WDFDEVICE_INIT`] structure was originally from a [`EvtDriverDeviceAdd`] callback, or
    /// - a physical device object (PDO) if the [`WDFDEVICE_INIT`] structure was originally from a [`EvtChildListCreateDevice`] or [`WdfPdoInitAllocate`]
    ///
    /// On successful creation, the place passed in `DeviceInit` is set to `NULL` (i.e. it steals ownership of the [`WDFDEVICE_INIT`] structure).
    ///
    /// If `DriverAttributes` is specified, the `ParentObject` must be `NULL`.
    /// If any of [`EvtCleanupCallback`] and/or [`EvtDestroyCallback`] are specified, note that they will be called at IRQL = `PASSIVE_LEVEL`.
    ///
    /// [`WDFDEVICE_INIT`]: wdf_kmdf_sys::WDFDEVICE_INIT
    /// [`EvtDriverDeviceAdd`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
    /// [`EvtChildListCreateDevice`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfchildlist/nc-wdfchildlist-evt_wdf_child_list_create_device
    /// [`EvtCleanupCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_cleanup
    /// [`EvtDestroyCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_destroy
    ///
    /// ## Note for Miniport Drivers
    ///
    /// Miniport drivers must use [`WdfDeviceMiniportCreate`] to create the framework device object.
    ///
    /// ## Return Value
    ///
    /// The newly allocated device object is stored in the place given by `Device`.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if `DeviceInit` is invalid (e.g. already taken)
    /// - `STATUS_INVALID_DEVICE_STATE` if there's already been a device object created for the given `DeviceInit`
    /// - `STATUS_INVALID_SECURITY_DESCR` if [`WdfDeviceInitAssignSDDLString`] or [`WdfDeviceInitSetDeviceClass`] were called,
    ///   but no name was provided.
    /// - `STATUS_INSUFFICIENT_RESOURCES` if the object couldn't be allocated
    /// - `STATUS_OBJECT_NAME_COLLISION` if the name provided by [`WdfDeviceInitAssignName`] already exists
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: PASSIVE_LEVEL
    /// - ([AccessHardwareKey]) Must not access the hardware-specific registry key from [`EvtChildListCreateDevice`]
    /// - ([AddPdotoStaticChildlist]) For a PDO device, after calling [`WdfPdoInitAllocate`] and [`WdfDeviceCreate`], [`WdfFdoAddStaticChild`] must be called too
    /// - ([ChangeQueueState]) Must not be called concurrently with other state-changing functions
    /// - ([ChildDeviceInitApi]) The child device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([ChildListConfiguration]) If the driver supports [Dynamic Enumeration], [`WdfFdoInitSetDefaultChildListConfig`] must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceDeleted]) If a PnP driver creates a control device object, there are additional constraints on when it must be deleted
    /// - ([ControlDeviceInitAllocate]) If a control device object is to be created, [`WdfControlDeviceInitAllocate`] must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([CtlDeviceFinishInitDeviceAdd]) If a PnP driver creates a control device object in [`EvtDriverDeviceAdd`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`EvtDriverDeviceAdd`]
    /// - ([CtlDeviceFinishInitDrEntry]) If a PnP driver creates a control device object in [`DriverEntry`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`DriverEntry`]
    /// - ([DeviceCreateFail]) If [`WdfDeviceCreate`] fails in [`EvtDriverDeviceAdd`], the error status must be returned as [`EvtDriverDeviceAdd`]'s status
    /// - ([DeviceInitAllocate]) For a PDO device or control device object, [`WdfPdoInitAllocate`] or [`WdfControlDeviceInitAllocate`] respectively
    ///   must be called before [`WdfDeviceCreate`]
    /// - ([DeviceInitAPI]) The device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([InitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    /// - ([InitFreeDeviceCreateType2]) Must not call [`WdfDeviceCreate`] after [`WdfDeviceInitFree`]
    /// - ([InitFreeDeviceCreateType4]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during [`WdfDeviceCreate`],
    ///   [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([InitFreeNull]) In addition to [InitFreeDeviceCreate], [InitFreeDeviceCreateType2], and/or [InitFreeDeviceCreateType4],
    ///   or [PdoInitFreeDeviceCreate], [PdoInitFreeDeviceCreateType2], and/or [PdoInitFreeDeviceCreateType4] for PDO devices specifically,
    ///   after a successful call to [`WdfDeviceInitFree`], the original pointer place should be set to `NULL`
    /// - ([PdoDeviceInitAPI]) The device object initialization methods and [`WdfPdoInitAllocate`] must be called before [`WdfDeviceCreate`]
    /// - ([PdoInitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    /// - ([PdoInitFreeDeviceCreateType2]) Must not call [`WdfDeviceCreate`] after [`WdfDeviceInitFree`]
    /// - ([PdoInitFreeDeviceCreateType4]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs during [`WdfDeviceCreate`],
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [AccessHardwareKey]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-AccessHardwareKey
    /// [AddPdotoStaticChildlist]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-AddPdoToStaticChildList
    /// [ChangeQueueState]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChangeQueueState
    /// [ChildDeviceInitApi]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChildDeviceInitAPI
    /// [ChildListConfiguration]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChildListConfiguration
    /// [Dynamic Enumeration]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/dynamic-enumeration
    /// [ControlDeviceDeleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceDeleted
    /// [ControlDeviceInitAllocate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceInitAllocate
    /// [ControlDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceInitAPI
    /// [CtlDeviceFinishInitDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDeviceAdd
    /// [CtlDeviceFinishInitDrEntry]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDrEntry
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DeviceCreateFail]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeviceCreateFail
    /// [DeviceInitAllocate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeviceInitAllocate
    /// [DeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeviceInitAPI (weird redundant 2nd para)
    /// [InitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreate
    /// [InitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType2
    /// [InitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType4
    /// [InitFreeNull]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeNull
    /// [PdoDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoDeviceInitAPI
    /// [PdoInitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreate
    /// [PdoInitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreateType2
    /// [PdoInitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreateType4
    ///
    // TODO: As proper intra-doc links
    /// [`WdfControlDeviceInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfcontrol/nf-wdfcontrol-wdfcontroldeviceinitallocate
    /// [`WdfControlFinishInitializing`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfcontrol/nf-wdfcontrol-wdfcontrolfinishinitializing
    /// [`WdfDeviceInitAssignSDDLString`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitassignsddlstring
    /// [`WdfDeviceInitAssignName`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitassignname
    /// [`WdfDeviceInitFree`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitfree
    /// [`WdfDeviceInitSetDeviceClass`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitsetdeviceclass
    /// [`WdfDeviceMiniportCreate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfminiport/nf-wdfminiport-wdfdeviceminiportcreate
    /// [`WdfFdoAddStaticChild`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdffdo/nf-wdffdo-wdffdoaddstaticchild
    /// [`WdfFdoInitSetDefaultChildListConfig`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdffdo/nf-wdffdo-wdffdoinitsetdefaultchildlistconfig
    /// [`WdfPdoInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfpdo/nf-wdfpdo-wdfpdoinitallocate
    ///
    #[must_use]
    pub unsafe fn WdfDeviceCreate(
        DeviceInit: &mut PWDFDEVICE_INIT,                 // in, out
        DeviceAttributes: Option<PWDF_OBJECT_ATTRIBUTES>, // in, optional, stack_local
        Device: &mut WDFDEVICE,                           // out
    ) -> NTSTATUS {
        // Impossible to return STATUS_INVALID_PARAMETER based on immediate parameters since `&mut` is guaranteed to be valid
        dispatch!(WdfDeviceCreate(
            DeviceInit,
            DeviceAttributes.unwrap_or(WDF_NO_OBJECT_ATTRIBUTES!()),
            Device
        ))
    }

    // endregion: wdfdevice

    // region: wdfdriver

    /// Creates a framework driver object for calling the driver
    ///
    /// `DriverObject` and `RegistryPath` should be from the inputs in recieved from the `DriverEntry` point.
    /// If `DriverAttributes` is specified, the `ParentObject` must be `NULL`.
    ///
    /// ## Return Value
    ///
    /// The newly allocated driver object is stored in the place given by `Driver`.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_DRIVER_INTERNAL_ERROR` if called more than once
    /// - `STATUS_INVALID_PARAMETER` if a [`EvtDriverDeviceAdd`] was specified, but the init flags indicate that a Non-PnP driver is to be created.
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    /// [`EvtDriverDeviceAdd`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: PASSIVE_LEVEL
    /// - ([ChangeQueueState]) Must not be called concurrently with other state-changing functions
    /// - ([DriverAttributeChanged]) The existing execution level or syncronization scope must not be modified(?)
    /// - ([DriverCreate]) Must only be called from the `DriverEntry` point
    /// - ([MiniportOnlyWdmDevice]) ???
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChangeQueueState]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChangeQueueState
    /// [DriverAttributeChanged]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverAttributeChanged
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [MiniportOnlyWdmDevice]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MiniportOnlyWdmDevice
    ///
    #[must_use]
    pub unsafe fn WdfDriverCreate(
        DriverObject: PDRIVER_OBJECT,                     // in
        RegistryPath: PCUNICODE_STRING,                   // in
        DriverAttributes: Option<PWDF_OBJECT_ATTRIBUTES>, // in, optional, stack_local
        DriverConfig: PWDF_DRIVER_CONFIG,                 // in
        Driver: Option<&mut WDFDRIVER>,                   // out, optional
    ) -> NTSTATUS {
        // note: a wdf driver can be non-pnp, miniport, both, or neither
        // non-pnp would be subset of non-miniport or miniport (NoDispatchOverride) variations
        dispatch!(WdfDriverCreate(
            DriverObject,
            RegistryPath,
            DriverAttributes.unwrap_or(WDF_NO_OBJECT_ATTRIBUTES!()),
            DriverConfig,
            Driver.map(|p| p as *mut _).unwrap_or(WDF_NO_HANDLE!()),
        ))
    }

    /// Gets the framework driver object, or `None` if the driver hasn't been created yet.
    ///
    /// ## Safety
    ///
    /// Must not be called while [`WdfDriverCreate`] is executing
    #[must_use]
    pub unsafe fn WdfGetDriver() -> Option<WDFDRIVER> {
        // SAFETY: Caller handles racing accesses, and is effectively initialized & immutable after WdfDriverCreate
        let globals = unsafe { &*wdf_kmdf_sys::WdfDriverGlobals };
        let driver = globals.Driver;

        Some(driver).filter(|drv| drv.is_null())
    }

    // endregion: driver
}

pub mod object {

    use wdf_kmdf_sys::_WDF_OBJECT_CONTEXT_TYPE_INFO;

    pub type ContextInfo = _WDF_OBJECT_CONTEXT_TYPE_INFO;

    #[macro_export]
    macro_rules! impl_context_space {
        ($ty:ty) => {
            unsafe impl $crate::object::IntoContextSpace for $ty {
                const CONTEXT_INFO: &'static $crate::object::ContextInfo =
                    &$crate::object::ContextInfo {
                        // Size is known to be small
                        Size: ::core::mem::size_of::<$crate::object::ContextInfo>() as u32,
                        ContextName: match ::core::ffi::CStr::from_bytes_until_nul(
                            concat!(stringify!($ty), "\0").as_bytes(),
                        ) {
                            Ok(v) => v.as_ptr(),
                            Err(_) => panic!("forgor nul byte 💀"),
                        },
                        ContextSize: ::core::mem::size_of::<$ty>(),
                        UniqueType: ::core::ptr::null(),
                        EvtDriverGetUniqueContextType: None,
                    };
            }
        };
    }

    /// Converting a data struct into a context space
    ///
    /// ## Safety
    ///
    /// Type sizing must be accurate.
    /// Use the [`impl_context_space`] macro to do it safely.
    pub unsafe trait IntoContextSpace {
        const CONTEXT_INFO: &'static ContextInfo;
    }

    /// The maximum IRQL the object's event callback functions will be called at
    ///
    /// Execution levels can only be specified for:
    ///
    ///
    /// - Driver objects
    /// - Device objects
    /// - File objects
    /// - General objects
    ///
    /// And since WDF 1.9 and later:
    ///
    /// - Queue objects
    /// - Timer objects
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
    #[repr(i32)]
    pub enum ExecutionLevel {
        /// Use the default execution level for an object
        ///
        /// For [`Driver`](crate::driver::Driver) and [`MiniportDriver`](crate::driver::MiniportDriver):
        /// - In KMDF, the default execution level is [`ExecutionLevel::Dispatch`]
        /// - In UMDF, the default execution level is [`ExecutionLevel::Passive`]
        ///
        /// For all other objects, the default is [`ExecutionLevel::InheritFromParent`]
        #[default]
        ObjectDefault,
        /// Use the execution level from the parent device object
        InheritFromParent = wdf_kmdf_sys::_WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent,
        /// Always execute event callbacks at IRQL == PASSIVE_LEVEL
        Passive = wdf_kmdf_sys::_WDF_EXECUTION_LEVEL::WdfExecutionLevelPassive,
        /// Execute event callbacks at IRQL <= DISPATCH_LEVEL (KMDF only)
        Dispatch = wdf_kmdf_sys::_WDF_EXECUTION_LEVEL::WdfExecutionLevelDispatch,
    }

    /// What scope to sequentialize event callbacks at
    ///
    /// Synchronization scopes can only be specified for
    ///
    /// - Driver objects
    /// - Device objects
    /// - Queue objects
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
    #[repr(i32)]
    pub enum SynchronizationScope {
        /// Use the default synchronization level for an object
        ///
        /// For [`Driver`](crate::driver::Driver) and [`MiniportDriver`](crate::driver::MiniportDriver),
        /// the default is [`SynchronizationScope::None`].
        ///
        /// For all other objects, the default is [`SynchronizationScope::InheritFromParent`]
        #[default]
        ObjectDefault,
        /// Use the synchronization scope from the parent device object
        InheritFromParent =
            wdf_kmdf_sys::_WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent,
        /// Event callbacks for all of the device's descendant objects will be executed sequentially (i.e.
        /// before executing an event callback, the device's synchronization lock is acquired).
        ///
        /// This applies to:
        ///
        /// - Queue objects
        /// - File objects
        ///
        /// If the `AutomaticSerialization` flag is true in the object's config struct,
        /// this also applies to the event callbacks of:
        ///
        /// - Interrupt objects
        /// - DPC objects
        /// - Work Item objects
        /// - Timer objects
        ///
        /// Note that this is sequentialization does not happen between device object hierarchies,
        /// which are free to independently execute event callbacks concurrently.
        Device = wdf_kmdf_sys::_WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeDevice,
        /// Event callbacks for a queue will be executed sequentially (i.e. before executing a queue
        /// event callback, the queue's synchronization lock is acquired).
        ///
        /// If the `AutomaticSerialization` flag is true in the object's config struct,
        /// this also applies to the event callbacks of:
        ///
        /// - Interrupt objects
        /// - DPC objects
        /// - Work Item objects
        /// - Timer objects
        ///
        /// Note that this is sequentialization does not happen between queue object hierarchies,
        /// which are free to independently execute event callbacks concurrently.
        ///
        /// Since WDF 1.9, `SynchronizationScope::Queue` should be specified on the queue objects
        /// themselves.
        /// For older WDF versions, the queue object should specify `SynchronizationScope::InheritFromParent`,
        /// and the parent object should specify `SynchronizationScope::Queue`
        Queue = wdf_kmdf_sys::_WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeQueue,
        /// No synchronization is performed, so event callbacks may execute concurrently
        None = wdf_kmdf_sys::_WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeNone,
    }
}

pub mod driver {
    use core::marker::PhantomData;

    use wdf_kmdf_sys::WDFDRIVER;

    use crate::object::IntoContextSpace;

    pub struct Driver<T: IntoContextSpace> {
        handle: WDFDRIVER,
        _context: PhantomData<T>,
    }

    /// Opaque driver handle used during the initialization of the driver's context space
    pub struct DriverHandle(WDFDRIVER);

    pub struct MiniportDriver<T: IntoContextSpace> {
        handle: WDFDRIVER,
        _context: PhantomData<T>,
    }

    /// Opaque miniport driver handle used during the initialization of the miniport driver's context space
    pub struct MiniportDriverHandle(WDFDRIVER);
}
