//! Rust KMDF Abstractions
#![no_std]
#![deny(unsafe_op_in_unsafe_fn)]

pub mod raw {
    //! Raw bindings to KMDF functions
    #![allow(non_snake_case)] // Preserving the names of the original WDF functions

    use wdf_kmdf_sys::{
        PCWDF_OBJECT_CONTEXT_TYPE_INFO, PWDFDEVICE_INIT, PWDF_DRIVER_CONFIG,
        PWDF_OBJECT_ATTRIBUTES, WDFDEVICE, WDFDRIVER, WDFOBJECT, WDF_NO_HANDLE,
        WDF_NO_OBJECT_ATTRIBUTES, _WDF_DEVICE_IO_TYPE,
    };
    use windows_kernel_sys::{NTSTATUS, PCUNICODE_STRING, PDRIVER_OBJECT, PVOID};

    // we refer to this struct a lot, so bring it into scope
    // even though we never use it
    #[cfg(doc)]
    use wdf_kmdf_sys::WDFDEVICE_INIT;

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

                // SAFETY: Read-only, initialized by the time we use it, and checked to be in bounds
                let fn_handle = unsafe { function_table!()
                    .add(FN_INDEX)
                    .cast::<wdf_kmdf_sys::[<PFN_ $name:upper>]>()
                };

                // SAFETY: Ensured that this is present by the static assert
                unsafe { fn_handle.read().unwrap_unchecked() }
            }};

            // SAFETY: It is up to the caller to pass unsafety
            unsafe { fn_handle(wdf_kmdf_sys::WdfDriverGlobals, $($args),*) }
        }};
    }

    // region: wdfcontrol

    /// Allocates a [`WDFDEVICE_INIT`] struct for creating a new control device object (CDO)
    ///
    /// The yielded [`WDFDEVICE_INIT`] eventually gets passed to [`WdfDeviceCreate`], which actually creates the control device object.
    ///
    /// `SDDLString` specifies the security descriptor for the new device object, using SDDL syntax
    /// (see [Securing Device Objects] for more information). The `SDDL_DEVOBJ_Xxx` constants can be used to conveniently specify common
    /// security descriptor sets. This can be overriden by a later call to [`WdfDeviceInitAssignSDDLString`].
    ///
    /// On successful creation, returns a pointer to a framework-allocated [`WDFDEVICE_INIT`] struct,
    /// otherwise returns `NULL`.
    ///
    /// [Securing Device Objects]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/securing-device-objects
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: PASSIVE_LEVEL
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([CtlDeviceFinishInitDeviceAdd]) If a PnP driver creates a control device object in [`EvtDriverDeviceAdd`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`EvtDriverDeviceAdd`]
    /// - ([CtlDeviceFinishInitDrEntry]) If a PnP driver creates a control device object in [`DriverEntry`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`DriverEntry`]
    /// - ([DriverCreate]) Must only be called from the `DriverEntry` point
    /// - ([InitFreeDeviceCallback]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs while initializing
    ///   a new framework device object, [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([InitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    /// - ([InitFreeDeviceCreateType2]) Must not call [`WdfDeviceCreate`] after [`WdfDeviceInitFree`]
    /// - ([InitFreeDeviceCreateType4]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during [`WdfDeviceCreate`],
    ///   [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ControlDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceInitAPI
    /// [CtlDeviceFinishInitDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDeviceAdd
    /// [`EvtDriverDeviceAdd`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
    /// [CtlDeviceFinishInitDrEntry]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDrEntry
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [InitFreeDeviceCallback]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCallback
    /// [InitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreate
    /// [InitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType2
    /// [InitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType4
    ///
    // TODO: As proper intra-doc links
    /// [`WdfDeviceInitAssignSDDLString`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitassignsddlstring
    ///
    #[must_use]
    pub unsafe fn WdfControlDeviceInitAllocate(
        Driver: WDFDRIVER,            // in
        SDDLString: PCUNICODE_STRING, // in
    ) -> PWDFDEVICE_INIT {
        dispatch!(WdfControlDeviceInitAllocate(Driver, SDDLString))
    }

    // FIXME: WdfControlDeviceInitSetShutdownNotification

    /// Informs the framework that initialization of the control device object has finished
    ///
    /// Until [`WdfControlFinishInitializing`] is called, the system will not send I/O requests
    /// or Windows Management Instrumentation (WMI) requests to the given control device object.
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: PASSIVE_LEVEL
    /// - ([CtlDeviceFinishInitDeviceAdd]) If a PnP driver creates a control device object in [`EvtDriverDeviceAdd`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`EvtDriverDeviceAdd`]
    /// - ([CtlDeviceFinishInitDrEntry]) If a PnP driver creates a control device object in [`DriverEntry`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`DriverEntry`]
    /// - ([DriverCreate]) Must only be called from the [`DriverEntry`] point
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [CtlDeviceFinishInitDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDeviceAdd
    /// [`EvtDriverDeviceAdd`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
    /// [CtlDeviceFinishInitDrEntry]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDrEntry
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    ///
    pub unsafe fn WdfControlFinishInitializing(Device: WDFDEVICE /* in */) {
        dispatch!(WdfControlFinishInitializing(Device))
    }

    // endregion: wdfcontrol

    // region: wdfdevice

    /// Creates a framework device object
    ///
    /// The device object created could be either:
    /// - a function device object (FDO) if the [`WDFDEVICE_INIT`] structure was originally from a [`EvtDriverDeviceAdd`] callback, or
    /// - a physical device object (PDO) if the [`WDFDEVICE_INIT`] structure was originally from a [`EvtChildListCreateDevice`] or [`WdfPdoInitAllocate`]
    /// - a control device object (CDO) if the [`WDFDEVICE_INIT`] structure was originally from [`WdfControlDeviceInitAllocate`]
    ///
    /// On successful creation, the place passed in `DeviceInit` is set to `NULL` (i.e. it steals ownership of the [`WDFDEVICE_INIT`] structure).
    ///
    /// If `DriverAttributes` is specified, the `ParentObject` must be `NULL`.
    /// If any of [`EvtCleanupCallback`] and/or [`EvtDestroyCallback`] are specified, note that they will be called at IRQL = `PASSIVE_LEVEL`.
    ///
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
    /// [`EvtDriverDeviceAdd`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
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
    /// [`WdfDeviceInitAssignSDDLString`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitassignsddlstring
    /// [`WdfDeviceInitAssignName`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitassignname
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

    /// Assigns a device name to the new device object
    ///
    /// Must be called before [`WdfDeviceCreate`].
    ///
    /// If a name was assigned with a previous call to [`WdfDeviceInitAssignName`],
    /// the name can be cleared by calling [`WdfDeviceInitAssignName`] again with `DeviceName` as `None`.
    /// If a name is required (e.g. because it represents a PDO or a control device), a name will be assigned
    /// by the operating system.
    ///
    /// For more information about naming device objects, see [Controlling Device Access in Framework-Based Drivers].
    /// [Controlling Device Access in Framework-Based Drivers]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/controlling-device-access-in-kmdf-drivers
    ///
    /// ## Return Value
    ///
    /// `STATUS_SUCCES` if the name was assigned successfully.
    /// Otherwise, may return `STATUS_INSUFFICIENT_RESOURCES` if there isn't enough space to store the device name.
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: PASSIVE_LEVEL
    /// - ([ChildDeviceInitApi]) The child device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DeviceInitAPI]) The device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DriverCreate]) Must only be called from the [`DriverEntry`] point
    /// - ([InitFreeDeviceCallback]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs while initializing
    ///   a new framework device object, [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([InitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    /// - ([InitFreeNull]) In addition to [InitFreeDeviceCreate], [InitFreeDeviceCreateType2], and/or [InitFreeDeviceCreateType4],
    ///   or [PdoInitFreeDeviceCreate], [PdoInitFreeDeviceCreateType2], and/or [PdoInitFreeDeviceCreateType4] for PDO devices specifically,
    ///   after a successful call to [`WdfDeviceInitFree`], the original pointer place should be set to `NULL`
    /// - ([PdoDeviceInitAPI]) The device object initialization methods and [`WdfPdoInitAllocate`] must be called before [`WdfDeviceCreate`]
    /// - ([PdoInitFreeDeviceCallback]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs while initializing
    ///   a new framework device object, [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([PdoInitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChildDeviceInitApi]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChildDeviceInitAPI
    /// [ControlDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceInitAPI
    /// [DeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeviceInitAPI
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [InitFreeDeviceCallback]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCallback
    /// [InitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreate
    /// [InitFreeNull]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeNull
    /// [PdoDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoDeviceInitAPI
    /// [PdoInitFreeDeviceCallback]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCallback
    /// [PdoInitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreate
    #[must_use]
    pub unsafe fn WdfDeviceInitAssignName(
        DeviceInit: PWDFDEVICE_INIT,          // in
        DeviceName: Option<PCUNICODE_STRING>, // in, optional
    ) -> NTSTATUS {
        dispatch!(WdfDeviceInitAssignName(
            DeviceInit,
            DeviceName.unwrap_or(core::ptr::null())
        ))
    }

    // FIXME: WdfDeviceInitAssignSDDLString
    // FIXME: WdfDeviceInitAssignWdmIrpPreprocessCallback

    /// Deallocates a [`WDFDEVICE_INIT`] struct
    ///
    /// If an error occurs from calling a device object initialization method or [`WdfDeviceCreate`],
    /// and the [`WDFDEVICE_INIT`] struct came from [`WdfPdoInitAllocate`] or [`WdfControlDeviceInitAllocate`],
    /// [`WdfDeviceInitFree`] must be called.
    ///
    /// If the [`WDFDEVICE_INIT`] came from a [`DriverCallbacks::device_add`](crate::driver::DriverCallbacks::device_add) callback,
    /// the framework handles the lifetime of the struct, so this does not need to be called in that case.
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= DISPATCH_LEVEL
    /// - ([DoubleDeviceInitFree]) Should not be called twice on the same [`WDFDEVICE_INIT`] struct
    /// - ([InitFreeDeviceCallback]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs while initializing
    ///   a new framework device object, [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([InitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    /// - ([InitFreeDeviceCreateType2]) Must not call [`WdfDeviceCreate`] after [`WdfDeviceInitFree`]
    /// - ([InitFreeDeviceCreateType4]) If a [`WDFDEVICE_INIT`] is from a [`WdfControlDeviceInitAllocate`] and an error occurs during [`WdfDeviceCreate`],
    ///   [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([PdoInitFreeDeviceCallback]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs while initializing
    ///   a new framework device object, [`WdfDeviceInitFree`] must be called on the [`WDFDEVICE_INIT`] structure
    /// - ([PdoInitFreeDeviceCreate]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs during one of the device object
    ///   initialization methods, [`WdfDeviceInitFree`] must be called instead of [`WdfDeviceCreate`]
    /// - ([PdoInitFreeDeviceCreateType2]) Must not call [`WdfDeviceCreate`] after [`WdfDeviceInitFree`]
    /// - ([PdoInitFreeDeviceCreateType4]) If a [`WDFDEVICE_INIT`] is from a [`WdfPdoInitAllocate`] and an error occurs during [`WdfDeviceCreate`],
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DoubleDeviceInitFree]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DoubleDeviceInitFree
    /// [InitFreeDeviceCallback]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCallback
    /// [InitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreate
    /// [InitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType2
    /// [InitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType4
    /// [PdoInitFreeDeviceCallback]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCallback
    /// [PdoInitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreate
    /// [PdoInitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreateType2
    /// [PdoInitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreateType4
    ///
    // TODO: As proper intra-doc links
    /// [`WdfPdoInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfpdo/nf-wdfpdo-wdfpdoinitallocate
    pub unsafe fn WdfDeviceInitFree(DeviceInit: PWDFDEVICE_INIT) {
        dispatch!(WdfDeviceInitFree(DeviceInit))
    }

    // FIXME: WdfDeviceInitRegisterPnpStateChangeCallback
    // FIXME: WdfDeviceInitRegisterPowerPolicyStateChangeCallback
    // FIXME: WdfDeviceInitRegisterPowerStateChangeCallback
    // FIXME: WdfDeviceInitSetCharacteristics
    // FIXME: WdfDeviceInitSetDeviceClass
    // FIXME: WdfDeviceInitSetDeviceType
    // FIXME: WdfDeviceInitSetExclusive
    // FIXME: WdfDeviceInitSetFileObjectConfig
    // FIXME: WdfDeviceInitSetIoInCallerContextCallback

    /// Sets the preference for how a driver will access data buffers in read and write requests for a specific device
    ///
    /// Must be called before [`WdfDeviceCreate`].
    ///
    /// For KMDF 1.13+, [`WdfDeviceInitSetIoTypeEx`] should be used instead.
    ///
    /// If this is not called, the default buffer access method is `WdfDeviceIoBuffered` for devices.
    ///
    /// For more info on buffer access methods, see [Accessing Data Buffers]
    ///
    /// [Accessing Data Buffers]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/accessing-data-buffers-in-wdf-drivers
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= DISPATCH_LEVEL
    /// - ([ChildDeviceInitApi]) The child device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DeviceInitAPI]) The device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DriverCreate]) Must only be called from the [`DriverEntry`] point
    /// - ([PdoDeviceInitAPI]) The device object initialization methods and [`WdfPdoInitAllocate`] must be called before [`WdfDeviceCreate`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChildDeviceInitApi]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChildDeviceInitAPI
    /// [ControlDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceInitAPI
    /// [DeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeviceInitAPI
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [PdoDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoDeviceInitAPI
    ///
    // TODO: As proper intra-doc links
    /// [`WdfDeviceSetIoTypeEx`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitsetiotypeex
    pub unsafe fn WdfDeviceInitSetIoType(
        DeviceInit: PWDFDEVICE_INIT,       // in
        IoType: _WDF_DEVICE_IO_TYPE::Type, // in
    ) {
        dispatch!(WdfDeviceInitSetIoType(DeviceInit, IoType))
    }

    // FIXME: WdfDeviceInitSetIoTypeEx
    // FIXME: WdfDeviceInitSetPnpPowerEventCallbacks
    // FIXME: WdfDeviceInitSetPowerInrush
    // FIXME: WdfDeviceInitSetPowerNotPageable
    // FIXME: WdfDeviceInitSetPowerPageable
    // FIXME: WdfDeviceInitSetPowerPolicyEventCallbacks
    // FIXME: WdfDeviceInitSetPowerPolicyOwnership
    // FIXME: WdfDeviceInitSetReleaseHardwareOrderOnFailure
    // FIXME: WdfDeviceInitSetRemoveLockOptions
    // FIXME: WdfDeviceInitSetRequestAttributes

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
    /// - ([DriverAttributeChanged]) The existing execution level or synchronization scope must not be modified(?)
    /// - ([DriverCreate]) Must only be called from the [`DriverEntry`] point
    /// - ([MiniportOnlyWdmDevice]) FIXME: ???
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChangeQueueState]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChangeQueueState
    /// [DriverAttributeChanged]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverAttributeChanged
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
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

    // endregion: wdfdriver

    // region: wdfobject

    /// If `TypeInfo` has a context space associated with the object, returns a pointer to the context space.
    /// Otherwise, returns `NULL`
    ///
    /// ## Safety
    ///
    /// - `Handle` must point to a valid WDFOBJECT
    /// - `TypeInfo` must point to a vaid context space info
    pub unsafe fn WdfObjectGetTypedContextWorker(
        Handle: WDFOBJECT,
        TypeInfo: PCWDF_OBJECT_CONTEXT_TYPE_INFO,
    ) -> PVOID {
        dispatch!(WdfObjectGetTypedContextWorker(Handle, TypeInfo))
    }

    // endregion: wdfobject
}

pub mod object {

    use wdf_kmdf_sys::{WDFOBJECT, WDF_OBJECT_ATTRIBUTES, _WDF_OBJECT_CONTEXT_TYPE_INFO};

    #[doc(hidden)]
    pub mod __macro_internals {
        pub use static_assertions::const_assert;
        pub use windows_kernel_sys::MEMORY_ALLOCATION_ALIGNMENT;
    }

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

            // Alignment of context type should be smaller than or equal to the arch's defined alignment
            $crate::object::__macro_internals::const_assert!(
                ::core::mem::align_of::<$ty>()
                    <= $crate::object::__macro_internals::MEMORY_ALLOCATION_ALIGNMENT as usize
            );
        };
    }

    /// Converting a data struct into a context space
    ///
    /// ## Safety
    ///
    /// - Type sizing must be accurate.
    /// - Alignment must be smaller than or equal to the architecture defined alignment
    ///   (16 on 64-bit systems, 8 on 32-bit systems)
    ///
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
    #[derive(Copy, Clone, PartialEq, Eq, Default)]
    #[repr(i32)]
    pub enum ExecutionLevel {
        /// Use the execution level from the parent device object.
        ///
        /// This is also used as the default execution level for [`Driver`](crate::driver::Driver),
        /// where:
        /// - In KMDF, the default execution level is [`ExecutionLevel::Dispatch`]
        /// - In UMDF, the default execution level is [`ExecutionLevel::Passive`]
        #[default]
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
    #[derive(Copy, Clone, PartialEq, Eq, Default)]
    #[repr(i32)]
    pub enum SynchronizationScope {
        /// Use the synchronization scope from the parent device object.
        ///
        /// This is also used as the default synchronization level for [`Driver`](crate::driver::Driver),
        /// where it is [`SynchronizationScope::None`].
        #[default]
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

    pub(crate) fn default_object_attributes<T: IntoContextSpace>() -> WDF_OBJECT_ATTRIBUTES {
        let mut object_attrs = WDF_OBJECT_ATTRIBUTES::init();

        // Always set the context info
        object_attrs.ContextTypeInfo = T::CONTEXT_INFO as *const _;

        object_attrs
    }

    /// Converts the typed handle into the generic version
    pub trait AsObjectHandle {
        fn as_handle(&self) -> WDFOBJECT;
        fn as_handle_mut(&mut self) -> WDFOBJECT;
    }

    /// Gets a mut ref to the associated context space, or `None` if not found
    ///
    /// ## Safety
    ///
    /// The object's context space must be initialized
    pub(crate) unsafe fn get_context<T: IntoContextSpace>(
        handle: &impl AsObjectHandle,
    ) -> Option<&T> {
        let handle = handle.as_handle();

        // SAFETY: `handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space =
            unsafe { crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO) };
        let context_space = context_space.cast::<T>();

        // SAFETY:
        // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        // - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - It's the caller's responsibility to ensure that the context space is initialized
        unsafe { context_space.as_ref() }
    }

    /// Gets a mut ref to the associated context space, or `None` if not found
    ///
    /// ## Safety
    ///
    /// The object's context space must be initialized
    pub(crate) unsafe fn get_context_mut<T: IntoContextSpace>(
        handle: &mut impl AsObjectHandle,
    ) -> Option<&mut T> {
        let handle = handle.as_handle_mut();

        // SAFETY: `handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space =
            unsafe { crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO) };
        let context_space = context_space.cast::<T>();

        // SAFETY:
        // - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //   (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        // - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - It's the caller's responsibility to ensure that the context space is initialized
        // - &mut on the original handle guarantees exclusivity
        unsafe { context_space.as_mut() }
    }

    /// Initializes the object's context area
    ///
    /// ## Safety
    ///
    /// - Must not reinitialize an object's context space
    /// - Object must actually have the context space
    pub(crate) unsafe fn context_pin_init<T: IntoContextSpace, E>(
        handle: &mut impl AsObjectHandle,
        pin_init: impl pinned_init::PinInit<T, E>,
    ) -> Result<(), E> {
        let handle = handle.as_handle_mut();

        // SAFETY: `handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space =
            unsafe { crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO) };
        let context_space = context_space.cast::<T>();

        // SAFETY:
        // - The following ensures that the context space is valid pinned uninitialized memory:
        //   - WDF aligns memory to MEMORY_ALLOCATION_ALIGNMENT
        //     (see https://github.com/microsoft/Windows-Driver-Frameworks/blob/3b9780e847/src/framework/shared/inc/private/common/fxhandle.h#L98)
        //   - WDF does not move the allocation for the original object context
        //   - `IntoContextSpace` requires alignemt to MEMORY_ALLOCATION_ALIGNMENT or smaller
        // - We directly return the produced error
        unsafe { pin_init.__pinned_init(context_space) }
    }
}

pub mod driver {
    use core::marker::PhantomData;

    use pinned_init::PinInit;
    use vtable::vtable;
    use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDRIVER, WDFOBJECT, _WDF_DRIVER_INIT_FLAGS};
    use windows_kernel_rs::{string::UnicodeString, DriverObject};
    use windows_kernel_sys::{
        sys::Win32::Foundation::{NTSTATUS, STATUS_INVALID_PARAMETER},
        Error,
    };

    use crate::{
        object::{self, default_object_attributes, IntoContextSpace},
        raw,
    };

    pub struct Driver<T: DriverCallbacks> {
        _context: PhantomData<T>,
    }

    /// Opaque driver handle used during the initialization of the driver's context space
    pub struct DriverHandle(WDFDRIVER);

    impl DriverHandle {
        /// Wraps the handle in a raw driver object
        ///
        /// SAFETY:
        ///
        /// Respect aliasing rules, since this can be used to
        /// generate aliasing mutable references to the context space
        unsafe fn wrap(handle: WDFDRIVER) -> Self {
            Self(handle)
        }

        /// Gets the driver handle for use with WDF functions that don't have clean wrappers yet
        pub fn raw_handle(&mut self) -> WDFDRIVER {
            self.0
        }
    }

    impl object::AsObjectHandle for DriverHandle {
        fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }

        fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }
    }

    #[derive(Default)]
    pub struct DriverConfig {
        /// If the driver is a PnP driver, and thus requires an `EvtDriverDeviceAdd` callback.
        pub pnp_mode: PnpMode,
        /// Tag to mark allocations made by WDF
        pub pool_tag: Option<u32>,
    }

    #[derive(Default, Clone, Copy, PartialEq, Eq)]
    pub enum PnpMode {
        /// This is a pnp driver, and if there isn't already an `device_add` callback,
        /// the default one is used.
        #[default]
        Pnp,
        /// This is a non-pnp driver, and the `device_add` callback shouldn't be present
        NonPnp,
    }

    /// Event callbacks for a driver
    #[vtable]
    pub trait DriverCallbacks: IntoContextSpace {
        // FIXME: Just a stub, not really the real thing
        #[allow(unused)]
        fn device_add(&self, device_init: PWDFDEVICE_INIT) -> Result<(), Error> {
            Ok(())
        }

        /// Performs operations that must take place before unloading the driver.
        ///
        /// Must deallocate any non-device-specific system resources allocated during
        /// the driver entry function.
        ///
        /// ## IRQL: Passive
        ///
        /// ## Note
        ///
        /// Most ownership-related unload tasks should instead be done via the fields
        /// implementing `Drop`, rather than being manually deallocated here.
        fn unload(&mut self) {}
    }

    impl<T: DriverCallbacks> Driver<T> {
        /// Creates a new WDF Driver Object
        ///
        /// ## IRQL: Passive
        ///
        /// ## Returns
        ///
        /// Driver object wrapper, or:
        ///
        /// - `STATUS_INVALID_PARAMETER` if non-pnp mode is specified, but `device_add` is also specified
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn create<F, I>(
            driver_object: DriverObject,
            registry_path: UnicodeString,
            config: DriverConfig,
            init_context: F,
        ) -> Result<(), Error>
        where
            F: FnOnce(&mut DriverHandle) -> Result<I, Error>,
            I: PinInit<T, Error>,
        {
            if matches!(config.pnp_mode, PnpMode::NonPnp) && T::HAS_DEVICE_ADD {
                // Non-pnp drivers shouldn't specify the `device_add` callback
                return Err(Error(STATUS_INVALID_PARAMETER));
            }

            // NOTE: Since we can't `WdfObjectDelete` a driver, the framework handles
            // uninitializing the driver via EvtDriverUnload, so we don't need to set
            // EvtCleanupCallback and EvtDestroyCallback.
            //
            // We do set EvtDestroyCallback however, since it's always guaranteed to
            // be present (we use it for dropping the driver context area)
            let mut object_attrs = default_object_attributes::<T>();
            object_attrs.EvtDestroyCallback = Some(Self::__dispatch_destroy);

            let mut driver_config = wdf_kmdf_sys::WDF_DRIVER_CONFIG::init(
                T::HAS_DEVICE_ADD.then_some(Self::__dispatch_driver_device_add),
            );
            driver_config.EvtDriverUnload = T::HAS_UNLOAD.then_some(Self::__dispatch_driver_unload);

            if matches!(config.pnp_mode, PnpMode::NonPnp) {
                driver_config.DriverInitFlags |=
                    _WDF_DRIVER_INIT_FLAGS::WdfDriverInitNonPnpDriver as u32;
            }

            // NOTE: This is always available since we're targeting KMDF versions after 1.5
            if let Some(tag) = config.pool_tag {
                driver_config.DriverPoolTag = tag
            }

            // Make it!
            let mut handle = {
                let driver_object = driver_object.into_raw();

                // SAFETY: contained entirely within this block, will never outlive original
                // WDF also copies the contents anyways
                let registry_path = unsafe { registry_path.as_raw_ptr() };

                // SAFETY: Replaced with the real driver pointer next
                let mut handle = unsafe { DriverHandle::wrap(core::ptr::null_mut()) };

                // SAFETY: Owned `DriverObject` means that it only gets called once
                unsafe {
                    Error::to_err(raw::WdfDriverCreate(
                        driver_object,
                        registry_path,
                        Some(&mut object_attrs),
                        &mut driver_config,
                        Some(&mut handle.0),
                    ))?
                }

                handle
            };

            // Initialize context
            let pin_init = init_context(&mut handle)?;

            // SAFETY:
            // - It's WDF's responsibility to insert the context area, since we create
            //   the default object attributes with T's context area
            // - The driver object was just created
            unsafe { object::context_pin_init::<T, Error>(&mut handle, pin_init) }
        }

        unsafe extern "C" fn __dispatch_driver_device_add(
            driver: WDFDRIVER,
            device_init: PWDFDEVICE_INIT,
        ) -> NTSTATUS {
            // NOTE: Unsure if this can be called concurrently, so for safety
            // we only use an immutable handle.
            // SAFETY: Only used behind an immutable reference, so we prevent
            // concurrent immutable mutations
            let handle = unsafe { DriverHandle::wrap(driver) };

            // SAFETY: Initialized by this point
            let context_space = unsafe { object::get_context(&handle) };
            let Some(context_space) = context_space else {
                return windows_kernel_sys::sys::Win32::Foundation::STATUS_SUCCESS;
            };

            match T::device_add(context_space, device_init) {
                Ok(()) => windows_kernel_sys::sys::Win32::Foundation::STATUS_SUCCESS,
                Err(err) => err.0,
            }
        }

        unsafe extern "C" fn __dispatch_driver_unload(driver: WDFDRIVER) {
            // SAFETY: Driver unload only gets called once, and after everything?
            let mut handle = unsafe { DriverHandle::wrap(driver) };

            // SAFETY: Initialized by this point
            let context_space = unsafe { object::get_context_mut(&mut handle) };
            let Some(context_space) = context_space else {
                // Nothing to do
                return;
            };

            if T::HAS_UNLOAD {
                // Do the unload callback...
                T::unload(context_space);
            }
        }

        unsafe extern "C" fn __dispatch_destroy(driver: WDFOBJECT) {
            // SAFETY: Can construct mutable handle since destroy is only
            // called once and exclusively
            let mut handle = unsafe { DriverHandle::wrap(driver.cast()) };

            // SAFETY: Initialized by this point
            let context_space = unsafe { object::get_context_mut::<T>(&mut handle) };
            let Some(context_space) = context_space else {
                return;
            };

            unsafe {
                windows_kernel_sys::DbgPrintEx(
                    windows_kernel_sys::_DPFLTR_TYPE::DPFLTR_IHVDRIVER_ID as u32,
                    windows_kernel_sys::DPFLTR_INFO_LEVEL,
                    b"KmdfHewwoWowwd: EvtDestroy\n\0".as_ptr().cast(),
                )
            };

            // Drop it!
            // SAFETY:
            // Guaranteed to be unused by this point, since the destroy callback is the last one
            // called.
            // Object initialization guarantees that this was valid initialized memory
            unsafe { core::ptr::drop_in_place(context_space) };
        }
    }
}
