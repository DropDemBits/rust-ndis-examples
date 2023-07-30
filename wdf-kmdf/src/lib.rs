//! Rust KMDF Abstractions
#![no_std]
#![deny(
    unsafe_op_in_unsafe_fn,
    clippy::multiple_unsafe_ops_per_block,
    clippy::undocumented_unsafe_blocks
)]

pub mod raw {
    //! Raw bindings to KMDF functions
    #![allow(non_snake_case)] // Preserving the names of the original WDF functions

    use wdf_kmdf_sys::{
        PCWDF_OBJECT_CONTEXT_TYPE_INFO, PWDFDEVICE_INIT, PWDF_DRIVER_CONFIG,
        PWDF_FILEOBJECT_CONFIG, PWDF_IO_QUEUE_CONFIG, PWDF_OBJECT_ATTRIBUTES, WDFDEVICE, WDFDRIVER,
        WDFOBJECT, WDFQUEUE, WDFSPINLOCK, WDF_DEVICE_IO_TYPE, WDF_NO_HANDLE,
        WDF_NO_OBJECT_ATTRIBUTES,
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
                const FN_INDEX: usize = wdf_kmdf_sys::WDFFUNCENUM::[<$name TableIndex>].0 as usize;

                // Must be in the always-available function category
                static_assertions::const_assert!(FN_INDEX < wdf_kmdf_sys::WDF_ALWAYS_AVAILABLE_FUNCTION_COUNT as usize);

                // SAFETY: Immutable by the time we're loaded
                let fn_table = unsafe { function_table!() };

                // SAFETY: Read-only, initialized by the time we use it, and checked to be in bounds
                let fn_handle = unsafe {
                    fn_table
                    .add(FN_INDEX)
                    .cast::<wdf_kmdf_sys::[<PFN_ $name:upper>]>()
                };

                // SAFETY: Ensured that this is present by the static assert
                let fn_handle = unsafe { fn_handle.read()};
                // SAFETY: All available function handles are not null
                let fn_handle = unsafe { fn_handle.unwrap_unchecked() };

                fn_handle
            }};

            // SAFETY: Pointer to globals is immutable by the time we're loaded
            let globals = unsafe {wdf_kmdf_sys::WdfDriverGlobals};

            // SAFETY: It is up to the caller to pass unsafety
            unsafe { fn_handle(globals, $($args),*) }
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
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `PASSIVE_LEVEL`
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([CtlDeviceFinishInitDeviceAdd]) If a PnP driver creates a control device object in [`EvtDriverDeviceAdd`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`EvtDriverDeviceAdd`]
    /// - ([CtlDeviceFinishInitDrEntry]) If a PnP driver creates a control device object in [`DriverEntry`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`DriverEntry`]
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
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
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `PASSIVE_LEVEL`
    /// - ([CtlDeviceFinishInitDeviceAdd]) If a PnP driver creates a control device object in [`EvtDriverDeviceAdd`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`EvtDriverDeviceAdd`]
    /// - ([CtlDeviceFinishInitDrEntry]) If a PnP driver creates a control device object in [`DriverEntry`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`DriverEntry`]
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
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
        dispatch!(WdfControlFinishInitializing(Device));
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
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `PASSIVE_LEVEL`
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

    // FIXME: WdfDeviceCreateDeviceInterface

    /// Creates a symbolic link to the specified device
    ///
    /// Once a symbolic link is created, applications can access the device
    /// via the symbolic link. However, the usual way of providing applications
    /// access to devices is via [device interfaces].
    ///
    /// The symbolic link is removed upon surprise removal of the device, allowing
    /// reuse by a new instance of the device.
    ///
    /// [device interfaces]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/using-device-interfaces
    ///
    /// ## Return Value
    ///
    /// `STATUS_SUCCESS` if the symbolic link was successfully created successfully, otherwise:
    ///
    /// - `STATUS_INSUFFICIENT_RESOURCES` if there isn't enough space to store the device name.
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `PASSIVE_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    #[must_use]
    pub unsafe fn WdfDeviceCreateSymbolicLink(
        Device: WDFDEVICE,                  // in
        SymbolicLinkName: PCUNICODE_STRING, // in
    ) -> NTSTATUS {
        dispatch!(WdfDeviceCreateSymbolicLink(Device, SymbolicLinkName))
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
    /// For more information about naming device objects, see Controlling Device Access in Framework-Based Drivers at
    /// <https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/controlling-device-access-in-kmdf-drivers>.
    ///
    /// ## Return Value
    ///
    /// `STATUS_SUCCESS` if the name was assigned successfully, otherwise:
    ///
    /// - `STATUS_INSUFFICIENT_RESOURCES` if there isn't enough space to store the device name.
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `PASSIVE_LEVEL`
    /// - ([ChildDeviceInitApi]) The child device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DeviceInitAPI]) The device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
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
    /// [InitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType2
    /// [InitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InitFreeDeviceCreateType4
    /// [PdoInitFreeDeviceCreateType2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreateType2
    /// [PdoInitFreeDeviceCreateType4]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreateType4
    /// [PdoDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoDeviceInitAPI
    /// [PdoInitFreeDeviceCallback]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCallback
    /// [PdoInitFreeDeviceCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoInitFreeDeviceCreate
    ///
    // TODO: As proper intra-doc links
    /// [`WdfPdoInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfpdo/nf-wdfpdo-wdfpdoinitallocate
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
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
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
        dispatch!(WdfDeviceInitFree(DeviceInit));
    }

    // FIXME: WdfDeviceInitRegisterPnpStateChangeCallback
    // FIXME: WdfDeviceInitRegisterPowerPolicyStateChangeCallback
    // FIXME: WdfDeviceInitRegisterPowerStateChangeCallback
    // FIXME: WdfDeviceInitSetCharacteristics
    // FIXME: WdfDeviceInitSetDeviceClass
    // FIXME: WdfDeviceInitSetDeviceType
    // FIXME: WdfDeviceInitSetExclusive

    /// Sets configuration information for the device's associated framework file objects
    ///
    /// Must be called before [`WdfDeviceCreate`].
    ///
    /// If the parent device object isn't set to [`SynchronizationScope::None`](crate::object::Synchronization::None) and
    /// [`ExecutionLevel::Passive`](crate::object::ExecutionLevel::Passive), the default object attributes cannot be used
    /// and must also specify [`SynchronizationScope::None`](crate::object::Synchronization::None),
    /// and [`ExecutionLevel::Passive`](crate::object::ExecutionLevel::Passive).
    ///
    /// This is to ensure that the [`EvtWdfDeviceFileCreate`], [`EvtWdfFileClose`], and [`EvtWdfFileCleanup`] are all called
    /// at IRQL `PASSIVE_LEVEL`.
    ///
    /// [`EvtWdfDeviceFileCreate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nc-wdfdevice-evt_wdf_device_file_create
    /// [`EvtWdfFileClose`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nc-wdfdevice-evt_wdf_file_close
    /// [`EvtWdfFileCleanup`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nc-wdfdevice-evt_wdf_file_cleanup
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([ChildDeviceInitApi]) The child device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DeviceInitAPI]) The device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([FileObjectConfigured]) [`WdfDeviceInitSetFileObjectConfig`] must be called before [`WdfRequestGetFileObject`]
    /// - ([PdoDeviceInitAPI]) The device object initialization methods and [`WdfPdoInitAllocate`] must be called before [`WdfDeviceCreate`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChildDeviceInitApi]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChildDeviceInitAPI
    /// [ControlDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceInitAPI
    /// [DeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeviceInitAPI
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [`FileObjectConfigured`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-FileObjectConfigured
    /// [PdoDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoDeviceInitAPI
    ///
    // TODO: As proper intra-doc links
    /// [`WdfRequestGetFileObject`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestgetfileobject
    pub unsafe fn WdfDeviceInitSetFileObjectConfig(
        DeviceInit: PWDFDEVICE_INIT,                          // in
        FileObjectConfig: PWDF_FILEOBJECT_CONFIG,             // in
        FileObjectAttributes: Option<PWDF_OBJECT_ATTRIBUTES>, // in, optional
    ) {
        dispatch!(WdfDeviceInitSetFileObjectConfig(
            DeviceInit,
            FileObjectConfig,
            FileObjectAttributes.unwrap_or(WDF_NO_OBJECT_ATTRIBUTES!())
        ));
    }

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
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([ChildDeviceInitApi]) The child device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([ControlDeviceInitAPI]) The control device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DeviceInitAPI]) The device object initialization methods must be called before [`WdfDeviceCreate`]
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
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
    /// [`WdfDeviceInitSetIoTypeEx`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nf-wdfdevice-wdfdeviceinitsetiotypeex
    /// [`WdfPdoInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfpdo/nf-wdfpdo-wdfpdoinitallocate
    pub unsafe fn WdfDeviceInitSetIoType(
        DeviceInit: PWDFDEVICE_INIT, // in
        IoType: WDF_DEVICE_IO_TYPE,  // in
    ) {
        dispatch!(WdfDeviceInitSetIoType(DeviceInit, IoType));
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
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `PASSIVE_LEVEL`
    /// - ([ChangeQueueState]) Must not be called concurrently with other queue state-changing functions
    /// - ([DriverAttributeChanged]) The existing execution level or synchronization scope must not be modified(?)
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
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
            Driver.map(|p| p as _).unwrap_or(WDF_NO_HANDLE!()),
        ))
    }

    /// Gets the framework driver object, or `None` if the driver hasn't been created yet.
    ///
    /// ## Safety
    ///
    /// Must not be called while [`WdfDriverCreate`] is executing
    #[must_use]
    pub unsafe fn WdfGetDriver() -> Option<WDFDRIVER> {
        // SAFETY: Location to globals is immutable by the time we're loaded
        let globals = unsafe { wdf_kmdf_sys::WdfDriverGlobals };

        // SAFETY: Caller handles racing accesses, and is effectively initialized & immutable after WdfDriverCreate
        let globals = unsafe { &*globals };

        let driver = globals.Driver;

        Some(driver).filter(|drv| drv.is_null())
    }

    // endregion: wdfdriver

    // region: wdfio

    /// Creates an IO queue for the given device
    ///
    /// A device can have multiple IO queues.
    ///
    /// While the default parent object for the queue is the passed-in device object,
    /// the `ParentObject` in the object attributes can be set to another framework
    /// device object, or an object who has a framework device object as an ancestor.
    /// The queue object will be deleted when the parent object is deleted.
    ///
    /// If [`EvtCleanupCallback`] or [`EvtDestroyCallback`] is specified,
    /// the framework will always call those callbacks at IRQL `PASSIVE_LEVEL`.
    ///
    /// ## Return Value
    ///
    /// The newly allocated queue object is stored in the place given by `Queue`.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if an input parameter is invalid.
    /// - `STATUS_INFO_LENGTH_MISMATCH` if the size of the [`WDF_IO_QUEUE_CONFIG`](wdf_kmdf_sys::WDF_IO_QUEUE_CONFIG)
    /// - `STATUS_POWER_STATE_INVALID` if a power manangement operation is currently in progress
    /// - `STATUS_INSUFFICIENT_RESOURCES` if there is not enough resources to allocate the queue object
    /// - `STATUS_WDF_NO_CALLBACK` if no request handlers were specified, and the dispatching method is not
    ///   [`WdfIoQueueDispatchManual`](wdf_kmdf_sys::WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatch)
    /// - `STATUS_UNSUCCESSFUL` if a default queue is trying to be created, but a default queue already exists for the device,
    ///   or an internal error occured
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([ChangeQueueState]) Must not be called concurrently with other queue state-changing functions
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([DrvAckIoStop]) When the power-managed queue is getting powered down, remaining pending requests
    ///   should be acknowledge, cancelled, or completed as appropriate.
    ///   For self-managed IO requests, they should also be correctly handled inside of [`EvtDeviceSelfManagedIoSuspend`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChangeQueueState]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChangeQueueState
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DrvAckIoStop]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DrvAckIoStop
    /// [`EvtDeviceSelfManagedIoSuspend`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DrvAckIoStop
    #[must_use]
    pub unsafe fn WdfIoQueueCreate(
        Device: WDFDEVICE,                               // in
        Config: PWDF_IO_QUEUE_CONFIG,                    // in
        QueueAttributes: Option<PWDF_OBJECT_ATTRIBUTES>, // in, optional
        Queue: Option<&mut WDFQUEUE>,                    // in, optional
    ) -> NTSTATUS {
        dispatch!(WdfIoQueueCreate(
            Device,
            Config,
            QueueAttributes.unwrap_or(WDF_NO_OBJECT_ATTRIBUTES!()),
            Queue.map_or(core::ptr::null_mut(), |p| p as _)
        ))
    }

    // endregion: wdfio

    // region: wdfobject

    /// Deletes the framework object and its descendants
    ///
    /// If there are any other references to the object, the object will only be deleted once all of the other references are dereferenced.
    ///
    /// The following objects cannot be deleted by `WdfObjectDelete` since they are managed by the framework:
    ///
    /// - `WDFCHILDLIST`
    /// - `WDFDEVICE`, unless it is a control device which sometimes needs to be deleted
    /// - `WDFDRIVER`
    /// - `WDFFILEOBJECT`
    /// - `WDFINTERRUPT`
    /// - `WDFQUEUE`, if it represents the default IO queue for an object, or if [`WdfDeviceConfigureRequestDispatching`]
    ///   to setup the queue to receive all IO requests of a given type
    /// - `WDFUSBPIPE`
    /// - `WDFUSBDEVICEINTERFACE`
    /// - `WDFWMIPROVIDER`
    /// - `WDFIORESLIST`
    /// - `WDFCMRESLIST`
    /// - `WDFIORESREQLIST`
    ///
    /// There are no guarantees on the order in which child objects are deleted, and the object may still remain undeleted for an unspecified amount
    /// of time after `WdfObjectDelete` is called.
    ///
    /// ## Note about deleting timer objects
    ///
    /// Timer objects cannot be deleted inside of callbacks that are called at `PASSIVE_LEVEL`.
    /// See [`EVT_WDF_TIMER`] for more information.
    ///
    /// [`EVT_WDF_TIMER`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdftimer/nc-wdftimer-evt_wdf_timer#remarks
    ///
    /// ## Safety
    ///
    /// - `Handle` must point to a valid WDFOBJECT
    /// - ([KmdfIrqlDependent], [KmdfIrql2])
    ///   If a Control Device or common buffer is being deleted, IRQL: `PASSIVE_LEVEL`,
    ///   otherwise IRQL: <= `DISPATCH_LEVEL`
    /// - ([AddPdotoStaticChildlist]) For a PDO device, after calling [`WdfPdoInitAllocate`] and [`WdfDeviceCreate`], [`WdfFdoAddStaticChild`] must be called too
    /// - ([ControlDeviceDeleted]) If a PnP driver creates a control device object, there are additional constraints on when it must be deleted
    /// - ([CtlDeviceFinishInitDeviceAdd]) If a PnP driver creates a control device object in [`EvtDriverDeviceAdd`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`EvtDriverDeviceAdd`]
    /// - ([CtlDeviceFinishInitDrEntry]) If a PnP driver creates a control device object in [`DriverEntry`], [`WdfControlFinishInitializing`]
    ///   must be called after [`WdfDeviceCreate`] but before the end of [`DriverEntry`]
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([InvalidReqAccessLocal]) Locally created requests must not be accessed after being completed or cancelled.
    /// - ([MemAfterReqCompletedIntIoctlA]) Inside of [`EvtIoInternalDeviceControl`], the framework memory object must not be accessed after the IO request is completed
    /// - ([MemAfterReqCompletedIoctlA]) Inside of [`EvtIoDeviceControl`], the framework memory object must not be accessed after the IO request is completed
    /// - ([MemAfterReqCompletedReadA]) Inside of [`EvtIoRead`], the framework memory object must not be accessed after the IO request is completed
    /// - ([MemAfterReqCompletedWriteA]) Inside of [`EvtIoWrite`], the framework memory object must not be accessed after the IO request is completed
    /// - ([ReqDelete]) Driver-created requests must be deleted upon completion instead of being passed to the `WdfRequestCompleteXxx` family of functions
    /// - ([ReqSendFail]) The correct completion status must be set in cases where [`WdfRequestSend`] can fail
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [AddPdotoStaticChildlist]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-AddPdoToStaticChildList
    /// [ControlDeviceDeleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceDeleted
    /// [CtlDeviceFinishInitDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDeviceAdd
    /// [CtlDeviceFinishInitDrEntry]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDrEntry
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    /// [MemAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIntIoctlA
    /// [MemAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIoctlA
    /// [MemAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedReadA
    /// [MemAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedWriteA
    /// [ReqDelete]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqDelete
    /// [ReqSendFail]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqSendFail
    /// [`EvtIoInternalDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_internal_device_control
    /// [`EvtIoDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_device_control
    /// [`EvtIoRead`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_read
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    //
    // TODO: As proper intra-doc links
    /// [`WdfDeviceConfigureRequestDispatching`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nf-wdfobject-wdfobjectdelete
    /// [`WdfRequestSend`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestsend
    pub unsafe fn WdfObjectDelete(Object: WDFOBJECT) {
        dispatch!(WdfObjectDelete(Object))
    }

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

    // region: wdfsync

    /// Creates a framework spin-lock object
    ///
    /// By default, the parent object of the spin-lock is the driver object.
    /// The parent of the spin-lock can be changed using `SpinLockAttributes`, but if it is not changed
    /// the spin-lock object should be deleted once done with it, as otherwise instances of spin-locks will
    /// persist until the driver is unloaded.
    ///
    /// ## Return Value
    ///
    /// The newly allocated queue object is stored in the place given by `SpinLock`.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([ParentObjectCheckLock]) A parent object should be set for the lock object
    /// - ([WdfSpinLock]) Calls to [`WdfSpinLockAcquire`] should be balanced with the number of calls to [`WdfSpinLockRelease`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [ParentObjectCheckLock]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ParentObjectCheckLock
    /// [WdfSpinlock]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-WdfSpinlock
    #[must_use]
    pub unsafe fn WdfSpinLockCreate(
        SpinLockAttributes: Option<PWDF_OBJECT_ATTRIBUTES>, // in, optional
        SpinLock: &mut WDFSPINLOCK,                         // out
    ) -> NTSTATUS {
        dispatch!(WdfSpinLockCreate(
            SpinLockAttributes.unwrap_or(WDF_NO_OBJECT_ATTRIBUTES!()),
            SpinLock
        ))
    }

    /// Acquires the specified spin lock
    ///
    /// This also raises the IRQL level to `DISPATCH_LEVEL`
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - This cannot be called on the spin lock of a [`WDF_INTERRUPT_CONFIG`]
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([WdfSpinLock]) Calls to [`WdfSpinLockAcquire`] should be balanced with the number of calls to [`WdfSpinLockRelease`]
    /// - ([WdfSpinLockRelease]) Calls to [`WdfSpinLockRelease`] should be balanced with the number of calls to [`WdfSpinLockAcquire`]
    /// - ([ReqSendWhileSpinlock]) While a driver holds a spinlock, no IO requests should be sent, otherwise a deadlock could occur
    ///
    /// [`WDF_INTERRUPT_CONFIG`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfinterrupt/ns-wdfinterrupt-_wdf_interrupt_config
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [WdfSpinlock]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-WdfSpinlock
    /// [WdfSpinlockRelease]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-WdfSpinlockRelease
    /// [ReqSendWhileSpinlock]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqSendWhileSpinlock
    //
    // Note: while the original WDF documentation mentions that UMDF changes the IRQL to a passive level,
    // the SAL2 annotations for `WdfSpinLockAcquire` specifically indicate it should always raise it to `DISPATCH_LEVEL`
    pub unsafe fn WdfSpinLockAcquire(SpinLock: WDFSPINLOCK) {
        dispatch!(WdfSpinLockAcquire(SpinLock))
    }

    /// Releases the specified spin lock
    ///
    /// This also adjusts the IRQL level back to what it was before acquiring the spin lock
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: `DISPATCH_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([WdfSpinLock]) Calls to [`WdfSpinLockAcquire`] should be balanced with the number of calls to [`WdfSpinLockRelease`]
    /// - ([WdfSpinLockRelease]) Calls to [`WdfSpinLockRelease`] should be balanced with the number of calls to [`WdfSpinLockAcquire`]
    /// - ([ReqSendWhileSpinlock]) While a driver holds a spinlock, no IO requests should be sent, otherwise a deadlock could occur
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [WdfSpinlock]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-WdfSpinlock
    /// [WdfSpinlockRelease]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-WdfSpinlockRelease
    /// [ReqSendWhileSpinlock]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqSendWhileSpinlock
    pub unsafe fn WdfSpinLockRelease(SpinLock: WDFSPINLOCK) {
        dispatch!(WdfSpinLockRelease(SpinLock))
    }

    // endregion: wdfsync
}

pub mod object {

    use wdf_kmdf_sys::{
        WDFOBJECT, WDF_EXECUTION_LEVEL, WDF_OBJECT_ATTRIBUTES, WDF_OBJECT_CONTEXT_TYPE_INFO,
        WDF_SYNCHRONIZATION_SCOPE,
    };

    #[doc(hidden)]
    pub mod __macro_internals {
        pub use static_assertions::const_assert;
        pub use windows_kernel_sys::MEMORY_ALLOCATION_ALIGNMENT;
    }

    pub type ContextInfo = WDF_OBJECT_CONTEXT_TYPE_INFO;

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
        InheritFromParent = WDF_EXECUTION_LEVEL::WdfExecutionLevelInheritFromParent.0,
        /// Always execute event callbacks at IRQL == PASSIVE_LEVEL
        Passive = WDF_EXECUTION_LEVEL::WdfExecutionLevelPassive.0,
        /// Execute event callbacks at IRQL <= DISPATCH_LEVEL (KMDF only)
        Dispatch = WDF_EXECUTION_LEVEL::WdfExecutionLevelDispatch.0,
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
        InheritFromParent = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeInheritFromParent.0,
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
        Device = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeDevice.0,
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
        Queue = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeQueue.0,
        /// No synchronization is performed, so event callbacks may execute concurrently
        None = WDF_SYNCHRONIZATION_SCOPE::WdfSynchronizationScopeNone.0,
    }

    pub fn default_object_attributes<T: IntoContextSpace>() -> WDF_OBJECT_ATTRIBUTES {
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
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO)
        };
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
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO)
        };
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
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(handle, T::CONTEXT_INFO)
        };
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

pub mod sync {
    //! WDF-Based synchronization primatives

    use core::{
        cell::{Cell, UnsafeCell},
        marker::PhantomPinned,
        ops::{Deref, DerefMut},
        pin::Pin,
    };

    use pinned_init::{pin_data, PinInit};
    use wdf_kmdf_sys::WDFSPINLOCK;
    use windows_kernel_sys::Error;

    use crate::raw;

    /// A spin-lock based mutex protecting some data.
    ///
    /// The data is guaranteed to be pinned.
    #[pin_data]
    pub struct SpinMutex<T> {
        /// The backing spin lock
        spin_lock: SpinLock,
        /// If the mutex has been locked
        is_locked: Cell<bool>,
        /// Data we are protecting
        #[pin]
        data: UnsafeCell<T>,
    }

    impl<T> SpinMutex<T> {
        /// Creates a new mutex
        ///
        /// ## IRQL: <= Dispatch
        ///
        /// ## Errors
        ///
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn new(value: impl PinInit<T, Error>) -> impl PinInit<Self, Error> {
            pinned_init::try_pin_init!(SpinMutex {
                spin_lock: SpinLock::new()?,
                is_locked: Cell::new(false),
                data <- unsafe { pinned_init::pin_init_from_closure(move |slot: *mut UnsafeCell<T>| {
                    value.__pinned_init(slot.cast())
                })}
            }? Error)
        }

        /// Tries to acquire the mutex, returning a guard if successful
        ///
        /// ## IRQL: <= Dispatch
        pub fn try_lock(&self) -> Option<Pin<SpinMutexGuard<'_, T>>> {
            let _guard = self.spin_lock.acquire();

            if self.is_locked.get() {
                // Lock is already acquired
                return None;
            }

            self.is_locked.set(true);

            // SAFETY: Uhhhh
            Some(unsafe {
                Pin::new_unchecked(SpinMutexGuard {
                    mutex: self,
                    _pin: PhantomPinned,
                })
            })
        }

        /// Locks the mutex, busy-looping until the lock is acquired
        ///
        /// ## IRQL: <= Dispatch
        pub fn lock(&self) -> Pin<SpinMutexGuard<'_, T>> {
            loop {
                let Some(guard) = self.try_lock() else {
                    core::hint::spin_loop();
                    continue;
                };
                break guard;
            }
        }
    }

    // Can't send `SpinMutex` to another thread because we've guaranteed that the storage will never move
    // impl<T> !Send for SpinMutex<T> {}

    // Can send &SpinMutex<T> to other threads as we can only observe `T` changing when we hold the lock.
    //
    // `T` also needs to be `Send` as we need to be able to manifest a `&mut T` on any thread.
    // This is so that `SpinMutex<Rc<_>>` is invalid, as otherwise we could have any number of `Rc`'s on different threads.
    unsafe impl<T> Sync for SpinMutex<T> where T: Send {}

    /// Lock guard for a [`SpinMutex`]
    pub struct SpinMutexGuard<'a, T> {
        mutex: &'a SpinMutex<T>,
        _pin: PhantomPinned,
    }

    impl<'a, T> Drop for SpinMutexGuard<'a, T> {
        fn drop(&mut self) {
            let spin_guard = self.mutex.spin_lock.acquire();
            self.mutex.is_locked.set(false);
            drop(spin_guard)
        }
    }

    impl<'a, T> Deref for SpinMutexGuard<'a, T> {
        type Target = T;

        #[inline]
        fn deref(&self) -> &Self::Target {
            // SAFETY: We have exclusive access to the data
            unsafe { &*self.mutex.data.get() }
        }
    }

    impl<'a, T> DerefMut for SpinMutexGuard<'a, T> {
        #[inline]
        fn deref_mut(&mut self) -> &mut Self::Target {
            // SAFETY: We have exclusive access to the data
            unsafe { &mut *self.mutex.data.get() }
        }
    }

    /// Wrapper around a framework-based spin lock.
    pub struct SpinLock(WDFSPINLOCK);

    impl SpinLock {
        /// Creates a new spin lock
        ///
        /// ## IRQL: <= Dispatch
        ///
        /// ## Errors
        ///
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn new() -> Result<Self, Error> {
            let mut lock = core::ptr::null_mut();

            // SAFETY:
            // - Caller ensures we're calling this at the right IRQL
            // - We're manually managing the lifetime of the spinlock (easier that way)
            Error::to_err(unsafe { raw::WdfSpinLockCreate(None, &mut lock) })?;

            Ok(Self(lock))
        }

        /// Acquires the spin lock, returning a guard that releases the lock once dropped
        ///
        /// ## IRQL: <= Dispatch
        pub fn acquire(&self) -> SpinLockGuard<'_> {
            // SAFETY:
            // - We created the spin lock ourselves, so it's not part of an interrupt config struct
            // - Caller ensures we're calling this at the right IRQL
            unsafe {
                raw::WdfSpinLockAcquire(self.0);
            }

            SpinLockGuard { lock: self }
        }
    }

    impl Drop for SpinLock {
        fn drop(&mut self) {
            // - Cleanup callbacks are always called at either `DISPATCH_LEVEL` or `PASSIVE_LEVEL`
            // - We're always deleting a spin lock, which has no special deletion requirements
            unsafe { raw::WdfObjectDelete(self.0.cast()) }
        }
    }

    // Can manifest SpinLock to another thread
    unsafe impl Send for SpinLock {}
    // Can send &SpinLock to other threads (no interior mutability observable)
    unsafe impl Sync for SpinLock {}

    /// A guard for a [`SpinLock`]
    pub struct SpinLockGuard<'a> {
        lock: &'a SpinLock,
    }

    impl<'a> Drop for SpinLockGuard<'a> {
        fn drop(&mut self) {
            // SAFETY:
            // Having a guard guarantees we've called `WdfSpinLockAcquire`,
            // and also implies we're at IRQL `DISPATCH_LEVEL`
            unsafe { raw::WdfSpinLockRelease(self.lock.0) }
        }
    }
}

pub mod driver {
    use core::marker::PhantomData;

    use pinned_init::PinInit;
    use vtable::vtable;
    use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDRIVER, WDFOBJECT, WDF_DRIVER_INIT_FLAGS};
    use windows_kernel_rs::{string::unicode_string::NtUnicodeStr, DriverObject};
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
        /// ## Safety
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

    #[derive(Default, Clone, Copy)]
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
        /// ## Errors
        ///
        /// - `STATUS_INVALID_PARAMETER` if non-pnp mode is specified, but `device_add` is also specified
        /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
        ///
        /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
        /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
        pub fn create<F, I>(
            driver_object: DriverObject,
            registry_path: NtUnicodeStr<'_>,
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
            // In fact, the framework destroys itself after the EvtUnloadCallback,
            // so it's best to always drop the driver context area in our unload
            // trampoline.
            let mut object_attrs = default_object_attributes::<T>();

            let mut driver_config = wdf_kmdf_sys::WDF_DRIVER_CONFIG::init(
                T::HAS_DEVICE_ADD.then_some(Self::__dispatch_driver_device_add),
            );
            driver_config.EvtDriverUnload = Some(Self::__dispatch_driver_unload);

            if matches!(config.pnp_mode, PnpMode::NonPnp) {
                driver_config.DriverInitFlags |=
                    WDF_DRIVER_INIT_FLAGS::WdfDriverInitNonPnpDriver.0 as u32;
            }

            // NOTE: This is always available since we're targeting KMDF versions after 1.5
            if let Some(tag) = config.pool_tag {
                driver_config.DriverPoolTag = tag;
            }

            // Make it!
            let mut handle = {
                let driver_object = driver_object.into_raw();

                // SAFETY: Replaced with the real driver pointer next
                let mut handle = unsafe { DriverHandle::wrap(core::ptr::null_mut()) };

                // SAFETY: Owned `DriverObject` means that it only gets called once
                unsafe {
                    Error::to_err(raw::WdfDriverCreate(
                        driver_object,
                        registry_path.as_ptr().cast(),
                        Some(&mut object_attrs),
                        &mut driver_config,
                        Some(&mut handle.0),
                    ))?;
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

            // And then drop it!
            // SAFETY:
            // Guaranteed to be unused by this point, since the destroy callback is the last one
            // called.
            // Object initialization guarantees that this was valid initialized memory
            unsafe { core::ptr::drop_in_place(context_space) };
        }
    }
}
