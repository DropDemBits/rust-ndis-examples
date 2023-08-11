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
        PCWDF_OBJECT_CONTEXT_TYPE_INFO, PFN_WDF_IO_QUEUE_STATE, PWDFDEVICE_INIT,
        PWDF_DRIVER_CONFIG, PWDF_FILEOBJECT_CONFIG, PWDF_IO_QUEUE_CONFIG, PWDF_OBJECT_ATTRIBUTES,
        WDFCONTEXT, WDFDEVICE, WDFDRIVER, WDFFILEOBJECT, WDFOBJECT, WDFQUEUE, WDFREQUEST,
        WDFSPINLOCK, WDF_DEVICE_IO_TYPE, WDF_NO_CONTEXT, WDF_NO_HANDLE, WDF_NO_OBJECT_ATTRIBUTES,
    };
    use windows_kernel_sys::{
        KPROCESSOR_MODE, NTSTATUS, PCUNICODE_STRING, PDRIVER_OBJECT, PMDL, PVOID, ULONG_PTR,
    };

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
    ///
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
    /// If the parent device object isn't set to [`SynchronizationScope::None`](crate::object::SynchronizationScope::None) and
    /// [`ExecutionLevel::Passive`](crate::object::ExecutionLevel::Passive), the default object attributes cannot be used
    /// and must also specify [`SynchronizationScope::None`](crate::object::SynchronizationScope::None),
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
    /// [FileObjectConfigured]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-FileObjectConfigured
    /// [PdoDeviceInitAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-PdoDeviceInitAPI
    ///
    // TODO: As proper intra-doc links
    /// [`WdfPdoInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfpdo/nf-wdfpdo-wdfpdoinitallocate
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
    /// - ([MiniportOnlyWdmDevice]) Non-miniport drivers must not use [`IoCreateDevice`] and [`IoCreateDeviceSecure`], and instead must
    ///   use the framework device creation functions
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChangeQueueState]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChangeQueueState
    /// [DriverAttributeChanged]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverAttributeChanged
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [MiniportOnlyWdmDevice]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MiniportOnlyWdmDevice
    /// [`IoCreateDevice`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdm/nf-wdm-iocreatedevice
    /// [`IoCreateDeviceSecure`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdmsec/nf-wdmsec-wdmlibiocreatedevicesecure
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
    /// [`EvtCleanupCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_cleanup
    /// [`EvtDestroyCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_destroy
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
    ///   [`WdfIoQueueDispatchManual`](wdf_kmdf_sys::WDF_IO_QUEUE_DISPATCH_TYPE::WdfIoQueueDispatchManual)
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

    /// Informs the framework to stop queuing IO requests to the queue
    /// and cancel any unprocessed and driver-owned cancellable requests
    ///
    /// Returns after all of the unprocessed and driver-owned requests are
    /// completed or canceled.
    ///
    /// After purging, an IO queue can be restarted using [`WdfIoQueueStart`].
    ///
    /// If the framework receives additional requests while purging the queue,
    /// it completes the requests with a status of `STATUS_INVALID_DEVICE_STATE`.
    ///
    /// For more information about queue management, see [Managing I/O Queues]
    ///
    /// [Managing I/O Queues]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/managing-i-o-queues
    ///
    /// ## Note
    ///
    /// This must not be called from the following queue object event callbacks for **any queue**:
    ///
    /// - [`EvtIoDefault`]
    /// - [`EvtIoRead`]
    /// - [`EvtIoWrite`]
    /// - [`EvtIoDeviceControl`]
    /// - [`EvtIoInternalDeviceControl`]
    ///
    /// See [When CAN You Call `WdfIoQueuePurgeSynchronously`] for why this is the case.
    ///
    /// [`EvtIoDefault`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_default
    /// [`EvtIoRead`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_read
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    /// [`EvtIoDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_device_control
    /// [`EvtIoInternalDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_internal_device_control
    /// [When CAN You Call `WdfIoQueuePurgeSynchronously`]: https://www.osronline.com/article.cfm%5Eid=614.htm
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([ChangeQueueState]) Must not be called concurrently with other queue state-changing functions
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([EvtSurpriseRemoveNoSuspendQueue]) While inside [`EvtDeviceSurpriseRemoval`], queues must not be drained, stopped, or purged,
    ///   and should instead be using the self-managed IO callback functions
    /// - ([NoIoQueuePurgeSynchronously]) See note above
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [ChangeQueueState]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ChangeQueueState
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [EvtSurpriseRemoveNoSuspendQueue]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtSurpriseRemoveNoSuspendQueue
    /// [`EvtDeviceSurpriseRemoval`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nc-wdfdevice-evt_wdf_device_surprise_removal
    /// [NoIoQueuePurgeSynchronously]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/noioqueuepurgesynchronously
    pub unsafe fn WdfIoQueuePurgeSynchronously(Queue: WDFQUEUE) {
        dispatch!(WdfIoQueuePurgeSynchronously(Queue))
    }

    /// (Un)registers an event callback for when the IO queue goes from empty to non-empty
    ///
    /// This can only be used for IO queues that use [manual dispatching].
    ///
    /// `Context` is passed to the [`EvtIoQueueState`] callback, but it is not required and can be `None`.
    ///
    /// The callback is invoked even when there are still requests that were taken out of the queue but
    /// not completed yet as the queue is still considered empty in that case. Once the callback is registered
    /// it may also be invoked before the end of the call to [`WdfIoQueueReadyNotify`].
    /// The callback is invoked at the `ExecutionLevel` of the queue.
    ///
    /// The [`EvtIoQueueState`] callback typically consumes all of the requests that have arrived since the last
    /// time the callback was called using [`WdfIoQueueRetrieveNextRequest`] or [`WdfIoQueueRetrieveRequestByFileObject`].
    ///
    /// To stop the given callback from being called, [`WdfIoQueueReadyNotify`] can be called again with `QueueReady` as
    /// `None`, but the queue must be stopped first using [`WdfIoQueueStop`] or [`WdfIoQueueStopSynchronously`].
    /// [`WdfIoQueueStart`] can then be used to restart the queue.
    ///
    /// For more information about [`WdfIoQueueReadyNotify`], see [Dispatching Methods for IO requests].
    ///
    /// [manual dispatching]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/dispatching-methods-for-i-o-requests
    /// [`EvtIoQueueState`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_state
    /// [Dispatching Methods for IO requests]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/dispatching-methods-for-i-o-requests
    ///
    /// ## Return value
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if `Queue` is invalid
    /// - `STATUS_INVALID_DEVICE_REQUEST` if
    ///   - The `DispatchType` of the queue is not `WdfIoQueueDispatchManual`
    ///   - The queue already has a [`EvtIoQueueState`] callback and a new one is trying to be registered
    ///   - The existing [`EvtIoQueueState`] callback is trying to be unregistered but one hasn't been registered
    ///     or the queue is started and [`WdfIoQueueStop`] or [`WdfIoQueueStopSynchronously`] hasn't been called
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    ///
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    //
    // TODO: As proper intra-doc links
    /// [`WdfIoQueueStop`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nf-wdfio-wdfioqueuestop
    /// [`WdfIoQueueStopSynchronously`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nf-wdfio-wdfioqueuestopsynchronously
    /// [`WdfIoQueueRetrieveRequestByFileObject`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nf-wdfio-wdfioqueueretrieverequestbyfileobject
    #[must_use]
    pub unsafe fn WdfIoQueueReadyNotify(
        Queue: WDFQUEUE,                    // in
        QueueReady: PFN_WDF_IO_QUEUE_STATE, // in, optional
        Context: Option<WDFCONTEXT>,        // in, optional
    ) -> NTSTATUS {
        dispatch!(WdfIoQueueReadyNotify(
            Queue,
            QueueReady,
            Context.unwrap_or(WDF_NO_CONTEXT!())
        ))
    }

    /// Retrieves the next available IO request from the queue
    ///
    /// If the queue has been configured for manual dispatch, [`WdfIoQueueRetrieveNextRequest`] is typically called to take requests
    /// from the queue. [`WdfIoQueueRetrieveNextRequest`] can also be called on queues configured for sequential dispatch. For more
    /// information on using [`WdfIoQueueRetrieveNextRequest`] with the manual or sequential dispatching modes, see [Dispatching Methods for IO requests].
    ///
    /// After the request is taken, the caller takes ownership of the request and must [process the IO request].
    /// For more information on request ownership, see [Request Ownership].
    ///
    /// For more information about [`WdfIoQueueRetrieveNextRequest`], see [Dispatching Methods for IO requests].
    ///
    /// [process the IO request]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/accessing-data-buffers-in-wdf-drivers
    /// [Request Ownership]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/request-ownership
    /// [Dispatching Methods for IO requests]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/dispatching-methods-for-i-o-requests
    ///
    /// ## Return value
    ///
    /// `OutRequest` is set to the next request entry, or if the queue was empty or the last request has already been retrieved
    /// it is set to `NULL`.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if `Queue` is invalid
    /// - `STATUS_NO_MORE_ENTRIES` if the end of the queue was reached
    /// - `STATUS_INVALID_DEVICE_STATE` if the queue is configured for parallel dispatch
    /// - `STATUS_WDF_PAUSED` if the queue is [power-managed] and either its device is in a low power state, or the queue has been stopped
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    ///
    /// [power-managed]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/using-power-managed-i-o-queues
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DoubleCompletion]) Requests must not be completed multiple times
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([WdfIoQueueRetrieveNextRequest]) Must not call [`WdfIoQueueRetrieveNextRequest`] after calling [`WdfIoQueueFindRequest`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DoubleCompletion]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DoubleCompletion
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [WdfIoQueueRetrieveNextRequest]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-WdfIoQueueRetrieveNextRequest
    //
    // TODO: As proper intra-doc links
    /// [`WdfIoQueueFindRequest`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nf-wdfio-wdfioqueuefindrequest
    #[must_use]
    pub unsafe fn WdfIoQueueRetrieveNextRequest(
        Queue: WDFQUEUE,             // in
        OutRequest: &mut WDFREQUEST, // out
    ) -> NTSTATUS {
        dispatch!(WdfIoQueueRetrieveNextRequest(Queue, OutRequest))
    }

    /// Enables the IO queue to start delivering and receiving new IO requests
    ///
    /// If IO requests are present when [`WdfIoQueueStart`] is called, the queue's [request handlers]
    /// can be called before the end of the call to [`WdfIoQueueStart`]. Therefore, any locks that
    /// the request handlers may acquire must not be held before calling [`WdfIoQueueStart`], otherwise
    /// a deadlock will happen.
    ///
    /// For more information about queue management, see [Managing I/O Queues]
    ///
    /// [request handlers]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/request-handlers
    /// [Managing I/O Queues]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/managing-i-o-queues
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    pub unsafe fn WdfIoQueueStart(Queue: WDFQUEUE) {
        dispatch!(WdfIoQueueStart(Queue))
    }

    // endregion: wdfio

    // region: wdfobject

    /// Allocates additional context space for the object
    ///
    /// A context space can be created when creating an object for the first time,
    /// but this can be used to allocate additional context spaces after an object is created.
    /// Each `WdfObjectAllocateContext` must use a unique context type, as only one instance of
    /// a context type is allowed per object.
    ///
    /// Each context space can have its own [`EvtCleanupCallback`] and [`EvtDestroyCallback`] associated with it,
    /// but the other properties (`ExecutionLevel`, `SynchronizationScope`, and `ParentObject`) are set on initial
    /// object creation and should not be specified.
    ///
    /// Upon allocating the context space, it is zero-initialized.
    ///
    /// For more information about object context space, see [Framework Object Context Space]
    ///
    /// [`EvtCleanupCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_cleanup
    /// [`EvtDestroyCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_destroy
    /// [Framework Object Context Space]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-life-cycle
    ///
    /// ## Return value
    ///
    /// The newly allocated context space is stored in the place given by `Context`.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if an input parameter is invalid
    /// - `STATUS_OBJECT_NAME_INVALID` if the `ContextTypeInfo` member of `ContextAttributes` is invalid
    /// - `STATUS_INSUFFICIENT_RESOURCES` if the context space couldn't be allocated
    /// - `STATUS_DELETE_PENDING` if `Handle` is being deleted (no context space is allocated in this case)
    /// - Other `NTSTATUS` values (see [Framework Object Creation Errors] and [`NTSTATUS` values])
    ///
    /// [Framework Object Creation Errors]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-creation-errors
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - IRQL: <= `DISPATCH_LEVEL`
    /// - `ContextPointer` must be a context for an object
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    ///
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    #[must_use]
    pub unsafe fn WdfObjectAllocateContext(
        Handle: WDFOBJECT,                         // in
        ContextAttributes: PWDF_OBJECT_ATTRIBUTES, // in
        Context: &mut PVOID,                       // out
    ) -> NTSTATUS {
        dispatch!(WdfObjectAllocateContext(Handle, ContextAttributes, Context))
    }

    /// Returns the original framework object handle that the given context space is associated with
    ///
    /// For more information about object context space, see [Framework Object Context Space]
    ///
    /// [Framework Object Context Space]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-object-life-cycle
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - IRQL: Any
    /// - `ContextPointer` must be a context for an object
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    ///
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    #[must_use]
    pub unsafe fn WdfObjectContextGetObject(ContextPointer: PVOID) -> WDFOBJECT {
        dispatch!(WdfObjectContextGetObject(ContextPointer))
    }

    // FIXME: WdfObjectCreate

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
    /// - ([MemAfterReqCompletedIntIoctlA], [MemAfterReqCompletedIoctlA], [MemAfterReqCompletedReadA], [MemAfterReqCompletedWriteA])
    ///   The framework memory object must not be accessed after the IO request is completed
    /// - ([ReqDelete]) Driver-created requests must be deleted upon completion instead of being passed to the `WdfRequestCompleteXxx` family of functions
    /// - ([ReqSendFail]) The correct completion status must be set in cases where [`WdfRequestSend`] can fail
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [AddPdotoStaticChildlist]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-AddPdoToStaticChildList
    /// [ControlDeviceDeleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ControlDeviceDeleted
    /// [CtlDeviceFinishInitDeviceAdd]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CtlDeviceFinishInitDeviceAdd
    /// [`EvtDriverDeviceAdd`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdriver/nc-wdfdriver-evt_wdf_driver_device_add
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
    //
    // TODO: As proper intra-doc links
    /// [`WdfDeviceConfigureRequestDispatching`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nf-wdfobject-wdfobjectdelete
    /// [`WdfRequestSend`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestsend
    /// [`WdfPdoInitAllocate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfpdo/nf-wdfpdo-wdfpdoinitallocate
    /// [`WdfFdoAddStaticChild`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdffdo/nf-wdffdo-wdffdoaddstaticchild
    pub unsafe fn WdfObjectDelete(Object: WDFOBJECT) {
        dispatch!(WdfObjectDelete(Object))
    }

    // FIXME: WdfObjectDereferenceActual

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

    // FIXME: WdfObjectReferenceActual

    // endregion: wdfobject

    // region: wdfrequest

    /// Completes the specified IO request with the specified the completion status
    ///
    /// For read, write, and IOCTL requests, [`WdfRequestCompleteWithInformation`] must be called so as
    /// to provide sizing information. See the associated documentation for more information.
    ///
    /// `Status` can be any `NTSTATUS` value, though some of the common ones are:
    ///
    /// - `STATUS_SUCCESS` upon successful completion of the request
    /// - `STATUS_CANCELLED` if the driver is cancelling the request
    /// - `STATUS_UNSUCCESSFUL` if an error occurred during the request
    ///
    /// Drivers higher up on the driver stack can call [`WdfRequestGetStatus`] to obtain the `Status` that
    /// was passed here, which is usually called inside of a [`CompletionRoutine`].
    ///
    /// This takes ownership of the passed in `Request`, and unless another reference has been added via
    /// [`WdfObjectReference`], the request will be dropped (and thus invoking `Request`'s
    /// [`EvtCleanupCallback`]). Even in the case of additional references, the request's associated IRP
    /// structure should not be accessed. This extends to calling methods that access it, including
    /// [`WdfRequestRetrieveOutputBuffer`] or [`WdfRequestRetrieveInputBuffer`].
    ///
    /// By default, the framework uses a default value that is passed to the system to boost the run-time
    /// priority of the invoking thread (See [Specifying Priority Boosts When Completing I/O Requests]
    /// for more info). [`WdfRequestCompleteWithPriorityBoost`] can be called instead to change the boost
    /// value.
    ///
    /// More information on completeing requests can be found at [Completing I/O Requests].
    ///
    /// [Specifying Priority Boosts When Completing I/O Requests]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/specifying-priority-boosts-when-completing-i-o-requests
    /// [Completing I/O Requests]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/completing-i-o-requests
    /// [`CompletionRoutine`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nc-wdfrequest-evt_wdf_request_completion_routine
    /// [`EvtCleanupCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_cleanup
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([BufAfterReqCompletedIntIoctl], [BufAfterReqCompletedIntIoctlA], [BufAfterReqCompletedIoctl], [BufAfterReqCompletedIoctlA], [BufAfterReqCompletedRead], [BufAfterReqCompletedReadA], [BufAfterReqCompletedWrite], [BufAfterReqCompletedWriteA])
    ///   The IO request buffer associated with the request must not be accessed after the request is completed.
    /// - ([CompleteCanceledReq]) If a request has already been cancelled, it cannot also be completed.
    /// - ([DeferredRequestCompleted]) If a request is deferred for later completion, it must be completed in the deferred processing callback function,
    ///   unless it has been forwarded (and delivered) to the framework, or stopped processing via [`WdfRequestStopAcknowledge`].
    /// - ([DoubleCompletion], [DoubleCompletionLocal]) Requests must not be completed multiple times
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([EvtIoStopCancel]) A request from [`EvtIoStop`] which is uncancellabe must be either completed using one of the `WdfRequestCompleteXxx` functions, or requeued using [`WdfRequestStopAcknowledge`]
    /// - ([EvtIoStopCompleteOrStopAck]) A request from [`EvtIoStop`] must either be completed using one of the `WdfRequestCompleteXxx` functions, or requeued using [`WdfRequestStopAcknowledge`]
    /// - ([EvtSurpriseRemoveNoRequestComplete]) Requests from [`EvtDeviceSurpriseRemoval`] must not be completed, and instead must use self-managed IO callback functions
    /// - ([NoCancelFromEvtSurpriseRemove]) Requests from [`EvtDeviceSurpriseRemoval`] must not be completed, and instead must use self-managed IO callback functions
    /// - ([InvalidReqAccess]) Requests must not be accessed after having been completed or cancelled
    /// - ([MarkCancOnCancReqLocal]) Requests cannot be marked cancelable multiple times by [`WdfRequestMarkCancelable`]
    /// - ([MdlAfterReqCompletedIntIoctl], [MdlAfterReqCompletedIntIoctlA], [MdlAfterReqCompletedIoctl], [MdlAfterReqCompletedIoctlA], [MdlAfterReqCompletedRead], [MdlAfterReqCompletedReadA], [MdlAfterReqCompletedWrite], [MdlAfterReqCompletedWriteA])
    ///   The MDL associated with the request must not be accesd after the request is completed
    /// - ([MemAfterReqCompletedIntIoctl], [MemAfterReqCompletedIntIoctlA], [MemAfterReqCompletedIoctl], [MemAfterReqCompletedIoctlA], [MemAfterReqCompletedRead], [MemAfterReqCompletedReadA], [MemAfterReqCompletedWrite], [MemAfterReqCompletedWriteA])
    ///   The framework memory object must not be accessed after the IO request is completed
    /// - ([ReqDelete]) Driver-created requests must be deleted upon completion instead of being passed to the `WdfRequestCompleteXxx` family of functions
    /// - ([ReqIsCancOnCancReq]) [`WdfRequestIsCanceled`] must only be called on requests not marked as cancelable
    /// - ([ReqNotCanceledLocal])
    /// - ([ReqSendFail]) The correct completion status must be set in cases where [`WdfRequestSend`] can fail
    /// - ([RequestCompleted]) Requests from the default IO queue must either be completed, deferred, or forwarded to another queue
    /// - ([RequestCompletedLocal]) Requests from [`EvtIoDefault`], [`EvtIoRead`], [`EvtIoWrite`], [`EvtIoDeviceControl`], and [`EvtIoInternalDeviceControl`]
    ///   should either be completed or marked as cancelable using [`WdfRequestMarkCancelable`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [BufAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctl
    /// [BufAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctlA
    /// [BufAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctl
    /// [BufAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctlA
    /// [BufAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedRead
    /// [BufAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedReadA
    /// [BufAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWrite
    /// [BufAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWriteA
    /// [CompleteCanceledReq]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CompleteCanceledReq
    /// [DeferredRequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeferredRequestCompleted
    /// [DoubleCompletion]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DoubleCompletion
    /// [DoubleCompletionLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DoubleCompletionLocal
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [EvtIoStopCancel]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtIoStopCancel
    /// [`EvtIoStop`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_stop
    /// [EvtIoStopCompleteOrStopAck]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtIoStopCompleteOrStopAck
    /// [EvtSurpriseRemoveNoRequestComplete]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtSurpriseRemoveNoRequestComplete
    /// [`EvtDeviceSurpriseRemoval`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nc-wdfdevice-evt_wdf_device_surprise_removal
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [MarkCancOnCancReqLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MarkCancOnCancReqLocal
    /// [MdlAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIntIoctl
    /// [MdlAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIntIoctlA
    /// [MdlAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIoctl
    /// [MdlAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIoctlA
    /// [MdlAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedRead
    /// [MdlAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedReadA
    /// [MdlAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedWrite
    /// [MdlAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedWriteA
    /// [MemAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIntIoctl
    /// [MemAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIntIoctlA
    /// [MemAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIoctl
    /// [MemAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIoctlA
    /// [MemAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedRead
    /// [MemAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedReadA
    /// [MemAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedWrite
    /// [MemAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedWriteA
    /// [NoCancelFromEvtSurpriseRemove]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-NoCancelFromEvtSurpriseRemove
    /// [ReqDelete]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqDelete
    /// [ReqIsCancOnCancReq]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqIsCancOnCancReq
    /// [ReqNotCanceledLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqNotCanceledLocal
    /// [ReqSendFail]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqSendFail
    /// [RequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-RequestCompleted
    /// [RequestCompletedLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-RequestCompletedLocal
    /// [`EvtIoDefault`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_default
    /// [`EvtIoRead`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_read
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    /// [`EvtIoDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_device_control
    /// [`EvtIoInternalDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_internal_device_control
    //
    // TODO: As proper-intra doc links
    /// [`WdfRequestGetStatus`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nc-wdfrequest-evt_wdf_request_completion_routine
    /// [`WdfObjectReference`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/wdfobjectreference
    /// [`WdfRequestCompleteWithPriorityBoost`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestcompletewithpriorityboost
    /// [`WdfRequestStopAcknowledge`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequeststopacknowledge
    /// [`WdfRequestMarkCancelable`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestmarkcancelable
    /// [`WdfRequestIsCanceled`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestiscanceled
    /// [`WdfRequestSend`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestsend
    pub unsafe fn WdfRequestComplete(Request: WDFREQUEST, Status: NTSTATUS) {
        dispatch!(WdfRequestComplete(Request, Status))
    }

    /// Stores the completion information and the completes the specified IO request with the specified completion status
    ///
    /// This is equivalent to calling [`WdfRequestSetInformation`] and then calling [`WdfRequestComplete`].
    ///
    /// For read, write, and IOCTL requests, [`WdfRequestCompleteWithInformation`] must be called so as
    /// to provide sizing information. See the associated documentation for more information.
    /// [`WdfRequestComplete`] can also be called for requests that never transfer data.
    ///
    /// `Status` can be any `NTSTATUS` value, though some of the common ones are:
    ///
    /// - `STATUS_SUCCESS` upon successful completion of the request
    /// - `STATUS_CANCELLED` if the driver is cancelling the request
    /// - `STATUS_UNSUCCESSFUL` if an error occurred during the request
    ///
    /// Drivers higher up on the driver stack can call [`WdfRequestGetStatus`] to obtain the `Status` that
    /// was passed here, which is usually called inside of a [`CompletionRoutine`].
    ///
    /// This takes ownership of the passed in `Request`, and unless another reference has been added via
    /// [`WdfObjectReference`], the request will be dropped (and thus invoking `Request`'s
    /// [`EvtCleanupCallback`]). Even in the case of additional references, the request's associated IRP
    /// structure should not be accessed. This extends to calling methods that access it, including
    /// [`WdfRequestRetrieveOutputBuffer`] or [`WdfRequestRetrieveInputBuffer`].
    ///
    /// By default, the framework uses a default value that is passed to the system to boost the run-time
    /// priority of the invoking thread (See [Specifying Priority Boosts When Completing I/O Requests]
    /// for more info). [`WdfRequestCompleteWithPriorityBoost`] can be called instead to change the boost
    /// value.
    ///
    /// More information on completeing requests can be found at [Completing I/O Requests].
    ///
    /// The [VirtualSerial2 driver sample] has a code example which shows how to use [`WdfRequestCompleteWithInformation`] to retrieve the number of bytes copied.
    ///
    /// [Specifying Priority Boosts When Completing I/O Requests]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/specifying-priority-boosts-when-completing-i-o-requests
    /// [Completing I/O Requests]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/completing-i-o-requests
    /// [`CompletionRoutine`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nc-wdfrequest-evt_wdf_request_completion_routine
    /// [`EvtCleanupCallback`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfobject/nc-wdfobject-evt_wdf_object_context_cleanup
    /// [VirtualSerial2 driver sample]: https://github.com/Microsoft/Windows-driver-samples/blob/df271b80bdbb556707d9b4af1b06151ded561884/serial/VirtualSerial2/queue.c#L542
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([BufAfterReqCompletedIntIoctl], [BufAfterReqCompletedIntIoctlA], [BufAfterReqCompletedIoctl], [BufAfterReqCompletedIoctlA], [BufAfterReqCompletedRead], [BufAfterReqCompletedReadA], [BufAfterReqCompletedWrite], [BufAfterReqCompletedWriteA])
    ///   The IO request buffer associated with the request must not be accessed after the request is completed.
    /// - ([CompleteCanceledReq]) If a request has already been cancelled, it cannot also be completed.
    /// - ([DeferredRequestCompleted]) If a request is deferred for later completion, it must be completed in the deferred processing callback function,
    ///   unless it has been forwarded (and delivered) to the framework, or stopped processing via [`WdfRequestStopAcknowledge`].
    /// - ([DoubleCompletion], [DoubleCompletionLocal]) Requests must not be completed multiple times
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([EvtIoStopCancel]) A request from [`EvtIoStop`] which is uncancellabe must be either completed using one of the `WdfRequestCompleteXxx` functions, or requeued using [`WdfRequestStopAcknowledge`]
    /// - ([EvtIoStopCompleteOrStopAck]) A request from [`EvtIoStop`] must either be completed using one of the `WdfRequestCompleteXxx` functions, or requeued using [`WdfRequestStopAcknowledge`]
    /// - ([EvtSurpriseRemoveNoRequestComplete]) Requests from [`EvtDeviceSurpriseRemoval`] must not be completed, and instead must use self-managed IO callback functions
    /// - ([NoCancelFromEvtSurpriseRemove]) Requests from [`EvtDeviceSurpriseRemoval`] must not be completed, and instead must use self-managed IO callback functions
    /// - ([InvalidReqAccess]) Requests must not be accessed after having been completed or cancelled
    /// - ([MarkCancOnCancReqLocal]) Requests cannot be marked cancelable multiple times by [`WdfRequestMarkCancelable`]
    /// - ([MdlAfterReqCompletedIntIoctl], [MdlAfterReqCompletedIntIoctlA], [MdlAfterReqCompletedIoctl], [MdlAfterReqCompletedIoctlA], [MdlAfterReqCompletedRead], [MdlAfterReqCompletedReadA], [MdlAfterReqCompletedWrite], [MdlAfterReqCompletedWriteA])
    ///   The MDL associated with the request must not be accesd after the request is completed
    /// - ([MemAfterReqCompletedIntIoctl], [MemAfterReqCompletedIntIoctlA], [MemAfterReqCompletedIoctl], [MemAfterReqCompletedIoctlA], [MemAfterReqCompletedRead], [MemAfterReqCompletedReadA], [MemAfterReqCompletedWrite], [MemAfterReqCompletedWriteA])
    ///   The framework memory object must not be accessed after the IO request is completed
    /// - ([ReqDelete]) Driver-created requests must be deleted upon completion instead of being passed to the `WdfRequestCompleteXxx` family of functions
    /// - ([ReqIsCancOnCancReq]) [`WdfRequestIsCanceled`] must only be called on requests not marked as cancelable
    /// - ([ReqNotCanceledLocal])
    /// - ([ReqSendFail]) The correct completion status must be set in cases where [`WdfRequestSend`] can fail
    /// - ([RequestCompleted]) Requests from the default IO queue must either be completed, deferred, or forwarded to another queue
    /// - ([RequestCompletedLocal]) Requests from [`EvtIoDefault`], [`EvtIoRead`], [`EvtIoWrite`], [`EvtIoDeviceControl`], and [`EvtIoInternalDeviceControl`]
    ///   should either be completed or marked as cancelable using [`WdfRequestMarkCancelable`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [BufAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctl
    /// [BufAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctlA
    /// [BufAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctl
    /// [BufAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctlA
    /// [BufAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedRead
    /// [BufAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedReadA
    /// [BufAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWrite
    /// [BufAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWriteA
    /// [CompleteCanceledReq]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-CompleteCanceledReq
    /// [DeferredRequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeferredRequestCompleted
    /// [DoubleCompletion]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DoubleCompletion
    /// [DoubleCompletionLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DoubleCompletionLocal
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [EvtIoStopCancel]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtIoStopCancel
    /// [`EvtIoStop`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_stop
    /// [EvtIoStopCompleteOrStopAck]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtIoStopCompleteOrStopAck
    /// [EvtSurpriseRemoveNoRequestComplete]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-EvtSurpriseRemoveNoRequestComplete
    /// [`EvtDeviceSurpriseRemoval`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/nc-wdfdevice-evt_wdf_device_surprise_removal
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [MarkCancOnCancReqLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MarkCancOnCancReqLocal
    /// [MdlAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIntIoctl
    /// [MdlAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIntIoctlA
    /// [MdlAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIoctl
    /// [MdlAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedIoctlA
    /// [MdlAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedRead
    /// [MdlAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedReadA
    /// [MdlAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedWrite
    /// [MdlAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MdlAfterReqCompletedWriteA
    /// [MemAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIntIoctl
    /// [MemAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIntIoctlA
    /// [MemAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIoctl
    /// [MemAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedIoctlA
    /// [MemAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedRead
    /// [MemAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedReadA
    /// [MemAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedWrite
    /// [MemAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-MemAfterReqCompletedWriteA
    /// [NoCancelFromEvtSurpriseRemove]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-NoCancelFromEvtSurpriseRemove
    /// [ReqDelete]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqDelete
    /// [ReqIsCancOnCancReq]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqIsCancOnCancReq
    /// [ReqNotCanceledLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqNotCanceledLocal
    /// [ReqSendFail]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-ReqSendFail
    /// [RequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-RequestCompleted
    /// [RequestCompletedLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-RequestCompletedLocal
    /// [`EvtIoDefault`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_default
    /// [`EvtIoRead`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_read
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    /// [`EvtIoDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_device_control
    /// [`EvtIoInternalDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_internal_device_control
    //
    // TODO: As proper-intra doc links
    /// [`WdfRequestSetInformation`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestsetinformation
    /// [`WdfRequestGetStatus`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nc-wdfrequest-evt_wdf_request_completion_routine
    /// [`WdfObjectReference`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/wdfobjectreference
    /// [`WdfRequestCompleteWithPriorityBoost`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestcompletewithpriorityboost
    /// [`WdfRequestStopAcknowledge`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequeststopacknowledge
    /// [`WdfRequestMarkCancelable`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestmarkcancelable
    /// [`WdfRequestIsCanceled`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestiscanceled
    /// [`WdfRequestSend`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestsend
    pub unsafe fn WdfRequestCompleteWithInformation(
        Request: WDFREQUEST,    // in
        Status: NTSTATUS,       // in
        Information: ULONG_PTR, // in
    ) {
        dispatch!(WdfRequestCompleteWithInformation(
            Request,
            Status,
            Information
        ))
    }

    /// Requeues the IO request to another driver-owned IO queue
    ///
    /// The driver must have ownership of the request (see [Request Ownership]) and must have obtained
    /// the request from one of its IO queues.
    ///
    /// If the intent is to put the request back on the same queue, [`WdfRequestRequeue`] must be used instead.
    ///
    /// Both the source and destination queues must be owned by the same device.
    ///
    /// If the request has previously been marked cancelable using [`WdfRequestMarkCancelable`] or [`WdfRequestMarkCancelableEx`],
    /// [`WdfRequestUnmarkCancelable`] must be called before the call to [`WdfRequestForwardToIoQueue`].
    ///
    /// Upon calling [`WdfRequestForwardToIoQueue`], ownership of the request is transferred to the framework until it is delivered
    /// back to the driver. While the request is sitting in the queue, the framework retains ownership of the reqeust and can cancel
    /// the reqeust at any time.
    ///
    /// Before [`WdfRequestForwardToIoQueue`] returns, any of these events can occur:
    ///
    /// - If the destination queue was empty, the request can be delivered to the relevant [request handler] of the destination queue
    /// - If the source queue is using sequential or parallel dispatch methods, another request can be delivered to the relevant [request handler] of the source queue
    ///
    /// [Request Ownership]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/request-ownership
    /// [request handler]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/request-handlers
    ///
    /// ## Return value
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_DEVICE_REQUEST` if
    ///   - The request was not obtained from an IO queue (e.g. if the request was just created)
    ///   - The source and destination queues are the same
    ///   - The source and destination queues do not belong to the same device
    ///   - The driver does not have ownership of the request
    ///   - The request is cancelable
    /// - `STATUS_WDF_BUSY` if the destination queue is not accepting new requests
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    ///
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DeferredRequestCompleted]) If a request is deferred for later completion, it must be completed in the deferred processing callback function,
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([InvalidReqAccess], [InvalidReqAccessLocal]) Requests must not be accessed after having been completed or cancelled
    /// - ([RequestCompleted]) Requests from the default IO queue must either be completed, deferred, or forwarded to another queue
    /// - ([RequestCompletedLocal]) Requests from [`EvtIoDefault`], [`EvtIoRead`], [`EvtIoWrite`], [`EvtIoDeviceControl`], and [`EvtIoInternalDeviceControl`]
    ///   should either be completed or marked as cancelable using [`WdfRequestMarkCancelable`]
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DeferredRequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeferredRequestCompleted
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    /// [RequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-RequestCompleted
    /// [RequestCompletedLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-RequestCompletedLocal
    /// [`EvtIoDefault`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_default
    /// [`EvtIoRead`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_read
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    /// [`EvtIoDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_device_control
    /// [`EvtIoInternalDeviceControl`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_internal_device_control
    ///
    /// ## See Also
    ///
    /// - [Requeueing I/O Requests](https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/requeuing-i-o-requests)
    /// - [Managing I/O Queues](https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/managing-i-o-queues)
    ///
    // TODO: As proper intra-doc links
    /// [`WdfRequestRequeue`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestrequeue
    /// [`WdfRequestMarkCancelable`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestmarkcancelable
    /// [`WdfRequestMarkCancelableEx`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestmarkcancelableex
    /// [`WdfRequestUnmarkCancelable`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestunmarkcancelable
    #[must_use]
    pub unsafe fn WdfRequestForwardToIoQueue(
        Request: WDFREQUEST,        // in
        DestinationQueue: WDFQUEUE, // in
    ) -> NTSTATUS {
        dispatch!(WdfRequestForwardToIoQueue(Request, DestinationQueue))
    }

    /// Returns the processor access mode of the request originator
    ///
    /// ## Return value
    ///
    /// If the originator was executing in kernel mode, `KernelMode` is returned.
    /// Otherwise, returns `Usermode`.
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DeferredRequestCompleted]) If a request is deferred for later completion, it must be completed in the deferred processing callback function,
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([InvalidReqAccess], [InvalidReqAccessLocal]) Requests must not be accessed after having been completed or cancelled
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DeferredRequestCompleted]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DeferredRequestCompleted
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    ///
    /// ## See Also
    ///
    /// - [Obtaining Information About an I/O Request]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/obtaining-information-about-an-i-o-request
    pub unsafe fn WdfRequestGetRequestorMode(Request: WDFREQUEST) -> KPROCESSOR_MODE {
        dispatch!(WdfRequestGetRequestorMode(Request))
    }

    /// Gets a request's input buffer
    ///
    /// The input buffer of a request contains information that was supplied by the request's originator.
    /// [`WdfRequestRetrieveInputBuffer`] can be used to get the input buffer for a write or IO control request,
    /// but not for a read request as read requests expect data to be written to the output buffer.
    ///
    /// [`WdfRequestRetrieveInputBuffer`] can be used to get the input buffer for requests using the buffered or
    /// direct IO buffer access methods. If the request's IO control code is [`IRP_MJ_INTERNAL_DEVICE_CONTROL`],
    /// or the request came from another kernel-mode driver, this can also be used for requests using the neither
    /// buffer access mode.
    ///
    /// Accessing the retrieved input buffer is valid until the associated request [is completed].
    ///
    /// [`WdfRequestRetrieveInputMemory`] can be called instead which creates a framework memory object representing
    /// the input buffer.
    ///
    /// [`IRP_MJ_INTERNAL_DEVICE_CONTROL`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/irp-mj-internal-device-control
    /// [is completed]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/completing-i-o-requests
    ///
    /// ## Return value
    ///
    /// Upon successful completion, `Buffer` (and optionally `Length`) are updated with the location and size of
    /// the input buffer respectively.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if an input parameter is invalid
    /// - `STATUS_BUFFER_TOO_SMALL` if the input buffer's length is zero or smaller than `MinimumRequiredLength`
    /// - `STATUS_INVALID_DEVICE_REQUEST` if the request type is not valid or the request is using the neither
    ///    buffer access mode and the request type does not support neither IO
    /// - `STATUS_INTERNAL_ERROR` if the request has already been completed
    /// - `STATUS_INSUFFICIENT_RESOURCES` if there is insufficient memory
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    ///
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([BufAfterReqCompletedIntIoctl], [BufAfterReqCompletedIntIoctlA], [BufAfterReqCompletedIoctl], [BufAfterReqCompletedIoctlA], [BufAfterReqCompletedRead], [BufAfterReqCompletedReadA], [BufAfterReqCompletedWrite], [BufAfterReqCompletedWriteA])
    ///   The IO request buffer associated with the request must not be accessed after the request is completed.
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([InputBufferAPI]) The correct DDI functions for buffer retrieval are used in the [`EvtIoRead`] callback
    ///    (i.e. not using the `WdfRequestRetrieveInputXxx` family of methods)
    /// - ([InvalidReqAccess], [InvalidReqAccessLocal]) Requests must not be accessed after having been completed or cancelled
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [BufAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctl
    /// [BufAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctlA
    /// [BufAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctl
    /// [BufAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctlA
    /// [BufAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedRead
    /// [BufAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedReadA
    /// [BufAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWrite
    /// [BufAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWriteA
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [InputBufferAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InputBufferAPI
    /// [`EvtIoRead`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_read
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    ///
    /// ## See Also
    ///
    /// - [Accessing Data Buffers in Framework-based Drivers](https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/accessing-data-buffers-in-wdf-drivers)
    ///
    // TODO: As proper-intra doc links
    /// [`WdfRequestRetrieveInputMemory`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestretrieveinputmemory
    #[must_use]
    pub unsafe fn WdfRequestRetrieveInputBuffer(
        Request: WDFREQUEST,          // in
        MinimumRequiredLength: usize, // in
        Buffer: &mut PVOID,           // out
        Length: Option<&mut usize>,   // out, optional
    ) -> NTSTATUS {
        dispatch!(WdfRequestRetrieveInputBuffer(
            Request,
            MinimumRequiredLength,
            Buffer,
            Length.map_or(core::ptr::null_mut(), |it| it as *mut _)
        ))
    }

    /// Gets a request's output buffer
    ///
    /// The output buffer of a request contains information that was supplied by the request's originator.
    /// [`WdfRequestRetrieveOutputBuffer`] can be used to get the output buffer for a read or IO control request,
    /// but not for a write request as write requests expect data to be read from the input buffer.
    ///
    /// [`WdfRequestRetrieveOutputBuffer`] can be used to get the output buffer for requests using the buffered or
    /// direct IO buffer access methods. If the request's IO control code is [`IRP_MJ_INTERNAL_DEVICE_CONTROL`],
    /// or the request came from another kernel-mode driver, this can also be used for requests using the neither
    /// buffer access mode.
    ///
    /// Accessing the retrieved output buffer is valid until the associated request [is completed].
    ///
    /// [`WdfRequestRetrieveOutputMemory`] can be called instead which creates a framework memory object representing
    /// the output buffer.
    ///
    /// [`IRP_MJ_INTERNAL_DEVICE_CONTROL`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/irp-mj-internal-device-control
    /// [is completed]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/completing-i-o-requests
    ///
    /// ## Return value
    ///
    /// Upon successful completion, `Buffer` (and optionally `Length`) are updated with the location and size of
    /// the output buffer respectively.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if an input parameter is invalid
    /// - `STATUS_BUFFER_TOO_SMALL` if the output buffer's length is zero or smaller than `MinimumRequiredSize`
    /// - `STATUS_INVALID_DEVICE_REQUEST` if the request type is not valid or the request is using the neither
    ///    buffer access mode and the request type does not support neither IO
    /// - `STATUS_INTERNAL_ERROR` if the request has already been completed
    /// - `STATUS_INSUFFICIENT_RESOURCES` if there is insufficient memory
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    ///
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([BufAfterReqCompletedIntIoctl], [BufAfterReqCompletedIntIoctlA], [BufAfterReqCompletedIoctl], [BufAfterReqCompletedIoctlA], [BufAfterReqCompletedRead], [BufAfterReqCompletedReadA], [BufAfterReqCompletedWrite], [BufAfterReqCompletedWriteA])
    ///   The IO request buffer associated with the request must not be accessed after the request is completed.
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([InvalidReqAccess], [InvalidReqAccessLocal]) Requests must not be accessed after having been completed or cancelled
    /// - ([OutputBufferAPI]) The correct DDI functions for buffer retrieval are used in the [`EvtIoWrite`] callback
    ///    (i.e. not using the `WdfRequestRetrieveOutputXxx` family of methods)
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [BufAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctl
    /// [BufAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctlA
    /// [BufAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctl
    /// [BufAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctlA
    /// [BufAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedRead
    /// [BufAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedReadA
    /// [BufAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWrite
    /// [BufAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWriteA
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    /// [OutputBufferAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-OutputBufferAPI
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    ///
    /// ## See Also
    ///
    /// - [Accessing Data Buffers in Framework-based Drivers](https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/accessing-data-buffers-in-wdf-drivers)
    ///
    // TODO: As proper-intra doc links
    /// [`WdfRequestRetrieveOutputMemory`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/nf-wdfrequest-wdfrequestretrieveoutputmemory
    #[must_use]
    pub unsafe fn WdfRequestRetrieveOutputBuffer(
        Request: WDFREQUEST,        // in
        MinimumRequiredSize: usize, // in
        Buffer: &mut PVOID,         // out
        Length: Option<&mut usize>, // out, optional
    ) -> NTSTATUS {
        dispatch!(WdfRequestRetrieveOutputBuffer(
            Request,
            MinimumRequiredSize,
            Buffer,
            Length.map_or(core::ptr::null_mut(), |it| it as *mut _)
        ))
    }

    /// Gets a memory descriptor list (MDL) representing the request's output buffer
    ///
    /// The output buffer of a request contains information that was supplied by the request's originator.
    /// [`WdfRequestRetrieveOutputWdmMdl`] can be used to get the MDL of the output buffer for a read or IO control request,
    /// but not for a write request as write requests expect data to be read from the input buffer.
    ///
    /// [`WdfRequestRetrieveOutputWdmMdl`] can be used to get the output buffer's MDL for requests using the buffered or
    /// direct IO buffer access methods. If the request's IO control code is [`IRP_MJ_INTERNAL_DEVICE_CONTROL`],
    /// or the request came from another kernel-mode driver, this can also be used for requests using the neither
    /// buffer access mode.
    ///
    /// Accessing the retrieved output buffer MDL is valid until the associated request [is completed].
    ///
    /// [`IRP_MJ_INTERNAL_DEVICE_CONTROL`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/irp-mj-internal-device-control
    /// [is completed]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/completing-i-o-requests
    ///
    /// ## Return value
    ///
    /// Upon successful completion, `Mdl` is updated with the MDL representing the output buffer.
    ///
    /// `STATUS_SUCCESS` is returned if the operation was successful, otherwise:
    ///
    /// - `STATUS_INVALID_PARAMETER` if an input parameter is invalid
    /// - `STATUS_BUFFER_TOO_SMALL` if the output buffer's length is zero or smaller than `MinimumRequiredSize`
    /// - `STATUS_INVALID_DEVICE_REQUEST` if the request type is not valid or the request is using the neither
    ///    buffer access mode and the request type does not support neither IO
    /// - `STATUS_INTERNAL_ERROR` if the request has already been completed
    /// - `STATUS_INSUFFICIENT_RESOURCES` if there is insufficient memory
    /// - Other `NTSTATUS` values (see [`NTSTATUS` values])
    ///
    /// [`NTSTATUS` values]: https://learn.microsoft.com/en-us/windows-hardware/drivers/kernel/ntstatus-values
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([BufAfterReqCompletedIntIoctl], [BufAfterReqCompletedIntIoctlA], [BufAfterReqCompletedIoctl], [BufAfterReqCompletedIoctlA], [BufAfterReqCompletedRead], [BufAfterReqCompletedReadA], [BufAfterReqCompletedWrite], [BufAfterReqCompletedWriteA])
    ///   The IO request buffer associated with the request must not be accessed after the request is completed.
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([InvalidReqAccess], [InvalidReqAccessLocal]) Requests must not be accessed after having been completed or cancelled
    /// - ([OutputBufferAPI]) The correct DDI functions for buffer retrieval are used in the [`EvtIoWrite`] callback
    ///    (i.e. not using the `WdfRequestRetrieveOutputXxx` family of methods)
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [BufAfterReqCompletedIntIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctl
    /// [BufAfterReqCompletedIntIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIntIoctlA
    /// [BufAfterReqCompletedIoctl]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctl
    /// [BufAfterReqCompletedIoctlA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedIoctlA
    /// [BufAfterReqCompletedRead]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedRead
    /// [BufAfterReqCompletedReadA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedReadA
    /// [BufAfterReqCompletedWrite]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWrite
    /// [BufAfterReqCompletedWriteA]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-BufAfterReqCompletedWriteA
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    /// [OutputBufferAPI]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-OutputBufferAPI
    /// [`EvtIoWrite`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfio/nc-wdfio-evt_wdf_io_queue_io_write
    ///
    /// ## See Also
    ///
    /// - [Accessing Data Buffers in Framework-based Drivers](https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/accessing-data-buffers-in-wdf-drivers)
    #[must_use]
    pub unsafe fn WdfRequestRetrieveOutputWdmMdl(
        Request: WDFREQUEST, // in
        Mdl: &mut PMDL,      // out
    ) -> NTSTATUS {
        dispatch!(WdfRequestRetrieveOutputWdmMdl(Request, Mdl))
    }

    /// Gets the framework file object associated with the given IO request
    ///
    /// Returns a handle to the framework file object, or null if:
    ///
    /// - [`WdfDeviceInitSetFileObjectConfig`] has not been called with a
    ///   [`WDF_FILEOBJECT_CLASS`] that creates file objects (e.g. [`WdfFileObjectCanBeOptional`])
    /// - Another driver sent a read, write, or IO control request without previously sending a [`WdfRequestTypeCreate`] request
    ///
    /// For the most part, null checking only needs to be done if the first situation is true.
    ///
    /// For more information about framework file objects, see [Framework File Objects].
    ///
    /// [`WdfFileObjectCanBeOptional`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/ne-wdfdevice-_wdf_fileobject_class
    /// [`WdfRequestTypeCreate`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfrequest/ne-wdfrequest-_wdf_request_type
    /// [`WDF_FILEOBJECT_CLASS`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/ddi/wdfdevice/ne-wdfdevice-_wdf_fileobject_class
    /// [Framework File Objects]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/framework-file-objects
    ///
    /// ## Safety
    ///
    /// In addition to all passed-in pointers pointing to valid memory locations:
    ///
    /// - ([KmdfIrqlDependent], [KmdfIrql2]) IRQL: <= `DISPATCH_LEVEL`
    /// - ([DriverCreate]) [`WdfDriverCreate`] must only be called from the [`DriverEntry`] point
    /// - ([FileObjectConfigured]) [`WdfDeviceInitSetFileObjectConfig`] must be called before [`WdfRequestGetFileObject`]
    /// - ([InvalidReqAccess], [InvalidReqAccessLocal]) Requests must not be accessed after having been completed or cancelled
    ///
    /// [KmdfIrqlDependent]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql
    /// [KmdfIrql2]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-KmdfIrql2
    /// [DriverCreate]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-DriverCreate
    /// [`DriverEntry`]: https://learn.microsoft.com/en-us/windows-hardware/drivers/wdf/driverentry-for-kmdf-drivers
    /// [FileObjectConfigured]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-FileObjectConfigured
    /// [InvalidReqAccess]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccess
    /// [InvalidReqAccessLocal]: https://learn.microsoft.com/en-us/windows-hardware/drivers/devtest/kmdf-InvalidReqAccessLocal
    pub unsafe fn WdfRequestGetFileObject(Request: WDFREQUEST) -> WDFFILEOBJECT {
        dispatch!(WdfRequestGetFileObject(Request))
    }

    // endregion: wdfrequest

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

pub mod file_object {
    use core::marker::PhantomData;

    use wdf_kmdf_sys::WDFFILEOBJECT;

    use crate::object::{self, IntoContextSpace};

    pub struct FileObject<T>(WDFFILEOBJECT, PhantomData<T>);

    impl<T> core::fmt::Debug for FileObject<T> {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            f.debug_tuple("FileObject").field(&self.0).finish()
        }
    }

    impl<T> FileObject<T> {
        /// Wraps the raw handle in a file object wrapper
        ///
        /// ## Safety
        ///
        /// Respect aliasing rules, since this can be used to
        /// generate aliasing mutable references to the context space.
        /// Also, the context space must be initialized.
        // FIXME: Make a proper wrapper eventually
        pub unsafe fn wrap(handle: WDFFILEOBJECT) -> Self {
            Self(handle, PhantomData)
        }

        /// Gets the file object handle for use with WDF functions that don't have clean wrappers yet
        pub fn raw_handle(&mut self) -> WDFFILEOBJECT {
            self.0
        }
    }

    impl<T> FileObject<T>
    where
        T: IntoContextSpace,
    {
        pub fn get_context(&self) -> &T {
            // SAFETY: Contruction guarantees that the space is initialized
            unsafe { object::get_context(self).unwrap() }
        }

        pub fn get_context_mut(&mut self) -> &mut T {
            // SAFETY: Contruction guarantees that the space is initialized
            unsafe { object::get_context_mut(self).unwrap() }
        }
    }

    impl<T> object::AsObjectHandle for FileObject<T> {
        fn as_handle(&self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }

        fn as_handle_mut(&mut self) -> wdf_kmdf_sys::WDFOBJECT {
            self.0.cast()
        }
    }
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

    /// Initializes the object's context area, using the closure provided
    ///
    /// ## Safety
    ///
    /// - Must not reinitialize an object's context space
    /// - Object must actually have the context space
    pub(crate) unsafe fn context_pin_init<T, Handle, I, Err>(
        handle: &mut Handle,
        init_context: impl FnOnce(&mut Handle) -> Result<I, Err>,
    ) -> Result<(), Err>
    where
        T: IntoContextSpace,
        Handle: AsObjectHandle,
        I: pinned_init::PinInit<T, Err>,
    {
        let raw_handle = handle.as_handle_mut();

        // SAFETY: `raw_handle` validity assured by `AsObjectHandle`, and context info validity assured by `IntoContextSpace`
        let context_space = unsafe {
            // filler line as a work-around for https://github.com/rust-lang/rust-clippy/issues/10832
            crate::raw::WdfObjectGetTypedContextWorker(raw_handle, T::CONTEXT_INFO)
        };
        let context_space = context_space.cast::<T>();

        // Get closure to initialize context with
        // Note: while this can panic, since we're assuming we're
        // in a `panic=abort` context, we won't ever have access
        // to an uninitialized context area.
        //
        // However, if we were to be in a `panic=unwind` context
        // (which might supported if we link ucrt in), then we'd
        // need to set an initialization flag stored after the
        // real context area so that we don't drop uninitialized
        // memory.
        let pin_init = init_context(handle)?;

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
        cell::UnsafeCell,
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
    ///
    /// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the mutex is locked
    #[pin_data]
    pub struct SpinMutex<T> {
        /// The backing spin lock
        spin_lock: SpinLock,
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
                data <- {
                    let init = move |slot: *mut UnsafeCell<T>| {
                        // SAFETY: by guarantees of `pin_init_from_closure`
                        unsafe { value.__pinned_init(slot.cast()) }
                    };

                    // SAFETY: `data` is pinned too, and initialization requirements are guaranteed
                    // by nature of delegating to `value`'s pinned init
                    unsafe { pinned_init::pin_init_from_closure(init)}
                }
            }? Error)
        }

        /// Tries to acquire the mutex, returning a guard if successful
        ///
        /// ## IRQL: <= Dispatch
        pub fn try_lock(&self) -> Option<Pin<SpinMutexGuard<'_, T>>> {
            let guard = self.spin_lock.acquire();

            // SAFETY: Uhhhh
            Some(unsafe {
                Pin::new_unchecked(SpinMutexGuard {
                    mutex: self,
                    _guard: guard,
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

    // SAFETY: Can send &SpinMutex<T> to other threads as we can only observe `T` changing
    // when we hold the lock, and only one thread can hold the lock.
    //
    // `T` also needs to be `Send` as we need to be able to manifest a `&mut T` on any thread.
    // This is so that `SpinMutex<Rc<_>>` is invalid, as otherwise we could have any number of `Rc`'s on different threads.
    unsafe impl<T> Sync for SpinMutex<T> where T: Send {}

    /// Lock guard for a [`SpinMutex`]
    pub struct SpinMutexGuard<'a, T> {
        mutex: &'a SpinMutex<T>,
        _guard: SpinLockGuard<'a>,
        _pin: PhantomPinned,
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
    ///
    /// IRQL-aware, and adjusts the IRQL to `DISPATCH_LEVEL` while the lock is acquired
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
            // SAFETY:
            // - Cleanup callbacks are always called at either `DISPATCH_LEVEL` or `PASSIVE_LEVEL`
            // - We're always deleting a spin lock, which has no special deletion requirements
            unsafe { raw::WdfObjectDelete(self.0.cast()) }
        }
    }

    // SAFETY: Can manifest SpinLock to another thread (as-if we'd moved a pointer)
    unsafe impl Send for SpinLock {}
    // SAFETY: Can send &SpinLock to other threads (no interior mutability observable)
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
    use wdf_kmdf_sys::{PWDFDEVICE_INIT, WDFDRIVER, WDF_DRIVER_INIT_FLAGS};
    use windows_kernel_rs::{string::unicode_string::NtUnicodeStr, DriverObject};
    use windows_kernel_sys::{result::STATUS, Error, NTSTATUS};

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
        pub fn create<I>(
            driver_object: DriverObject,
            registry_path: NtUnicodeStr<'_>,
            config: DriverConfig,
            init_context: impl FnOnce(&mut DriverHandle) -> Result<I, Error>,
        ) -> Result<(), Error>
        where
            I: PinInit<T, Error>,
        {
            if matches!(config.pnp_mode, PnpMode::NonPnp) && T::HAS_DEVICE_ADD {
                // Non-pnp drivers shouldn't specify the `device_add` callback
                return Err(Error(STATUS::INVALID_PARAMETER));
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

            // SAFETY:
            // - It's WDF's responsibility to insert the context area, since we create
            //   the default object attributes with T's context area
            // - The driver object was just created
            unsafe { object::context_pin_init(&mut handle, init_context) }
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
                return STATUS::SUCCESS.to_u32();
            };

            match T::device_add(context_space, device_init) {
                Ok(()) => STATUS::SUCCESS.to_u32(),
                Err(err) => err.0.to_u32(),
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
