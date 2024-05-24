//! DriverEntry and NT dispatch functions for the NDIS MUX IM miniport driver sample.

use core::{
    pin::Pin,
    sync::atomic::{AtomicI32, AtomicU32, Ordering},
};

use alloc::vec::Vec;
use vtable::vtable;
use wdf_kmdf::{
    handle::HasContext,
    miniport::{MiniportDevice, MiniportDriver, MiniportDriverCallbacks, MiniportDriverConfig},
    sync::WaitMutex,
};
use windows_kernel_rs::{
    log::debug,
    string::{nt_unicode_str, unicode_string::NtUnicodeStr},
    DriverObject,
};
use windows_kernel_sys::{
    result::STATUS, Error, IoCompleteRequest, IoGetCurrentIrpStackLocation, NdisDeregisterDeviceEx,
    NdisDeregisterProtocolDriver, NdisIMAssociateMiniport, NdisMDeregisterMiniportDriver,
    NdisMRegisterMiniportDriver, NdisRegisterDeviceEx, NdisRegisterProtocolDriver, IO_NO_INCREMENT,
    IRP_MJ_CLEANUP, IRP_MJ_CLOSE, IRP_MJ_CREATE, IRP_MJ_DEVICE_CONTROL, IRP_MJ_MAXIMUM_FUNCTION,
    NDIS_DEVICE_OBJECT_ATTRIBUTES, NDIS_DEVICE_OBJECT_ATTRIBUTES_REVISION_1, NDIS_HANDLE,
    NDIS_INTERMEDIATE_DRIVER, NDIS_MINIPORT_DRIVER_CHARACTERISTICS,
    NDIS_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1, NDIS_OBJECT_TYPE_DEFAULT,
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1,
    NDIS_SIZEOF_DEVICE_OBJECT_ATTRIBUTES_REVISION_1,
    NDIS_SIZEOF_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1,
    NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1, NDIS_STATUS, NTSTATUS, PDEVICE_OBJECT,
    PDRIVER_DISPATCH, PIRP,
};

use crate::{
    miniport, ndis_status_to_nt_status, protocol,
    public::{GLOBAL_LINKNAME_STRING, NTDEVICE_STRING},
    Mux, NdisHandle, MUX_MAJOR_NDIS_VERSION, MUX_MINOR_NDIS_VERSION, MUX_TAG,
};

/// First entry point to be called when this driver is loaded.
/// Register with NDIS as an intermediate driver.
pub(super) fn driver_entry(
    driver_object: DriverObject,
    registry_path: NtUnicodeStr<'_>,
) -> Result<(), Error> {
    let driver = MiniportDriver::create(
        driver_object,
        registry_path,
        MiniportDriverConfig {
            pool_tag: Some(MUX_TAG),
        },
        |_| {
            Ok(pinned_init::try_pin_init! {
                Mux {
                    AdapterList <- WaitMutex::new(Vec::new()),
                    NextVElanNumber: AtomicU32::new(0),

                    ProtHandle: NdisHandle::empty(),
                    DriverHandle: NdisHandle::empty(),

                    MiniportCount: AtomicI32::new(0),
                    ControlDevice <- WaitMutex::new(None),
                }? Error
            })
        },
    )?;

    // Register the miniport with NDIS.
    //
    // Note that it is the miniport which was started as a driver and not the protocol.
    //
    // Also the miniport must be registered prior to the protocol since the
    // protocol's `BindAdapter` handler can be initiated anytime, and when it is, it
    // must be ready to start driver instances.
    let mut MChars = unsafe { core::mem::zeroed::<NDIS_MINIPORT_DRIVER_CHARACTERISTICS>() };
    let MiniportDriverContext: NDIS_HANDLE = core::ptr::null_mut();

    MChars.Header.Type = NDIS_OBJECT_TYPE_DEFAULT as u8;
    MChars.Header.Size = NDIS_SIZEOF_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1;
    MChars.Header.Revision = NDIS_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1 as u8;

    MChars.MajorNdisVersion = MUX_MAJOR_NDIS_VERSION;
    MChars.MinorNdisVersion = MUX_MINOR_NDIS_VERSION;

    MChars.SetOptionsHandler = Some(mp_set_options);

    MChars.InitializeHandlerEx = Some(miniport::mp_initialize);
    MChars.UnloadHandler = Some(miniport::mp_unload);
    MChars.HaltHandlerEx = Some(miniport::mp_halt);

    MChars.OidRequestHandler = Some(miniport::mp_oid_request);

    MChars.CancelSendHandler = Some(miniport::mp_cancel_send_nbls);
    MChars.DevicePnPEventNotifyHandler = Some(miniport::mp_device_pnp_event);
    MChars.ShutdownHandlerEx = Some(miniport::mp_adapter_shutdown);
    MChars.CancelOidRequestHandler = Some(miniport::mp_cancel_oid_request);

    // We will disable the check for hang timeout so we do not
    // need a check for hang handler!
    MChars.CheckForHangHandlerEx = None;

    MChars.ReturnNetBufferListsHandler = Some(miniport::mp_return_nbls);
    MChars.SendNetBufferListsHandler = Some(miniport::mp_send_nbls);

    MChars.PauseHandler = Some(miniport::mp_pause);
    MChars.RestartHandler = Some(miniport::mp_restart);

    MChars.Flags = NDIS_INTERMEDIATE_DRIVER;

    Error::to_err(unsafe {
        NdisMRegisterMiniportDriver(
            driver.wdm_driver_object(),
            registry_path.as_ptr().cast_mut().cast(),
            MiniportDriverContext,
            &mut MChars,
            driver.get_context().DriverHandle.as_ptr(),
        )
    })?;

    // Now register the protocol.
    let mut PChars = unsafe { core::mem::zeroed::<NDIS_PROTOCOL_DRIVER_CHARACTERISTICS>() };
    let ProtocolDriverContext: NDIS_HANDLE = core::ptr::null_mut();

    PChars.Header.Type = NDIS_OBJECT_TYPE_DEFAULT as u8;
    PChars.Header.Size = NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1;
    PChars.Header.Revision = NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1 as u8;

    PChars.MajorNdisVersion = MUX_MAJOR_NDIS_VERSION;
    PChars.MinorNdisVersion = MUX_MINOR_NDIS_VERSION;

    PChars.SetOptionsHandler = Some(pt_set_options);

    // Make sure the protocol name matches the service name from the INF under
    // which this protocol is installed. This is needed to ensure that NDIS can
    // correctly determin the binding call and call us to bind to the miniports
    // below.
    const NAME: NtUnicodeStr<'static> = nt_unicode_str!("MUXP");
    PChars.Name = unsafe { *NAME.as_ptr().cast() };
    PChars.OpenAdapterCompleteHandlerEx = Some(protocol::pt_open_adapter_complete);
    PChars.CloseAdapterCompleteHandlerEx = Some(protocol::pt_close_adapter_complete);

    PChars.ReceiveNetBufferListsHandler = Some(protocol::pt_receive_nbl);
    PChars.SendNetBufferListsCompleteHandler = Some(protocol::pt_send_nbl_complete);
    PChars.OidRequestCompleteHandler = Some(protocol::pt_request_complete);
    PChars.StatusHandlerEx = Some(protocol::pt_status);
    PChars.BindAdapterHandlerEx = Some(protocol::pt_bind_adapter);
    PChars.UnbindAdapterHandlerEx = Some(protocol::pt_unbind_adapter);
    PChars.NetPnPEventHandler = Some(protocol::pt_pnp_handler);

    if let Err(err) = Error::to_err(unsafe {
        NdisRegisterProtocolDriver(
            ProtocolDriverContext,
            &mut PChars,
            driver.get_context().ProtHandle.as_ptr(),
        )
    }) {
        unsafe { NdisMDeregisterMiniportDriver(driver.get_context().DriverHandle.get()) };
        return Err(err);
    }

    // Let NDIS know of the association between our protocol and miniport instances
    unsafe {
        NdisIMAssociateMiniport(
            driver.get_context().DriverHandle.get(),
            driver.get_context().ProtHandle.get(),
        )
    };

    Ok(())
}

#[vtable]
impl MiniportDriverCallbacks for Mux {
    fn unload(self: Pin<&Self>) {
        if let Some(prot_handle) = self.ProtHandle.take() {
            unsafe { NdisDeregisterProtocolDriver(prot_handle) };
        }

        if let Some(mp_handle) = self.DriverHandle.take() {
            unsafe { NdisMDeregisterMiniportDriver(mp_handle) };
        }

        // All adapters should've been closed
        debug_assert!(self.AdapterList.lock().is_empty());

        // The control device should've been cleaned up.
        debug_assert!(self.ControlDevice.lock().is_none());
    }
}

/// Registers optional handlers for the mux miniport with NDIS
unsafe extern "C" fn mp_set_options(
    _NdisDriverHandle: NDIS_HANDLE,
    _DriverContext: NDIS_HANDLE,
) -> NDIS_STATUS {
    STATUS::SUCCESS.to_u32()
}

/// Registers optional handlers for the mux protocol with NDIS
unsafe extern "C" fn pt_set_options(
    _NdisDriverHandle: NDIS_HANDLE,
    _DriverContext: NDIS_HANDLE,
) -> NDIS_STATUS {
    STATUS::SUCCESS.to_u32()
}

/// Register an ioctl interface - a device object to be used for this purpose is
/// created by NDIS when we call `NdisMRegisterDevice`.
///
/// This routine is called whenever a new miniport instance is initialized. This
/// routine handles potential race conditions with `pt_deregister_device` via
/// the `ControlDevice` mutex.
///
/// NOTE: Do not call this from `DriverEntry`, it will prevent the driver from
/// being unloaded (e.g. on uninstall)
fn pt_register_device() -> Result<(), Error> {
    fn inner() -> Result<(), Error> {
        let driver = MiniportDriver::<Mux>::get();
        let globals = driver.get_context();

        let mut cdo = globals.ControlDevice.lock();

        if globals.MiniportCount.fetch_add(1, Ordering::Relaxed) == 0 {
            // This is the first ioctl device to be created
            let mut device_object_attributes =
                unsafe { core::mem::zeroed::<NDIS_DEVICE_OBJECT_ATTRIBUTES>() };

            let mut dispatch_table: [PDRIVER_DISPATCH; (IRP_MJ_MAXIMUM_FUNCTION + 1) as usize] =
                [None; (IRP_MJ_MAXIMUM_FUNCTION + 1) as usize];

            dispatch_table[IRP_MJ_CREATE as usize] = Some(pt_dispatch);
            dispatch_table[IRP_MJ_CLEANUP as usize] = Some(pt_dispatch);
            dispatch_table[IRP_MJ_CLOSE as usize] = Some(pt_dispatch);
            dispatch_table[IRP_MJ_DEVICE_CONTROL as usize] = Some(pt_dispatch);

            device_object_attributes.Header.Type = NDIS_OBJECT_TYPE_DEFAULT as u8;
            device_object_attributes.Header.Revision =
                NDIS_DEVICE_OBJECT_ATTRIBUTES_REVISION_1 as u8;
            device_object_attributes.Header.Size = NDIS_SIZEOF_DEVICE_OBJECT_ATTRIBUTES_REVISION_1;
            device_object_attributes.DeviceName = NTDEVICE_STRING.as_ptr().cast_mut().cast();
            device_object_attributes.SymbolicName =
                GLOBAL_LINKNAME_STRING.as_ptr().cast_mut().cast();
            device_object_attributes.MajorFunctions = dispatch_table.as_mut_ptr();
            device_object_attributes.ExtensionSize = 0;
            device_object_attributes.DefaultSDDLString = core::ptr::null();
            device_object_attributes.DeviceClassGuid = core::ptr::null();

            let mut out_cdo = core::ptr::null_mut();
            let mut out_device_handle = core::ptr::null_mut();

            // Create the base NDIS device
            ndis_status_to_nt_status(unsafe {
                NdisRegisterDeviceEx(
                    globals.DriverHandle.get(),
                    &mut device_object_attributes,
                    &mut out_cdo,
                    &mut out_device_handle,
                )
            })?;

            // Create the wrapper WDF device
            // This CDO is at the top of the driver stack, so we don't need to
            // pass in the attached device or the underlying PDO
            let out_cdo = MiniportDevice::create(driver.as_ref(), out_cdo, None, None, |_| Ok(()))?;

            *cdo = Some(crate::ControlDeviceObject {
                cdo: out_cdo,
                device_handle: NdisHandle::new(out_device_handle),
            });
        }

        Ok(())
    }

    debug!("==> pt_register_device");
    let status = inner();
    debug!(
        "<== pt_register_device: {:x?}",
        status.err().map_or(0, |err| err.0.to_u32())
    );

    status
}

/// Process IRPs sent to this device
unsafe extern "C" fn pt_dispatch(_DeviceObject: PDEVICE_OBJECT, Irp: PIRP) -> NTSTATUS {
    let irp = unsafe { &mut *Irp };
    let irp_stack = IoGetCurrentIrpStackLocation(Irp);
    let irp_stack = unsafe { &mut *irp_stack };
    let status = STATUS::SUCCESS;

    debug!("==> pt_dispatch {}", irp_stack.MajorFunction);

    match irp_stack.MajorFunction as u32 {
        IRP_MJ_CREATE | IRP_MJ_CLEANUP | IRP_MJ_CLOSE => {}
        IRP_MJ_DEVICE_CONTROL => {
            let _buffer = irp.AssociatedIrp.SystemBuffer;
            let _in_len = irp_stack.Parameters.DeviceIoControl.InputBufferLength;

            match irp_stack.Parameters.DeviceIoControl.IoControlCode {
                _ => {
                    // Add code here to handle ioctl commands.
                }
            }
        }
        _ => {}
    }

    irp.IoStatus.Information = 0;
    *irp.IoStatus.__bindgen_anon_1.Status.as_mut() = status.to_u32();
    IoCompleteRequest(Irp, IO_NO_INCREMENT as i8);

    debug!("<== pt_dispatch {:x?}", status.to_u32());

    status.to_u32()
}

/// Deregister the ioctl interface. This is called whenever a miniport instance
/// is halted. When the last miniport instance is halted, we request NDIS to
/// delete the device object.
fn pt_deregister_device() -> Result<(), Error> {
    fn inner() -> Result<(), Error> {
        let driver = MiniportDriver::<Mux>::get();
        let globals = driver.get_context();

        let old_count = globals.MiniportCount.fetch_sub(1, Ordering::Release);

        // Count must always have been previously positive or 0
        assert!(old_count >= 1);

        if old_count == 1 {
            // All VELan miniport instances have been halted.
            // Deregister the control device.
            if let Some(cdo) = globals.ControlDevice.lock().take() {
                unsafe { NdisDeregisterDeviceEx(cdo.device_handle.get()) }
            }
        }

        Ok(())
    }

    debug!("==> pt_deregister_device");
    let status = inner();
    debug!(
        "<== pt_deregister_device {}",
        status.err().map_or(0, |err| err.0.to_u32())
    );

    status
}
