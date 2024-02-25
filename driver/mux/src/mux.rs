//! DriverEntry and NT dispatch functions for the NDIS MUX IM miniport driver sample.

use core::{
    pin::Pin,
    sync::atomic::{AtomicI32, AtomicU32},
};

use vtable::vtable;
use wdf_kmdf::{
    handle::HasContext,
    miniport::{MiniportDriver, MiniportDriverCallbacks, MiniportDriverConfig},
    sync::{WaitMutex, WaitPinMutex},
};
use windows_kernel_rs::{
    string::{nt_unicode_str, unicode_string::NtUnicodeStr},
    DriverObject,
};
use windows_kernel_sys::{
    result::STATUS, Error, NdisDeregisterProtocolDriver, NdisIMAssociateMiniport,
    NdisMDeregisterMiniportDriver, NdisMRegisterMiniportDriver, NdisRegisterProtocolDriver,
    NDIS_HANDLE, NDIS_INTERMEDIATE_DRIVER, NDIS_MINIPORT_DRIVER_CHARACTERISTICS,
    NDIS_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1, NDIS_OBJECT_TYPE_DEFAULT,
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1,
    NDIS_STATUS, NTSTATUS, PDEVICE_OBJECT, PIRP,
};

use crate::{
    miniport, protocol, Mux, NdisHandle, TrustMeList, MUX_MAJOR_NDIS_VERSION,
    MUX_MINOR_NDIS_VERSION, MUX_TAG,
};

/// First entry point to be called when this driver is loaded.
/// Register with NDIS as an intermediate driver.
pub(super) fn driver_entry(
    driver_object: DriverObject,
    registry_path: NtUnicodeStr<'_>,
) -> Result<(), Error> {
    const NDIS_SIZEOF_PROTOCOL_DRIVER_CHARACTERISTICS_REVISION_1: u16 = (core::mem::offset_of!(
        NDIS_PROTOCOL_DRIVER_CHARACTERISTICS,
        SendNetBufferListsCompleteHandler
    ) + core::mem::size_of::<
        windows_kernel_sys::SEND_NET_BUFFER_LISTS_COMPLETE_HANDLER,
    >()) as u16;

    const NDIS_SIZEOF_MINIPORT_DRIVER_CHARACTERISTICS_REVISION_1: u16 = (core::mem::offset_of!(
        NDIS_MINIPORT_DRIVER_CHARACTERISTICS,
        CancelOidRequestHandler
    ) + core::mem::size_of::<
        windows_kernel_sys::MINIPORT_CANCEL_OID_REQUEST_HANDLER,
    >()) as u16;

    let driver = MiniportDriver::create(
        driver_object,
        registry_path,
        MiniportDriverConfig {
            pool_tag: Some(MUX_TAG),
        },
        |_| {
            Ok(pinned_init::try_pin_init! {
                Mux {
                    AdapterList <- WaitPinMutex::new(TrustMeList::new()),
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
        debug_assert!(self.AdapterList.lock().as_ref().as_list().is_empty());

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
    Ok(())
}

/// Process IRPs sent to this device
unsafe extern "C" fn pt_dispatch(DeviceObject: PDEVICE_OBJECT, Irp: PIRP) -> NTSTATUS {
    STATUS::SUCCESS.to_u32()
}

/// Deregister the ioctl interface. This is called whenever a miniport instance
/// is halted. When the last miniport instance is halted, we request NDIS to
/// delete the device object.
fn pt_deregister_device() -> Result<(), Error> {
    Ok(())
}
