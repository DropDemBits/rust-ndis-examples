//! DriverEntry and NT dispatch functions for the NDIS MUX IM miniport driver sample.

use core::{
    pin::Pin,
    sync::atomic::{AtomicI32, AtomicU32},
};

use vtable::vtable;
use wdf_kmdf::{
    miniport::{MiniportDriver, MiniportDriverCallbacks, MiniportDriverConfig},
    sync::{WaitMutex, WaitPinMutex},
};
use windows_kernel_rs::{
    string::{nt_unicode_str, unicode_string::NtUnicodeStr},
    DriverObject,
};
use windows_kernel_sys::{
    Error, NDIS_HANDLE, NDIS_MEDIUM, NDIS_MINIPORT_DRIVER_CHARACTERISTICS,
    NDIS_PROTOCOL_DRIVER_CHARACTERISTICS, NDIS_STATUS,
};

use crate::{Mux, NdisHandle, TrustMeList, MUX_TAG};

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

    let mut Status: NDIS_STATUS = 0;
    let mut PChars = unsafe { core::mem::zeroed::<NDIS_PROTOCOL_DRIVER_CHARACTERISTICS>() };
    let mut MChars = unsafe { core::mem::zeroed::<NDIS_MINIPORT_DRIVER_CHARACTERISTICS>() };
    let mut MiniportDriverContext: NDIS_HANDLE = core::ptr::null_mut();
    let mut ProtocolDriverContext: NDIS_HANDLE = core::ptr::null_mut();
    const NAME: NtUnicodeStr<'static> = nt_unicode_str!("MUXP");

    let _driver = MiniportDriver::create(
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

    Ok(())
}

#[vtable]
impl MiniportDriverCallbacks for Mux {
    fn unload(self: Pin<&Self>) {}
}
