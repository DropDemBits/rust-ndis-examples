#![allow(non_snake_case, unused_variables)]

use windows::{
    core::*,
    Win32::{Foundation::E_NOTIMPL, NetworkManagement::NetManagement::INetCfgPnpReconfigCallback},
    Win32::{
        Foundation::{BOOL, HWND},
        NetworkManagement::NetManagement::{
            INetCfg, INetCfgBindingPath, INetCfgComponent, INetCfgComponentControl,
            INetCfgComponentControl_Impl, INetCfgComponentNotifyBinding,
            INetCfgComponentNotifyBinding_Impl, INetCfgComponentNotifyGlobal,
            INetCfgComponentNotifyGlobal_Impl, INetCfgComponentPropertyUi,
            INetCfgComponentPropertyUi_Impl, INetCfgComponentSetup, INetCfgComponentSetup_Impl,
        },
    },
};

#[implement(
    INetCfgComponentControl,
    INetCfgComponentSetup,
    INetCfgComponentNotifyBinding,
    INetCfgComponentNotifyGlobal,
    INetCfgComponentPropertyUi
)]
pub(crate) struct CMuxNotify {
    //
}

impl Default for CMuxNotify {
    fn default() -> Self {
        CMuxNotify {}
    }
}

impl CMuxNotify {
    // Mux GUID
    pub(crate) const CLSID: GUID = GUID::from_u128(0xee6cd6fc_a32b_4a26_8a1c_dbc97988ad7a);
}

impl INetCfgComponentControl_Impl for CMuxNotify {
    fn Initialize(
        &self,
        picomp: Option<&INetCfgComponent>,
        pinetcfg: Option<&INetCfg>,
        finstalling: BOOL,
    ) -> Result<()> {
        Ok(())
    }

    fn ApplyRegistryChanges(&self) -> Result<()> {
        Ok(())
    }

    fn ApplyPnpChanges(&self, picallback: Option<&INetCfgPnpReconfigCallback>) -> Result<()> {
        Ok(())
    }

    fn CancelChanges(&self) -> Result<()> {
        Ok(())
    }
}

impl INetCfgComponentSetup_Impl for CMuxNotify {
    fn Install(&self, dwsetupflags: u32) -> Result<()> {
        Ok(())
    }

    fn Upgrade(&self, dwsetupflags: u32, dwupgradefombuildno: u32) -> Result<()> {
        Ok(())
    }

    fn ReadAnswerFile(&self, pszwanswerfile: &PCWSTR, pszwanswersections: &PCWSTR) -> Result<()> {
        Ok(())
    }

    fn Removing(&self) -> Result<()> {
        Ok(())
    }
}

impl INetCfgComponentNotifyBinding_Impl for CMuxNotify {
    fn QueryBindingPath(
        &self,
        dwchangeflag: u32,
        pipath: ::core::option::Option<&INetCfgBindingPath>,
    ) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn NotifyBindingPath(
        &self,
        dwchangeflag: u32,
        pipath: ::core::option::Option<&INetCfgBindingPath>,
    ) -> Result<()> {
        Err(E_NOTIMPL.into())
    }
}

impl INetCfgComponentNotifyGlobal_Impl for CMuxNotify {
    fn GetSupportedNotifications(&self) -> Result<u32> {
        Err(E_NOTIMPL.into())
    }

    fn SysQueryBindingPath(
        &self,
        dwchangeflag: u32,
        pipath: ::core::option::Option<&INetCfgBindingPath>,
    ) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn SysNotifyBindingPath(
        &self,
        dwchangeflag: u32,
        pipath: ::core::option::Option<&INetCfgBindingPath>,
    ) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn SysNotifyComponent(
        &self,
        dwchangeflag: u32,
        picomp: ::core::option::Option<&INetCfgComponent>,
    ) -> Result<()> {
        Err(E_NOTIMPL.into())
    }
}

impl INetCfgComponentPropertyUi_Impl for CMuxNotify {
    fn QueryPropertyUi(&self, punkreserved: ::core::option::Option<&IUnknown>) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn SetContext(&self, punkreserved: ::core::option::Option<&IUnknown>) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn MergePropPages(
        &self,
        pdwdefpages: *mut u32,
        pahpspprivate: *mut *mut u8,
        pcpages: *mut u32,
        hwndparent: HWND,
        pszstartpage: *const PCWSTR,
    ) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn ValidateProperties(&self, hwndsheet: HWND) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn ApplyProperties(&self) -> Result<()> {
        Err(E_NOTIMPL.into())
    }

    fn CancelProperties(&self) -> Result<()> {
        Err(E_NOTIMPL.into())
    }
}
