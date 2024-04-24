//! Common declarations shared across the user/kernel mode boundary

use windows_kernel_rs::string::{nt_unicode_str, unicode_string::NtUnicodeStr};
use windows_kernel_sys::WCHAR;

pub const GLOBAL_LINKNAME_STRING: NtUnicodeStr<'static> =
    nt_unicode_str!(r"\DosDevices\Global\Mux");
pub const NTDEVICE_STRING: NtUnicodeStr<'static> = nt_unicode_str!(r"\Device\Mux");

pub const MUX_CUSTOM_EVENT: u32 = 1;
pub const NOTIFY_SIGNATURE: u32 = 0xAFCDABAB;

#[repr(C)]
pub struct NotifyCustomEvent {
    uSignature: u32,
    uEvent: u32,
    szMiniport: [WCHAR; 0],
}
