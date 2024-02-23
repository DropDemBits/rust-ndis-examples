use notify::CMuxNotify;

pub mod com_helpers;
mod notify;

inproc_dll_module![(CMuxNotify::CLSID, CMuxNotify),];
