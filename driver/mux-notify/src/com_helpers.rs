//! Helpers for working with COM Servers.
//! Parts borrowed from `com-rs` since `windows-rs` doesn't have a way of covering most of the things.
use std::{marker::PhantomData, ptr::NonNull};

use windows::{
    core::{implement, Error, IUnknown, Interface, Result, GUID, HRESULT},
    Win32::{
        Foundation::{CLASS_E_NOAGGREGATION, E_NOTIMPL, E_POINTER, HINSTANCE, HMODULE},
        System::{
            Com::{IClassFactory, IClassFactory_Impl},
            LibraryLoader::GetModuleFileNameA,
            Ole::SELFREG_E_CLASS,
        },
    },
};
use windows_core::BOOL;

#[implement(IClassFactory)]
pub struct ComClassFactory<C>(PhantomData<C>)
where
    C: Default + 'static,
    IUnknown: From<C>;

impl<C> ComClassFactory<C>
where
    C: Default,
    IUnknown: From<C>,
{
    pub fn new() -> IClassFactory {
        IClassFactory::from(Self(PhantomData))
    }
}

#[allow(non_snake_case)] // That's just how the names are defined
impl<C> IClassFactory_Impl for ComClassFactory_Impl<C>
where
    C: Default + 'static,
    IUnknown: From<C>,
{
    fn CreateInstance(
        &self,
        p_unk_outer: windows_core::Ref<'_, IUnknown>,
        riid: *const GUID,
        ppv_object: *mut *mut std::ffi::c_void,
    ) -> Result<()> {
        let Some(ppv_object) = NonNull::new(ppv_object) else {
            return Err(E_POINTER.into());
        };

        let Some(riid) = NonNull::new(riid.cast_mut()) else {
            return Err(E_POINTER.into());
        };

        if p_unk_outer.is_some() {
            return Err(CLASS_E_NOAGGREGATION.into());
        }

        let object = IUnknown::from(C::default());

        // SAFETY: We check that `ppv_object` (and `riid` too) isn't null and
        // `IClassFactory::query` ensures that `ppv_object` is a valid place for
        // an interface.
        unsafe {
            object
                .query(riid.as_ptr().cast_const(), ppv_object.as_ptr())
                .ok()
        }
    }

    fn LockServer(&self, _f_lock: BOOL) -> Result<()> {
        Err(E_NOTIMPL.into())
    }
}

#[doc(hidden)]
pub struct ModuleHandle(HMODULE);

impl ModuleHandle {
    #[doc(hidden)]
    pub unsafe fn from_instance(h_instance: HINSTANCE) -> Self {
        Self(h_instance.into())
    }

    #[doc(hidden)]
    pub fn raw_handle(&self) -> HMODULE {
        self.0
    }
}

unsafe impl Sync for ModuleHandle {}
unsafe impl Send for ModuleHandle {}

#[doc(hidden)]
pub struct RegistryKeyInfo {
    key_path: String,
    key_value_name: String,
    key_value_data: String,
}

#[doc(hidden)]
impl RegistryKeyInfo {
    pub fn new(key_path: &str, key_value_name: &str, key_value_data: &str) -> RegistryKeyInfo {
        RegistryKeyInfo {
            key_path: key_path.into(),
            key_value_name: key_value_name.into(),
            key_value_data: key_value_data.into(),
        }
    }
}

// To guarantee the display format of a GUID
struct CLSID(GUID);

impl std::fmt::Display for CLSID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:08X?}-{:04X?}-{:04X?}-{:02X?}{:02X?}-{:02X?}{:02X?}{:02X?}{:02X?}{:02X?}{:02X?}",
            self.0.data1,
            self.0.data2,
            self.0.data3,
            self.0.data4[0],
            self.0.data4[1],
            self.0.data4[2],
            self.0.data4[3],
            self.0.data4[4],
            self.0.data4[5],
            self.0.data4[6],
            self.0.data4[7]
        )
    }
}

#[doc(hidden)]
pub fn get_dll_file_path(expect: Option<HMODULE>) -> String {
    const MAX_FILE_PATH_LENGTH: usize = 260;

    let mut path = [0u8; MAX_FILE_PATH_LENGTH];

    let len = unsafe { GetModuleFileNameA(expect, &mut path) };

    String::from_utf8(path[..len as usize].to_vec()).unwrap()
}

#[doc(hidden)]
pub fn class_key_path(clsid: GUID) -> String {
    let clsid = CLSID(clsid);
    format!("CLSID\\{{{clsid}}}")
}

#[doc(hidden)]
pub fn class_inproc_key_path(clsid: GUID) -> String {
    let clsid = CLSID(clsid);
    format!("CLSID\\{{{clsid}}}\\InprocServer32")
}

#[doc(hidden)]
#[inline]
pub fn dll_register_server(relevant_registry_keys: Vec<RegistryKeyInfo>) -> HRESULT {
    if let Err(err) = register_keys(&relevant_registry_keys) {
        let _ = dll_unregister_server(relevant_registry_keys);
        err.into()
    } else {
        windows::Win32::Foundation::S_OK
    }
}

#[doc(hidden)]
#[inline]
pub fn dll_unregister_server(mut relevant_registry_keys: Vec<RegistryKeyInfo>) -> HRESULT {
    relevant_registry_keys.reverse();
    match unregister_keys(&relevant_registry_keys) {
        Ok(()) => windows::Win32::Foundation::S_OK,
        Err(err) => err.into(),
    }
}

fn register_keys(keys_to_add: &[RegistryKeyInfo]) -> Result<()> {
    for key_info in keys_to_add {
        add_class_key(key_info).map_err(|_| Error::from(SELFREG_E_CLASS))?;
    }

    Ok(())
}

fn unregister_keys(keys_to_remove: &[RegistryKeyInfo]) -> Result<()> {
    for key_info in keys_to_remove {
        remove_class_key(key_info).map_err(|_| Error::from(SELFREG_E_CLASS))?;
    }

    Ok(())
}

fn add_class_key(key_info: &RegistryKeyInfo) -> Result<()> {
    let key = windows_registry::CLASSES_ROOT.create(&key_info.key_path)?;
    key.set_string(&key_info.key_value_name, &key_info.key_value_data)
}

fn remove_class_key(key_info: &RegistryKeyInfo) -> Result<()> {
    windows_registry::CLASSES_ROOT.remove_tree(&key_info.key_path)
}

#[macro_export]
macro_rules! inproc_dll_module {
    (($class_id_first:expr, $class_type_first:ty), $(($class_id:expr, $class_type:ty)),*) => {
        static _HMODULE: ::std::sync::OnceLock<$crate::com_helpers::ModuleHandle> =
            ::std::sync::OnceLock::new();

        #[no_mangle]
        unsafe extern "system" fn DllMain(
            h_instance: ::windows::Win32::Foundation::HINSTANCE,
            fdw_reason: u32,
            _reserved: *mut ::std::ffi::c_void,
        ) -> ::windows::core::BOOL {
            const DLL_PROCESS_ATTACH: u32 = 1;
            if fdw_reason == DLL_PROCESS_ATTACH {
                // SAFETY: We trust that `h_instance` is a non-null and valid module handle
                let h_module = unsafe { $crate::com_helpers::ModuleHandle::from_instance(h_instance) };

                unsafe { _ = ::windows::Win32::System::LibraryLoader::DisableThreadLibraryCalls(h_module.raw_handle()); }

                _ = _HMODULE.set(h_module);
            }
            true.into()
        }

        #[no_mangle]
        unsafe extern "system" fn DllGetClassObject(
            class_id: *const ::windows::core::GUID,
            iid: *const ::windows::core::GUID,
            interface: *mut *mut ::std::ffi::c_void,
        ) -> ::windows::core::HRESULT {
            use ::windows::core::Interface;

            // SAFETY: We trust that `class_id` is a non-null and valid pointer
            let class_id = unsafe {
                class_id
                    .as_ref()
                    .expect("class id passed to DllGetClassObject should never be null")
            };

            if class_id == &$class_id_first {
                // SAFETY: We trust that `interface` is a non-null and valid pointer
                unsafe { $crate::com_helpers::ComClassFactory::<$class_type_first>::new().query(iid, interface) }
            } $(else if class_id == &$class_id {
                // SAFETY: We trust that `interface` is a non-null and valid pointer
                unsafe { $crate::com_helpers::ComClassFactory::<$class_type>::new().query(iid, interface) }
            })* else {
                ::windows::Win32::Foundation::CLASS_E_CLASSNOTAVAILABLE
            }
        }

        #[no_mangle]
        extern "system" fn DllCanUnloadNow() -> ::windows::core::HRESULT {
            // FIXME: Figure out a way to track when instances are created or destroyed
            ::windows::Win32::Foundation::S_FALSE
        }

        #[no_mangle]
        extern "system" fn DllRegisterServer() -> ::windows::core::HRESULT {
            $crate::com_helpers::dll_register_server(relevant_registry_keys())
        }

        #[no_mangle]
        extern "system" fn DllUnregisterServer() -> ::windows::core::HRESULT {
            $crate::com_helpers::dll_unregister_server(relevant_registry_keys())
        }

        fn relevant_registry_keys() -> ::std::vec::Vec<$crate::com_helpers::RegistryKeyInfo> {
            use $crate::com_helpers::RegistryKeyInfo;
            let file_path = $crate::com_helpers::get_dll_file_path(
                Some(_HMODULE.get().expect("Dll should have finished attaching").raw_handle()),
            );

            ::std::vec![
                RegistryKeyInfo::new(
                    &$crate::com_helpers::class_key_path($class_id_first),
                    "",
                    ::std::stringify!($class_type_first),
                ),
                RegistryKeyInfo::new(
                    &$crate::com_helpers::class_inproc_key_path($class_id_first),
                    "",
                    &file_path
                ),
                $(RegistryKeyInfo::new(
                    &$crate::com_helpers::class_key_path($class_id),
                    "",
                    ::std::stringify!($class_type),
                ),
                RegistryKeyInfo::new(
                    &$crate::com_helpers::class_inproc_key_path($class_id),
                    "",
                    &file_path
                )),*
            ]
        }
    };
}
