use std::path::PathBuf;
use thiserror::Error;
use winreg::enums::HKEY_LOCAL_MACHINE;
use winreg::RegKey;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error("cannot find the directory")]
    DirectoryNotFound,
}

/// Retrieves the path to the Windows Kits directory. The default should be
/// `C:\Program Files (x86)\Windows Kits\10`.
pub fn get_windows_kits_dir() -> Result<PathBuf, Error> {
    let hklm = RegKey::predef(HKEY_LOCAL_MACHINE);
    let key = r"SOFTWARE\Microsoft\Windows Kits\Installed Roots";
    let dir: String = hklm.open_subkey(key)?.get_value("KitsRoot10")?;

    Ok(dir.into())
}

#[derive(Clone, Copy)]
pub enum DirectoryType {
    Include,
    Library,
}

/// Retrieves the path to the kernel mode libraries. The path may look something like:
/// `C:\Program Files (x86)\Windows Kits\10\lib\10.0.18362.0\km`.
pub fn get_km_dir(dir_type: DirectoryType) -> Result<PathBuf, Error> {
    // We first append lib to the path and read the directory..
    let dir = get_windows_kits_dir()?
        .join(match dir_type {
            DirectoryType::Include => "Include",
            DirectoryType::Library => "Lib",
        })
        .read_dir()?;

    // In the lib directory we may have one or more directories named after the version of Windows,
    // we will be looking for the highest version number.
    let dir = dir
        .filter_map(Result::ok)
        .map(|dir| dir.path())
        .filter(|dir| {
            dir.components()
                .last()
                .and_then(|c| c.as_os_str().to_str())
                .map_or(false, |c| c.starts_with("10.") && dir.join("km").is_dir())
        })
        .max()
        .ok_or_else(|| Error::DirectoryNotFound)?;

    // Finally append km to the path to get the path to the kernel mode libraries.
    Ok(dir.join("km"))
}

/// Retrieves the path to the shared headers. The path may look something like:
/// `C:\Program Files (x86)\Windows Kits\10\lib\10.0.18362.0\shared`.
pub fn get_shared_dir() -> Result<PathBuf, Error> {
    // We first append lib to the path and read the directory..
    let dir = get_windows_kits_dir()?.join("Include").read_dir()?;

    // In the lib directory we may have one or more directories named after the version of Windows,
    // we will be looking for the highest version number.
    let dir = dir
        .filter_map(Result::ok)
        .map(|dir| dir.path())
        .filter(|dir| {
            dir.components()
                .last()
                .and_then(|c| c.as_os_str().to_str())
                .map_or(false, |c| {
                    c.starts_with("10.") && dir.join("shared").is_dir()
                })
        })
        .max()
        .ok_or_else(|| Error::DirectoryNotFound)?;

    // Finally append shared to the path to get the path to the shared headers.
    Ok(dir.join("shared"))
}

pub fn get_kmdf_dir(dir_type: DirectoryType) -> Result<PathBuf, Error> {
    Ok(get_windows_kits_dir()?.join(match dir_type {
        DirectoryType::Include => PathBuf::from_iter(["Include", "wdf", "kmdf", "1.31"]),
        DirectoryType::Library => PathBuf::from_iter(["Lib", "wdf", "kmdf", "x64", "1.31"]),
    }))
}

// Other half of
// https://github.com/rust-lang/rust-bindgen/issues/753#issuecomment-459851952
#[derive(Debug)]
struct RenameTyped;

impl bindgen::callbacks::ParseCallbacks for RenameTyped {
    fn item_name(&self, original_item_name: &str) -> Option<String> {
        Some(
            original_item_name
                .trim_start_matches("__rename_typed_")
                .to_owned(),
        )
    }
}

fn generate() {
    // Tell Cargo to re-run this if src/wrapper.h gets changed.
    println!("cargo:rerun-if-changed=src/wrapper.h");

    // Find the include directory containing the kernel headers.
    let include_dir = get_km_dir(DirectoryType::Include).unwrap();
    let wdf_dir = get_kmdf_dir(DirectoryType::Include).unwrap();
    let wdf_lib = get_kmdf_dir(DirectoryType::Library).unwrap();

    // Supplimentary headers (presumably from ntddk)
    let crt_dir = include_dir.join("crt");
    let shared_dir = get_shared_dir().unwrap();

    // Get the build directory.
    let out_path = PathBuf::from(
        std::env::var_os("OUT_DIR").expect("the environment variable OUT_DIR is undefined"),
    );

    // Generate the bindings
    bindgen::Builder::default()
        .header("src/wrapper.h")
        .use_core()
        .derive_debug(false)
        .layout_tests(false)
        .ctypes_prefix("::core::ffi")
        .default_enum_style(bindgen::EnumVariation::ModuleConsts)
        .bitfield_enum("_WDF_DEVICE_SHUTDOWN_FLAGS")
        .bitfield_enum("_WDF_DISPATCH_IRP_TO_IO_QUEUE_FLAGS")
        .bitfield_enum("_WDF_DEVICE_STATE_FLAGS")
        .bitfield_enum("_WDF_DMA_ENABLER_CONFIG_FLAGS")
        .bitfield_enum("_WDF_DRIVER_INIT_FLAGS")
        .bitfield_enum("_WDF_REMOVE_LOCK_OPTIONS_FLAGS")
        .bitfield_enum("_WDF_REQUEST_FORWARD_OPTIONS_FLAGS")
        .bitfield_enum("_WDF_REQUEST_REUSE_FLAGS")
        .bitfield_enum("_WDF_REQUEST_SEND_OPTIONS_FLAGS")
        .bitfield_enum("_WDF_REQUEST_STOP_ACTION_FLAGS")
        .bitfield_enum("_WDF_RETRIEVE_CHILD_FLAGS")
        .bitfield_enum("_WDF_TASK_SEND_OPTIONS_FLAGS")
        .bitfield_enum("_WDF_WMI_PROVIDER_FLAGS")
        .clang_arg(format!("-I{}", include_dir.to_str().unwrap()))
        .clang_arg(format!("-I{}", wdf_dir.to_str().unwrap()))
        .parse_callbacks(Box::new(RenameTyped))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Declared in lib.rs
        // .blocklist_item("WdfMinimumVersionRequired")
        // Just so that we don't have to include typedefs for KIDTENTRY64 and KGDTENTRY64
        .blocklist_type("_?P?KPCR.*")
        .blocklist_type("_?P?KIDTENTRY64")
        .blocklist_type("_?P?KGDTENTRY64")
        // Depends on windows-kernel-sys for the rest of the definitions
        .allowlist_recursively(false)
        .allowlist_file(".*wdf.*h")
        .generate()
        .unwrap()
        .write_to_file(out_path.join("bindings.rs"))
        .unwrap();

    // Rerun if the wrapper stubs changed
    println!("cargo:rerun-if-changed=src/wrapper.c");
    cc::Build::new()
        .flag("/kernel")
        .include(include_dir)
        .include(wdf_dir)
        .include(crt_dir)
        .include(shared_dir)
        .object(wdf_lib.join("wdfldr.lib"))
        .file("src/wrapper.c")
        .compile("wrapper_bindings");
}

fn main() {
    generate();
}
