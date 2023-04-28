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
        .filter_map(|dir| dir.ok())
        .map(|dir| dir.path())
        .filter(|dir| {
            dir.components()
                .last()
                .and_then(|c| c.as_os_str().to_str())
                .map(|c| c.starts_with("10.") && dir.join("km").is_dir())
                .unwrap_or(false)
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
        .filter_map(|dir| dir.ok())
        .map(|dir| dir.path())
        .filter(|dir| {
            dir.components()
                .last()
                .and_then(|c| c.as_os_str().to_str())
                .map(|c| c.starts_with("10.") && dir.join("shared").is_dir())
                .unwrap_or(false)
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

fn build_dir() -> PathBuf {
    PathBuf::from(
        std::env::var_os("OUT_DIR").expect("the environment variable OUT_DIR is undefined"),
    )
}

fn generate() {
    // Tell Cargo to re-run this if src/wrapper.h gets changed.
    println!("cargo:rerun-if-changed=src/wrapper.h");

    // Find the include directory containing the kernel headers.
    let include_dir = get_km_dir(DirectoryType::Include).unwrap();

    // Supplimentary headers (presumably from ntddk)
    let crt_dir = include_dir.join("crt");
    let shared_dir = get_shared_dir().unwrap();

    // Get the build directory.
    let out_path = build_dir();

    // Generate the bindings
    bindgen::Builder::default()
        .header("src/wrapper.h")
        .use_core()
        .derive_debug(false)
        .layout_tests(false)
        .ctypes_prefix("::core::ffi")
        .default_enum_style(bindgen::EnumVariation::ModuleConsts)
        .clang_arg(format!("-I{}", include_dir.to_str().unwrap()))
        .clang_arg(format!("-I{}", out_path.to_str().unwrap()))
        .parse_callbacks(Box::new(RenameTyped))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        // Just so that we don't have to include typedefs for KIDTENTRY64 and KGDTENTRY64
        .blocklist_type("_?P?KPCR.*")
        .blocklist_type("_?P?KIDTENTRY64")
        .blocklist_type("_?P?KGDTENTRY64")
        .generate()
        .unwrap()
        .write_to_file(out_path.join("bindings.rs"))
        .unwrap();

    // Rerun if the wrapper stubs changed
    println!("cargo:rerun-if-changed=src/wrapper.c");
    cc::Build::new()
        .flag("/kernel")
        .include(include_dir)
        .include(crt_dir)
        .include(shared_dir)
        .include(out_path)
        .file("src/wrapper.c")
        .compile("wrapper_bindings");
}

fn header_hacks_dir() -> PathBuf {
    build_dir().join("header_hacks")
}

fn header_hacks() {
    println!("cargo:rerun-if-changed=build.rs");

    let hacks = header_hacks_dir();
    std::fs::create_dir_all(&hacks).unwrap();

    let include_dir = get_km_dir(DirectoryType::Include).unwrap();

    // ndis.h has a macro that depends on MSVC-specific expansion behaviour,
    // so make a patched header that fixes it (i.e. defaults to true)
    let ndis_header = include_dir.join("ndis.h");
    let mut contents = std::fs::read_to_string(ndis_header).unwrap();
    contents = contents.replacen("#define NDIS_API_VERSION_AVAILABLE(major,minor) ((((0x ## major) << 8) + (0x ## minor)) >= NDIS_MIN_API)", "#define NDIS_API_VERSION_AVAILABLE(major,minor) (1)", 1);

    std::fs::write(hacks.join("ndis.h"), contents).unwrap();
}

fn main() {
    header_hacks();
    generate();
}
