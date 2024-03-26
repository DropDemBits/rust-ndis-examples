use std::path::PathBuf;
use thiserror::Error;

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
    let hklm = windows_registry::LOCAL_MACHINE;
    let key = r"SOFTWARE\Microsoft\Windows Kits\Installed Roots";
    let dir: String = hklm
        .open(key)
        .map_err(|_| Error::DirectoryNotFound)?
        .get_string("KitsRoot10")
        .map_err(|_| Error::DirectoryNotFound)?;

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

fn build_dir() -> PathBuf {
    PathBuf::from(
        std::env::var_os("OUT_DIR").expect("the environment variable OUT_DIR is undefined"),
    )
}

fn src_dir() -> PathBuf {
    PathBuf::from(
        std::env::var_os("CARGO_MANIFEST_DIR")
            .expect("the environment variable CARGO_MANIFEST_DIR is undefined"),
    )
}

fn generate() {
    // Tell Cargo to re-run this if src/wrapper.h or src/wrapper.c gets changed.
    println!("cargo:rerun-if-changed=src/wrapper.h");
    println!("cargo:rerun-if-changed=src/wrapper.c");

    // Find the include directory containing the kernel headers.
    let include_dir = get_km_dir(DirectoryType::Include).unwrap();

    // Get the build directory.
    let out_path = build_dir();
    let in_dir = src_dir();

    // Collect binding information
    let bindgen = bindgen::Builder::default()
        .header("src/wrapper.h")
        .use_core()
        .derive_debug(false)
        .layout_tests(false)
        .ctypes_prefix("::core::ffi")
        .default_enum_style(bindgen::EnumVariation::NewType {
            is_bitfield: false,
            is_global: false,
        })
        .clang_arg(format!("-I{}", include_dir.to_str().unwrap()))
        .clang_arg(format!("-I{}", out_path.to_str().unwrap()))
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Just so that we don't have to include typedefs for KIDTENTRY64 and KGDTENTRY64
        .blocklist_type("_?P?KPCR.*")
        .blocklist_type("_?P?KIDTENTRY64")
        .blocklist_type("_?P?KGDTENTRY64")
        // override as u32, and we'll always use `winresult`'s constants
        .blocklist_type("P?C?NTSTATUS")
        .blocklist_type("P?NDIS_STATUS");

    // Generate the wrappers
    let obj_path = out_path.join("wrapper_bindings.obj");
    let lib_path = out_path.join("wrapper_bindings.lib");

    let clang_output = std::process::Command::new("clang-cl")
        .arg("-flto=thin")
        .arg("/O1")
        .arg("/c")
        .arg("/kernel")
        .arg("/guard:cf")
        .arg(format!("/Fo{}", obj_path.display()))
        .arg(&in_dir.join("src").join("wrapper.c"))
        .arg("/I")
        .arg(&include_dir)
        .arg("/I")
        .arg(&out_path)
        .arg("/FI")
        .arg(&in_dir.join("src").join("wrapper.h"))
        .output()
        .unwrap();

    if !clang_output.status.success() {
        panic!(
            "Could not compile object file:\n{}",
            String::from_utf8_lossy(&clang_output.stderr)
        );
    }

    // turn the obj into a static binary
    let lib_output = std::process::Command::new("llvm-lib")
        .arg(&obj_path)
        .arg(format!("/OUT:{}", lib_path.display()))
        .output()
        .unwrap();

    if !lib_output.status.success() {
        panic!(
            "Could not emit library file:\n{}",
            String::from_utf8_lossy(&lib_output.stderr)
        );
    }

    println!("cargo:rustc-link-search=native={}", out_path.display());

    // Generate the bindings
    bindgen
        .generate()
        .unwrap()
        .write_to_file(out_path.join("bindings.rs"))
        .unwrap();
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

    let path = hacks.join("ndis.h");

    // check if no change happened (this is faster than just always doing it, and likely more reliable than checking mtime of the original header)
    match std::fs::read_to_string(&path) {
        Ok(old_contents) if contents == old_contents => (),
        _ => {
            std::fs::write(&path, contents).unwrap();
        }
    }
}

fn main() {
    let km_dir = get_km_dir(DirectoryType::Library).unwrap();

    let target = std::env::var("TARGET").unwrap();
    if !target.contains("x86_64") {
        panic!("The target {target} is currently not supported.");
    }

    let km_dir = km_dir.join("x64");

    println!(
        "cargo:rustc-link-search=native={}",
        get_km_dir(DirectoryType::Library).unwrap().display()
    );
    println!("cargo:rustc-link-search={}", km_dir.display());

    header_hacks();
    generate();
}
