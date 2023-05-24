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

pub fn get_kmdf_dir(dir_type: DirectoryType) -> Result<PathBuf, Error> {
    Ok(get_windows_kits_dir()?.join(match dir_type {
        DirectoryType::Include => PathBuf::from_iter(["Include", "wdf", "kmdf", "1.31"]),
        DirectoryType::Library => PathBuf::from_iter(["Lib", "wdf", "kmdf", "x64", "1.31"]),
    }))
}

fn main() {
    let wdf_lib = get_kmdf_dir(DirectoryType::Library).unwrap();
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
    println!("cargo:rustc-link-search={}", wdf_lib.display());

    println!("cargo:rustc-link-lib=ntoskrnl");
    // for __security_cookie
    println!("cargo:rustc-link-lib=bufferoverflowk");
    println!("cargo:rustc-link-lib=wdmsec");

    println!("cargo:rustc-link-lib=wdfldr");
    println!("cargo:rustc-link-lib=wdfdriverentry");
}
