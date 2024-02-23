fn main() {
    // Emit link flags
    let link_flags = &[
        "/NOLOGO",
        "/NXCOMPAT",
        "/NODEFAULTLIB",
        "/SUBSYSTEM:NATIVE",
        "/DRIVER",
        "/DYNAMICBASE",
        "/MANIFEST:NO",
        "/OPT:REF,ICF",
        // See <https://github.com/SergiusTheBest/FindWDK/blob/c941028b26565f756a16ea815d51ac9781f00e79/cmake/FindWdk.cmakeL174>
        "/ENTRY:FxDriverEntry",
        "/MERGE:.edata=.rdata",
        "/MERGE:.rustc=.data",
        "/INTEGRITYCHECK",
    ];
    for flag in link_flags {
        println!("cargo:rustc-cdylib-link-arg={flag}");
    }
}
