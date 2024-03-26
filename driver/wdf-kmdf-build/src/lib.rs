//! Shared build flags required for any KMDF-based driver.

#[derive(Default, Debug)]
pub struct Config {
    /// If lld-link is used to perform the final linking.
    /// This comes with the benefit of enabling cross-language LTO,
    /// at the cost of working around lld-link's missing support for drivers.
    pub use_lld_link: bool,
}

/// Emits the build flags required to build a KMDF driver
pub fn emit_build_flags(config: Config) {
    // Emit link flags
    let link_flags = &[
        "/NOLOGO",
        "/NXCOMPAT",
        "/NODEFAULTLIB",
        "/SUBSYSTEM:NATIVE",
        // the flag that indicates that we want our pages to be non-pagable too (except for INIT & PAGE*)
        //
        // lld-link currently doesn't
        // - apply the non-pagable attribute to the rest of the sections, nor
        // - mark the INIT section as discardable.
        //
        // lld-link technically mark sections starting with `PAGE` as pageable, but only because sections
        // need to be explicitly marked as being non-pagable.
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

    if config.use_lld_link {
        // Emit lld-link compat link flags:
        let link_flags = &[
            // - get rid of .retplne section
            "/MERGE:.retplne=INIT",
            // - mark sections as not pagable
            //   note: this is the opposite of what msvc specifies, we can't do !P
            //   note: this also overrides the base section attributes, so we have to specify them again
            "/SECTION:.text,PRE",
            "/SECTION:.rdata,PR",
            "/SECTION:.data,PRW",
            "/SECTION:.pdata,PR",
            // - mark INIT as discardable
            "/SECTION:INIT,DRE",
        ];

    for flag in link_flags {
        println!("cargo:rustc-cdylib-link-arg={flag}");
    }
    }
}
