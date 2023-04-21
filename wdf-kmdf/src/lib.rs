//! Rust KMDF Abstractions
#![no_std]

pub mod raw {
    //! Raw dispatch helpers bindings to KMDF functions

    #[doc(hidden)]
    pub use paste::paste;
    #[doc(hidden)]
    pub use static_assertions::const_assert;

    /// Which function table to use
    #[macro_export]
    macro_rules! function_table {
        () => {
            wdf_kmdf_sys::WdfFunctions_01031
        };
    }

    /// Dispatches to an always-available WDF Function
    #[macro_export]
    macro_rules! dispatch_always {
        ($name:ident ( $($args:expr),* $(,)? )) => {{
            let fn_handle = {$crate::raw::paste! {
                const FN_INDEX: usize = wdf_kmdf_sys::_WDFFUNCENUM::[<$name TableIndex>] as usize;

                // Must be in the always-available function category
                $crate::raw::const_assert!(FN_INDEX < wdf_kmdf_sys::WDF_ALWAYS_AVAILABLE_FUNCTION_COUNT as usize);

                let fn_handle = $crate::function_table!()
                    .add(FN_INDEX)
                    .cast::<wdf_kmdf_sys::[<PFN_ $name:upper>]>();

                // SAFETY: Ensured that this is present by the static assert
                unsafe { fn_handle.read().unwrap_unchecked() }
            }};

            // Pass unsafety to caller
            fn_handle(wdf_kmdf_sys::WdfDriverGlobals, $($args),*)
        }};
    }
}
