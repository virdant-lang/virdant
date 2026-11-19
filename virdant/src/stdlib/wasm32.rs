//! The `wasm32` variant: the standard library embedded at compile
//! time — the WASI mirror of [`crate::stdlib::native`] (the host
//! resolves `lib/` beside the executable, which has no meaning under
//! wasmtime).  Identical API, deterministic order.

use bstr::BStr;

/// The file for stdlib package `name`: never a filesystem artifact
/// on `wasm32` — the sources are embedded.  Present for API parity
/// with [`crate::stdlib::native`].
pub fn package_path(_name: &BStr) -> Option<std::path::PathBuf> {
    None
}

/// The standard library as `(package name, source text)` pairs, in
/// deterministic order — embedded via `include_str!`, so the `.wasm`
/// artifact is self-contained.  Edited `lib/*.vir` files trigger
/// recompilation automatically (rustc dep-info tracks them).
pub fn sources() -> Vec<(bstr::BString, bstr::BString)> {
    LIB_SOURCES
        .iter()
        .map(|(name, text)| (bstr::BString::from(*name), bstr::BString::from(*text)))
        .collect()
}

const LIB_SOURCES: &[(&str, &str)] = &[
    ("builtin", include_str!("../../../lib/builtin.vir")),
    ("ice40", include_str!("../../../lib/ice40.vir")),
    ("virt", include_str!("../../../lib/virt.vir")),
    ("xilinx", include_str!("../../../lib/xilinx.vir")),
];
