//! Scripting support for Virdant simulation.
//!
//! Supports Rhai scripting when the `rhai` feature is enabled (default),
//! and Lua scripting when the `lua` feature is enabled.
//!
//! Before running a script, the working directory is changed to the script's directory.
//! This allows scripts to use relative paths to reference Virdant files.

#[cfg(feature = "rhai")]
pub mod rhai;

#[cfg(feature = "lua")]
pub mod lua;

#[cfg(feature = "rhai")]
pub use rhai::run_script_file as run_rhai_script_file;

#[cfg(feature = "lua")]
pub use lua::run_script_file as run_lua_script_file;

/// Run a script file, dispatching based on file extension.
/// - `.lua` files are run with the Lua engine (requires `lua` feature)
/// - `.rhai` files are run with the Rhai engine (requires `rhai` feature)
///
/// The working directory is changed to the script's directory before execution,
/// so relative paths in the script are resolved relative to the script location.
pub fn run_script_file(path: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
    // Canonicalize the path to get an absolute path
    let abs_path = path.canonicalize()
        .map_err(|e| format!("Failed to resolve script path '{}': {}", path.display(), e))?;

    // Change to the script's directory
    if let Some(script_dir) = abs_path.parent() {
        std::env::set_current_dir(script_dir)
            .map_err(|e| format!("Failed to change to script directory '{}': {}", script_dir.display(), e))?;
    }

    // Get just the filename for the script engines (they'll read from current dir)
    let script_name = abs_path.file_name()
        .ok_or_else(|| format!("Invalid script path: {}", path.display()))?;
    let script_path = std::path::Path::new(script_name);

    let ext = script_path.extension().and_then(|e| e.to_str()).unwrap_or("");

    match ext {
        #[cfg(feature = "lua")]
        "lua" => lua::run_script_file(script_path),
        #[cfg(not(feature = "lua"))]
        "lua" => Err("Lua scripting requires the 'lua' feature to be enabled".into()),
        #[cfg(feature = "rhai")]
        _ => rhai::run_script_file(script_path).map_err(|e| e.into()),
        #[cfg(not(feature = "rhai"))]
        _ => Err("Rhai scripting requires the 'rhai' feature to be enabled".into()),
    }
}
