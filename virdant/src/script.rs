//! Scripting support for Virdant simulation.
//!
//! Before running a script, the working directory is changed to the
//! script's directory.
//! This allows scripts to use relative paths to reference Virdant files.

/// Run a script file.
///
/// The working directory is changed to the script's directory before
/// execution,
/// so relative paths in the script are resolved relative to the script
/// location.
pub fn run_script_file(path: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
    Err(format!(
        "Scripting is no longer supported ('.{}' files are not runnable)",
        path.extension()
            .and_then(|e| e.to_str())
            .unwrap_or("<no ext>")
    )
    .into())
}