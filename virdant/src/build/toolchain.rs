//! FPGA toolchain dispatch and runners.  `toolchain_for` is keyed on
//! the platform's `@fpga` family string and is the *only* family check
//! in the codebase: platforms are unvalidated data (Decision 13), so
//! an unknown family simply fails here when a build is attempted.

use std::path::Path;

use bstr::{BStr, BString, ByteSlice};

#[derive(Debug, Clone, Copy)]
pub enum Toolchain {
    Ice40,
}

pub fn toolchain_for(fpga: &str) -> Option<Toolchain> {
    match fpga {
        "ice40" => Some(Toolchain::Ice40),
        _ => None,
    }
}

/// Runs synthesis, place-and-route, and bitstream packing for the
/// given toolchain.  `part` is the platform's `@part` value, if any.
pub fn run_toolchain(
    builddir: &Path,
    project: &str,
    top: &str,
    part: Option<&BString>,
    toolchain: Toolchain,
) -> Result<(), String> {
    match toolchain {
        Toolchain::Ice40 => run_ice40(builddir, project, top, part),
    }
}

/// Flashes the built bitstream with the given programmer, e.g. `iceprog`
/// (iCEstick) or `icesprog` (iceSUGAR).  The caller selects the
/// programmer from the resolved platform.
pub fn flash_bitstream(cwd: &Path, project: &str, tool: &str) -> Result<(), String> {
    let builddir = cwd.join("build");
    let project_bin = builddir.join(format!("{project}.bin"));

    let output = std::process::Command::new(tool)
        .arg(&project_bin)
        .output()
        .unwrap();
    if !output.status.success() {
        eprintln!("{tool} failed");
        eprintln!("{}", BStr::new(&output.stderr));
        return Err(format!("{tool} failed"));
    }
    println!("Uploaded {}", project_bin.to_string_lossy());
    Ok(())
}

fn run_ice40(
    builddir: &Path,
    project: &str,
    top: &str,
    part: Option<&BString>,
) -> Result<(), String> {
    let builddir = builddir.to_path_buf();

    // Write yosys script
    let sv_files = glob_sv_files(&builddir);
    let sv_list: Vec<String> = sv_files.iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect();
    let project_json = builddir.join(format!("{project}.json"));
    let script_content = format!(
        "read_verilog -I {} {}; synth_ice40 -top {top} -json {}",
        builddir.to_string_lossy(),
        sv_list.join(" "),
        project_json.to_string_lossy(),
    );
    let script_path = builddir.join("script.ys");
    std::fs::write(&script_path, &script_content).unwrap();

    // Run yosys
    let yosys_log = builddir.join("yosys.log");
    let output = std::process::Command::new("yosys")
        .arg("-l").arg(&yosys_log)
        .arg(&script_path)
        .output()
        .unwrap();
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        return Err("yosys failed".to_string());
    }
    println!("{}", BStr::new(&output.stdout));
    println!("yosys OK");

    // Run nextpnr-ice40
    let chip = chip_flag(part)?;
    let pcf = builddir.join(format!("{project}.pcf"));
    let project_asc = builddir.join(format!("{project}.asc"));
    let output = std::process::Command::new("nextpnr-ice40")
        .arg(format!("--{chip}"))
        .arg("--top").arg(top)
        .arg("--json").arg(&project_json)
        .arg("--pcf").arg(&pcf)
        .arg("--asc").arg(&project_asc)
        .arg("--package").arg(package_str(part))
        .output()
        .unwrap();
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        return Err("nextpnr-ice40 failed".to_string());
    }
    println!("{}", BStr::new(&output.stdout));
    println!("nextpnr-ice40 OK");

    // Run icepack
    let project_bin = builddir.join(format!("{project}.bin"));
    let output = std::process::Command::new("icepack")
        .arg("-s")
        .arg(&project_asc)
        .arg(&project_bin)
        .output()
        .unwrap();
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        return Err("icepack failed".to_string());
    }
    println!("{}", BStr::new(&output.stdout));
    println!("Bitstream written to {}", project_bin.to_string_lossy());

    Ok(())
}

/// The nextpnr `--package` value: the trailing component of `@part`
/// (`"up5k-sg48"` -> `"sg48"`), defaulting to `"sg48"` when absent.
fn package_str(part: Option<&BString>) -> String {
    match part {
        Some(part) => part.to_str_lossy()
            .rsplit('-')
            .next()
            .unwrap_or("sg48")
            .to_string(),
        None => "sg48".to_string(),
    }
}

/// The ice40 chip identifiers nextpnr-ice40 accepts as a `--<chip>`
/// flag (e.g. `--up5k`, `--hx1k`).
const ICE40_CHIPS: &[&str] = &[
    "lp384", "lp1k", "lp4k", "lp8k",
    "hx1k", "hx4k", "hx8k",
    "up3k", "up5k",
    "u1k", "u2k", "u4k",
];

/// The nextpnr `--<chip>` flag name (without the leading `--`),
/// derived from the leading component of `@part`
/// (`"hx1k-tq144"` -> `"hx1k"`), defaulting to `"up5k"` when `@part`
/// is absent. Errors if the leading component is not a chip nextpnr
/// knows about.
fn chip_flag(part: Option<&BString>) -> Result<String, String> {
    let chip = match part {
        Some(part) => part.to_str_lossy()
            .split('-')
            .next()
            .unwrap_or("up5k")
            .to_string(),
        None => "up5k".to_string(),
    };
    if ICE40_CHIPS.contains(&chip.as_str()) {
        Ok(chip)
    } else {
        Err(format!(
            "Unknown ice40 chip '{chip}' in @part (expected one of: {})",
            ICE40_CHIPS.join(", "),
        ))
    }
}

fn glob_sv_files(dir: &Path) -> Vec<std::path::PathBuf> {
    let mut sources: Vec<std::path::PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .filter_map(|entry| entry.ok().map(|entry| std::fs::canonicalize(entry.path()).unwrap()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "sv"))
        .collect();
    sources.sort();
    sources
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ice40_dispatches() {
        assert!(matches!(toolchain_for("ice40"), Some(Toolchain::Ice40)));
    }

    #[test]
    fn unknown_family_rejected() {
        for fpga in ["ecp5", "nexus", "gowin", "xilinx", ""] {
            assert!(toolchain_for(fpga).is_none(), "{fpga} should not dispatch");
        }
    }

    #[test]
    fn package_extraction() {
        let up5k = BString::from("up5k-sg48".as_bytes());
        assert_eq!(package_str(Some(&up5k)), "sg48");
        let plain = BString::from("sg48".as_bytes());
        assert_eq!(package_str(Some(&plain)), "sg48");
        assert_eq!(package_str(None), "sg48");
    }

    #[test]
    fn chip_flag_extraction() {
        let up5k = BString::from("up5k-sg48".as_bytes());
        assert_eq!(chip_flag(Some(&up5k)).unwrap(), "up5k");
        let hx1k = BString::from("hx1k-tq144".as_bytes());
        assert_eq!(chip_flag(Some(&hx1k)).unwrap(), "hx1k");
        assert_eq!(chip_flag(None).unwrap(), "up5k");
    }

    #[test]
    fn chip_flag_rejects_unknown_chip() {
        let bogus = BString::from("notachip-sg48".as_bytes());
        assert!(chip_flag(Some(&bogus)).is_err());
    }
}
