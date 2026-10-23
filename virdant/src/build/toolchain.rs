//! FPGA toolchain dispatch and runners.  `toolchain_for` is keyed on
//! the platform's `@fpga` family string and is the *only* family check
//! in the codebase: platforms are unvalidated data (Decision 13), so
//! an unknown family simply fails here when a build is attempted.

use std::path::{Path, PathBuf};
use std::process::Stdio;

use bstr::{BStr, BString, ByteSlice};

#[derive(Debug, Clone, Copy)]
pub enum Toolchain {
    Ice40,
    Xilinx,
}

pub fn toolchain_for(fpga: &str) -> Option<Toolchain> {
    match fpga {
        "ice40" => Some(Toolchain::Ice40),
        "xilinx" => Some(Toolchain::Xilinx),
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
    freq_mhz: Option<f64>,
    toolchain: Toolchain,
) -> Result<(), String> {
    match toolchain {
        Toolchain::Ice40 => run_ice40(builddir, project, top, part, freq_mhz),
        // `--freq` for nextpnr-xilinx is a separate follow-up; the
        // Xilinx runner is left untouched until then.
        Toolchain::Xilinx => run_xilinx(builddir, project, top, part),
    }
}

/// Flashes the built bitstream with the given programmer, e.g. `iceprog`
/// (iCEstick) or `icesprog` (iceSUGAR).  The caller selects the
/// programmer from the resolved platform.
pub fn flash_bitstream(
    cwd: &Path,
    project: &str,
    tool: &str,
    tool_args: &[&str],
) -> Result<(), String> {
    let builddir = cwd.join("build");
    let project_bin = builddir.join(format!("{project}.bin"));

    let output = std::process::Command::new(tool)
        .args(tool_args)
        .arg(&project_bin)
        .output();
    let output = match output {
        Ok(output) => output,
        Err(err) => return Err(format!("could not run '{tool}': {err}")),
    };
    if !output.status.success() {
        // dfu-util reports failure when the device reboots into the freshly
        // downloaded bitstream before its final status poll: reaching that
        // poll means every byte was sent, and a Fomu drops off USB the moment
        // the new bitstream boots.  That is evidence of success, not failure,
        // so treat it as done.  Every other error path still fails below.
        let diagnostics = BStr::new(&output.stderr).to_str_lossy().into_owned();
        if diagnostics.contains("unable to read DFU status after completion") {
            eprintln!("{diagnostics}");
            println!("Uploaded {}", project_bin.to_string_lossy());
            return Ok(());
        }
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
    freq_mhz: Option<f64>,
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
    let mut command = std::process::Command::new("nextpnr-ice40");
    command
        .arg(format!("--{chip}"))
        .arg("--top").arg(top)
        .arg("--json").arg(&project_json)
        .arg("--pcf").arg(&pcf)
        .arg("--asc").arg(&project_asc)
        .arg("--package").arg(package_str(part));
    if let Some(mhz) = freq_mhz {
        command.arg("--freq").arg(format!("{mhz}"));
    }
    // nextpnr writes its progress (including the Fmax summary) to
    // stderr, so capturing it would hide everything during a successful
    // build.  Inherit the terminal's stdio instead, so the output
    // streams live the way yosys's does; the exit status is all the
    // success/failure signal we need.
    let status = command
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .unwrap();
    if !status.success() {
        return Err("nextpnr-ice40 failed".to_string());
    }
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

/// Runs synthesis, place-and-route, and bitstream assembly for Xilinx
/// 7-series FPGAs: yosys `synth_xilinx`, nextpnr-xilinx, then Project
/// X-Ray's frame tools (`fasm2frames`, `xc7frames2bit`).
///
/// `part` is the platform's `@part` value, e.g. `xc7a35tcsg324-1`, and
/// is required: nextpnr-xilinx needs the chipdb named for the die and
/// package, and the frame tools need the part name.  The chipdb
/// directory comes from `NEXTPNR_CHIPDB_DIR` and the Project X-Ray
/// database root from `PRJXRAY_DB_DIR` (the variable the openXC7
/// toolchain's `export.sh` sets).  The invocation matches the
/// openXC7 flow (`nextpnr-xilinx --chipdb ... --xdc ... --json ...
/// --fasm ...`, `fasm2frames --part ... --db-root ...`, `xc7frames2bit
/// --part_file ... --part_name ...`), which is also the archived
/// nextpnr-xilinx CLI.
fn run_xilinx(
    builddir: &Path,
    project: &str,
    top: &str,
    part: Option<&BString>,
) -> Result<(), String> {
    let part = match part {
        Some(part) => part.to_str_lossy().into_owned(),
        None => {
            return Err(
                "Platform is missing @part (nextpnr-xilinx needs a part string like xc7a35tcsg324-1)"
                    .to_string(),
            )
        }
    };

    // Fail fast on the toolchain's external state before synthesizing.
    let chipdb = chipdb_path(&part)?;
    let db_root = xray_db_root(&part)?;

    let builddir = builddir.to_path_buf();

    // Write yosys script
    let sv_files = glob_sv_files(&builddir);
    let sv_list: Vec<String> = sv_files.iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect();
    let project_json = builddir.join(format!("{project}.json"));
    let script_content = format!(
        "read_verilog -I {} {}; synth_xilinx -flatten -abc9 -arch xc7 -top {top}; write_json {}",
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

    // Run nextpnr-xilinx
    let xdc = builddir.join(format!("{project}.xdc"));
    let fasm = builddir.join(format!("{project}.fasm"));
    let output = match std::process::Command::new("nextpnr-xilinx")
        .arg("--chipdb").arg(&chipdb)
        .arg("--xdc").arg(&xdc)
        .arg("--json").arg(&project_json)
        .arg("--fasm").arg(&fasm)
        .output()
    {
        Ok(output) => output,
        Err(err) => return Err(format!("could not run 'nextpnr-xilinx': {err}")),
    };
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        return Err("nextpnr-xilinx failed".to_string());
    }
    println!("{}", BStr::new(&output.stdout));
    println!("nextpnr-xilinx OK");

    // Run fasm2frames: the FASM goes to frames on stdout, so the file
    // is the child's stdout and only stderr is captured.
    let frames = builddir.join(format!("{project}.frames"));
    let frames_out = std::fs::File::create(&frames)
        .map_err(|err| format!("could not create {frames:?}: {err}"))?;
    let output = match std::process::Command::new("fasm2frames")
        .arg("--part").arg(&part)
        .arg("--db-root").arg(&db_root)
        .arg(&fasm)
        .stdout(Stdio::from(frames_out))
        .output()
    {
        Ok(output) => output,
        Err(err) => return Err(format!("could not run 'fasm2frames': {err}")),
    };
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        return Err("fasm2frames failed".to_string());
    }
    println!("fasm2frames OK");

    // Run xc7frames2bit
    let project_bin = builddir.join(format!("{project}.bit"));
    let part_file = db_root.join(&part).join("part.yaml");
    let output = match std::process::Command::new("xc7frames2bit")
        .arg("--part_file").arg(&part_file)
        .arg("--part_name").arg(&part)
        .arg("--frm_file").arg(&frames)
        .arg("--output_file").arg(&project_bin)
        .output()
    {
        Ok(output) => output,
        Err(err) => return Err(format!("could not run 'xc7frames2bit': {err}")),
    };
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        return Err("xc7frames2bit failed".to_string());
    }
    println!("{}", BStr::new(&output.stdout));
    println!("Bitstream written to {}", project_bin.to_string_lossy());

    Ok(())
}

/// The prjxray database root for a 7-series part: `PRJXRAY_DB_DIR`
/// plus the part's family directory (`"xc7a35tcsg324-1"` ->
/// `<db>/artix7`).  Requires the part's `part.yaml` to exist there,
/// since both frame tools need it.
fn xray_db_root(part: &str) -> Result<std::path::PathBuf, String> {
    let db = std::env::var("PRJXRAY_DB_DIR").map_err(|_| {
        "PRJXRAY_DB_DIR is not set (point it at the Project X-Ray database root, \
         e.g. as the openXC7 toolchain's export.sh does)"
            .to_string()
    })?;
    let family = xray_family(part)?;
    let part_file = PathBuf::from(&db).join(family).join(part).join("part.yaml");
    if !part_file.exists() {
        return Err(format!(
            "PRJXRAY_DB_DIR ({db:?}) has no {family}/{part}/part.yaml (is it a Project X-Ray database root?)"
        ));
    }
    Ok(PathBuf::from(&db).join(family))
}

/// The prjxray database family directory for a 7-series part
/// (`"xc7a35tcsg324-1"` -> `"artix7"`).
fn xray_family(part: &str) -> Result<&'static str, String> {
    if part.starts_with("xc7a") {
        Ok("artix7")
    } else if part.starts_with("xc7k") {
        Ok("kintex7")
    } else if part.starts_with("xc7s") {
        Ok("spartan7")
    } else if part.starts_with("xc7z") {
        Ok("zynq7")
    } else {
        Err(format!(
            "Unknown 7-series part '{part}' (expected an xc7a/xc7k/xc7s/xc7z part)"
        ))
    }
}

/// The part string minus its trailing speed grade, as nextpnr-xilinx
/// chipdb files are named (`"xc7a35tcsg324-1"` -> `"xc7a35tcsg324"`).
fn chipdb_name(part: &str) -> String {
    let Some((head, tail)) = part.rsplit_once('-') else {
        return part.to_string();
    };
    let tail = tail.strip_suffix('L').unwrap_or(tail);
    if !tail.is_empty() && tail.chars().all(|c| c.is_ascii_digit()) {
        head.to_string()
    } else {
        part.to_string()
    }
}

/// The chipdb file nextpnr-xilinx `--chipdb` wants: the part with the
/// speed-grade suffix stripped, resolved against `NEXTPNR_CHIPDB_DIR`.
/// Both plain (`xc7a35tcsg324.bin`) and `chipdb-` prefixed file names
/// are accepted, matching the chipdb layouts in the wild.
fn chipdb_path(part: &str) -> Result<PathBuf, String> {
    let die_package = chipdb_name(part);
    let dir = std::env::var("NEXTPNR_CHIPDB_DIR").map_err(|_| {
        "NEXTPNR_CHIPDB_DIR is not set (point it at a directory of nextpnr-xilinx \
         chip databases, e.g. one containing xc7a35tcsg324.bin)"
            .to_string()
    })?;
    let dir = PathBuf::from(dir);
    for name in [format!("{die_package}.bin"), format!("chipdb-{die_package}.bin")] {
        let path = dir.join(&name);
        if path.exists() {
            return Ok(path);
        }
    }
    Err(format!(
        "No chipdb for part '{part}' in {dir:?} (expected {die_package}.bin; generate it \
         with nextpnr-xilinx's xilinx_gen.py, or point NEXTPNR_CHIPDB_DIR at the installed \
         chip databases)"
    ))
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
    let mut sources: Vec<std::path::PathBuf> = vec![];
    collect_sv_files(dir, &mut sources);
    sources.sort();
    sources
}

fn collect_sv_files(dir: &Path, sources: &mut Vec<std::path::PathBuf>) {
    for entry in std::fs::read_dir(dir).unwrap() {
        let path = std::fs::canonicalize(entry.unwrap().path()).unwrap();
        if path.is_dir() {
            collect_sv_files(&path, sources);
        } else if path.extension().is_some_and(|ext| ext == "sv") {
            sources.push(path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ice40_dispatches() {
        assert!(matches!(toolchain_for("ice40"), Some(Toolchain::Ice40)));
    }

    #[test]
    fn xilinx_dispatches() {
        assert!(matches!(toolchain_for("xilinx"), Some(Toolchain::Xilinx)));
    }

    #[test]
    fn unknown_family_rejected() {
        for fpga in ["ecp5", "nexus", "gowin", ""] {
            assert!(toolchain_for(fpga).is_none(), "{fpga} should not dispatch");
        }
    }

    #[test]
    fn chipdb_name_strips_speed_grade() {
        assert_eq!(chipdb_name("xc7a35tcsg324-1"), "xc7a35tcsg324");
        assert_eq!(chipdb_name("xc7a100tcsg324-1"), "xc7a100tcsg324");
        assert_eq!(chipdb_name("xc7k325tffg900-2"), "xc7k325tffg900");
        assert_eq!(chipdb_name("xc7a35tcsg324"), "xc7a35tcsg324");
        assert_eq!(chipdb_name("xc7a35tcsg324-1L"), "xc7a35tcsg324");
    }

    #[test]
    fn xray_family_directories() {
        assert_eq!(xray_family("xc7a35tcsg324-1").unwrap(), "artix7");
        assert_eq!(xray_family("xc7a100tcsg324-1").unwrap(), "artix7");
        assert_eq!(xray_family("xc7k325tffg900-2").unwrap(), "kintex7");
        assert_eq!(xray_family("xc7s50csg324-1").unwrap(), "spartan7");
        assert_eq!(xray_family("xc7z020clg484-1").unwrap(), "zynq7");
        assert!(xray_family("xc6slx16csg324").is_err());
    }

    #[test]
    fn xilinx_part_required() {
        assert!(run_xilinx(Path::new("/nonexistent"), "arty", "Top", None).is_err());
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
