//! The `vir-run-icarus` binary: run a built Virdant package with Icarus
//! Verilog (formerly the `run-icarus` subcommand of `vir`).
//!
//! `vir run-icarus` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.

use std::ffi::CString;
use std::path::PathBuf;

use bstr::BStr;
use clap::value_parser;
use nix::unistd::execvp;

#[path = "../common/help_json.rs"]
mod help_json;

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use common::ProjectLocation;

fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-run-icarus")
        .about("Run Virdant package with Icarus Verilog")
        .arg(
            clap::Arg::new("path")
                .value_name("PATH")
                .value_parser(value_parser!(PathBuf))
                .required(false),
        )
        .arg(
            clap::Arg::new("vcd")
                .long("vcd")
                .value_name("VCD")
                .value_parser(value_parser!(String)),
        );
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    help_json::maybe_dump(command);

    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);
    let _path = matches.get_one::<PathBuf>("path");
    let vcd = matches.get_one::<String>("vcd");

    run_icarus(&location, vcd.map(String::as_str));
}

fn run_icarus(location: &ProjectLocation, vcd: Option<&str>) {
    let cwd = location.resolve_cwd();

    let virdant_toml_text =
        std::fs::read_to_string(common::project_manifest_or_exit(&cwd)).unwrap();
    let virtant_toml: toml::Value = toml::from_str(&virdant_toml_text).unwrap();

    let project = virtant_toml["project"]["name"].as_str().unwrap();
    let builddir = cwd.join("build");

    let source_dir = cwd.join("src");
    let db = virdant::util::db_from_dir(source_dir);
    common::dump_diagnostics(&db);
    if virdant::util::check_db(&db).is_err() {
        eprintln!("Build failed");
        std::process::exit(1);
    }

    std::fs::create_dir_all(&builddir).unwrap();
    common::copy_ext_dir(&builddir);
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());

    let bin_name = project;
    let bin = builddir.join(&bin_name).to_string_lossy().to_string();

    let mut command = std::process::Command::new("iverilog");
    command.arg("-g2012");
    command.arg("verilog/tb.sv");

    for source in common::glob_sv_files(&builddir) {
        command.arg(source);
    }
    let output = command
        .arg("-o")
        .arg(bin.clone()).output().unwrap();

    if !output.status.success() {
        eprintln!("iverilog: {}", BStr::new(&output.stderr));
        std::process::exit(1);
    } else {
        println!("Icarus Verilog output: {bin}");
    }

    // Canonicalize to an absolute path before changing directory, so the
    // binary remains findable after the chdir below.
    let bin_abs = std::fs::canonicalize(&bin).unwrap().to_string_lossy().to_string();

    // Change into the build directory so that relative paths in $readmemh
    // (and similar Verilog system tasks) resolve against the right directory.
    std::env::set_current_dir(&builddir).unwrap();

    let program = CString::new(bin_abs.clone()).unwrap();
    let mut args: Vec<CString> = vec![
        CString::new(bin_abs).unwrap(),
    ];
    if let Some(vcd) = vcd {
        args.push(CString::new(format!("+vcd={vcd}")).unwrap());
    }

    let _ = execvp(&program, &args);
}
