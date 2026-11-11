//! The `vir-run` binary: simulate a design on the `virt::Virt` board
//! (`vir run`).
//!
//! `vir run` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.
//!
//! The board itself lives in [`virt`]: a Rust simulation of the
//! `virt::Virt` platform contract (UART on stdin/stdout, SPI flash,
//! `exit`/`debug` ports) driving the design through the [`Sim`]
//! engine.

mod virt;
mod vga;

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use common::{
    db_from_dir, dump_diagnostic, dump_diagnostics, project_manifest_or_exit, read_prog_key_from,
    read_virt_baud, ProjectLocation,
};

use bstr::ByteSlice;
use clap::{value_parser, Arg, ArgAction};
use std::path::PathBuf;
use virdant::analysis::symbols::SymbolKind;
use virdant::util::check_db;

/// Assemble the `vir-run` command line interface.
fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-run")
        .about("Run your design in the Virdant simulator")
        .arg(Arg::new("trace").long("trace").num_args(0..=1)
            .default_missing_value("trace.vcd")
            .help("Write a VCD trace (default: trace.vcd)"))
        .arg(Arg::new("fps-no-cap").long("fps-no-cap").action(ArgAction::SetTrue)
            .help("Run as fast as possible instead of pacing to real time"))
        .arg(Arg::new("fps").long("fps").action(ArgAction::SetTrue)
            .help("On exit, report the average clock frequency achieved (MHz)"))
        .arg(Arg::new("flash").long("flash").num_args(1)
            .value_parser(value_parser!(PathBuf))
            .help("Preload the SPI flash image (up to 16 MiB)"))
        .arg(Arg::new("cycles").long("cycles").num_args(1)
            .value_parser(value_parser!(u64))
            .help("Stop after this many emulated clock cycles (exit code 124)"))
        .arg(Arg::new("baud").long("baud").num_args(1)
            .value_name("BAUD")
            .value_parser(value_parser!(u64))
            .help("UART baud rate for the virt board (default 115200, overridable via [virt] in virdant.toml)"))
        .arg(Arg::new("verbose").short('v').long("verbose").action(ArgAction::SetTrue)
            .help("Print informational board messages"));
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);

    let trace = matches.get_one::<String>("trace").cloned();
    let flash = matches.get_one::<PathBuf>("flash").cloned();
    let cycles = matches.get_one::<u64>("cycles").copied();
    let baud = matches.get_one::<u64>("baud").copied();

    run(
        &location,
        &trace,
        matches.get_flag("fps-no-cap"),
        matches.get_flag("fps"),
        &flash,
        &cycles,
        &baud,
        matches.get_flag("verbose"),
    );
}

fn run(location: &ProjectLocation, trace: &Option<String>, fps_no_cap: bool, fps: bool, flash: &Option<PathBuf>, cycles: &Option<u64>, baud: &Option<u64>, verbose: bool) {
    let cwd = location.resolve_cwd();
    let manifest = project_manifest_or_exit(&cwd);
    let Some(top_name) = read_prog_key_from(&manifest, "top") else {
        eprintln!("virdant.toml is missing [prog] top");
        std::process::exit(1);
    };

    let db = match db_from_dir(cwd.join("src")) {
        Ok(db) => db,
        Err(e) => {
            eprintln!("ERROR: {e}");
            std::process::exit(1);
        }
    };
    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        eprintln!("Check failed");
        std::process::exit(1);
    }

    // Resolve the top module named by [prog] top.
    let symboltable = db.get_symboltable();
    let top_symbol = symboltable.items().into_iter()
        .find(|sym| sym.fqn.as_bytes() == top_name.as_bytes()
            && sym.kind == SymbolKind::ModDef)
        .unwrap_or_else(|| {
            eprintln!("Top module '{top_name}' not found");
            std::process::exit(1);
        });

    // The top module's `for` clause selects the platform; `vir run`
    // simulates the `virt::Virt` and `virt::VirtVga` boards.
    let mut builder = virdant::db::Builder::new(&db);
    let platform_id = match virdant::analysis::platform::resolve_platform_for(
        &mut builder, top_symbol.id())
    {
        Ok(Some(platform_id)) => platform_id,
        Ok(None) => {
            eprintln!("Top module '{top_name}' does not implement a platform (no `for` clause)");
            std::process::exit(1);
        }
        Err(diagnostic) => {
            dump_diagnostic(&db, &diagnostic);
            std::process::exit(1);
        }
    };
    let platform_name = symboltable.symbol(platform_id).name().to_string();
    let fpga = virdant::analysis::platform::platform_fpga(&mut builder, platform_id);
    let board = match (platform_name.as_str(), fpga.as_deref().map(|f| f.as_bytes())) {
        ("Virt", Some(f)) if f == b"virt" => virt::VirtBoard::Virt,
        ("VirtVga", Some(f)) if f == b"virt" => virt::VirtBoard::VirtVga,
        (platform_name, _) => {
            eprintln!(
                "Top module '{top_name}' implements platform '{platform_name}', \
                 but `vir run` simulates virt::Virt and virt::VirtVga only"
            );
            std::process::exit(1);
        }
    };

    // The clock runs at the frequency the platform declares (@period_ns).
    // `platform_clock_meta` allows the virtual platforms' pinless clocks
    // (virt::Virt declares @period_ns but no @pin).
    let clocks = virdant::analysis::platform::platform_clock_meta(&mut builder, platform_id);
    let Some((clock_port, _, Some(period_ns))) = clocks.into_iter().next() else {
        eprintln!("Platform '{}' declares no clock", platform_name);
        std::process::exit(1);
    };
    let _ = clock_port; // resolved by name inside the board
    let clock_hz = (1e9 / period_ns) as u64;

    // Resolve the UART baud: --baud (CLI) > [virt].baud
    // (virdant.toml) > 115200.  Validated here, before the simulator
    // starts: at least 1, and slow enough for at least 2 cycles per
    // bit on the platform's clock.
    let baud = match baud {
        Some(baud) => *baud,
        None => read_virt_baud(&cwd).unwrap_or(115_200),
    };
    if baud < 1 {
        eprintln!("error: baud must be at least 1");
        std::process::exit(1);
    }
    if baud > clock_hz / 2 {
        eprintln!(
            "error: baud {baud} is too fast for a {clock_hz} Hz clock (need at least 2 cycles per bit)"
        );
        std::process::exit(1);
    }

    let exit_code = virt::run(
        &db,
        top_symbol.id(),
        clock_hz,
        board,
        virt::VirtOptions {
            flash: flash.clone(),
            trace: trace.clone().map(PathBuf::from),
            fps_no_cap: fps_no_cap,
            cycles: cycles.clone(),
            baud,
            fps: fps,
            verbose,
        },
    );
    std::process::exit(exit_code);
}
