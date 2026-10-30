//! The main `vir` CLI entry point dispatching subcommands for
//! type-checking, Verilog compilation, running, place-and-route,
//! project scaffolding, and delegating companion binaries (`vir-doc`,
//! `vir-run-icarus`, `vir-inspect`, and any other `vir-*` binary found
//! on `$PATH` or next to the `vir` executable).

mod cli;
mod virt;

use cli::{Args, Command};

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use bstr::{BStr, BString, ByteSlice};
use nix::unistd::execvp;
use virdant::util::{check_db, db_from_dir, db_from_files};
use std::ffi::{CString, OsString};
use std::sync::OnceLock;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use virdant::db::Db;
use virdant::common::PortDir;
use virdant::analysis::symbols::SymbolKind;

use common::{
    copy_ext_dir, dump_diagnostic, dump_diagnostics, read_prog_key, read_prog_key_from,
    read_project_name_from,
};

// The `Args` struct and `Command` enum are defined in `cli`, built up
// with the vanilla clap builder API (see `cli.rs`).

/// The version string baked in at compile time by `vir/build.rs`, e.g.
/// `v0.4.0 (29ace3e 2026-10-29)`.
pub fn version_string() -> &'static str {
    env!("VIR_VERSION_STRING")
}

// The command listing lives in the file `vir/commands` (from the project
// root), embedded
// into the binary at compile time and parsed lazily, once, on first use.
// See the comments in that file for the format and its purpose: commands
// are organized into groups, and any command not listed there is a
// development command, hidden from help.
struct CommandGroup {
    name: String,
    commands: Vec<CommandEntry>,
}

/// One command line from `vir/commands`: the command name and its
/// help message (`None` if the line names the command only).
struct CommandEntry {
    name: String,
    help: Option<String>,
}

fn command_groups() -> &'static Vec<CommandGroup> {
    static COMMAND_GROUPS: OnceLock<Vec<CommandGroup>> = OnceLock::new();
    static COMMANDS: &str = include_str!("../../commands");
    COMMAND_GROUPS.get_or_init(|| {
        let mut groups: Vec<CommandGroup> = Vec::new();
        for line in COMMANDS.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            if let Some(name) = line.strip_suffix(':') {
                groups.push(CommandGroup { name: name.trim().to_string(), commands: Vec::new() });
            } else if let Some(group) = groups.last_mut() {
                // A command line is the command name, optionally
                // followed by its help message; whitespace separates
                // them, and the help message is trimmed on either end.
                let (name, help) = match line.split_once(char::is_whitespace) {
                    Some((name, help)) => (name.to_string(), Some(help.trim().to_string())),
                    None => (line.to_string(), None),
                };
                group.commands.push(CommandEntry { name, help });
            }
            // A command before any group header is ignored.
        }
        groups
    })
}

/// Returns `true` if `name` is listed in `vir/commands`.
/// Unlisted commands are development commands and are hidden from help.
fn is_listed_command(name: &str) -> bool {
    command_groups().iter().any(|g| g.commands.iter().any(|c| c.name == name))
}

fn main() {
    let args = cli::parse();

    match args.command {
        Command::Version {} => {
            println!("vir {}", version_string());
        }
        Command::Check { } => check(&args),
        Command::Compile { path } => compile(path),
        Command::Build { } => build(&args),
        Command::Run { ref trace, fps_no_cap, fps, ref flash, ref cycles, verbose } => run(&args, trace, fps_no_cap, fps, flash, cycles, verbose),
        Command::New { ref project } => new_project(&args, project),
        Command::Bitstream { } => { bitstream(&args); },
        Command::Synth { } => synth(&args),
        Command::Pnr { ref tries, ref seed, allow_timing_failure } => {
            pnr(&args, *tries, *seed, allow_timing_failure)
        },
        Command::Upload { ref tries, ref seed, allow_timing_failure } => {
            upload(&args, *tries, *seed, allow_timing_failure)
        },
        Command::External(args) => exec_external(args),
    }
}

/// Canonicalize `args.location`'s cwd if present, else the current directory.
fn resolve_cwd(args: &Args) -> PathBuf {
    args.location.resolve_cwd()
}

fn project_db(args: &Args) -> Db {
    args.location.project_db()
}

fn check(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);
    match check_db(&db) {
        Err(_) => {
            eprintln!("Check failed");
            std::process::exit(1);
        }
        Ok(_) => {
            eprintln!("Check OK");
        }
    }
}

fn build(args: &Args) {
    let (db, builddir) = if let Some(virfile) = &args.location.virfile {
        let paths: Vec<std::path::PathBuf> = virfile
            .split(',')
            .map(|s| std::path::PathBuf::from(s.trim()))
            .collect();
        for path in &paths {
            if !path.is_file() {
                eprintln!("ERROR: file not found: {}", path.display());
                std::process::exit(1);
            }
        }
        let db = db_from_files(paths);
        let builddir = std::env::current_dir().unwrap().join("build");
        (db, builddir)
    } else {
        let cwd = resolve_cwd(args);

        let virdant_toml_text =
            std::fs::read_to_string(common::project_manifest_or_exit(&cwd)).unwrap();
        let virtant_toml: toml::Value = toml::from_str(&virdant_toml_text).unwrap();
        let _ = virtant_toml["project"]["name"].as_str().unwrap();
        let builddir = cwd.join("build");
        let source_dir = cwd.join("src");
        let db = db_from_dir(source_dir);
        (db, builddir)
    };

    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        eprintln!("Build failed");
        std::process::exit(1);
    }

    std::fs::create_dir_all(&builddir).unwrap();
    copy_ext_dir(&builddir);
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());

    // `build/virmanifest.json`: the resolved platform's metadata, for
    // external tools that drive the built design (e.g. the `virt`
    // simulator for `virt::Virt`/`virt::VirtVga`).  Emitted only when
    // the `[prog] top` module resolves a platform `for` clause.
    emit_manifest(args, &db, &builddir);
}

/// Emit `build/virmanifest.json` for the `[prog] top` module's platform.
///
/// The manifest carries what a simulator cannot recover from the
/// emitted `.sv` alone: the platform (name and FPGA family) the top
/// implements, its clock periods, and its port list.  Skipped (with a
/// note, not an error) when the top names no platform — plain
/// Verilog-only designs still build.
fn emit_manifest(args: &Args, db: &Db, builddir: &Path) {
    // Resolve the top module named by [prog] top, exactly as
    // `vir bitstream` does.
    let cwd = resolve_cwd(args);
    let Some(top_name) = read_prog_key(&cwd, "top") else {
        println!("No [prog] top: skipped virmanifest.json");
        return;
    };
    let symboltable = db.get_symboltable();
    let Some(top_symbol) = symboltable
        .items()
        .into_iter()
        .find(|sym| sym.fqn.as_bytes() == top_name.as_bytes()
            && sym.kind == SymbolKind::ModDef)
    else {
        println!("Top module '{top_name}' not found: skipped virmanifest.json");
        return;
    };

    // The top module's `for` clause selects the platform.
    let mut builder = virdant::db::Builder::new(db);
    let platform_id = match virdant::analysis::platform::resolve_platform_for(
        &mut builder, top_symbol.id(),
    ) {
        Ok(Some(platform_id)) => platform_id,
        Ok(None) => {
            println!("Top module '{top_name}' implements no platform: skipped virmanifest.json");
            return;
        }
        Err(diagnostic) => {
            dump_diagnostic(db, &diagnostic);
            println!("Platform resolution failed: skipped virmanifest.json");
            return;
        }
    };
    let platform_symbol = symboltable.symbol(platform_id);
    let platform_fqn = platform_symbol.fqn().to_string();
    let platform_name = platform_symbol.name().to_string();

    let fpga = virdant::analysis::platform::platform_fpga(&mut builder, platform_id)
        .unwrap_or_else(|| {
            eprintln!("Platform '{platform_fqn}' is missing @fpga");
            std::process::exit(1);
        });
    let part = virdant::analysis::platform::platform_part(&mut builder, platform_id);

    let mut root = json::object::Object::new();
    root.insert("vir", 1.into());
    root.insert("platform", platform_fqn.clone().into());
    root.insert("platform_name", platform_name.clone().into());
    root.insert("fpga", fpga.to_str_lossy().to_string().into());
    if let Some(part) = &part {
        root.insert("part", part.to_str_lossy().to_string().into());
    }

    // Clocks: period in ns plus the rounded frequency in Hz, so tools
    // do not have to redo (or disagree on) the 1e9/period rounding.
    let mut clocks = json::JsonValue::new_array();
    for (name, pin, period) in virdant::analysis::platform::platform_clock_meta(
        &mut builder, platform_id,
    ) {
        let mut entry = json::object::Object::new();
        entry.insert("name", name.to_str_lossy().to_string().into());
        match pin {
            Some(pin) => entry.insert("pin", pin.to_str_lossy().to_string().into()),
            None => entry.insert("pin", json::JsonValue::Null),
        }
        if let Some(period) = period {
            entry.insert("period_ns", period.into());
            entry.insert("clock_hz", ((1e9 / period).round() as u64).into());
        }
        clocks.push(json::JsonValue::Object(entry)).unwrap();
    }
    root.insert("clocks", clocks);

    // Ports: name, direction, and bit width (from the platform type —
    // the top module's ports are checked to match exactly).
    let mut ports = json::JsonValue::new_array();
    for port in virdant::analysis::platform::platform_port_list(&mut builder, platform_id).iter() {
        let mut entry = json::object::Object::new();
        entry.insert("name", port.path.to_str_lossy().to_string().into());
        entry.insert(
            "dir",
            match port.dir {
                PortDir::Input => "input",
                PortDir::Output => "output",
                PortDir::Bidir => "inout",
            }
            .into(),
        );
        let bits = virdant::analysis::platform::platform_port_width(&mut builder, port);
        entry.insert("bits", (bits as u64).into());
        ports.push(json::JsonValue::Object(entry)).unwrap();
    }
    root.insert("ports", ports);

    let path = builddir.join("virmanifest.json");
    std::fs::write(&path, json::stringify_pretty(json::JsonValue::Object(root), 2)).unwrap();
    println!("Wrote manifest to {}", path.to_string_lossy());
}

fn compile(path: PathBuf) {
    let project = path.file_name().unwrap().to_string_lossy().to_owned();
    let builddir = PathBuf::from("build").join(project.as_ref());

    let db = db_from_dir(path.clone());
    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        eprintln!("Build failed");
        std::process::exit(1);
    }

    std::fs::create_dir_all(&builddir).unwrap();
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());
}

fn run(args: &Args, trace: &Option<String>, fps_no_cap: bool, fps: bool, flash: &Option<PathBuf>, cycles: &Option<u64>, verbose: bool) {
    let cwd = resolve_cwd(args);
    let manifest = common::project_manifest_or_exit(&cwd);
    let Some(top_name) = read_prog_key_from(&manifest, "top") else {
        eprintln!("virdant.toml is missing [prog] top");
        std::process::exit(1);
    };

    let db = db_from_dir(cwd.join("src"));
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
    // simulates the `virt::Virt` board only.
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
    if platform_name != "Virt" || fpga.as_deref().map(|f| f.as_bytes()) != Some(b"virt".as_bstr()) {
        eprintln!(
            "Top module '{top_name}' implements platform '{}', but `vir run` simulates virt::Virt only",
            platform_name
        );
        std::process::exit(1);
    }

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

    let exit_code = virt::run(
        &db,
        top_symbol.id(),
        clock_hz,
        virt::VirtOptions {
            flash: flash.clone(),
            trace: trace.clone().map(PathBuf::from),
            fps_no_cap: fps_no_cap,
            cycles: cycles.clone(),
            fps: fps,
            verbose,
        },
    );
    std::process::exit(exit_code);
}

fn new_project(args: &Args, project: &str) {
    let cwd = resolve_cwd(args);

    let project_dir = cwd.join(project);

    if project_dir.exists() {
        eprintln!("Error: directory '{}' already exists", project_dir.display());
        std::process::exit(1);
    }

    std::fs::create_dir_all(project_dir.join("src")).unwrap();

    let toml_content = format!(
        "[project]\nname = \"{project}\"\n\n[prog]\ntop = \"top::Top\"\n"
    );
    std::fs::write(project_dir.join(common::PROJECT_MANIFEST), toml_content).unwrap();

    let top_vir_content = include_str!("../../../assets/vir_new_top.vir");
    std::fs::write(project_dir.join("src").join("top.vir"), top_vir_content).unwrap();

    // Walk up from cwd looking for an existing .git directory
    let mut search = cwd.clone();
    let mut found_git = false;
    loop {
        if search.join(".git").exists() {
            found_git = true;
            break;
        }
        match search.parent() {
            Some(parent) => search = parent.to_path_buf(),
            None => break,
        }
    }

    if !found_git {
        let output = std::process::Command::new("git")
            .arg("init")
            .current_dir(&project_dir)
            .output()
            .unwrap();
        if !output.status.success() {
            eprintln!("git init failed");
            eprintln!("{}", BStr::new(&output.stderr));
            std::process::exit(1);
        }
        std::fs::write(project_dir.join(".gitignore"), "build\n").unwrap();
    }

    println!("Created project '{project}'");
}

/// Everything `vir bitstream`/`vir pnr`/`vir upload` need from the
/// project sources: the resolved platform, the toolchain, and the file
/// names the toolchain will read and write in `build/`.  Shared by the
/// full build (`bitstream`) and the PnR-only re-run (`pnr`).
struct BitstreamPlan {
    platform_name: String,
    project: String,
    builddir: PathBuf,
    verilog_top: String,
    part: Option<BString>,
    freq_mhz: Option<f64>,
    toolchain: virdant::build::Toolchain,
}

fn plan_bitstream(args: &Args) -> BitstreamPlan {
    let cwd = resolve_cwd(args);
    let manifest = common::project_manifest_or_exit(&cwd);

    let project = read_project_name_from(&manifest).unwrap_or_else(|| {
        eprintln!("virdant.toml is missing [project] name");
        std::process::exit(1);
    });
    let Some(top_name) = read_prog_key_from(&manifest, "top") else {
        eprintln!("virdant.toml is missing [prog] top");
        std::process::exit(1);
    };

    let builddir = cwd.join("build");
    let source_dir = cwd.join("src");

    let db = db_from_dir(source_dir);
    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        eprintln!("Build failed");
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

    {
        let location = top_symbol.location();
        let parsing = db.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        if let Some(decl) = virdant::types::typedef::generics_decl_of(&node) {
            dump_diagnostic(&db, &virdant::diagnostics::Diagnostic::new(
                decl.region(),
                virdant::diagnostics::TopGenerics {
                    item: top_symbol.fqn().to_owned(),
                },
            ));
            eprintln!("Build failed");
            std::process::exit(1);
        }
    }

    // The top module's `for` clause selects the platform.
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

    // Defensive re-check: ports must exactly match the platform.
    // (Normally already enforced by `vir check` via check_platform_ports.)
    let mut diagnostics = vec![];
    virdant::analysis::platform::check_platform_ports_for(
        &mut builder, top_symbol.id(), &mut diagnostics);
    if !diagnostics.is_empty() {
        for diagnostic in &diagnostics {
            dump_diagnostic(&db, diagnostic);
        }
        eprintln!("Build failed");
        std::process::exit(1);
    }

    // Platform data via the Phase 4 accessors.
    let fpga = virdant::analysis::platform::platform_fpga(&mut builder, platform_id)
        .unwrap_or_else(|| {
            eprintln!("Platform is missing @fpga");
            std::process::exit(1);
        });
    let part = virdant::analysis::platform::platform_part(&mut builder, platform_id);

    std::fs::create_dir_all(&builddir).unwrap();
    copy_ext_dir(&builddir);
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());

    let Some(toolchain) = virdant::build::toolchain_for(fpga.to_str_lossy().as_ref()) else {
        eprintln!("FPGA family '{}' is not supported", fpga.to_str_lossy());
        std::process::exit(1);
    };

    match virdant::build::emit_constraints(&mut builder, platform_id, toolchain) {
        Ok((extension, mut constraints_text)) => {
            // An optional <project>.extra.<extension> in the project
            // root is appended to the emitted constraints.  Platform
            // annotations can only constrain top-level ports; the
            // escape hatch covers what they cannot express, such as
            // the frequency of a clock derived inside the design.
            let extra_path = cwd.join(format!("{project}.extra.{extension}"));
            if extra_path.is_file() {
                match std::fs::read_to_string(&extra_path) {
                    Ok(extra) => constraints_text.push_str(&extra),
                    Err(err) => {
                        eprintln!("Could not read {}: {err}", extra_path.display());
                        std::process::exit(1);
                    }
                }
                println!("Appended extra constraints from {}", extra_path.display());
            }
            let constraints = builddir.join(format!("{project}.{extension}"));
            std::fs::write(&constraints, constraints_text).unwrap();
        }
        Err(diagnostics) => {
            // e.g. a platform port with no @pin (Unknown diagnostics).
            for diagnostic in &diagnostics {
                dump_diagnostic(&db, diagnostic);
            }
            eprintln!("Build failed");
            std::process::exit(1);
        }
    }

    // Yosys and nextpnr see the Verilog emitter's module names, not
    // Virdant's fully-qualified names (see `verilog_top_name`).
    let verilog_top = verilog_top_name(&db, top_symbol.id());

    // `--freq` feeds nextpnr-ice40's timing-driven placer.  It takes a
    // single frequency, so it is only passed when the platform declares
    // exactly one clock; the PCF's `set_frequency` lines (emitted
    // above) still drive static timing analysis in every case.
    let clocks = virdant::analysis::platform::platform_clocks(&mut builder, platform_id);
    let freq_mhz = virdant::build::constraints::freq_flag_mhz(&clocks);
    if clocks.len() > 1 {
        println!("multiple clocks: nextpnr placer runs without --freq");
    }

    BitstreamPlan {
        platform_name,
        project,
        builddir,
        verilog_top,
        part,
        freq_mhz,
        toolchain,
    }
}

/// Builds the PnR options shared by `vir pnr` and `vir upload`,
/// rejecting a zero `--tries`.
fn pnr_options(tries: u32, seed: u32, allow_timing_failure: bool) -> virdant::build::PnrOptions {
    if tries == 0 {
        eprintln!("--tries must be at least 1");
        std::process::exit(1);
    }
    virdant::build::PnrOptions { tries, seed, allow_timing_failure }
}

/// Synthesize, place-and-route, and pack a bitstream (`vir bitstream`).
fn bitstream(args: &Args) -> String {
    let plan = plan_bitstream(args);
    virdant::build::run_toolchain(&plan.builddir, &plan.project, &plan.verilog_top, plan.part.as_ref(), plan.freq_mhz, plan.toolchain, virdant::build::PnrOptions::default())
        .unwrap_or_else(|e| {
            eprintln!("Toolchain error: {e}");
            std::process::exit(1);
        });

    plan.platform_name
}

/// Synthesize only (`vir synth`): run yosys on the Verilog
/// `plan_bitstream` emitted, leaving the netlist at
/// `build/<project>.json` for `vir pnr` to place-and-route.  The
/// counterpart of `pnr`, for when the netlist is what you want
/// (e.g. to inspect it, or to re-run PnR separately).
fn synth(args: &Args) {
    let plan = plan_bitstream(args);
    virdant::build::run_synth(&plan.builddir, &plan.project, &plan.verilog_top, plan.part.as_ref(), plan.toolchain)
        .unwrap_or_else(|e| {
            eprintln!("Toolchain error: {e}");
            std::process::exit(1);
        });
}

/// Place-and-route (and pack) an already-synthesized design
/// (`vir pnr`): reuses the netlist `vir bitstream` left at
/// `build/<project>.json`, re-running only nextpnr (optionally across
/// several seeds, when `--tries > 1`) and the pack step.  The
/// source-level checks and constraint emission still run, so the
/// constraints stay in sync with the sources — but the netlist is not
/// resynthesized, so source edits since the last `vir bitstream` are
/// not reflected in it.
fn pnr(args: &Args, tries: u32, seed: u32, allow_timing_failure: bool) {
    let plan = plan_bitstream(args);
    let opts = pnr_options(tries, seed, allow_timing_failure);
    virdant::build::run_pnr(&plan.builddir, &plan.project, &plan.verilog_top, plan.part.as_ref(), plan.freq_mhz, plan.toolchain, opts)
        .unwrap_or_else(|e| {
            eprintln!("PnR error: {e}");
            std::process::exit(1);
        });
}

fn verilog_top_name(
    db: &Db,
    top_symbol_id: virdant::analysis::symbols::SymbolId,
) -> String {
    let symboltable = db.get_symboltable();
    let symbol = symboltable.symbol(top_symbol_id);
    symbol.fqn().to_str_lossy().into_owned()
}

/// Selects the on-board programmer for a resolved platform name.
/// The platform is determined by the top module's `for` clause, so the
/// programmer is inferred from it rather than configured in `virdant.toml`.
/// The tuple carries flags that go before the bitstream path (e.g. dfu-util's
/// `-D`, which iceprog/icesprog do not need).
fn programmer_for(platform: &str) -> (&'static str, &'static [&'static str]) {
    match platform {
        "IceSugar" => ("icesprog", &[]),
        "IceStick" => ("iceprog", &[]),
        // Fomu is programmed over USB with dfu-util.  `-d` pins the
        // Fomu DFU bootloader's USB ID (1209:5bf0) so dfu-util can never
        // grab some other DFU-capable device that happens to be plugged
        // in (webcams, for instance, expose a DFU interface).
        "Fomu" => ("dfu-util", &["-d", "1209:5bf0", "-D"]),
        // The Arty A7's FTDI JTAG.  The board selects cable and flash
        // model; the -1L variants are programmed like their -1 parts.
        "ArtyA7" => ("openFPGALoader", &["-b", "arty_a7_35t"]),
        "ArtyA7_100T" => ("openFPGALoader", &["-b", "arty_a7_100t"]),
        other => {
            eprintln!("No programmer tool known for platform '{other}'");
            std::process::exit(1);
        }
    }
}

fn upload(args: &Args, tries: u32, seed: u32, allow_timing_failure: bool) {
    let plan = plan_bitstream(args);
    let opts = pnr_options(tries, seed, allow_timing_failure);
    virdant::build::run_toolchain(&plan.builddir, &plan.project, &plan.verilog_top, plan.part.as_ref(), plan.freq_mhz, plan.toolchain, opts)
        .unwrap_or_else(|e| {
            eprintln!("Toolchain error: {e}");
            std::process::exit(1);
        });

    let cwd = resolve_cwd(args);
    let project = plan.project;
    let (tool, tool_args) = programmer_for(&plan.platform_name);

    virdant::build::flash_bitstream(&cwd, &project, tool, tool_args)
        .unwrap_or_else(|e| {
            eprintln!("Flash error: {e}");
            std::process::exit(1);
        });
}

fn exec_external(args: Vec<OsString>) {
    let Some(command) = args.first() else {
        cli::command().print_help().unwrap();
        std::process::exit(3);
    };

    let command = command.to_string_lossy().into_owned();
    let bin_name = format!("vir-{command}");
    if let Some(bin) = cli::find_bin(&bin_name) {
        let program = CString::new(bin).unwrap();

        // Linux-style exec: the companion binary is invoked as
        // `vir-${CMD} ...remaining args`, i.e. the first element of the
        // command line is replaced with the `vir-` prefixed binary name.
        let mut c_args: Vec<CString> = vec![CString::new(bin_name).unwrap()];
        c_args.extend(
            args.into_iter()
                .skip(1)
                .map(|arg| CString::new(arg.as_os_str().as_bytes()).unwrap()),
        );

        let _ = execvp(&program, &c_args);
    } else {
        cli::command().print_help().unwrap();
        std::process::exit(3);
    }
}
