//! Definition of the `vir` command-line interface, built up with the
//! vanilla clap builder API (no derive).
//!
//! [`command`] assembles the `clap::Command` for `vir`, and [`parse`]
//! runs it and unpacks the resulting [`clap::ArgMatches`] into [`Args`]
//! and [`Command`], which `main.rs` dispatches on.
//!
//! "Virtual" subcommands are also discovered dynamically: any executable
//! named `vir-*` on `$PATH` is registered as a subcommand of `vir` (so it
//! shows up in `vir --help`), and running it delegates to the companion
//! binary with the remaining arguments.

use clap::{Arg, ArgAction, ArgMatches, Command as ClapCommand, value_parser};
use std::collections::HashSet;
use std::ffi::OsString;
use std::path::{Path, PathBuf};

/// The parsed command line for `vir`.
#[derive(Debug)]
pub struct Args {
    pub cwd: Option<PathBuf>,
    pub virfile: Option<String>,
    pub command: Command,
}

#[derive(Debug, PartialEq)]
pub enum Command {
    Parse { file: PathBuf },
    Tokenize { file: PathBuf },
    Check {},
    Types {},
    Symbols {},
    Typedefs {},
    Exprroots {},
    Typing {},
    Db { outpath: Option<PathBuf>, json: bool },
    Components { moddef_fqn: String },
    Ports { moddef_fqn: String },
    Elaborate { top: String },
    Compile { path: PathBuf },
    Build {},
    Run {
        trace: Option<String>,
        fps_no_cap: bool,
        fps: bool,
        flash: Option<PathBuf>,
        cycles: Option<u64>,
        verbose: bool,
    },
    RunIcarus { path: Option<PathBuf>, vcd: Option<String> },
    New { project: String },
    Bitstream {},
    Pnr { tries: u32, seed: u32, allow_timing_failure: bool },
    Upload { tries: u32, seed: u32, allow_timing_failure: bool },
    Doc { open: bool },
    External(Vec<OsString>),
}

/// Assemble the full `vir` command line interface.
pub fn command() -> ClapCommand {
    let mut subcommands = vec![
            ClapCommand::new("parse")
                .about("Parse and dump the AST for a Virdant source file")
                .arg(path_arg("file", "FILE")),
            ClapCommand::new("tokenize")
                .about("Tokenize a Virdant source file")
                .arg(path_arg("file", "FILE")),
            ClapCommand::new("check")
                .about("Typecheck a Virdant project"),
            ClapCommand::new("types")
                .about("Dump inferred expression types"),
            ClapCommand::new("symbols")
                .about("Dump the symbol table"),
            ClapCommand::new("typedefs")
                .about("Dump typedefs"),
            ClapCommand::new("exprroots")
                .about("Dump expression roots"),
            ClapCommand::new("typing")
                .about("Dump typing results"),
            ClapCommand::new("db")
                .about("Dump database state, optionally saving a Graphviz file")
                .arg(path_arg("outpath", "OUTPATH").required(false))
                .arg(
                    Arg::new("json")
                        .long("json")
                        .action(ArgAction::SetTrue)
                        .help("Render the output as JSON"),
                ),
            ClapCommand::new("components")
                .about("Dump component analysis for a module definition")
                .arg(string_arg("moddef_fqn", "MODDEF_FQN")),
            ClapCommand::new("ports")
                .about("List ports for a module definition")
                .arg(string_arg("moddef_fqn", "MODDEF_FQN")),
            ClapCommand::new("elaborate")
                .about("Dump the elaborated design rooted at a top module")
                .arg(string_arg("top", "TOP")),
            ClapCommand::new("compile")
                .about("Compile Virdant package")
                .arg(path_arg("path", "PATH")),
            ClapCommand::new("build")
                .about("Build Virdant package"),
            ClapCommand::new("run")
                .about("Run the `virt::Virt` board simulation for the project's top module.")
                .long_about(
                    "Run the `virt::Virt` board simulation for the project's top module.\n\
                    \n\
                    The top module (from Virdant.toml `[prog] top`) must be `for` the\n\
                    `virt::Virt` platform; the simulation then behaves like the\n\
                    virdant-virt board: UART on stdin/stdout, 16 MiB SPI flash, and\n\
                    the `exit` port.  The clock runs at the platform's declared\n\
                    frequency, paced to real time unless `--fps-no-cap`.",
                )
                .arg(
                    Arg::new("trace")
                        .long("trace")
                        .value_name("TRACE")
                        .value_parser(value_parser!(String))
                        .num_args(0..=1)
                        .default_missing_value("trace.vcd")
                        .help("Write a VCD trace to FILE (default: trace.vcd)"),
                )
                .arg(
                    Arg::new("fps-no-cap")
                        .long("fps-no-cap")
                        .action(ArgAction::SetTrue)
                        .help("Run the simulation at full speed (no real-time cap)"),
                )
                .arg(
                    Arg::new("fps")
                        .long("fps")
                        .action(ArgAction::SetTrue)
                        .help("On exit, report the average clock frequency achieved (MHz)"),
                )
                .arg(
                    Arg::new("flash")
                        .long("flash")
                        .value_name("FLASH")
                        .value_parser(value_parser!(PathBuf))
                        .help("Preload the 16 MiB SPI flash image from FILE"),
                )
                .arg(
                    Arg::new("cycles")
                        .long("cycles")
                        .value_name("CYCLES")
                        .value_parser(value_parser!(u64))
                        .help("Stop after this many clock cycles (exit code 124)"),
                )
                .arg(
                    Arg::new("verbose")
                        .short('v')
                        .long("verbose")
                        .action(ArgAction::SetTrue)
                        .help("Print informational board messages (builtin attachments, flash load)"),
                ),
            ClapCommand::new("run-icarus")
                .about("Run Virdant package with Icarus Verilog")
                .arg(path_arg("path", "PATH").required(false))
                .arg(
                    Arg::new("vcd")
                        .long("vcd")
                        .value_name("VCD")
                        .value_parser(value_parser!(String)),
                ),
            ClapCommand::new("new")
                .about("Create a new Virdant project")
                .arg(string_arg("project", "PROJECT")),
            ClapCommand::new("bitstream")
                .about("Synthesize, place-and-route, and pack a bitstream"),
            ClapCommand::new("pnr")
                .about("Place-and-route (and pack) an already-synthesized design")
                .args(place_and_route_args()),
            ClapCommand::new("upload")
                .about("Synthesize, place-and-route, pack, and upload a bitstream")
                .args(place_and_route_args()),
            ClapCommand::new("doc")
                .about("Generate HTML documentation")
                .arg(
                    Arg::new("open")
                        .long("open")
                        .action(ArgAction::SetTrue)
                        .help("Open the docs in a browser after generation"),
                ),
    ];

    let internal_names: HashSet<String> = subcommands
        .iter()
        .map(|subcommand| subcommand.get_name().to_string())
        .collect();
    for name in discover_virtual_subcommands() {
        if !internal_names.contains(&name) {
            subcommands.push(virtual_subcommand(&name));
        }
    }

    ClapCommand::new("vir")
        .author(env!("CARGO_PKG_AUTHORS"))
        .version(env!("CARGO_PKG_VERSION"))
        .about("The Virdant Hardware Language")
        .disable_help_subcommand(true)
        .arg_required_else_help(true)
        .external_subcommand_value_parser(value_parser!(OsString))
        .arg(
            Arg::new("cwd")
                .short('C')
                .value_name("CWD")
                .value_parser(value_parser!(PathBuf))
                .conflicts_with("virfile"),
        )
        .arg(
            Arg::new("virfile")
                .short('F')
                .value_name("VIRFILE")
                .value_parser(value_parser!(String))
                .conflicts_with("cwd")
                .help("Load .vir file(s) as a self-contained project (comma-separated, no spaces; exclusive with -C)"),
        )
        .subcommands(subcommands)
}

/// Args shared by the `pnr` and `upload` subcommands.
fn place_and_route_args() -> Vec<Arg> {
    vec![
        Arg::new("tries")
            .long("tries")
            .value_name("TRIES")
            .value_parser(value_parser!(u32))
            .default_value("1")
            .help("Number of place-and-route attempts, varying --seed"),
        Arg::new("seed")
            .long("seed")
            .value_name("SEED")
            .value_parser(value_parser!(u32))
            .default_value("1")
            .help("Seed for the first attempt (later attempts increment by one)"),
        Arg::new("allow-timing-failure")
            .long("allow-timing-failure")
            .action(ArgAction::SetTrue)
            .help("Keep the bitstream even when timing constraints are not met"),
    ]
}

/// A required positional argument taking a path.
fn path_arg(id: &'static str, value_name: &'static str) -> Arg {
    Arg::new(id)
        .value_name(value_name)
        .value_parser(value_parser!(PathBuf))
}

/// A required positional argument taking a string.
fn string_arg(id: &'static str, value_name: &'static str) -> Arg {
    Arg::new(id)
        .value_name(value_name)
        .value_parser(value_parser!(String))
}

/// A subcommand that delegates to a `vir-{name}` companion binary found
/// on `$PATH`.  Help output is disabled so that flags like `--help` are
/// captured and passed through to the binary, and the `virtual_args`
/// positional swallows everything after the subcommand name,
/// hyphen-leading values included.
fn virtual_subcommand(name: &str) -> ClapCommand {
    ClapCommand::new(name.to_owned())
        .about(format!("Run the `vir-{name}` companion binary"))
        .disable_help_flag(true)
        .arg(
            Arg::new("virtual_args")
                .num_args(0..)
                .value_parser(value_parser!(OsString))
                .trailing_var_arg(true)
                .allow_hyphen_values(true)
                .hide(true),
        )
}

fn path_dirs() -> Vec<PathBuf> {
    std::env::var_os("PATH")
        .map(|path| std::env::split_paths(&path).collect())
        .unwrap_or_default()
}

/// Whether `path` is an executable file (following symlinks).
fn is_executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;

    let Ok(metadata) = std::fs::metadata(path) else {
        return false;
    };
    metadata.is_file() && metadata.permissions().mode() & 0o111 != 0
}

/// Search the `$PATH` directories for a binary named `bin_name`,
/// returning its path if found.
pub fn find_bin(bin_name: &str) -> Option<String> {
    find_bin_in(&path_dirs(), bin_name)
}

fn find_bin_in(dirs: &[PathBuf], bin_name: &str) -> Option<String> {
    for dir in dirs {
        let binpath = dir.join(bin_name);
        if is_executable(&binpath) {
            return Some(binpath.to_string_lossy().into_owned());
        }
    }

    None
}

/// Discover the names of all `vir-*` binaries available on `$PATH`.
/// For example, a binary named `vir-lsp` yields the subcommand name
/// `lsp`.  Names are deduplicated and sorted.
pub fn discover_virtual_subcommands() -> Vec<String> {
    discover_virtual_subcommands_in(&path_dirs())
}

fn discover_virtual_subcommands_in(dirs: &[PathBuf]) -> Vec<String> {
    let mut names: HashSet<String> = HashSet::new();

    for dir in dirs {
        let Ok(entries) = std::fs::read_dir(dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let Some(filename) = path.file_name().and_then(|name| name.to_str()) else {
                continue;
            };
            let Some(name) = filename.strip_prefix("vir-") else {
                continue;
            };
            if name.is_empty() || !is_executable(&path) {
                continue;
            }
            names.insert(name.to_string());
        }
    }

    let mut names: Vec<String> = names.into_iter().collect();
    names.sort();
    names
}

/// Parse the command line into [`Args`].
pub fn parse() -> Args {
    let matches = command().get_matches();
    from_matches(matches)
}

fn from_matches(matches: ArgMatches) -> Args {
    let cwd = matches.get_one::<PathBuf>("cwd").cloned();
    let virfile = matches.get_one::<String>("virfile").cloned();

    let command = match matches.subcommand() {
        Some(("parse", m)) => Command::Parse { file: path(m, "file") },
        Some(("tokenize", m)) => Command::Tokenize { file: path(m, "file") },
        Some(("check", _)) => Command::Check {},
        Some(("types", _)) => Command::Types {},
        Some(("symbols", _)) => Command::Symbols {},
        Some(("typedefs", _)) => Command::Typedefs {},
        Some(("exprroots", _)) => Command::Exprroots {},
        Some(("typing", _)) => Command::Typing {},
        Some(("db", m)) => Command::Db {
            outpath: opt_path(m, "outpath"),
            json: flag(m, "json"),
        },
        Some(("components", m)) => Command::Components { moddef_fqn: string(m, "moddef_fqn") },
        Some(("ports", m)) => Command::Ports { moddef_fqn: string(m, "moddef_fqn") },
        Some(("elaborate", m)) => Command::Elaborate { top: string(m, "top") },
        Some(("compile", m)) => Command::Compile { path: path(m, "path") },
        Some(("build", _)) => Command::Build {},
        Some(("run", m)) => Command::Run {
            trace: opt_string(m, "trace"),
            fps_no_cap: flag(m, "fps-no-cap"),
            fps: flag(m, "fps"),
            flash: opt_path(m, "flash"),
            cycles: opt_u64(m, "cycles"),
            verbose: flag(m, "verbose"),
        },
        Some(("run-icarus", m)) => Command::RunIcarus {
            path: opt_path(m, "path"),
            vcd: opt_string(m, "vcd"),
        },
        Some(("new", m)) => Command::New { project: string(m, "project") },
        Some(("bitstream", _)) => Command::Bitstream {},
        Some(("pnr", m)) => Command::Pnr {
            tries: u32(m, "tries"),
            seed: u32(m, "seed"),
            allow_timing_failure: flag(m, "allow-timing-failure"),
        },
        Some(("upload", m)) => Command::Upload {
            tries: u32(m, "tries"),
            seed: u32(m, "seed"),
            allow_timing_failure: flag(m, "allow-timing-failure"),
        },
        Some(("doc", m)) => Command::Doc { open: flag(m, "open") },
        // An unrecognized first token: either a virtual subcommand
        // (whose remaining tokens are collected into the `virtual_args`
        // positional) or a fully unknown subcommand (whose remaining
        // tokens are stored under the external-subcommand id "",
        // mirroring what clap-derive collects into
        // `External(Vec<OsString>)`).
        Some((name, m)) => {
            let external_args = if let Some(vals) = m.get_many::<OsString>("virtual_args") {
                vals.cloned().collect::<Vec<_>>()
            } else if let Some(vals) = m.get_many::<OsString>("") {
                vals.cloned().collect()
            } else {
                Vec::new()
            };
            Command::External(
                std::iter::once(OsString::from(name))
                    .chain(external_args)
                    .collect(),
            )
        }
        None => unreachable!("arg_required_else_help or a subcommand is always present"),
    };

    Args { cwd, virfile, command }
}

fn string(m: &ArgMatches, id: &str) -> String {
    m.get_one::<String>(id).cloned().unwrap()
}

fn path(m: &ArgMatches, id: &str) -> PathBuf {
    m.get_one::<PathBuf>(id).cloned().unwrap()
}

fn opt_string(m: &ArgMatches, id: &str) -> Option<String> {
    m.get_one::<String>(id).cloned()
}

fn opt_path(m: &ArgMatches, id: &str) -> Option<PathBuf> {
    m.get_one::<PathBuf>(id).cloned()
}

fn opt_u64(m: &ArgMatches, id: &str) -> Option<u64> {
    m.get_one::<u64>(id).cloned()
}

fn u32(m: &ArgMatches, id: &str) -> u32 {
    *m.get_one::<u32>(id).unwrap()
}

fn flag(m: &ArgMatches, id: &str) -> bool {
    m.get_flag(id)
}
