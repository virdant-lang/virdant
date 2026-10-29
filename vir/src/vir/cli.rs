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

use crate::common::{add_project_args, ProjectLocation};

/// The parsed command line for `vir`.
#[derive(Debug)]
pub struct Args {
    pub location: ProjectLocation,
    pub command: Command,
}

#[derive(Debug, PartialEq)]
pub enum Command {
    Check {},
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
    New { project: String },
    Bitstream {},
    Pnr { tries: u32, seed: u32, allow_timing_failure: bool },
    Upload { tries: u32, seed: u32, allow_timing_failure: bool },
    External(Vec<OsString>),
}

/// Assemble the full `vir` command line interface.
pub fn command() -> ClapCommand {
    let mut cmd = ClapCommand::new("vir")
        .about("Virdant Hardware Language Compiler")
        .arg_required_else_help(true)
        .subcommand(ClapCommand::new("check")
            .about("Typecheck a Virdant project"))
        .subcommand(ClapCommand::new("compile")
            .about("Compile a Virdant project to Verilog")
            .arg(Arg::new("path").value_name("PATH")
                .value_parser(value_parser!(PathBuf))
                .required(true)))
        .subcommand(ClapCommand::new("build")
            .about("Build the project"))
        .subcommand(ClapCommand::new("run")
            .about("Run the project on hardware/simulator")
            .arg(Arg::new("trace").long("trace").num_args(0..=1)
                .default_missing_value("trace.vcd"))
            .arg(Arg::new("fps-no-cap").long("fps-no-cap").action(ArgAction::SetTrue))
            .arg(Arg::new("fps").long("fps").action(ArgAction::SetTrue))
            .arg(Arg::new("flash").long("flash").num_args(1)
                .value_parser(value_parser!(PathBuf)))
            .arg(Arg::new("cycles").long("cycles").num_args(1)
                .value_parser(value_parser!(u64)))
            .arg(Arg::new("verbose").short('v').long("verbose").action(ArgAction::SetTrue)))
        .subcommand(ClapCommand::new("new")
            .about("Scaffold a new project")
            .arg(Arg::new("project").value_name("PROJECT").required(true)))
        .subcommand(ClapCommand::new("bitstream")
            .about("Generate a bitstream"))
        .subcommand(ClapCommand::new("pnr")
            .about("Place-and-route")
            .arg(Arg::new("tries").long("tries").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("seed").long("seed").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("allow-timing-failure").long("allow-timing-failure").action(ArgAction::SetTrue)))
        .subcommand(ClapCommand::new("upload")
            .about("Upload to device")
            .arg(Arg::new("tries").long("tries").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("seed").long("seed").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("allow-timing-failure").long("allow-timing-failure").action(ArgAction::SetTrue)));

    // Register dynamically-discovered `vir-*` companion binaries as
    // virtual subcommands, collecting their remaining args verbatim.
    for name in discover_virtual_subcommands() {
        cmd = cmd.subcommand(virtual_subcommand(&name));
    }

    add_project_args(&mut cmd);

    cmd
}

/// Parse the command line into [`Args`].
pub fn parse() -> Args {
    let matches = command().get_matches();
    from_matches(matches)
}

fn from_matches(matches: ArgMatches) -> Args {
    let location = ProjectLocation::from_matches(&matches);

    let command = match matches.subcommand() {
        Some(("check", _)) => Command::Check {},
        Some(("compile", m)) => Command::Compile {
            path: m.get_one::<PathBuf>("path").cloned().unwrap(),
        },
        Some(("build", _)) => Command::Build {},
        Some(("run", m)) => Command::Run {
            trace: m.get_one::<String>("trace").cloned(),
            fps_no_cap: m.get_flag("fps-no-cap"),
            fps: m.get_flag("fps"),
            flash: m.get_one::<PathBuf>("flash").cloned(),
            cycles: m.get_one::<u64>("cycles").copied(),
            verbose: m.get_flag("verbose"),
        },
        Some(("new", m)) => Command::New {
            project: m.get_one::<String>("project").cloned().unwrap(),
        },
        Some(("bitstream", _)) => Command::Bitstream {},
        Some(("pnr", m)) => Command::Pnr {
            tries: *m.get_one::<u32>("tries").unwrap(),
            seed: *m.get_one::<u32>("seed").unwrap(),
            allow_timing_failure: m.get_flag("allow-timing-failure"),
        },
        Some(("upload", m)) => Command::Upload {
            tries: *m.get_one::<u32>("tries").unwrap(),
            seed: *m.get_one::<u32>("seed").unwrap(),
            allow_timing_failure: m.get_flag("allow-timing-failure"),
        },
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
            let external_args: Vec<OsString> = std::iter::once(OsString::from(name))
                .chain(external_args.into_iter())
                .collect();
            Command::External(external_args)
        }
        None => Command::Check {},
    };

    Args { location, command }
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
