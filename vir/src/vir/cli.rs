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
use std::ffi::{OsStr, OsString};
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
    Version {},
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
    New { project: String, no_git: bool },
    Bitstream {},
    Synth {},
    Pnr { tries: u32, seed: u32, allow_timing_failure: bool },
    Upload { tries: u32, seed: u32, allow_timing_failure: bool },
    External(Vec<OsString>),
}

/// Assemble the full `vir` command line interface.
pub fn command() -> ClapCommand {
    command_filtered(false)
}

/// Assemble the `vir` command line interface, attaching only commands
/// listed in `vir/commands` when `listed_only` is true.
///
/// Completion generation uses this: clap_complete does not skip
/// hidden subcommands when generating scripts, and clap offers no
/// removal API, so unlisted commands are simply never attached.
fn command_filtered(listed_only: bool) -> ClapCommand {
    let mut subs: Vec<ClapCommand> = vec![
        ClapCommand::new("version"),
        ClapCommand::new("check"),
        ClapCommand::new("compile")
            .arg(Arg::new("path").value_name("PATH")
                .value_parser(value_parser!(PathBuf))
                .required(true)),
        ClapCommand::new("build"),
        ClapCommand::new("run")
            .arg(Arg::new("trace").long("trace").num_args(0..=1)
                .default_missing_value("trace.vcd"))
            .arg(Arg::new("fps-no-cap").long("fps-no-cap").action(ArgAction::SetTrue))
            .arg(Arg::new("fps").long("fps").action(ArgAction::SetTrue))
            .arg(Arg::new("flash").long("flash").num_args(1)
                .value_parser(value_parser!(PathBuf)))
            .arg(Arg::new("cycles").long("cycles").num_args(1)
                .value_parser(value_parser!(u64)))
            .arg(Arg::new("verbose").short('v').long("verbose").action(ArgAction::SetTrue)),
        ClapCommand::new("new")
            .arg(Arg::new("project").value_name("PROJECT").required(true))
            .arg(Arg::new("no-git").long("no-git").action(ArgAction::SetTrue)
                .help("Do not initialize a Git repository")),
        ClapCommand::new("bitstream"),
        ClapCommand::new("synth"),
        ClapCommand::new("pnr")
            .arg(Arg::new("tries").long("tries").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("seed").long("seed").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("allow-timing-failure").long("allow-timing-failure").action(ArgAction::SetTrue)),
        ClapCommand::new("upload")
            .arg(Arg::new("tries").long("tries").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("seed").long("seed").num_args(1)
                .value_parser(value_parser!(u32)).default_value("1"))
            .arg(Arg::new("allow-timing-failure").long("allow-timing-failure").action(ArgAction::SetTrue)),
        ClapCommand::new("completion")
            .about("Generate shell completion scripts")
            .arg(Arg::new("shell").value_name("SHELL")
                .required(true)
                .value_parser(value_parser!(clap_complete::shells::Shell))),
    ];
    if listed_only {
        subs.retain(|sub| crate::is_listed_command(sub.get_name()));
    }

    let mut cmd = ClapCommand::new("vir")
        .about("Virdant Hardware Language")
        // Provide our own version flag so that `-v` works;
        // clap's built-in version flag registers the uppercase short
        // instead. `ArgAction::Version` still requires `.version()`
        // to be set, and `disable_version_flag` only suppresses the
        // automatic flag, not the version string.
        .disable_version_flag(true)
        .version(crate::version_string())
        .arg_required_else_help(true);
    // The completion tree keeps only the listed subcommands, and drops
    // the plumbing flags (`-v`/`--version`, `-C`, `-F`, and the
    // automatic help flag): clap_complete would otherwise suggest them
    // even though they are not part of a command's core usage.
    if !listed_only {
        cmd = cmd
            .disable_help_flag(false)
            .disable_help_subcommand(false)
            .arg(Arg::new("version")
                .short('v').long("version")
                .action(ArgAction::Version)
                .help("Print version information"));
    } else {
        cmd = cmd
            .disable_help_flag(true)
            .disable_help_subcommand(true);
    }
    for sub in subs {
        // In the completion tree, drop the automatic help flag from
        // every subcommand (`-h`/`--help`).
        let sub = if listed_only {
            sub.disable_help_flag(true)
        } else {
            sub
        };
        cmd = cmd.subcommand(sub);
    }

    // Register dynamically-discovered `vir-*` companion binaries as
    // virtual subcommands, collecting their remaining args verbatim.
    // Companion binaries get the same filter as the static list: a
    // `vir-*` binary is attached only if its name is listed in
    // `vir/commands`.
    for name in discover_virtual_subcommands() {
        if listed_only && !crate::is_listed_command(&name) {
            continue;
        }
        cmd = cmd.subcommand(virtual_subcommand(&name, !listed_only));
    }

    // Set each subcommand's description from `vir/commands` — the
    // single source of command help text — and hide any subcommand
    // that `is_listed_command` does not cover (keep it fully
    // functional, but hidden from clap's help, so error messages and
    // suggestions never leak it either). Top-level help is rendered
    // separately, with groups, by `print_top_level_help`.
    for sub in cmd.get_subcommands_mut() {
        // clap's builder methods consume `self`, so swap through
        // `mem::take` to modify the subcommand in place.
        let taken = std::mem::take(sub);
        let taken = match commands_help(taken.get_name()) {
            Some(help) => taken.about(help),
            None => taken,
        };
        *sub = if crate::is_listed_command(taken.get_name()) {
            taken
        } else {
            taken.hide(true)
        };
    }

    // Project-location args (`-C`/`-F`) are parse plumbing: attach
    // them only to the full tree, not the completion tree.
    if !listed_only {
        add_project_args(&mut cmd, true);
    }

    cmd
}

/// The help message for command `name` as written in `vir/commands`.
fn commands_help(name: &str) -> Option<String> {
    crate::command_groups().iter()
        .find_map(|g| g.commands.iter().find(|e| e.name == name))
        .and_then(|e| e.help.clone())
}

/// The help-line description of subcommand `name`: its help message
/// from `vir/commands`, else the virtual-companion text for a
/// discovered `vir-*` binary, else empty (a command listed in
/// `vir/commands` without a help message).
fn about_of(name: &str, discovered: &[String]) -> String {
    if let Some(help) = commands_help(name) {
        return help;
    }
    if discovered.iter().any(|n| n == name) {
        return format!("Run the `vir-{name}` companion binary");
    }
    String::new()
}

/// Print the top-level help with command groups (driven by
/// `vir/commands`) and exit with `exit_code`.
///
/// clap cannot render group headers in its flat subcommand listing, so
/// the top-level help is rendered here instead; clap still handles all
/// actual argument parsing.
fn print_top_level_help(exit_code: i32) -> ! {
    let discovered = discover_virtual_subcommands();
    // A command exists if `command` actually assembles a subcommand
    // for it: a built-in or a discovered `vir-*` companion.
    let cmd = command();
    let exists = |name: &str| cmd.find_subcommand(name).is_some();

    // (group, name, about) rows in `vir/commands` order, deduplicated
    // (first group wins), listing only commands that actually exist.
    let mut rows: Vec<(&str, &str, String)> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for group in crate::command_groups() {
        for entry in &group.commands {
            if !seen.insert(entry.name.clone()) || !exists(&entry.name) {
                continue;
            }
            rows.push((&group.name, &entry.name, about_of(&entry.name, &discovered)));
        }
    }
    let width = rows.iter().map(|(_, name, _)| name.len()).max().unwrap_or(0);

    println!("Virdant Hardware Language\n");
    println!("Usage: vir [COMMAND]\n");
    let mut current_group: Option<&str> = None;
    for (group, name, about) in &rows {
        if Some(*group) != current_group {
            if current_group.is_some() {
                println!();
            }
            println!("{group}:");
            current_group = Some(group);
        }
        println!("  {name:<width$}  {about}");
    }
    std::process::exit(exit_code);
}

/// Parse the command line into [`Args`].
pub fn parse() -> Args {
    let argv: Vec<OsString> = std::env::args_os().collect();

    // Show our grouped help for bare `vir`, `-h/--help`, and bare `help`.
    if argv.len() == 1 {
        print_top_level_help(2);
    }
    if argv.len() == 2 {
        match argv[1].to_str() {
            Some("-h") | Some("--help") | Some("help") => print_top_level_help(0),
            _ => {}
        }
    }

    // Handle `vir completion <SHELL>` before regular parsing: print the
    // completion script to stdout and exit.  This runs before
    // `ProjectLocation` resolution, so completions can be generated
    // outside a project, and before dispatch, so `completion` is never
    // mistaken for a `vir-completion` companion binary.  The subcommand
    // is still registered in `command()` above, so it shows up in
    // `--help`-driven listings and typo suggestions.
    handle_completion_subcommand(&argv);

    let matches = command().get_matches_from(argv);
    from_matches(matches)
}

/// Print the usage message for `vir completion` and exit.
fn completion_usage(exit_code: i32) -> ! {
    eprintln!("usage: vir completion <bash|zsh|fish|powershell|elvish>");
    std::process::exit(exit_code)
}

/// If the command line is `vir completion ...`, generate the shell
/// completion script and exit; otherwise do nothing.
fn handle_completion_subcommand(argv: &[OsString]) {
    if argv.len() < 2 || argv[1] != OsStr::new("completion") {
        return;
    }
    if argv.len() == 2 {
        completion_usage(2);
    }
    match argv[2].to_str() {
        Some("-h") | Some("--help") => completion_usage(0),
        Some("bash") => write_completion(clap_complete::shells::Shell::Bash),
        Some("zsh") => write_completion(clap_complete::shells::Shell::Zsh),
        Some("fish") => write_completion(clap_complete::shells::Shell::Fish),
        Some("powershell") => write_completion(clap_complete::shells::Shell::PowerShell),
        Some("elvish") => write_completion(clap_complete::shells::Shell::Elvish),
        _ => completion_usage(2),
    }
}

/// Write the completion script for `shell` to stdout and exit.
fn write_completion(shell: clap_complete::shells::Shell) -> ! {
    // Omit commands not listed in `vir/commands`: clap_complete does
    // not skip hidden subcommands, and unlisted commands are kept out
    // of anything user-visible.
    let mut cmd = command_filtered(true);
    clap_complete::generate(shell, &mut cmd, "vir", &mut std::io::stdout());
    std::process::exit(0)
}

fn from_matches(matches: ArgMatches) -> Args {
    let location = ProjectLocation::from_matches(&matches);

    let command = match matches.subcommand() {
        Some(("version", _)) => Command::Version {},
        Some(("check", _)) => Command::Check {},
        Some(("compile", m)) => Command::Compile {
            path: m.get_one::<PathBuf>("path").cloned().unwrap(),
        },
        Some(("build", _)) => Command::Build {},
        Some(("synth", _)) => Command::Synth {},
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
            no_git: m.get_flag("no-git"),
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
fn virtual_subcommand(name: &str, with_args: bool) -> ClapCommand {
    let cmd = ClapCommand::new(name.to_owned())
        .about(format!("Run the `vir-{name}` companion binary"))
        .disable_help_flag(true);
    // The `virtual_args` positional is hidden: help output is disabled so
    // that flags like `--help` are captured and passed through to the
    // binary, and the positional swallows everything after the subcommand
    // name, hyphen-leading values included.  It is only attached when
    // `with_args` is set: completion scripts should not suggest hidden
    // args, and nothing else about the subcommand depends on it.
    if !with_args {
        return cmd;
    }
    cmd.arg(
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
/// For example, a binary named `vir-ls` yields the subcommand name
/// `ls`.  Names are deduplicated and sorted.
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
