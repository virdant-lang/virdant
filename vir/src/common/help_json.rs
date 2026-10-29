//! Shared `--help-json` support for the non-`vir` binaries.
//!
//! Each binary includes this module with `#[path = "../common/help_json.rs"]`,
//! and when `--help-json` appears on the command line it dumps a JSON
//! description of that binary's clap arg-parse data (flags, options,
//! positionals, and subcommands) to stdout and exits 0.
//!
//! For binaries with a derive `Parser` struct, the flag is a real
//! (hidden) declared arg so clap accepts it anywhere a flag is valid;
//! the binary inspects the parsed field and calls [`dump`] itself.
//! For binaries with hand-rolled argument parsing (`vir-format`,
//! `vir-filecheck`), [`maybe_dump`] scans argv up front against a
//! `Command` built with the builder API.

use clap::{Arg, Command};
use serde_json::{json, Map, Value};

/// Build the JSON description of a clap command, recursing into
/// subcommands.
pub fn command_to_json(cmd: &Command) -> Value {
    let mut obj = Map::new();
    obj.insert("name".into(), json!(cmd.get_name()));
    if let Some(display_name) = cmd.get_display_name() {
        obj.insert("display_name".into(), json!(display_name));
    }
    if let Some(version) = cmd.get_version() {
        obj.insert("version".into(), json!(version));
    }
    if let Some(about) = cmd.get_about() {
        obj.insert("about".into(), json!(about.to_string()));
    }
    if let Some(long_about) = cmd.get_long_about() {
        obj.insert("long_about".into(), json!(long_about.to_string()));
    }
    obj.insert(
        "subcommand_required".into(),
        json!(cmd.is_subcommand_required_set()),
    );
    obj.insert(
        "args".into(),
        Value::Array(cmd.get_arguments().map(arg_to_json).collect()),
    );
    obj.insert(
        "subcommands".into(),
        Value::Array(cmd.get_subcommands().map(command_to_json).collect()),
    );
    Value::Object(obj)
}

/// Build the JSON description of one clap argument.
fn arg_to_json(arg: &Arg) -> Value {
    let kind = if arg.is_positional() {
        "positional"
    } else if arg.get_action().takes_values() {
        "option"
    } else {
        "flag"
    };
    json!({
        "id": arg.get_id().as_str(),
        "short": arg.get_short(),
        "long": arg.get_long(),
        "aliases": arg.get_all_aliases().unwrap_or_default(),
        "kind": kind,
        "help": arg.get_help().map(|h| h.to_string()),
        "long_help": arg.get_long_help().map(|h| h.to_string()),
        "required": arg.is_required_set(),
        "hidden": arg.is_hide_set(),
        "global": arg.is_global_set(),
        // Value details are meaningless for pure flags (clap derives a
        // BOOL-ish value parser for `SetTrue` actions).
        "value_names": if kind == "flag" { None } else {
            arg.get_value_names().map(|names| {
                names.iter().map(|n| n.as_str().to_string()).collect::<Vec<_>>()
            })
        },
        "num_args": arg.get_num_args().map(|r| r.to_string()),
        "defaults": arg
            .get_default_values()
            .iter()
            .map(|v| v.to_string_lossy().into_owned())
            .collect::<Vec<_>>(),
        "possible_values": if kind == "flag" { Vec::new() } else {
            arg.get_possible_values()
                .iter()
                .filter(|pv| !pv.is_hide_set())
                .map(|pv| pv.get_name().to_string())
                .collect::<Vec<_>>()
        },
    })
}

/// Dump the JSON description of `command` to stdout and exit
/// successfully.
pub fn dump(command: &Command) -> ! {
    println!(
        "{}",
        serde_json::to_string_pretty(&command_to_json(command))
            .expect("help-json: serialize arg data")
    );
    std::process::exit(0);
}

/// If `--help-json` appears anywhere in argv, dump the JSON
/// description of the command built by `build` and exit; otherwise
/// return normally.
// Used only by the manually-parsing binaries (`vir-format`,
// `vir-filecheck`); derive binaries declare the flag and call `dump`.
#[allow(dead_code)]
pub fn maybe_dump(build: impl FnOnce() -> Command) {
    if std::env::args_os()
        .any(|a| a == std::ffi::OsStr::new("--help-json"))
    {
        dump(&build());
    }
}
