//! The `vir-value` binary: standalone value encode/decode for
//! Virdant projects.
//!
//! `vir value` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.
//!
//! ```
//! vir value encode @Valid(0) : Valid[Word[8]]   # value -> bit pattern
//! vir value decode 0b0000000010 Valid[Word[8]]  # bit pattern -> value
//! ```
//!
//! Outside a project (no `-F`, no `-C`, no manifest in or above the
//! working directory), the binary still runs, with only the standard
//! library loaded: builtin types (`Bit`, `Word[8]`, `Valid[...]`, ...)
//! work, project-local types do not.  From a project subdirectory
//! (eg, `src/`), the enclosing project is discovered and used.
//!
//! Types are fully-qualified (`pkg::Name[...]`) except names from the
//! `builtin` package, which are bare (`Bit`, `Word[8]`,
//! `Valid[Word[8]]`).  Non-pub types resolve the same way: lookup is
//! by fully-qualified name, so visibility plays no role.
//!
//! This binary owns only the command-line interface; the
//! encode/decode engine lives in the WASM-clean `vir_value` library
//! (`lib.rs`), which is also what a WASM front-end (WASI/wasmtime)
//! links.

use vir_value::run;

use vir_common as common;

use vir_common::help_json;

use common::ProjectLocation;

/// Assemble the `vir-value` command line interface.
fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-value")
        .about("Encode and decode Virdant values")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            clap::Command::new("encode")
                .about("Encode a value to a bit pattern")
                .arg(
                    clap::Arg::new("parts")
                        .value_name("VALUE [: TYPE]")
                        .num_args(1..)
                        .required(true)
                        .help("Value expression, optionally with a `: Type` ascription"),
                ),
        )
        .subcommand(
            clap::Command::new("decode")
                .about("Decode a bit pattern to a value")
                .arg(
                    clap::Arg::new("parts")
                        .value_name("PATTERN TYPE")
                        .num_args(2..)
                        .required(true)
                        .help("Bit pattern literal and the type to decode it as"),
                ),
        );
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    help_json::maybe_dump(|| command());
    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);

    let (subcommand, parts): (&str, Vec<String>) = match matches.subcommand() {
        Some(("encode", m)) => (
            "encode",
            m.get_many::<String>("parts")
                .map(|vs| vs.cloned().collect())
                .unwrap_or_default(),
        ),
        Some(("decode", m)) => (
            "decode",
            m.get_many::<String>("parts")
                .map(|vs| vs.cloned().collect())
                .unwrap_or_default(),
        ),
        _ => unreachable!("subcommand_required"),
    };

    // The CLI is shell-friendly: the positional arguments are joined
    // with spaces and parsed as one string, so both
    // `encode @Valid(0) : Valid[Word[8]]` and
    // `encode '@Valid(0):Valid[Word[8]]'` work.
    let input = parts.join(" ");

    let db = location.project_db_or_builtin();

    match run(&db, subcommand, &input) {
        Ok(output) => println!("{output}"),
        Err(message) => {
            eprintln!("ERROR: {message}");
            std::process::exit(1);
        }
    }
}
