//! The `vir-cones` binary: dumps the combinational cones of an
//! elaborated design, for every register and every top-level port.
//!
//! For each such signal, prints its *forward cone* (the registers and
//! boundary output ports reachable from its output through combinational
//! logic) and its *backward cone* (the registers and boundary input
//! ports feeding its input through combinational logic).  Registers are
//! cone endpoints: paths are traced through wires, ports, and socket
//! channels, but never through another register.
//!
//! Run directly as `vir-cones`, or via `vir cones` (companion binaries
//! named `vir-*` are discovered dynamically; see `vir/src/vir/cli.rs`).

use bstr::ByteSlice;
use clap::{Arg, Command as ClapCommand, value_parser};
use indexmap::IndexSet;

use virdant::analysis::elaboration::SignalId;
use virdant::common::ComponentKind;

use virdant::analysis::cones::Cones;
use virdant::analysis::elaboration::Elaboration;

mod svg;

#[path = "../common/help_json.rs"]
mod help_json;

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use common::ProjectLocation;

fn command() -> ClapCommand {
    let mut cmd = ClapCommand::new("vir-cones")
        .about("Dump the combinational cones of an elaborated design (registers and top-level ports)")
        .arg_required_else_help(true)
        .arg(
            Arg::new("top")
                .value_name("TOP")
                .help("Fully-qualified name of the top module")
                .value_parser(value_parser!(String))
                .required(true),
        )
        .arg(
            Arg::new("svg")
                .long("svg")
                .value_name("FILEPATH")
                .value_parser(value_parser!(String))
                .help("Also render the cone network as SVG: write it to FILEPATH, or print it to stdout with '--svg=-' (instead of the text dump)"),
        );
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    help_json::maybe_dump(command);

    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);
    let top = common::matches_string(&matches, "top");
    let svg_path = common::matches_opt_string(&matches, "svg");
    cones(&location, &top, svg_path.as_deref());
}

fn cones(location: &ProjectLocation, top: &str, svg_path: Option<&str>) {
    let db = location.project_db();
    common::dump_diagnostics(&db);
    if virdant::util::check_db(&db).is_err() {
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top_symbol = match symboltable.resolve_item_fqn(top.as_bytes().as_bstr()) {
        Some(symbol) => symbol.clone(),
        None => {
            eprintln!("Error: module '{top}' not found");
            std::process::exit(1);
        }
    };

    let elaboration = db.get_elaboration(top_symbol.id);
    let cones = db.get_cones(top_symbol.id);

    match svg_path {
        Some("-") => {
            println!("{}", svg::render(&elaboration, &cones, top));
        }
        Some(path) => {
            dump_cones(&elaboration, &cones, top);
            let svg = svg::render(&elaboration, &cones, top);
            if let Err(e) = std::fs::write(path, svg) {
                eprintln!("Error: cannot write '{path}': {e}");
                std::process::exit(1);
            }
            eprintln!("Wrote cone network SVG to {path}");
        }
        None => dump_cones(&elaboration, &cones, top),
    }
}

fn dump_cones(elaboration: &Elaboration, cones: &Cones, top: &str) {
    let signals: Vec<_> = cones.signals().iter().copied().collect();
    let num_registers = cones.registers().count();
    let num_ports = signals.len() - num_registers;

    println!(
        "Cones of '{top}' ({}, {}, {})",
        plural(signals.len(), "signal"),
        plural(num_registers, "register"),
        plural(num_ports, "port"),
    );
    if signals.is_empty() {
        return;
    }
    println!();

    for signal in signals {
        let component = elaboration.component(signal);
        let path = component.path().to_str_lossy().into_owned();

        let kind_line = if cones.is_register(signal) {
            let clock = component
                .clock()
                .map(|clock_id| {
                    elaboration.component(clock_id).path().to_str_lossy().into_owned()
                })
                .unwrap_or_else(|| "?".to_owned());
            format!("reg on {clock}")
        } else {
            match component.component_kind() {
                ComponentKind::Incoming => "incoming port".to_owned(),
                _ => "outgoing port".to_owned(),
            }
        };

        println!("{path} : {kind_line}");
        println!("  backward cone (what feeds {path} through combinational logic):");
        print_cone(elaboration, cones, cones.backward(signal));
        println!("  forward cone (what {path} feeds through combinational logic):");
        print_cone(elaboration, cones, cones.forward(signal));
        println!();
    }
}

fn plural(n: usize, word: &str) -> String {
    if n == 1 {
        format!("{n} {word}")
    } else {
        format!("{n} {word}s")
    }
}

fn print_cone(
    elaboration: &Elaboration,
    cones: &Cones,
    cone: Option<&IndexSet<SignalId>>,
) {
    match cone {
        Some(cone) if !cone.is_empty() => {
            for dep in cone {
                let path = elaboration.component(*dep).path();
                if cones.is_register(*dep) {
                    println!("    {}", path);
                } else {
                    println!("    {} (port)", path);
                }
            }
        }
        _ => println!("    (none)"),
    }
}
