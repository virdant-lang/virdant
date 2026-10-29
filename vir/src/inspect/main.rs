//! The `vir-inspect` binary: intermediate-state inspection for Virdant
//! projects (formerly the `parse`, `tokenize`, `types`, `symbols`,
//! `typedefs`, `exprroots`, `typing`, `db`, `components`, `ports`, and
//! `elaborate` subcommands of `vir`).
//!
//! `vir inspect <subcommand>` execs this binary via the `vir-*`
//! companion-binary mechanism; see `vir/src/vir/cli.rs`.

use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use bstr::{BStr, BString, ByteSlice};
use clap::{Arg, ArgAction, Command as ClapCommand, value_parser};
use colored::Colorize;

use virdant::common::source::{Region, Source};
use virdant::common::PortDir;
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::package::PackageTable;
use virdant::syntax::parsing::parse;
use virdant::syntax::token::tokenize;
use virdant::syntax::token::Token;

#[path = "../common/help_json.rs"]
mod help_json;

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;

use common::ProjectLocation;

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

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

fn command() -> ClapCommand {
    let mut cmd = ClapCommand::new("vir-inspect")
        .about("Inspect Virdant analysis state (parse trees, symbols, types, elaboration)")
        .arg_required_else_help(true)
        .subcommand(ClapCommand::new("parse")
            .about("Parse and dump the AST for a Virdant source file")
            .arg(path_arg("file", "FILE")))
        .subcommand(ClapCommand::new("tokenize")
            .about("Tokenize a Virdant source file")
            .arg(path_arg("file", "FILE")))
        .subcommand(ClapCommand::new("types")
            .about("Dump inferred expression types"))
        .subcommand(ClapCommand::new("symbols")
            .about("Dump the symbol table"))
        .subcommand(ClapCommand::new("typedefs")
            .about("Dump typedefs"))
        .subcommand(ClapCommand::new("exprroots")
            .about("Dump expression roots"))
        .subcommand(ClapCommand::new("typing")
            .about("Dump typing results"))
        .subcommand(ClapCommand::new("db")
            .about("Dump database state, optionally saving a Graphviz file")
            .arg(path_arg("outpath", "OUTPATH").required(false))
            .arg(
                Arg::new("json")
                    .long("json")
                    .action(ArgAction::SetTrue)
                    .help("Render the output as JSON"),
            ))
        .subcommand(ClapCommand::new("components")
            .about("Dump component analysis for a module definition")
            .arg(string_arg("moddef_fqn", "MODDEF_FQN")))
        .subcommand(ClapCommand::new("ports")
            .about("List ports for a module definition")
            .arg(string_arg("moddef_fqn", "MODDEF_FQN")))
        .subcommand(ClapCommand::new("elaborate")
            .about("Dump the elaborated design rooted at a top module")
            .arg(string_arg("top", "TOP")));
    common::add_project_args(&mut cmd);
    cmd
}

fn main() {
    help_json::maybe_dump(command);

    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);

    match matches.subcommand() {
        Some(("parse", m)) => parse_file(&common::matches_path(m, "file")),
        Some(("tokenize", m)) => tokenize_file(&common::matches_path(m, "file")),
        Some(("types", _)) => dump_types(&location),
        Some(("symbols", _)) => dump_symbols(&location),
        Some(("typedefs", _)) => dump_typedefs(&location),
        Some(("exprroots", _)) => dump_exprroots(&location),
        Some(("typing", _)) => dump_typing(&location),
        Some(("db", m)) => dump_db(&location, common::matches_opt_path(m, "outpath"), common::matches_flag(m, "json")),
        Some(("components", m)) => dump_components(&location, &common::matches_string(m, "moddef_fqn")),
        Some(("ports", m)) => dump_ports(&location, &common::matches_string(m, "moddef_fqn")),
        Some(("elaborate", m)) => elaborate(&location, &common::matches_string(m, "top")),
        _ => unreachable!("arg_required_else_help or a subcommand is always present"),
    }
}

// ---------------------------------------------------------------------------
// Parse / tokenize (single-file, no project)
// ---------------------------------------------------------------------------

fn parse_file(path: &Path) {
    let input = match std::fs::read(path) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("ERROR");
            eprintln!("{e:?}");
            std::process::exit(-1);
        }
    };

    let name = match path.file_stem() {
        Some(stem) => BString::from(stem.as_bytes()),
        None => {
            eprintln!("ERROR");
            eprintln!("could not determine package name from path: {}", path.display());
            std::process::exit(-1);
        }
    };

    let mut db = Db::new();
    db.set_packages(PackageTable::new(vec!["builtin".into(), name.clone()]));
    let package = db.get_packages().id(name.as_bstr()).unwrap();

    let source = Source::new(package, input.into());
    let parsing = parse(&source);
    parsing.dump();

    let diagnostics = parsing.diagnostics();
    let longest_region = diagnostics
        .iter()
        .map(|diag| diag.region().display(&db).len())
        .max()
        .unwrap_or_default();
    for diagnostic in diagnostics {
        let unpadded_region = diagnostic.region().display(&db);
        let padded_region = format!("{}{}", unpadded_region, " ".repeat(longest_region - unpadded_region.len()));
        if diagnostic.level() == DiagnosticLevel::Error {
            println!("{}   {}   {}", "ERROR  ".red(), padded_region, diagnostic.message());
        } else if diagnostic.level() == DiagnosticLevel::Warning {
            println!("{}   {}   {}", "WARNING".yellow(), padded_region, diagnostic.message());
        } else {
            println!("{}   {}   {}", "INFO   ".green(), padded_region, diagnostic.message());
        }
    }
}

fn tokenize_file(path: &Path) {
    let input = match std::fs::read(path) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("ERROR");
            eprintln!("{e:?}");
            std::process::exit(-1);
        }
    };

    for (i, token) in tokenize(input.as_bstr()).enumerate() {
        match token {
            Ok((start, token, end)) => {
                let start = usize::from(start);
                let end = usize::from(end);
                let loc = format!("{start}..{end}");
                let token_str = token.to_string();

                let snippet = match token {
                    Token::Ident |
                    Token::Nat |
                    Token::Word |
                    Token::Error => BStr::new(&input[start..end]),
                    _ => BStr::new(""),
                };

                let token_num = format!("{:>3}#", token as usize);
                println!("{i:>5} {loc:>10}   {token_str:>13} {token_num}      {snippet}");
            }
            Err(err) => {
                eprintln!("ERROR");
                eprintln!("{err:?}");
                std::process::exit(-1);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Db dumps (project-wide)
// ---------------------------------------------------------------------------

fn dump_types(location: &ProjectLocation) {
    let db = location.project_db();
    let _ = db.check();
    for (location, typ) in db.get_typeof_all() {
        let package = location.package();
        let parsing = db.get_parsing(package);
        let node = parsing.ast_node(location.ast_node_id());
        let spelling = node.spelling().to_owned();
        let region = Region::new(package, node.span());
        println!(
            "{location:?} has type {typ:?}   {:?}  {:?}",
            spelling,
            region,
        );
    }
}

fn dump_symbols(location: &ProjectLocation) {
    let db = location.project_db();
    common::dump_diagnostics(&db);

    let symboltable = db.get_symboltable();
    for symbol in symboltable.symbols() {
        if let Some(parent_symbol_id) = symbol.parent_id() {
            let kind_str = format!("{:?}", symbol.kind());
            println!("{:?} {:<20} {kind_str:<20} parent {parent_symbol_id:?}", symbol.id(), symbol.fqn());
        } else {
            println!("{:?} {:<20} {:?}", symbol.id(), symbol.fqn(), symbol.kind());
        }
    }
}

fn dump_typedefs(location: &ProjectLocation) {
    let db = location.project_db();
    common::dump_diagnostics(&db);

    let typedefs = db.get_typedefs();
    for typedef in typedefs.iter() {
        println!("{typedef:?}");
    }
}

fn dump_exprroots(location: &ProjectLocation) {
    let db = location.project_db();
    common::dump_diagnostics(&db);

    let exprroots = db.get_exprroots();
    for exprroot in exprroots.iter() {
        let package = exprroot.location().package();
        let parsing = db.get_parsing(package);
        let location = exprroot.location();
        let typ = db.get_expected_type(exprroot.clone());
        let node = parsing.ast_node(location.ast_node_id());
        let region = node.region();
        println!("{location:?} : {typ:?} at @{}", region.display(&db));
    }
}

fn dump_typing(location: &ProjectLocation) {
    let db = location.project_db();
    common::dump_diagnostics(&db);

    for (location, typ) in db.get_typeof_all() {
        let region = db.get_location_region(location.clone());
        let parsing = db.get_parsing(location.package());
        let text = parsing.text(region.span());
        println!("{} {location:?} {typ:?} ({text:?})", region.display(&db));
    }
}

fn dump_db(location: &ProjectLocation, outpath: Option<std::path::PathBuf>, json: bool) {
    let db = location.project_db();
    let _ = db.check();
    if json {
        let mut root = json::object::Object::new();
        root.insert("diagnostics", common::diagnostics_json(&db));
        if let Some(outpath) = &outpath {
            db.save_graphviz(outpath);
            root.insert("graphviz_path", outpath.to_string_lossy().to_string().into());
        }
        root.insert("trace", db.dump_json());
        println!("{}", json::stringify_pretty(json::JsonValue::Object(root), 2));
    } else {
        common::dump_diagnostics(&db);
        if let Some(outpath) = outpath {
            println!("Saving graphviz: {}", outpath.display());
            db.save_graphviz(outpath);
        }
        db.dump();
    }
}

fn dump_components(location: &ProjectLocation, moddef_fqn: &str) {
    let db = location.project_db();
    common::dump_diagnostics(&db);

    let symboltable = db.get_symboltable();
    let moddef = symboltable.resolve_item_fqn(moddef_fqn.as_bytes().as_bstr()).unwrap();
    let component_analysis = db.get_component_analysis(moddef.id());
    dbg!(&component_analysis);
}

fn dump_ports(location: &ProjectLocation, moddef_fqn: &str) {
    let db = location.project_db();
    common::dump_diagnostics(&db);

    let symboltable = db.get_symboltable();
    let moddef = symboltable.resolve_item_fqn(moddef_fqn.as_bytes().as_bstr()).unwrap();
    let ports = db.get_ports_of(moddef.id());

    println!("Ports for {moddef_fqn}:");
    for port in ports.iter() {
        let path = port.path.to_str_lossy();
        let dir = match port.dir {
            PortDir::Input => "input",
            PortDir::Output => "output",
            PortDir::Bidir => "bus",
        };
        let typ = port.typ
            .clone()
            .map(|t| format!("{:?}", t))
            .unwrap_or_else(|| "<unknown>".to_string());
        println!("  {} {} : {}", dir, path, typ);
    }
}

fn elaborate(location: &ProjectLocation, top: &str) {
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
    elaboration.dump();
}
