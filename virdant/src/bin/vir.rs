//! The main `vir` CLI entry point dispatching subcommands for parsing,
//! tokenizing, type-checking, elaboration, Verilog compilation,
//! running, documentation generation, and project scaffolding.

use clap::CommandFactory;
use clap::{Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice};
use colored::Colorize;
use nix::unistd::execvp;
use virdant::util::{check_db, db_from_dir, db_from_files};
use std::ffi::{CString, OsString};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::package::PackageTable;
use virdant::common::source::{Region, Source};
use virdant::analysis::symbols::SymbolKind;
use virdant::syntax::parsing::parse;
use virdant::syntax::token::tokenize;
use virdant::syntax::token::Token;

/// The Virdant Hardware Language
#[derive(Parser, Debug)]
#[command(name = "vir", author, version, about, disable_help_subcommand = true, arg_required_else_help = true)]
struct Args {
    #[arg(short = 'C', conflicts_with = "virfile")]
    cwd: Option<PathBuf>,

    /// Load .vir file(s) as a self-contained project (comma-separated, no spaces; exclusive with -C)
    #[arg(short = 'F', conflicts_with = "cwd")]
    virfile: Option<String>,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Parse and dump the AST for a Virdant source file
    Parse { file: PathBuf },
    /// Tokenize a Virdant source file
    Tokenize { file: PathBuf },
    /// Typecheck a Virdant project
    Check { },
    /// Dump inferred expression types
    Types { },
    /// Dump the symbol table
    Symbols { },
    /// Dump typedefs
    Typedefs { },
    /// Dump expression roots
    Exprroots { },
    /// Dump typing results
    Typing { },
    /// Dump database state, optionally saving a Graphviz file
    Db {
        outpath: Option<PathBuf>,
        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },
    /// Dump component analysis for a module definition
    Components { moddef_fqn: String },
    /// List ports for a module definition
    Ports { moddef_fqn: String },
    /// Dump the elaborated design rooted at a top module
    Elaborate { top: String },
    /// Compile Virdant package
    Compile { path: PathBuf },
    /// Build Virdant package
    Build { },
    /// Run Virdant package
    Run { path: PathBuf },
    /// Run Virdant package with Icarus Verilog
    RunIcarus { path: Option<PathBuf>, #[arg(long)] vcd: Option<String> },
    /// Create a new Virdant project
    New { project: String },
    /// Synthesize, place-and-route, and pack a bitstream
    Bitstream { },
    /// Synthesize, place-and-route, pack, and upload a bitstream
    Upload { },

    /// Generate HTML documentation
    Doc {
        /// Open the docs in a browser after generation
        #[arg(long)]
        open: bool,
    },

    #[command(external_subcommand)]
    External(Vec<OsString>),
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Parse { file } => parse_file(&file),
        Command::Tokenize { file } => tokenize_file(&file),
        Command::Check { } => check(&args),
        Command::Db { ref outpath, json } => dump_db(&args, outpath.clone(), json),
        Command::Types { } => dump_types(&args),
        Command::Components { ref moddef_fqn } => dump_components(&args, moddef_fqn),
        Command::Ports { ref moddef_fqn } => dump_ports(&args, moddef_fqn),
        Command::Elaborate { ref top } => elaborate(&args, top),
        Command::Symbols { } => dump_symbols(&args),
        Command::Typedefs { } => dump_typedefs(&args),
        Command::Exprroots { } => dump_exprroots(&args),
        Command::Typing { } => dump_typing(&args),
        Command::Compile { path } => compile(path),
        Command::Build { } => build(&args),
        Command::Run { ref path } => run(&args, path),
        Command::RunIcarus { ref path, ref vcd } => run_icarus(&args, path, vcd),
        Command::New { ref project } => new_project(&args, project),
        Command::Bitstream { } => bitstream(&args),
        Command::Upload { } => upload(&args),
        Command::Doc { open } => doc(&args, open),
        Command::External(args) => exec_external(args),
    }
}

fn project_db(args: &Args) -> Db {
    if let Some(virfile) = &args.virfile {
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
        return db_from_files(paths);
    }

    let cwd = resolve_cwd(args);

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let src_dir = cwd.join("src");
    if !src_dir.is_dir() {
        eprintln!("ERROR: source directory not found: {}", src_dir.display());
        std::process::exit(1);
    }

    db_from_dir(src_dir)
}

/// Canonicalize `args.cwd` if present, else the current directory.
fn resolve_cwd(args: &Args) -> PathBuf {
    if let Some(cwd) = &args.cwd {
        match std::fs::canonicalize(cwd) {
            Ok(path) => path,
            Err(e) => {
                eprintln!("ERROR: cannot resolve directory {}: {e}", cwd.display());
                std::process::exit(1);
            }
        }
    } else {
        match std::env::current_dir() {
            Ok(path) => path,
            Err(e) => {
                eprintln!("ERROR: cannot determine current directory: {e}");
                std::process::exit(1);
            }
        }
    }
}

/// Read `[prog].<key>` from `Virdant.toml` in `cwd`, tolerating a
/// missing file or missing keys.
fn read_prog_key(cwd: &Path, key: &str) -> Option<String> {
    let text = std::fs::read_to_string(cwd.join("Virdant.toml")).ok()?;
    let toml: toml::Value = toml::from_str(&text).ok()?;
    toml.get("prog")
        .and_then(|prog| prog.get(key))
        .and_then(|value| value.as_str())
        .map(|s| s.to_owned())
}

/// Read `[project].name` from `Virdant.toml` in `cwd`.
fn read_project_name(cwd: &Path) -> Option<String> {
    let text = std::fs::read_to_string(cwd.join("Virdant.toml")).ok()?;
    let toml: toml::Value = toml::from_str(&text).ok()?;
    toml.get("project")
        .and_then(|project| project.get("name"))
        .and_then(|value| value.as_str())
        .map(|s| s.to_owned())
}

/// Print one diagnostic in the same style as `dump_diagnostics`.
fn dump_diagnostic(db: &Db, diagnostic: &virdant::diagnostics::Diagnostic) {
    println!(
        "{}   {}   {}",
        "ERROR  ".red(),
        region_string(db, diagnostic.region()),
        diagnostic.message(),
    );
}

fn region_string(db: &Db, region: Region) -> String {
    use bstr::ByteSlice as _;

    let package = db.get_packages().name(region.package()).to_str_lossy().into_owned();
    let span = region.span().to_string();

    format!("{package}.vir {span}")
}

fn dump_diagnostics(db: &Db) {
    let diagnostics = match check_db(db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
    let longest_region = diagnostics
        .iter()
        .map(|diag| region_string(db, diag.region()).len())
        .max()
        .unwrap_or_default();

    let mut warning_count = 0;
    let mut error_count = 0;

    for diagnostic in diagnostics.iter() {
        let unpadded_region = region_string(db, diagnostic.region());
        let padded_region = format!("{}{}", unpadded_region, " ".repeat(longest_region - unpadded_region.len()));
        if diagnostic.level() == DiagnosticLevel::Error {
            println!("{}   {}   {}", "ERROR  ".red(), padded_region, diagnostic.message());
            error_count += 1;
        } else if diagnostic.level() == DiagnosticLevel::Warning {
            println!("{}   {}   {}", "WARNING".yellow(), padded_region, diagnostic.message());
            warning_count += 1;
        } else {
            println!("{}   {}   {}", "INFO   ".green(), padded_region, diagnostic.message());
        }
    }

    let failed = error_count > 0;

    if error_count > 0 || warning_count > 0 {
        println!();
        if failed {
            println!("{} with:", "FAILED".red());
        } else {
            println!("{} with:", "PASSED".green());
        }
        if error_count > 0 {
            println!("{error_count:>4} {}", "ERROR".red());
        }

        if warning_count > 0 {
            println!("{warning_count:>4} {}", "WARNING".yellow());
        }
    }
}

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

fn dump_db(args: &Args, outpath: Option<PathBuf>, json: bool) {
    let db = project_db(args);
    let _ = db.check();
    if json {
        let mut root = json::object::Object::new();
        root.insert("diagnostics", diagnostics_json(&db));
        if let Some(outpath) = &outpath {
            db.save_graphviz(outpath);
            root.insert("graphviz_path", outpath.to_string_lossy().to_string().into());
        }
        root.insert("trace", db.dump_json());
        println!("{}", json::stringify_pretty(json::JsonValue::Object(root), 2));
    } else {
        dump_diagnostics(&db);
        if let Some(outpath) = outpath {
            println!("Saving graphviz: {}", outpath.display());
            db.save_graphviz(outpath);
        }
        db.dump();
    }
}

fn diagnostics_json(db: &Db) -> json::JsonValue {
    let diagnostics = match check_db(db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
    let mut array = json::JsonValue::new_array();
    for diagnostic in diagnostics.iter() {
        let mut entry = json::object::Object::new();
        let level = match diagnostic.level() {
            DiagnosticLevel::Error => "error",
            DiagnosticLevel::Warning => "warning",
            DiagnosticLevel::Info => "info",
        };
        entry.insert("level", level.into());
        entry.insert("region", region_string(db, diagnostic.region()).into());
        entry.insert("message", diagnostic.message().to_string().into());
        array.push(json::JsonValue::Object(entry)).unwrap();
    }
    array
}

fn dump_types(args: &Args) {
    let db = project_db(args);
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

fn dump_components(args: &Args, moddef_fqn: &str) {
    let db = project_db(args);
    dump_diagnostics(&db);

    let symboltable = db.get_symboltable();
    let moddef = symboltable.resolve_item_fqn(moddef_fqn.as_bytes().as_bstr()).unwrap();
    let component_analysis = db.get_component_analysis(moddef.id());
    dbg!(&component_analysis);
}

fn dump_ports(args: &Args, moddef_fqn: &str) {
    use bstr::ByteSlice;
    let db = project_db(args);
    dump_diagnostics(&db);

    let symboltable = db.get_symboltable();
    let moddef = symboltable.resolve_item_fqn(moddef_fqn.as_bytes().as_bstr()).unwrap();
    let ports = db.get_ports_of(moddef.id());

    println!("Ports for {}:", moddef_fqn);
    for port in ports.iter() {
        let path = port.path.to_str_lossy();
        let dir = match port.dir {
            virdant::common::PortDir::Input => "input",
            virdant::common::PortDir::Output => "output",
        };
        let typ = port.typ
            .clone()
            .map(|t| format!("{:?}", t))
            .unwrap_or_else(|| "<unknown>".to_string());
        println!("  {} {} : {}", dir, path, typ);
    }
}

fn elaborate(args: &Args, top: &str) {
    let db = project_db(args);
    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        std::process::exit(1);
    }
    let symboltable = db.get_symboltable();
    let top_symbol = match symboltable.resolve_item_fqn(top.as_bytes().as_bstr()) {
        Some(symbol) => symbol.clone(),
        None => {
            eprintln!("Error: module '{}' not found", top);
            std::process::exit(1);
        }
    };
    let elaboration = db.get_elaboration(top_symbol.id);
    elaboration.dump();
}

fn dump_symbols(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);

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

fn dump_typedefs(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);

    let typedefs = db.get_typedefs();
    for typedef in typedefs.iter() {
        println!("{typedef:?}");
    }
}

fn dump_exprroots(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);

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

fn dump_typing(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);

    for (location, typ) in db.get_typeof_all() {
        let region = db.get_location_region(location.clone());
        let parsing = db.get_parsing(location.package());
        let text = parsing.text(region.span());
        println!("{} {location:?} {typ:?} ({text:?})", region.display(&db));
    }
}

fn doc(args: &Args, open: bool) {
    let cwd = resolve_cwd(args);

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let source_dir = cwd.join("src");
    if !source_dir.is_dir() {
        eprintln!("ERROR: source directory not found: {}", source_dir.display());
        std::process::exit(1);
    }

    let db = virdant::util::db_from_dir(source_dir);
    dump_diagnostics(&db);
    if virdant::util::check_db(&db).is_err() {
        eprintln!("Doc generation failed due to errors");
        std::process::exit(1);
    }

    let out_dir = cwd.join("build").join("doc");
    match virdant::docs::generate_docs(&db, &out_dir) {
        Ok(()) => {
            println!("Wrote docs to {}", out_dir.to_string_lossy());
            if open {
                open_in_browser(&out_dir.join("index.html"));
            }
        }
        Err(e) => {
            eprintln!("Doc generation error: {e}");
            std::process::exit(1);
        }
    }
}

/// Open a file or URL in the default OS browser.
fn open_in_browser(path: &std::path::Path) {
    let result = if cfg!(target_os = "macos") {
        std::process::Command::new("open").arg(path).status()
    } else if cfg!(target_os = "linux") {
        std::process::Command::new("xdg-open").arg(path).status()
    } else if cfg!(target_os = "windows") {
        std::process::Command::new("cmd")
            .arg("/c")
            .arg("start")
            .arg(path)
            .status()
    } else {
        eprintln!("Cannot open browser on this platform");
        return;
    };

    match result {
        Ok(status) if status.success() => {}
        Ok(status) => {
            eprintln!("Failed to open browser: process exited with {}", status);
        }
        Err(e) => {
            eprintln!("Failed to open browser: {e}");
        }
    }
}

fn build(args: &Args) {
    let (db, builddir) = if let Some(virfile) = &args.virfile {
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

        if !cwd.join("Virdant.toml").exists() {
            eprintln!("No Virdant.toml found");
            std::process::exit(1);
        }

        let virdant_toml_text = std::fs::read_to_string(cwd.join("Virdant.toml")).unwrap();
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
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());
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

fn run_icarus(args: &Args, _path: &Option<PathBuf>, vcd: &Option<String>) {
    let cwd = resolve_cwd(args);

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let virdant_toml_text = std::fs::read_to_string(cwd.join("Virdant.toml")).unwrap();
    let virtant_toml: toml::Value = toml::from_str(&virdant_toml_text).unwrap();

    let project = virtant_toml["project"]["name"].as_str().unwrap();
    let builddir = cwd.join("build");

    let source_dir = cwd.join("src");
    let db = db_from_dir(source_dir);
    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        eprintln!("Build failed");
        std::process::exit(1);
    }

    std::fs::create_dir_all(&builddir).unwrap();
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());


    let bin_name = project;
    let bin = builddir.join(&bin_name).to_string_lossy().to_string();

    let mut command = std::process::Command::new("iverilog");
    command.arg("-g2012");
    command.arg("verilog/tb.sv");

    for source in glob_sv_files(&builddir) {
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

fn run(_args: &Args, path: &PathBuf) {
    if let Err(e) = virdant::script::run_script_file(path) {
        eprintln!("Script error: {}", e);
        std::process::exit(1);
    }
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
        "[project]\nname = \"{project}\"\n\n[prog]\nplatform = \"icesugar\"\n"
    );
    std::fs::write(project_dir.join("Virdant.toml"), toml_content).unwrap();

    let top_vir_content = include_str!("../../../assets/blink.vir");
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

fn bitstream(args: &Args) {
    let cwd = resolve_cwd(args);
    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let project = read_project_name(&cwd).unwrap_or_else(|| {
        eprintln!("Virdant.toml is missing [project] name");
        std::process::exit(1);
    });
    let Some(top_name) = read_prog_key(&cwd, "top") else {
        eprintln!("Virdant.toml is missing [prog] top");
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
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());

    let Some(toolchain) = virdant::build::toolchain_for(fpga.to_str_lossy().as_ref()) else {
        eprintln!("FPGA family '{}' is not supported", fpga.to_str_lossy());
        std::process::exit(1);
    };

    match virdant::build::emit_pcf(&mut builder, platform_id) {
        Ok(pcf_text) => {
            let pcf = builddir.join(format!("{project}.pcf"));
            std::fs::write(&pcf, pcf_text).unwrap();
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

    virdant::build::run_toolchain(&builddir, &project, &top_name, part.as_ref(), toolchain)
        .unwrap_or_else(|e| {
            eprintln!("Toolchain error: {e}");
            std::process::exit(1);
        });
}

fn upload(args: &Args) {
    bitstream(args);

    let cwd = resolve_cwd(args);
    let project = read_project_name(&cwd).unwrap_or_else(|| {
        eprintln!("Virdant.toml is missing [project] name");
        std::process::exit(1);
    });

    virdant::build::flash_bitstream(&cwd, &project)
        .unwrap_or_else(|e| {
            eprintln!("Flash error: {e}");
            std::process::exit(1);
        });
}

fn glob_sv_files(dir: &Path) -> Vec<PathBuf> {
    let mut sources: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .filter_map(|entry| entry.ok().map(|entry| std::fs::canonicalize(entry.path()).unwrap()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "sv"))
        .collect();
    sources.sort();
    sources
}

fn exec_external(args: Vec<OsString>) {
    let Some(command) = args.first() else {
        Args::command().print_help().unwrap();
        std::process::exit(3);
    };

    let command = command.to_string_lossy();
    if let Some(bin) = find_bin(&format!("vir-{command}")) {
        let program = CString::new(bin).unwrap();
        let c_args: Vec<CString> = args
            .into_iter()
            .map(|arg| CString::new(arg.as_os_str().as_bytes()).unwrap())
            .collect();

        let _ = execvp(&program, &c_args);
    } else {
        Args::command().print_help().unwrap();
        std::process::exit(3);
    }
}

fn find_bin(command: &str) -> Option<String> {
    let path = std::env::var("PATH").unwrap();

    for dirpath in path.split(":") {
        let binpath: std::path::PathBuf = std::path::PathBuf::from(dirpath).join(command);
        if std::fs::exists(&binpath).unwrap() {
            return Some(binpath.to_string_lossy().to_string());
        }
    }

    None
}
