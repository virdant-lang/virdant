use clap::CommandFactory;
use clap::{Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice};
use colored::Colorize;
use nix::unistd::execvp;
use virdant::util::{check_db, db_from_dir, db_from_dir_with_platform, db_from_files, read_platform_from_toml};
use std::ffi::{CString, OsString};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::fqn::PackageFqn;
use virdant::common::source::{Region, Source};
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
    Db { outpath: Option<PathBuf> },
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
        Command::Db { ref outpath } => dump_db(&args, outpath.clone()),
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

    let cwd = if let Some(cwd) = &args.cwd {
        match std::fs::canonicalize(cwd) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("ERROR: cannot resolve directory {}: {e}", cwd.display());
                std::process::exit(1);
            }
        }
    } else {
        match std::env::current_dir() {
            Ok(p) => p,
            Err(e) => {
                eprintln!("ERROR: cannot determine current directory: {e}");
                std::process::exit(1);
            }
        }
    };

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let src_dir = cwd.join("src");
    if !src_dir.is_dir() {
        eprintln!("ERROR: source directory not found: {}", src_dir.display());
        std::process::exit(1);
    }

    match read_platform_from_toml(&cwd) {
        Ok(Some(platform)) => db_from_dir_with_platform(src_dir, &platform),
        Ok(None) => db_from_dir(src_dir),
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

fn region_string(region: Region) -> String {
    let package = region.package().to_string();
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
        .map(|diag| region_string(diag.region()).len())
        .max()
        .unwrap_or_default();

    let mut warning_count = 0;
    let mut error_count = 0;

    for diagnostic in diagnostics.iter() {
        let unpadded_region = region_string(diagnostic.region());
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

    let package = match path.file_stem() {
        Some(stem) => PackageFqn::new(BString::from(stem.as_bytes())),
        None => {
            eprintln!("ERROR");
            eprintln!("could not determine package name from path: {}", path.display());
            std::process::exit(-1);
        }
    };

    let source = Source::new(package, input.into());
    let parsing = parse(&source);
    parsing.dump();

    let diagnostics = parsing.diagnostics();
    let longest_region = diagnostics
        .iter()
        .map(|diag| diag.region().to_string().len())
        .max()
        .unwrap_or_default();
    for diagnostic in diagnostics {
        let unpadded_region = diagnostic.region().to_string();
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

fn dump_db(args: &Args, outpath: Option<PathBuf>) {
    let db = project_db(args);
    let _ = db.check();
    dump_diagnostics(&db);
    if let Some(outpath) = outpath {
        println!("Saving graphviz: {}", outpath.display());
        db.save_graphviz(outpath);
    }
    db.dump();
}

fn dump_types(args: &Args) {
    let db = project_db(args);
    let _ = db.check();
    for (location, typ) in db.get_typeof_all() {
        let package = location.package();
        let parsing = db.get_parsing(package.clone());
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
        let parsing = db.get_parsing(package.clone());
        let location = exprroot.location();
        let typ = db.get_expected_type(exprroot.clone());
        let node = parsing.ast_node(location.ast_node_id());
        let region = node.region();
        println!("{location:?} : {typ:?} at @{region}");
    }
}

fn dump_typing(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);

    for (location, typ) in db.get_typeof_all() {
        let region = db.get_location_region(location.clone());
        let parsing = db.get_parsing(location.package());
        let text = parsing.text(region.span());
        println!("{region} {location:?} {typ:?} ({text:?})");
    }
}

fn doc(args: &Args, open: bool) {
    let cwd = if let Some(cwd) = &args.cwd {
        match std::fs::canonicalize(cwd) {
            Ok(path) => path,
            Err(_) => {
                eprintln!("Directory not found: {}", cwd.display());
                std::process::exit(1);
            }
        }
    } else {
        std::env::current_dir().unwrap()
    };

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
        let cwd = if let Some(cwd) = &args.cwd {
            match std::fs::canonicalize(cwd) {
                Ok(path) => path,
                Err(_) => {
                    eprintln!("Directory not found: {}", cwd.display());
                    std::process::exit(1);
                }
            }
        } else {
            std::env::current_dir().unwrap()
        };

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

    std::fs::create_dir_all(&builddir).unwrap();
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&builddir).unwrap();
    println!("Wrote Verilog to {}", builddir.to_string_lossy());
}

fn run_icarus(args: &Args, _path: &Option<PathBuf>, vcd: &Option<String>) {
    let cwd = if let Some(cwd) = &args.cwd {
        std::fs::canonicalize(cwd).unwrap()
    } else {
        std::env::current_dir().unwrap()
    };

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
    let cwd = if let Some(cwd) = &args.cwd {
        std::fs::canonicalize(cwd).unwrap()
    } else {
        std::env::current_dir().unwrap()
    };

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

    let platform_name = match read_platform_from_toml(&cwd) {
        Ok(Some(p)) => p,
        Ok(None) => {
            eprintln!("Virdant.toml is missing [prog] platform");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    };

    let project = read_project_name(&cwd);
    let build_dir = cwd.join("build");
    let source_dir = cwd.join("src");

    let db = virdant::util::db_from_dir_with_platform(source_dir, &platform_name);
    dump_diagnostics(&db);
    if check_db(&db).is_err() {
        eprintln!("Build failed");
        std::process::exit(1);
    }

    std::fs::create_dir_all(&build_dir).unwrap();
    let verilog = virdant::verilog::conversion::convert_db_to_verilog(&db);
    verilog.write_in_dir(&build_dir).unwrap();
    println!("Wrote Verilog to {}", build_dir.to_string_lossy());

    let platform_pkg: PackageFqn = platform_name.clone().into();
    let pa = db.get_platform_analysis(platform_pkg.clone());
    if !pa.diagnostics.is_empty() {
        for d in &pa.diagnostics {
            eprintln!("{}", d.message());
        }
        eprintln!("platform analysis failed");
        std::process::exit(1);
    }

    let symboltable = db.get_symboltable();
    let top_id = resolve_top_module(&db, &symboltable)
        .unwrap_or_else(|e| {
            eprintln!("{e}");
            std::process::exit(1);
        });
    let top_symbol = symboltable.symbol(top_id);
    let top_name: String = top_symbol.name.to_string();

    let ports = db.get_ports_of(top_id);
    let port_names: Vec<&BStr> = ports.iter().map(|p| p.path.as_bstr()).collect();

    let fpga = pa.fpga.as_ref()
        .map(|f| f.to_string())
        .unwrap_or_else(|| {
            eprintln!("platform missing @fpga");
            std::process::exit(1);
        });
    let format = virdant::build::format_for(&fpga)
        .unwrap_or_else(|| {
            eprintln!("unknown FPGA family: {fpga}");
            std::process::exit(1);
        });
    let ext = virdant::build::extension_for(format);

    let constraints = virdant::build::emit_constraints(format, &pa, &port_names);
    let cpath = build_dir.join(format!("{}{}", pa.name, ext));
    std::fs::write(&cpath, &constraints).unwrap();

    for port in &port_names {
        if !pa.pins.iter().any(|p| p.port.as_bstr() == *port) {
            eprintln!(
                "warning: top port '{}' has no platform pin; left unconstrained",
                port
            );
        }
    }

    let toolchain = virdant::build::toolchain_for(&fpga)
        .unwrap_or_else(|| {
            eprintln!("unknown FPGA family: {fpga}");
            std::process::exit(1);
        });
    if let Err(e) = virdant::build::run_toolchain(
        &build_dir, &project, &top_name, &pa, toolchain, ext,
    ) {
        eprintln!("toolchain error: {e}");
        std::process::exit(1);
    }
}

fn resolve_cwd(args: &Args) -> PathBuf {
    if let Some(cwd) = &args.cwd {
        match std::fs::canonicalize(cwd) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("ERROR: cannot resolve directory {}: {e}", cwd.display());
                std::process::exit(1);
            }
        }
    } else {
        match std::env::current_dir() {
            Ok(p) => p,
            Err(e) => {
                eprintln!("ERROR: cannot determine current directory: {e}");
                std::process::exit(1);
            }
        }
    }
}

fn read_project_name(cwd: &Path) -> String {
    let text = std::fs::read_to_string(cwd.join("Virdant.toml")).unwrap();
    let v: toml::Value = toml::from_str(&text).unwrap();
    v["project"]["name"].as_str().unwrap().to_owned()
}

fn resolve_top_module(
    db: &Db,
    symboltable: &virdant::analysis::symbols::SymbolTable,
) -> Result<virdant::analysis::symbols::SymbolId, String> {
    use virdant::analysis::symbols::SymbolKind;
    use virdant::syntax::payload::AstNodePayload;
    let exported: Vec<_> = symboltable.items().into_iter()
        .filter(|s| s.kind == SymbolKind::ModDef)
        .filter(|s| {
            let parsing = db.get_parsing(s.location.package());
            let node = parsing.ast_node(s.location.ast_node_id());
            matches!(node.payload(), AstNodePayload::ModDef(m) if m.is_export)
        })
        .collect();
    if exported.len() != 1 {
        return Err(format!(
            "expected exactly one `export mod`, found {}",
            exported.len()
        ));
    }
    Ok(exported[0].id)
}

fn upload(args: &Args) {
    bitstream(args);

    let cwd = resolve_cwd(args);
    let platform_name = read_platform_from_toml(&cwd)
        .ok().flatten()
        .unwrap_or_else(|| {
            eprintln!("platform not set");
            std::process::exit(1);
        });
    let platform_pkg: PackageFqn = platform_name.clone().into();
    let db = virdant::util::db_from_dir_with_platform(cwd.join("src"), &platform_name);
    let pa = db.get_platform_analysis(platform_pkg);
    let fpga = pa.fpga.as_ref()
        .map(|f| f.to_string())
        .unwrap_or_else(|| {
            eprintln!("platform missing @fpga");
            std::process::exit(1);
        });
    let toolchain = virdant::build::toolchain_for(&fpga)
        .unwrap_or_else(|| {
            eprintln!("unknown FPGA family: {fpga}");
            std::process::exit(1);
        });
    let project = read_project_name(&cwd);
    if let Err(e) = virdant::build::flash_bitstream(&cwd.join("build"), &project, toolchain) {
        eprintln!("flash error: {e}");
        std::process::exit(1);
    }
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
