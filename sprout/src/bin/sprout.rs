use clap::CommandFactory;
use clap::{Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice};
use colored::Colorize;
use nix::unistd::execvp;
use virdant::util::{check_db, db_from_dir};
use std::ffi::{CString, OsString};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use virdant::db::Db;
use virdant::diagnostics::DiagnosticLevel;
use virdant::fqn::PackageFqn;
use virdant::common::{Flow, source::{Region, Source}};
use virdant::analysis::symbols::SymbolKind;
use virdant::syntax::parsing::parse;
use virdant::syntax::token::tokenize;
use virdant::syntax::token::Token;

/// The Virdant Hardware Language
#[derive(Parser, Debug)]
#[command(name = "vir", author, version, about, disable_help_subcommand = true, arg_required_else_help = true)]
struct Args {
    #[arg(short = 'C')]
    cwd: Option<PathBuf>,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /*
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
    /// Dump the elaborated design rooted at a top module
    Elaborate { top: String },
    /// Compile Virdant package
    Compile { path: PathBuf },
    /// Build Virdant package
    Build { },
    /// Run Virdant package
    Run { path: Option<PathBuf>, #[arg(long)] vcd: Option<String> },
    /// Create a new Virdant project
    New { project: String },
    /// Synthesize, place-and-route, and pack a bitstream
    Bitstream { },
    /// Synthesize, place-and-route, pack, and upload a bitstream
    Upload { },

    #[command(external_subcommand)]
    External(Vec<OsString>),
    */
}

fn main() {
    let args = Args::parse();

    match args.command {
//        Command::Parse { file } => parse_file(&file),
//        Command::Tokenize { file } => tokenize_file(&file),
//        Command::Check { } => check(&args),
//        Command::Db { ref outpath } => dump_db(&args, outpath.clone()),
//        Command::Types { } => dump_types(&args),
//        Command::Components { ref moddef_fqn } => dump_components(&args, moddef_fqn),
//        Command::Elaborate { ref top } => elaborate(&args, top),
//        Command::Symbols { } => dump_symbols(&args),
//        Command::Typedefs { } => dump_typedefs(&args),
//        Command::Exprroots { } => dump_exprroots(&args),
//        Command::Typing { } => dump_typing(&args),
//        Command::Compile { path } => compile(path),
//        Command::Build { } => build(&args),
//        Command::Run { ref path, ref vcd } => run(&args, path, vcd),
//        Command::New { ref project } => new_project(&args, project),
//        Command::Bitstream { } => bitstream(&args),
//        Command::Upload { } => upload(&args),
//        Command::External(args) => exec_external(args),
    }
}

/*
fn project_db(args: &Args) -> Db {
    let cwd = if let Some(cwd) = &args.cwd {
        std::fs::canonicalize(cwd).unwrap()
    } else {
        std::env::current_dir().unwrap()
    };

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    db_from_dir(cwd.join("src"))
}

fn dump_diagnostics(db: &Db) {
    let diagnostics = match check_db(db) {
        Ok(diags) => diags,
        Err(diags) => diags,
    };
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
    let cwd = if let Some(cwd) = &args.cwd {
        std::fs::canonicalize(cwd).unwrap()
    } else {
        std::env::current_dir().unwrap()
    };

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let source_dir = cwd.join("src");
    let db = db_from_dir(source_dir);
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
    for typedef in typedefs {
        println!("{typedef:?}");
    }
}

fn dump_exprroots(args: &Args) {
    let db = project_db(args);
    dump_diagnostics(&db);

    for exprroot in db.get_exprroots() {
        let package = exprroot.location().package();
        let parsing = db.get_parsing(package.clone());
        let location = exprroot.location();
        let typ = db.get_expected_type(exprroot);
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

fn build(args: &Args) {
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
    let _ = virtant_toml["project"]["name"].as_str().unwrap();
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

fn run(args: &Args, _path: &Option<PathBuf>, vcd: &Option<String>) {
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

    let top_vir_content = "export mod Top {\n    incoming clock : Clock;\n}\n";
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
    let virdant_toml: toml::Value = toml::from_str(&virdant_toml_text).unwrap();

    let project = virdant_toml["project"]["name"].as_str().unwrap().to_owned();

    let platform = virdant_toml
        .get("prog")
        .and_then(|p| p.get("platform"))
        .and_then(|p| p.as_str())
        .unwrap_or_else(|| {
            eprintln!("Virdant.toml is missing [prog] platform");
            std::process::exit(1);
        });
    assert_eq!(platform, "icesugar", "Only 'icesugar' platform is supported");

    let builddir = cwd.join("build");

    // Build Verilog
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

    // Write yosys script
    let sv_files = glob_sv_files(&builddir);
    let sv_list: Vec<String> = sv_files.iter()
        .map(|p| p.to_string_lossy().into_owned())
        .collect();
    let project_json = builddir.join(format!("{project}.json"));
    let script_content = format!(
        "read_verilog -I {} {}; synth_ice40 -top Top -json {}",
        builddir.to_string_lossy(),
        sv_list.join(" "),
        project_json.to_string_lossy(),
    );
    let script_path = builddir.join("script.ys");
    std::fs::write(&script_path, &script_content).unwrap();

    // Run yosys
    let yosys_log = builddir.join("yosys.log");
    let output = std::process::Command::new("yosys")
        .arg("-l").arg(&yosys_log)
        .arg(&script_path)
        .output().unwrap();
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        eprintln!("yosys failed");
        std::process::exit(1);
    }
    println!("{}", BStr::new(&output.stdout));
    println!("yosys OK");

    // Run nextpnr-ice40
    create_pcf_file(&db, "Top".into(), &builddir);
    let pcf = builddir.join("icesugar.pcf");
    let project_asc = builddir.join(format!("{project}.asc"));
    let output = std::process::Command::new("nextpnr-ice40")
        .arg("--up5k")
        .arg("--top").arg("Top")
        .arg("--json").arg(&project_json)
        .arg("--pcf").arg(&pcf)
        .arg("--asc").arg(&project_asc)
        .arg("--package").arg("sg48")
        .output().unwrap();
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        eprintln!("nextpnr-ice40 failed");
        std::process::exit(1);
    }
    println!("{}", BStr::new(&output.stdout));
    println!("nextpnr-ice40 OK");

    // Run icepack
    let project_bin = builddir.join(format!("{project}.bin"));
    let output = std::process::Command::new("icepack")
        .arg("-s")
        .arg(&project_asc)
        .arg(&project_bin)
        .output().unwrap();
    if !output.status.success() {
        eprintln!("{}", BStr::new(&output.stderr));
        eprintln!("icepack failed");
        std::process::exit(1);
    }
    println!("{}", BStr::new(&output.stdout));
    println!("Bitstream written to {}", project_bin.to_string_lossy());
}

fn create_pcf_file(db: &Db, top: &BStr, builddir: &PathBuf) {
    const ICESGUAR_PORTS: [(&'static str, usize); 45] = [
        ("clock", 35),
        ("led_red", 39),
        ("led_blue", 40),
        ("led_green", 41),
        ("switch0", 18),
        ("switch1", 19),
        ("switch2", 20),
        ("switch3", 21),
        ("uart_rx", 4),
        ("uart_tx", 6),
        ("USB_DP", 10),
        ("USB_DN", 9),
        ("USB_PULLUP", 11),
        ("P1_1", 10),
        ("P1_2", 6),
        ("P1_3", 3),
        ("P1_4", 48),
        ("P1_9", 47),
        ("P1_10", 2),
        ("P1_11", 4),
        ("P1_12", 9),
        ("P2_1", 46),
        ("P2_2", 44),
        ("P2_3", 42),
        ("P2_4", 37),
        ("P2_9", 36),
        ("P2_10", 38),
        ("P2_11", 43),
        ("P2_12", 45),
        ("P3_1", 34),
        ("P3_2", 31),
        ("P3_3", 27),
        ("P3_4", 25),
        ("P3_9", 23),
        ("P3_10", 26),
        ("P3_11", 28),
        ("P3_12", 32),
        ("P4_1", 21),
        ("P4_2", 20),
        ("P4_3", 19),
        ("P4_4", 18),
        ("spi_cs", 16),
        ("spi_clk", 15),
        ("spi_do", 14),
        ("spi_di", 17),
    ];
    // Collect the port names present on the top module
    let symboltable = db.get_symboltable();
    let top_symbol = symboltable.items()
        .into_iter()
        .find(|sym| sym.name.as_bstr() == top && sym.kind == SymbolKind::ModDef)
        .unwrap_or_else(|| panic!("module '{}' not found", top));
    let component_analysis = db.get_component_analysis(top_symbol.id);
    let port_names: indexmap::IndexSet<String> = component_analysis.components()
        .into_iter()
        .filter(|(path, component)| !path.contains(&b'.') && component.flow() != Flow::Duplex)
        .map(|(path, _)| path.to_str_lossy().into_owned())
        .collect();

    // Emit only the ICESUGAR_PORTS entries that exist on the top module
    let mut pcf_content = String::new();
    for (name, pin) in ICESGUAR_PORTS.iter() {
        if port_names.contains(*name) {
            pcf_content.push_str(&format!("set_io {name} {pin}\n"));
        }
    }
    std::fs::write(builddir.join("icesugar.pcf"), pcf_content).unwrap();
}

fn upload(args: &Args) {
    bitstream(args);

    let cwd = if let Some(cwd) = &args.cwd {
        std::fs::canonicalize(cwd).unwrap()
    } else {
        std::env::current_dir().unwrap()
    };

    let virdant_toml_text = std::fs::read_to_string(cwd.join("Virdant.toml")).unwrap();
    let virdant_toml: toml::Value = toml::from_str(&virdant_toml_text).unwrap();
    let project = virdant_toml["project"]["name"].as_str().unwrap().to_owned();

    let builddir = cwd.join("build");
    let project_bin = builddir.join(format!("{project}.bin"));

    let output = std::process::Command::new("icesprog")
        .arg(&project_bin)
        .output().unwrap();
    if !output.status.success() {
        eprintln!("icesprog failed");
        eprintln!("{}", BStr::new(&output.stderr));
        std::process::exit(1);
    }
    println!("Uploaded {}", project_bin.to_string_lossy());
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
*/
