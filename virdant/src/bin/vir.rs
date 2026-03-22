use clap::CommandFactory;
use virdant::conversion::convert_virir_to_verilog;
use clap::{Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice, ByteVec};
use nix::unistd::execvp;
use virdant::diagnostics::DiagnosticLevel;
use std::ffi::{CString, OsString};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use virdant::fqn::PackageFqn;
use virdant::source::Source;
use virdant::syntax::parsing::parse;
use virdant::syntax::token::tokenize;
use virdant::syntax::token::Token;
use virdant::transpile::transpile;

/// The Virdant Hardware Language
#[derive(Parser, Debug)]
#[command(name = "vir", author, version, about, disable_help_subcommand = true, arg_required_else_help = true)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Parse and dump the AST for a Virdant source file
    Parse { file: PathBuf },
    /// Tokenize a Virdant source file
    Tokenize { file: PathBuf },
    /// Typecheck all packages in a directory
    Check { path: PathBuf },
    /// Dump inferred expression types
    Types { path: PathBuf },
    /// Dump the symbol table
    Symbols { path: PathBuf },
    /// Dump typedefs
    Typedefs { path: PathBuf },
    /// Dump expression roots
    Exprroots { path: PathBuf },
    /// Dump typing results
    Typing { path: PathBuf },
    /// Dump database state, optionally saving a Graphviz file
    Db { path: PathBuf, outpath: Option<PathBuf> },
    /// Dump component analysis for a module definition
    Components { path: PathBuf, moddef_fqn: String },
    /// Compile a Virdant package directory to VirIr
    CompileToVirir { path: PathBuf, outfilepath: PathBuf },
    /// Run Virdant package
    Run { path: PathBuf, #[arg(long)] vcd: Option<String> },

    #[command(external_subcommand)]
    External(Vec<OsString>),
}

fn main() {
    let args = Args::parse();

    match args.command {
        Command::Parse { file } => parse_file(&file),
        Command::Tokenize { file } => tokenize_file(&file),
        Command::Check { path } => check_path(path),
        Command::Db { path, outpath } => dump_db(path, outpath),
        Command::Types { path } => dump_types(path),
        Command::Components { path, moddef_fqn } => dump_components(path, &moddef_fqn),
        Command::Symbols { path } => dump_symbols(path),
        Command::Typedefs { path } => dump_typedefs(path),
        Command::Exprroots { path } => dump_exprroots(path),
        Command::Typing { path } => dump_typing(path),
        Command::CompileToVirir { path, outfilepath } => compile_to_virir(path, outfilepath),
        Command::Run { path, vcd } => run(path, vcd),
        Command::External(args) => exec_external(args),
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

fn check_path(path: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    match vir.check() {
        Err(diags) => {
            vir.dump_diagnostics();
            eprintln!("Check failed");
            std::process::exit(1);
        }
        Ok(diags) => {
            vir.dump_diagnostics();
            eprintln!("Check OK");
        }
    };
}

fn dump_db(path: PathBuf, outpath: Option<PathBuf>) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.check();
    if let Some(outpath) = outpath {
        println!("Saving graphviz: {}", outpath.display());
        vir.db().save_graphviz(outpath);
    }
    dbg!(vir.db());
}

fn dump_types(path: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.check();
    for (location, typ) in vir.db().get_typeof_all() {
        println!(
            "{location:?} has type {typ:?}   {:?}  {:?}",
            vir.spelling(location.clone()),
            vir.region(location.clone())
        );
    }
}

fn dump_components(path: PathBuf, moddef_fqn: &str) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.dump_diagnostics();

    let symboltable = vir.db().get_symboltable();
    let moddef = symboltable.resolve_item_fqn(moddef_fqn.as_bytes().as_bstr()).unwrap();
    let component_analysis = vir.db().get_component_analysis(moddef.id());
    dbg!(&component_analysis);
}

fn dump_symbols(path: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.dump_diagnostics();

    let symboltable = vir.db().get_symboltable();
    for symbol in symboltable.symbols() {
        println!("{:?} {:<20} {:?}", symbol.id(), symbol.fqn(), symbol.kind());
    }
}

fn dump_typedefs(path: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.dump_diagnostics();

    let typedefs = vir.db().get_typedefs();
    for typedef in typedefs {
        println!("{typedef:?}");
    }
}

fn dump_exprroots(path: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.dump_diagnostics();

    for exprroot in vir.db().get_exprroots() {
        let package = exprroot.location().package();
        let parsing = vir.db().get_parsing(package.clone());
        let location = exprroot.location();
        let typ = vir.db().get_expected_type(exprroot);
        let node = parsing.ast_node(location.ast_node_id());
        let region = node.region();
        println!("{location:?} : {typ:?} at @{region}");
    }
}

fn dump_typing(path: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.dump_diagnostics();

    for (location, typ) in vir.db().get_typeof_all() {
        println!("{location:?} {typ:?}");
    }
}

fn compile_to_virir(path: PathBuf, outfilepath: PathBuf) {
    let mut vir = virdant::Vir::from_dir(path);
    vir.dump_diagnostics();

    let virir = &transpile(&vir.db());

    std::fs::write(outfilepath, virir.to_text()).unwrap();
}

fn run(path: PathBuf, vcd: Option<String>) {
    let project = path.file_name().unwrap().to_string_lossy().to_owned();
    let builddir = PathBuf::from("build").join(project.as_ref());

    let mut vir = virdant::Vir::from_dir(path.clone());
    vir.dump_diagnostics();

    let virir = &transpile(&vir.db());
    std::fs::create_dir_all(&builddir).unwrap();
    let virir_filepath = builddir.join(format!("{project}.virir"));

    std::fs::write(virir_filepath, virir.to_text()).unwrap();

    let virir = virdant::virir::parse(&virir.to_text()).unwrap();
    let verilog = convert_virir_to_verilog(virir);

    verilog.write_in_dir(&builddir).unwrap();

    let bin_name = path.file_stem().unwrap().to_string_lossy().to_string();
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
        use bstr::ByteSlice;
        eprintln!("iverilog: {}", BStr::new(&output.stderr));
        std::process::exit(1);
    }

    let program = CString::new(bin.clone()).unwrap();
    let mut args: Vec<CString> = vec![
        CString::new(bin).unwrap(),
    ];
    if let Some(vcd) = vcd {
        args.push(CString::new(format!("+vcd={vcd}")).unwrap());
    }

    let _ = execvp(&program, &args);
}

fn glob_sv_files(dir: &Path) -> Vec<PathBuf> {
    let mut sources: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap()
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
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
