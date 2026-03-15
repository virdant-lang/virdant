use clap::CommandFactory;
use clap::{Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice};
use nix::unistd::execvp;
use virdant::analysis::Location;
use virdant::transpile::transpile;
use std::ffi::CString;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use virdant::fqn::PackageFqn;
use virdant::source::Source;
use virdant::syntax::parsing::parse;
use virdant::syntax::token::tokenize;
use virdant::syntax::token::Token;

/// The Virdant Hardware Language
#[derive(Parser, Debug)]
#[command(author, version, about, disable_help_subcommand = true)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Compile a Virdant source file
    Compile { args: Vec<String> },
}

fn parse_file(path: &str) {
    let path = Path::new(path);
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

fn tokenize_file(path: &str) {
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

fn main() {
    let args: Vec<String> = std::env::args().into_iter().skip(1).collect();

    if args.len() == 0 {
        Args::command().print_help().unwrap();
        std::process::exit(3);
    }
    let command = &args[0];

    if command == "parse" {
        let path = match args.get(1) {
            Some(path) => path,
            None => {
                eprintln!("ERROR");
                eprintln!("usage: vir parse <file>");
                std::process::exit(3);
            }
        };

        parse_file(path);

        return;
    }
    if command == "tokenize" {
        let path = match args.get(1) {
            Some(path) => path,
            None => {
                eprintln!("ERROR");
                eprintln!("usage: vir tokenize <file>");
                std::process::exit(3);
            }
        };

        tokenize_file(path);

        return;
    }
    if command == "check" {
        let path = match args.get(1) {
            Some(path) => path,
            None => {
                eprintln!("ERROR");
                eprintln!("usage: vir check <file>");
                std::process::exit(3);
            }
        };

        let path = args.get(1).unwrap();
        virdant::Vir::check_all(path);

        return;
    }

    if command == "db" {
        let path = args.get(1).unwrap();
        let outpath = args.get(2).unwrap();

        let path = args.get(1).unwrap();
        let mut vir = virdant::Vir::from_dir(path);
        vir.check();
        vir.db().save_graphviz(outpath);

        return;
    }

    if command == "components" {
        let path = args.get(1).unwrap();
        let moddef_fqn = args.get(2).unwrap();

        let mut vir = virdant::Vir::from_dir(path);
        vir.dump_diagnostics();

        let symboltable = vir.db().get_symboltable();
        let moddef = symboltable.resolve_item_fqn(moddef_fqn.as_bytes().as_bstr()).unwrap();
        let component_analysis = vir.db().get_component_analysis(moddef.id());
        dbg!(&component_analysis);

        return;
    }

    if command == "symbols" {
        let path = args.get(1).unwrap();

        let mut vir = virdant::Vir::from_dir(path);
        vir.dump_diagnostics();

        let symboltable = vir.db().get_symboltable();
        for symbol in symboltable.symbols() {
            println!("{:?} {:<20} {:?}", symbol.id(), symbol.fqn(), symbol.kind());
        }

        return;
    }

    if command == "typedefs" {
        let path = args.get(1).unwrap();

        let mut vir = virdant::Vir::from_dir(path);
        vir.dump_diagnostics();

        let typedefs = vir.db().get_typedefs();
        for typedef in typedefs {
            println!("{typedef:?}");
        }

        return;
    }

    if command == "exprroots" {
        let path = args.get(1).unwrap();
        let mut vir = virdant::Vir::from_dir(path);
        vir.dump_diagnostics();

        for exprroot in vir.db().get_exprroots() {
            let package = exprroot.location().package();
            let parsing = vir.db().get_parsing(package.clone());
            let location = exprroot.location();
            let typ = vir.db().get_expected_type(location.clone());
            let node = parsing.ast_node(location.ast_node_id());
            let region = node.region();
            println!("{location:?} : {typ:?} at @{region}");
        }

        return;
    }

    if command == "typing" {
        let path = args.get(1).unwrap();
        let package = args.get(2).unwrap();
        let ast_node_id: u16 = args.get(3).unwrap().parse().unwrap();

        let mut vir = virdant::Vir::from_dir(path);
        vir.dump_diagnostics();

        let location = Location::new(PackageFqn::new(package.to_string().into()), virdant::syntax::ast::AstNodeId(ast_node_id));

        for exprroot in vir.db().get_exprroots() {
            if exprroot.location() == location {
                let typing = vir.db().get_typing(exprroot.clone());
                let package = exprroot.location().package();
                let parsing = vir.db().get_parsing(package.clone());
                let location = exprroot.location();
                let typ = vir.db().get_expected_type(location.clone());
                let node = parsing.ast_node(location.ast_node_id());
                let region = node.region();
                dbg!(&typing);
                break;
            }
        }

        return;
    }

    if command == "compile-to-virir" {
        let path = args.get(1).unwrap();
        let outfilepath = args.get(2).unwrap();

        let mut vir = virdant::Vir::from_dir(path);
        vir.dump_diagnostics();

        let virir = &transpile(&vir.db());
        dbg!(&virir);

        std::fs::write(outfilepath, virir.to_text());

        return;
    }

    let bin_path = std::env::current_exe().unwrap();
    let bin_dir = bin_path.parent().unwrap();
    let bin = bin_dir.join(format!("vir-{}", command));

    match std::fs::exists(&bin) {
        Ok(true) => {
            let program = CString::new(bin.to_str().unwrap()).unwrap();
            let c_args: Vec<CString> = args
                .into_iter()
                .map(|s| CString::new(s.as_str()).unwrap())
                .collect();

            let _ = execvp(&program, &c_args);
        }
        Ok(false) => {
            Args::command().print_help().unwrap();
            std::process::exit(3);
        }
        Err(e) => {
            eprintln!("ERROR");
            eprintln!("{e:?}");
            std::process::exit(-1);
        }
    }
}
