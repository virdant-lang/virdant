use clap::CommandFactory;
use clap::{Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice};
use nix::unistd::execvp;
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
