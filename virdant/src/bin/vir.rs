use clap::CommandFactory;
use clap::{Parser, Subcommand};

use bstr::{BStr, ByteSlice};
use nix::unistd::execvp;
use std::ffi::CString;
use virdant::token::tokenize;

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
                    virdant::token::Token::Ident |
                    virdant::token::Token::Nat |
                    virdant::token::Token::Word |
                    virdant::token::Token::Error => BStr::new(&input[start..end]),
                    _ => BStr::new(""),
                };
                println!("{i:>5} {loc:>10}   {token_str:>15}      {snippet}");
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
