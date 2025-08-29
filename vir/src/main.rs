use clap::CommandFactory;
use clap::{Parser, Subcommand};
use nix::unistd::execvp;

use std::ffi::CString;

/// The Virdant Hardware Language
#[derive(Parser, Debug)]
#[command(author, version, about, disable_help_subcommand = true)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    #[doc(hidden)]
    args: Vec<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Compile a Virdant source file
    Compile { source: String },
}

fn main() {
    let parse_args = Args::parse();
    let mut args: Vec<String> = vec![];

    match parse_args.command {
        Some(Command::Compile { source } ) => {
            args.push("compile".to_string());
            args.push(source);
        }
        None => {
            args.extend(parse_args.args.into_iter());
        }
    }

    if args.is_empty() {
        Args::command().print_help().unwrap();
        std::process::exit(2);
    }

    let command = &args[0];

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

            execvp(&program, &c_args).unwrap();
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
