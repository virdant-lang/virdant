use clap::CommandFactory;
use clap::{Parser, Subcommand};

use std::ffi::CString;
use nix::unistd::execvp;

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

fn main() {
    let args: Vec<String> = std::env::args().into_iter().skip(1).collect();

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
