use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use bstr::ByteSlice;
use bstr::BStr;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    project: String,
}

pub fn main() {
    let args = Args::parse();
    let name = &args.project;

    if !is_identifier(name) {
        eprintln!("Project name was given as {} but needs to be a C-style identifier", name);
        std::process::exit(1);
    }

    let project_dir = PathBuf::from(&args.project);
    match std::fs::exists(&project_dir) {
        Ok(true) => {
            eprintln!("Directory {} already exists", &args.project);
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("Error: {e:?}");
            std::process::exit(1);
        }
        Ok(false) => (),
    }

    std::fs::create_dir(&project_dir).unwrap_or_else(|e| {
        eprintln!("Error: {e:?}");
        std::process::exit(1);
    });

    macro_rules! make_template_file {
        ($name:literal) => {
            let filepath = project_dir.join($name);

            std::fs::create_dir_all(filepath.parent().unwrap())
                .unwrap_or_else(|e| {
                    eprintln!(concat!("Error creating ", $name, " directory"));
                    eprintln!("{e:?}");
                    std::process::exit(1);
                });

            let mut file = File::create(&filepath)
                .unwrap_or_else(|e| {
                    eprintln!(concat!("Error creating ", $name));
                    eprintln!("{e:?}");
                    std::process::exit(1);
                });

            #[rustfmt::skip]
            let text = BStr::new(include_bytes!(concat!("../templates/", $name)));
            let replaced_text = text.replace(b"__NAME__", &args.project);
            Write::write_all(&mut file, &replaced_text)
                .unwrap_or_else(|e| {
                    eprintln!(concat!("Error creating (2) ", $name));
                    eprintln!("Error: {e:?}");
                    std::process::exit(1);
                });
        }
    }

    make_template_file!("src/top.vir");
    make_template_file!("tb/testbench.v");
    make_template_file!("Makefile");
    make_template_file!("virdant.toml");
}

fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();

    // Must start with a letter or underscore
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }

    // Remaining chars can be alphanumeric or underscore
    for c in chars {
        if !(c.is_ascii_alphanumeric() || c == '_') {
            return false;
        }
    }

    true
}
