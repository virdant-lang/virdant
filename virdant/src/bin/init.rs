use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    project: String,
}

pub fn main() {
    let args = Args::parse();

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

    let src_dir = project_dir.join("src");
    std::fs::create_dir(&src_dir).unwrap_or_else(|e| {
        eprintln!("Error: {e:?}");
        std::process::exit(1);
    });

    let mut top_vir = File::create(src_dir.join("top.vir"))
        .unwrap_or_else(|e| {
            eprintln!("Error: {e:?}");
            std::process::exit(1);
        });

    #[rustfmt::skip]
    Write::write_all(&mut top_vir,
b"mod Top {
    incoming clock : Clock;
    incoming inp : Word[8];
    incoming out : Word[8];

    reg buf : Word[8] on clock;
    buf <= inpt;
    out := buf;
}
")
        .unwrap_or_else(|e| {
            eprintln!("Error: {e:?}");
            std::process::exit(1);
        });

    let mut makefile_vir = File::create(project_dir.join("Makefile"))
        .unwrap_or_else(|e| {
            eprintln!("Error: {e:?}");
            std::process::exit(1);
        });

    #[rustfmt::skip]
    Write::write_all(&mut makefile_vir,
b"
all: build/top.v

build/top.v: src/top.vir
	vir compile src/top.vir

clean:
	rm -rf build/

.PHONY: all clean
")
        .unwrap_or_else(|e| {
            eprintln!("Error: {e:?}");
            std::process::exit(1);
        });
}
