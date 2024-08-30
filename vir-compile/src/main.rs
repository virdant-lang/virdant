use clap::Parser;
use virdant::Virdant;

#[derive(Parser)]
#[command(name = "vir compile")]
#[command(about = "A simple example CLI", long_about = None)]
struct Args {
    filename: std::path::PathBuf,
}

fn main() {
    let args = Args::parse();

    let filepath = &args.filename;

    let mut virdant = Virdant::new(&filepath);
    match virdant.check() {
        Ok(design) => {
            design.verilog("build").unwrap();
            println!("Build complete: See build/");
        },
        Err(errs) => {
            for err in errs.iter() {
                eprintln!("{err}");
            }
            std::process::exit(1);
        }
    }
}
