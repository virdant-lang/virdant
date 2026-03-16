use std::path::PathBuf;

use clap::Parser;

use virdant::conversion::convert_virir_to_verilog;
use virdant::virir;

type MainResult<T> = Result<T, Box<dyn std::error::Error>>;

/// Parse VirIr and emit Verilog.
#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Path to the .virir file to parse.
    input: PathBuf,

    /// Directory where generated Verilog files should be written.
    #[arg(long)]
    builddir: Option<PathBuf>,
}

fn main() -> MainResult<()> {
    let args = Args::parse();
    let text = std::fs::read_to_string(&args.input)
        .map_err(|err| std::io::Error::other(format!("failed to read {}: {err}", args.input.display())))?;

    let virir = virir::parse(&text)
        .map_err(|err| std::io::Error::other(format!("failed to parse {}: {err}", args.input.display())))?;
    let verilog = convert_virir_to_verilog(virir);

    if let Some(builddir) = args.builddir {
        std::fs::create_dir_all(&builddir)
            .map_err(|err| std::io::Error::other(format!("failed to create {}: {err}", builddir.display())))?;
        verilog
            .write_in_dir(&builddir)
            .map_err(|err| std::io::Error::other(format!("failed to write Verilog to {}: {err}", builddir.display())))?;
    } else {
        verilog
            .write_to_stdout()
            .map_err(|err| std::io::Error::other(format!("failed to write Verilog to stdout: {err}")))?;
    }

    Ok(())
}