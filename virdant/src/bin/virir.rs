use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Parser;

use virdant::conversion::convert_virir_to_verilog;
use virdant::virir;

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

fn main() -> Result<()> {
    let args = Args::parse();
    let text = std::fs::read_to_string(&args.input)
        .with_context(|| format!("failed to read {}", args.input.display()))?;

    let virir = virir::parse(&text)
        .map_err(anyhow::Error::msg)
        .with_context(|| format!("failed to parse {}", args.input.display()))?;
    let verilog = convert_virir_to_verilog(virir);

    if let Some(builddir) = args.builddir {
        std::fs::create_dir_all(&builddir)
            .with_context(|| format!("failed to create {}", builddir.display()))?;
        verilog
            .write_in_dir(&builddir)
            .with_context(|| format!("failed to write Verilog to {}", builddir.display()))?;
    } else {
        verilog
            .write_to_stdout()
            .context("failed to write Verilog to stdout")?;
    }

    Ok(())
}