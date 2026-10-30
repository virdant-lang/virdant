//! The `vir-doc` binary: HTML documentation generation for Virdant
//! projects (formerly the `doc` subcommand of `vir`).
//!
//! `vir doc` execs this binary via the `vir-*` companion-binary
//! mechanism; see `vir/src/vir/cli.rs`.

#[path = "../common/help_json.rs"]
mod help_json;

// Shared helpers, compiled once as the `vircommon` library.
use vircommon as common;
mod docs;

use clap::ArgAction;

use common::ProjectLocation;

fn command() -> clap::Command {
    let mut cmd = clap::Command::new("vir-doc")
        .about("Generate HTML documentation")
        .arg(
            clap::Arg::new("open")
                .long("open")
                .action(ArgAction::SetTrue)
                .help("Open the docs in a browser after generation"),
        );
    common::add_project_args(&mut cmd, false);
    cmd
}

fn main() {
    help_json::maybe_dump(command);

    let matches = command().get_matches();
    let location = ProjectLocation::from_matches(&matches);
    let open = matches.get_flag("open");

    doc(&location, open);
}

fn doc(location: &ProjectLocation, open: bool) {
    let cwd = location.resolve_cwd();

    if !cwd.join("Virdant.toml").exists() {
        eprintln!("No Virdant.toml found");
        std::process::exit(1);
    }

    let source_dir = cwd.join("src");
    if !source_dir.is_dir() {
        eprintln!("ERROR: source directory not found: {}", source_dir.display());
        std::process::exit(1);
    }

    let db = virdant::util::db_from_dir(source_dir);
    common::dump_diagnostics(&db);
    if virdant::util::check_db(&db).is_err() {
        eprintln!("Doc generation failed due to errors");
        std::process::exit(1);
    }

    let out_dir = cwd.join("build").join("doc");
    match docs::generate_docs(&db, &out_dir) {
        Ok(()) => {
            println!("Wrote docs to {}", out_dir.to_string_lossy());
            if open {
                open_in_browser(&out_dir.join("index.html"));
            }
        }
        Err(e) => {
            eprintln!("Doc generation error: {e}");
            std::process::exit(1);
        }
    }
}

/// Open a file or URL in the default OS browser.
fn open_in_browser(path: &std::path::Path) {
    let result = if cfg!(target_os = "macos") {
        std::process::Command::new("open").arg(path).status()
    } else if cfg!(target_os = "linux") {
        std::process::Command::new("xdg-open").arg(path).status()
    } else if cfg!(target_os = "windows") {
        std::process::Command::new("cmd")
            .arg("/c")
            .arg("start")
            .arg(path)
            .status()
    } else {
        eprintln!("Cannot open browser on this platform");
        return;
    };

    match result {
        Ok(status) if status.success() => {}
        Ok(status) => {
            eprintln!("Failed to open browser: process exited with {}", status);
        }
        Err(e) => {
            eprintln!("Failed to open browser: {e}");
        }
    }
}
