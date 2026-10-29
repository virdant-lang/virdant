use std::path::Path;
use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(about = "The Virdant tutorial helper")]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Create a new tutorial workspace
    New,
    /// Check the current lesson
    Check,
}

fn main() {
    let args = Args::parse();
    match args.command {
        Command::New => new(),
        Command::Check => check(),
    }
}

fn tutorial_dir() -> PathBuf {
    let exe = std::env::current_exe().expect("Could not determine current executable");
    let root = exe.parent().unwrap().parent().unwrap();
    let tutorial = root.join("tutorial");
    std::fs::canonicalize(&tutorial).unwrap_or_else(|_| {
        panic!("Could not find tutorial directory {tutorial:?}")
    })
}

fn vir_binary() -> PathBuf {
    let exe = std::env::current_exe().expect("Could not determine current executable");
    let sibling = exe.parent().unwrap().join("vir");
    if sibling.exists() {
        sibling
    } else {
        PathBuf::from("vir")
    }
}

/// Parse a lesson directory name of the form `NNN_name`.
fn parse_lesson_name(name: &str) -> Option<(u32, String)> {
    let (num, rest) = name.split_once('_')?;
    let num = num.parse::<u32>().ok()?;
    if rest.is_empty() {
        None
    } else {
        Some((num, rest.to_string()))
    }
}

/// List the available lessons in `dir`, sorted by lesson number.
fn list_lessons(dir: &Path) -> Vec<(u32, String)> {
    let mut lessons = Vec::new();
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            if entry.file_type().map(|ty| ty.is_dir()).unwrap_or(false) {
                let name = entry.file_name();
                let name = name.to_string_lossy();
                if let Some(lesson) = parse_lesson_name(&name) {
                    lessons.push(lesson);
                }
            }
        }
    }
    lessons.sort_by_key(|(num, _)| *num);
    lessons
}

fn new() {
    let dest = PathBuf::from("tutorial");
    if dest.exists() {
        eprintln!("Error: tutorial/ already exists");
        std::process::exit(1);
    }

    let tutorial_dir = tutorial_dir();
    let lessons = list_lessons(&tutorial_dir);
    let Some((num, name)) = lessons.first() else {
        panic!("Could not find any lessons in {tutorial_dir:?}");
    };
    let first_dirname = format!("{num:03}_{name}");

    copy_dir(&tutorial_dir.join(&first_dirname), &dest.join(&first_dirname));
    println!("tutorial/ created");
}

fn check() {
    let cwd = std::env::current_dir().expect("Could not determine current directory");
    let Some(lesson_name) = cwd.file_name().map(|name| name.to_string_lossy().into_owned()) else {
        eprintln!("Error: not inside a tutorial lesson directory");
        std::process::exit(1);
    };
    let Some((lesson_num, _)) = parse_lesson_name(&lesson_name) else {
        eprintln!("Error: not inside a tutorial lesson directory");
        std::process::exit(1);
    };
    if !cwd.parent().map(|parent| parent.file_name() == Some(std::ffi::OsStr::new("tutorial"))).unwrap_or(false) {
        eprintln!("Error: not inside a tutorial lesson directory");
        std::process::exit(1);
    }

    let output = std::process::Command::new(vir_binary())
        .arg("check")
        .output()
        .unwrap_or_else(|_| panic!("Could not run {}", vir_binary().display()));
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    let has_diagnostics = stdout
        .lines()
        .any(|line| line.starts_with("ERROR") || line.starts_with("WARNING"));
    if !output.status.success() || has_diagnostics {
        println!("Failed...");
        print!("{}", stdout);
        eprint!("{}", stderr);
        std::process::exit(1);
    }

    println!("PASSED!");

    let local_tutorial = cwd.parent().unwrap();
    let existing_lessons = list_lessons(local_tutorial);
    let latest_num = existing_lessons.last().map(|(num, _)| *num).unwrap_or(lesson_num);
    if lesson_num < latest_num {
        // This lesson was already passed and unlocked its successor.
        return;
    }

    // The current lesson is the latest one, so a pass unlocks the next lesson.
    let tutorial_dir = tutorial_dir();
    let next = list_lessons(&tutorial_dir)
        .into_iter()
        .find(|(num, _)| *num > lesson_num);
    match next {
        Some((next_num, next_name)) => {
            let next_dirname = format!("{next_num:03}_{next_name}");
            copy_dir(&tutorial_dir.join(&next_dirname), &local_tutorial.join(&next_dirname));
            println!("You've unlocked the next lesson: {next_dirname}");
        }
        None => {
            println!("You've completed the tutorial!");
        }
    }
}

fn copy_dir(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).unwrap_or_else(|_| panic!("Could not create {dst:?}"));
    for entry in std::fs::read_dir(src).unwrap_or_else(|_| panic!("Could not read {src:?}")) {
        let entry = entry.unwrap();
        let to = dst.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_dir(&entry.path(), &to);
        } else {
            std::fs::copy(entry.path(), &to).unwrap_or_else(|_| panic!("Could not copy {:?} to {to:?}", entry.path()));
        }
    }
}
