use std::path::Path;
use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(about = "The Virdant tutorial helper")]
struct Args {
    /// Suppress Git initialization for a freshly created workspace.
    #[arg(long)]
    no_git: bool,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(clap::Subcommand)]
enum Command {
    /// Check the current lesson
    Check,
}

fn main() {
    let args = Args::parse();
    match args.command {
        None => new_workspace(args.no_git),
        Some(Command::Check) => check(),
    }
}

fn tutorial_dir() -> PathBuf {
    let exe = std::env::current_exe().expect("Could not determine current executable");
    // Walk up from the executable's directory looking for the bundled
    // tutorial.  It lives at `assets/tutorial` in the repository and is
    // installed as `assets/tutorial` beneath the installation root
    // (for example, `~/.local/virdant`).  Keep the bare `tutorial` path
    // as a fallback for installs made before the assets directory was
    // bundled as a unit.
    let mut dir = exe.parent().map(|p| p.to_path_buf());
    while let Some(ancestor) = dir {
        for candidate in ["assets/tutorial", "tutorial"] {
            let tutorial = ancestor.join(candidate);
            if tutorial.is_dir() {
                return std::fs::canonicalize(&tutorial).unwrap_or(tutorial);
            }
        }
        dir = ancestor.parent().map(|p| p.to_path_buf());
    }
    panic!("Could not find tutorial directory")
}

fn initialize_git(project_dir: &Path, no_git: bool) {
    // Git setup is opt-out, not forced: respect an existing repository
    // in an ancestor and a missing/unavailable Git executable.
    let cwd = std::env::current_dir().expect("Could not determine current directory");
    if no_git || vircommon::has_git_ancestor(&cwd) || !vircommon::git_is_executable() {
        return;
    }

    let status = std::process::Command::new("git")
        .arg("init")
        .current_dir(project_dir)
        .status()
        .unwrap_or_else(|_| panic!("Could not run git"));
    if !status.success() {
        panic!("git init failed");
    }
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

/// Marker file for the tutorial workspace itself.
const TUTORIAL_MARKER: &str = "tutorial.toml";

/// Marker file for a Virdant project (such as a tutorial lesson).
const PROJECT_MARKER: &str = "virdant.toml";

/// If `dir` contains a marker file, return which one.
fn find_marker(dir: &Path) -> Option<&'static str> {
    if dir.join(TUTORIAL_MARKER).exists() {
        Some(TUTORIAL_MARKER)
    } else if dir.join(PROJECT_MARKER).exists() {
        Some(PROJECT_MARKER)
    } else {
        None
    }
}

/// Refuse to run `vir tutor` where a tutorial or Virdant project already
/// exists: in the current directory, any of its ancestors, or any of its
/// immediate subdirectories.  This keeps `vir tutor` from being run a
/// second time inside the `tutorial/` workspace it creates.
fn guard_against_existing_setup() {
    let cwd = std::env::current_dir().expect("Could not determine current directory");

    // The current directory and each of its ancestors.
    let mut dir = Some(cwd);
    while let Some(d) = dir {
        if let Some(marker) = find_marker(&d) {
            already_setup_error(marker);
        }
        dir = d.parent().map(|parent| parent.to_path_buf());
    }

    // Immediate subdirectories.  This catches running `vir tutor` at the
    // root of a tutorial workspace, where the markers live down inside
    // the lesson directories.
    if let Ok(entries) = std::fs::read_dir(".") {
        for entry in entries.flatten() {
            if entry.file_type().map(|ty| ty.is_dir()).unwrap_or(false) {
                if let Some(marker) = find_marker(&entry.path()) {
                    already_setup_error(marker);
                }
            }
        }
    }
}

fn already_setup_error(marker: &str) -> ! {
    eprintln!("Welcome back!");
    eprintln!();
    if marker == TUTORIAL_MARKER {
        eprintln!("A Virdant tutorial is already set up here.");
        eprintln!();
        eprintln!("To start, go to 000_passsthrough/ and then:");
        eprintln!();
        eprintln!("  🌱 Read the README.md");
        eprintln!("  🌱 Work through src/passthrough.vir");
        eprintln!("  🌱 Have fun!");
    } else {
        eprintln!("There is already a Virdant project here.");
        eprintln!("Go to a fresh directory and run `vir tutor` there.");
    }
    eprintln!();
    eprintln!("If you get stuck or have questions:");
    eprintln!();
    eprintln!("  🌱 Visit the Virdant website: https://virdant.org/");
    eprintln!("  🌱 Join us at on Discord: https://discord.com/invite/F9trwvHPf6");
    std::process::exit(1);
}

fn new_workspace(no_git: bool) {
    let dest = PathBuf::from("tutorial");
    if dest.exists() {
        eprintln!("Welcome back!");
        eprintln!();
        eprintln!("The tutorial is already set up for you.");
        eprintln!("To start, go to tutorial/000_passsthrough/ and then:");
        eprintln!();
        eprintln!("  🌱 Read the README.md");
        eprintln!("  🌱 Work through src/passthrough.vir");
        eprintln!("  🌱 Have fun!");
        eprintln!();
        eprintln!("If you get stuck or have questions:");
        eprintln!();
        eprintln!("  🌱 Visit the Virdant website: https://virdant.org/");
        eprintln!("  🌱 Join us at on Discord: https://discord.com/invite/F9trwvHPf6");
        eprintln!();
        eprintln!("Please enjoy learning Virdant!");
        std::process::exit(1);
    } else {
        guard_against_existing_setup();

        let tutorial_dir = tutorial_dir();
        let lessons = list_lessons(&tutorial_dir);
        let Some((num, name)) = lessons.first() else {
            panic!("Could not find any lessons in {tutorial_dir:?}");
        };
        let first_dirname = format!("{num:03}_{name}");

        copy_dir(&tutorial_dir.join(&first_dirname), &dest.join(&first_dirname));
        std::fs::copy(
            tutorial_dir.join(TUTORIAL_MARKER),
            dest.join(TUTORIAL_MARKER),
        ).unwrap_or_else(|_| panic!("Could not copy the tutorial marker"));

        initialize_git(&dest, no_git);

        eprintln!("Welcome to Virdant 🌱");
        eprintln!();
        eprintln!("The tutorial is ready for you.");
        eprintln!("To start, go to tutorial/000_passsthrough/ and then:");
        eprintln!();
        eprintln!("  🌱 Read the README.md");
        eprintln!("  🌱 Work through src/passthrough.vir");
        eprintln!("  🌱 Have fun!");
        eprintln!();
        eprintln!("If you get stuck or have questions:");
        eprintln!();
        eprintln!("  🌱 Visit the Virdant website: https://virdant.org/");
        eprintln!("  🌱 Join us at on Discord: https://discord.com/invite/F9trwvHPf6");
        eprintln!();
        eprintln!("Please enjoy learning Virdant!");
    }
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
