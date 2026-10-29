//! A standalone FileCheck-style pattern matcher, reading `CHECK`,
//! `CHECK-NEXT`, and `CHECK-NOT` directives from a check file and
//! validating them against stdin.
//!
//! Moved here from the `virdant` crate: it is test tooling, not part of
//! the compiler under test.  The runner invokes [`run`] in-process; the
//! `vir-virtue --filecheck <check-file>` flag (and the `vir-filecheck`
//! compatibility binary) expose the same functionality standalone.

use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct Directive {
    kind: DirectiveKind,
    pattern: String,
    check_line: usize,
}

#[derive(Debug, PartialEq)]
enum DirectiveKind {
    Check,
    CheckNext,
    CheckNot,
}

fn parse_directives(content: &str) -> Vec<Directive> {
    let mut directives = Vec::new();
    for (i, line) in content.lines().enumerate() {
        let check_line = i + 1;
        // CHECK-NEXT and CHECK-NOT must be tested before CHECK to avoid prefix match
        if let Some(pos) = line.find("CHECK-NEXT:") {
            let pattern = line[pos + "CHECK-NEXT:".len()..].trim().to_string();
            directives.push(Directive { kind: DirectiveKind::CheckNext, pattern, check_line });
        } else if let Some(pos) = line.find("CHECK-NOT:") {
            let pattern = line[pos + "CHECK-NOT:".len()..].trim().to_string();
            directives.push(Directive { kind: DirectiveKind::CheckNot, pattern, check_line });
        } else if let Some(pos) = line.find("CHECK:") {
            let pattern = line[pos + "CHECK:".len()..].trim().to_string();
            directives.push(Directive { kind: DirectiveKind::Check, pattern, check_line });
        }
    }
    directives
}

/// Validate `input_lines` against the directives found in `check_content`.
/// The check path is used only for error messages.  Returns the full
/// rendered error report on failure (one or more `filecheck:` lines).
pub fn run(check_path: &Path, check_content: &str, input_lines: &[&str]) -> Result<(), String> {
    let directives = parse_directives(check_content);
    if directives.is_empty() {
        return Err(format!(
            "filecheck: no CHECK directives found in '{}'",
            check_path.display()
        ));
    }
    run_checks(&directives, input_lines, check_path)
}

/// Standalone entry point: match the CHECK directives in `check_file`
/// against stdin.  Prints errors to stderr and exits 1 on failure, with
/// the same exit-code contract as the former `filecheck` binary.
/// `check_file` is the argument the shim passes (`filecheck <file>`);
/// the runner passes it explicitly after `--filecheck`.
pub fn cli_main(check_file: Option<String>) -> ! {
    let check_file = match check_file {
        Some(path) => path,
        None => {
            eprintln!("Usage: vir-virtue --filecheck <check-file>");
            eprintln!("       Input is read from stdin.");
            std::process::exit(1);
        }
    };

    let check_path = Path::new(&check_file);
    let check_content = match std::fs::read_to_string(check_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("filecheck: {}: {}", check_path.display(), e);
            std::process::exit(1);
        }
    };

    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let input_lines: Vec<&str> = input.lines().collect();
    match run(check_path, &check_content, &input_lines) {
        Ok(()) => std::process::exit(0),
        Err(report) => {
            eprintln!("{report}");
            std::process::exit(1);
        }
    }
}

fn run_checks(directives: &[Directive], input_lines: &[&str], check_path: &Path) -> Result<(), String> {
    let mut cursor: usize = 0;
    let mut pending_nots: Vec<&Directive> = Vec::new();

    for directive in directives {
        match directive.kind {
            DirectiveKind::Check => {
                let found = input_lines[cursor..].iter().position(|l| l.contains(&directive.pattern));
                match found {
                    None => {
                        return Err(format!(
                            "filecheck: {}:{}: CHECK pattern not found: {:?}",
                            check_path.display(),
                            directive.check_line,
                            directive.pattern
                        ));
                    }
                    Some(offset) => {
                        let match_line = cursor + offset;
                        check_nots(&pending_nots, &input_lines[cursor..match_line], check_path)?;
                        pending_nots.clear();
                        cursor = match_line + 1;
                    }
                }
            }
            DirectiveKind::CheckNext => {
                if cursor >= input_lines.len() {
                    return Err(format!(
                        "filecheck: {}:{}: CHECK-NEXT: no more input lines",
                        check_path.display(),
                        directive.check_line
                    ));
                }
                if !input_lines[cursor].contains(&directive.pattern) {
                    return Err(format!(
                        "filecheck: {}:{}: CHECK-NEXT pattern not found on next line: {:?}\nfilecheck:   actual: {:?}",
                        check_path.display(),
                        directive.check_line,
                        directive.pattern,
                        input_lines[cursor]
                    ));
                }
                cursor += 1;
            }
            DirectiveKind::CheckNot => {
                pending_nots.push(directive);
            }
        }
    }

    // Any remaining CHECK-NOTs apply to the rest of the input
    check_nots(&pending_nots, &input_lines[cursor..], check_path)
}

fn check_nots(pending_nots: &[&Directive], lines: &[&str], check_path: &Path) -> Result<(), String> {
    for not_dir in pending_nots {
        for line in lines {
            if line.contains(&not_dir.pattern) {
                return Err(format!(
                    "filecheck: {}:{}: CHECK-NOT pattern unexpectedly matched: {:?}\nfilecheck:   matched: {:?}",
                    check_path.display(),
                    not_dir.check_line,
                    not_dir.pattern,
                    line
                ));
            }
        }
    }
    Ok(())
}
