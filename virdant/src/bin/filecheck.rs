use std::io::Read;
use std::path::PathBuf;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: filecheck <check-file>");
        eprintln!("       Input is read from stdin.");
        std::process::exit(1);
    }

    let check_path = PathBuf::from(&args[1]);
    let check_content = match std::fs::read_to_string(&check_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("filecheck: {}: {}", check_path.display(), e);
            std::process::exit(1);
        }
    };

    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();

    let directives = parse_directives(&check_content);
    if directives.is_empty() {
        eprintln!("filecheck: no CHECK directives found in '{}'", check_path.display());
        std::process::exit(1);
    }

    let input_lines: Vec<&str> = input.lines().collect();
    run_checks(&directives, &input_lines, &check_path);
}

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

fn run_checks(directives: &[Directive], input_lines: &[&str], check_path: &PathBuf) {
    let mut cursor: usize = 0;
    let mut pending_nots: Vec<&Directive> = Vec::new();

    for directive in directives {
        match directive.kind {
            DirectiveKind::Check => {
                let found = input_lines[cursor..].iter().position(|l| l.contains(&directive.pattern));
                match found {
                    None => {
                        eprintln!(
                            "filecheck: {}:{}: CHECK pattern not found: {:?}",
                            check_path.display(), directive.check_line, directive.pattern
                        );
                        std::process::exit(1);
                    }
                    Some(offset) => {
                        let match_line = cursor + offset;
                        check_nots(&pending_nots, &input_lines[cursor..match_line], check_path);
                        pending_nots.clear();
                        cursor = match_line + 1;
                    }
                }
            }
            DirectiveKind::CheckNext => {
                if cursor >= input_lines.len() {
                    eprintln!(
                        "filecheck: {}:{}: CHECK-NEXT: no more input lines",
                        check_path.display(), directive.check_line
                    );
                    std::process::exit(1);
                }
                if !input_lines[cursor].contains(&directive.pattern) {
                    eprintln!(
                        "filecheck: {}:{}: CHECK-NEXT pattern not found on next line: {:?}",
                        check_path.display(), directive.check_line, directive.pattern
                    );
                    eprintln!("filecheck:   actual: {:?}", input_lines[cursor]);
                    std::process::exit(1);
                }
                cursor += 1;
            }
            DirectiveKind::CheckNot => {
                pending_nots.push(directive);
            }
        }
    }

    // Any remaining CHECK-NOTs apply to the rest of the input
    check_nots(&pending_nots, &input_lines[cursor..], check_path);
}

fn check_nots(pending_nots: &[&Directive], lines: &[&str], check_path: &PathBuf) {
    for not_dir in pending_nots {
        for line in lines {
            if line.contains(&not_dir.pattern) {
                eprintln!(
                    "filecheck: {}:{}: CHECK-NOT pattern unexpectedly matched: {:?}",
                    check_path.display(), not_dir.check_line, not_dir.pattern
                );
                eprintln!("filecheck:   matched: {:?}", line);
                std::process::exit(1);
            }
        }
    }
}
