use crate::*;

const CHECK: char = '✅';
const BATSU: char = '❌';

const EXAMPLES_DIR: &'static str = "../examples";
const ERROR_EXAMPLES_DIR: &'static str = "examples/errors";

#[test]
fn parse_examples() {
    use parse::*;
    let mut errors = vec![];

    for filepath in example_files() {
        let text = std::fs::read_to_string(filepath.clone()).unwrap();

        let filename = filepath
            .file_name()
            .unwrap()
            .to_owned()
            .to_string_lossy()
            .into_owned();

        if let Err(_error) = std::panic::catch_unwind(|| {
            parse_package(&text).unwrap();
        }) {
            eprintln!("    {BATSU} {filename}");
            errors.push(filename);
        } else {
            eprintln!("    {CHECK} {filename}");
        }
    }
    if errors.len() > 0 {
        panic!("Errors in examples:\n  - {}", errors.join("\n  - "))
    }
}

fn example_files() -> impl Iterator<Item = std::path::PathBuf> {
    let mut results = vec![];
    let examples_dir = std::path::Path::new(EXAMPLES_DIR);
    let entries = std::fs::read_dir(examples_dir).unwrap();
    for entry in entries {
        let entry = entry.unwrap();
        let filename = entry.file_name().to_string_lossy().into_owned();
        if filename.ends_with(".vir") {
            results.push(entry.path())
        }
    }
    results.into_iter()
}

#[test]
fn test_check_syntax_error() {
    let error_examples_dir = std::path::Path::new(ERROR_EXAMPLES_DIR);
    let mut virdant = Virdant::new(&[
        ("top", error_examples_dir.join("syntax_error.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            if let VirErr::Parse(_err) = &errors[0] {
                ()
            } else {
                panic!()
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_no_such_package() {
    let error_examples_dir = std::path::Path::new(ERROR_EXAMPLES_DIR);
    let mut virdant = Virdant::new(&[
        ("top", error_examples_dir.join("no_such_package.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            if let VirErr::CantImport(_err) = &errors[0] {
                ()
            } else {
                panic!()
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_dup_import() {
    let error_examples_dir = std::path::Path::new(ERROR_EXAMPLES_DIR);
    let mut virdant = Virdant::new(&[
        ("top", error_examples_dir.join("dup_import.vir")),
        ("bar", error_examples_dir.join("bar.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            if let VirErr::DupImport(_err) = &errors[0] {
                ()
            } else {
                panic!()
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_duplicate_item() {
    let error_examples_dir = std::path::Path::new(ERROR_EXAMPLES_DIR);
    let mut virdant = Virdant::new(&[
        ("top", error_examples_dir.join("duplicate_item.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            if let VirErr::DupItem(_) = &errors[0] {
                ()
            } else {
                panic!()
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_items() {
    let examples_dir = std::path::Path::new(EXAMPLES_DIR);
    let mut virdant = Virdant::new(&[
        ("top", examples_dir.join("uart.vir")),
    ]);

    virdant.check().unwrap();

    let items: Vec<_> = ["top::UartState", "top::UartSender", "top::UartReceiver"]
        .iter()
        .map(|item| Id::new(item.to_string()))
        .collect();

    assert_eq!(virdant.items(), items);

    let moddefs: Vec<_> = ["top::UartSender", "top::UartReceiver"]
        .iter()
        .map(|item| Id::new(item.to_string()))
        .collect();

    assert_eq!(virdant.moddefs(), moddefs);
}
