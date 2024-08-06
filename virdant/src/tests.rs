use std::{collections::HashSet, sync::LazyLock};

use crate::*;

const CHECK: char = '✅';
const BATSU: char = '❌';

const LIB_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::path::PathBuf::from("../lib"));
const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::path::PathBuf::from("../examples"));
const TEST_EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::path::PathBuf::from("examples"));
const ERROR_EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::path::PathBuf::from("examples/errors"));


#[test]
fn parse_examples() {
    use crate::parse::*;
    let mut errors = vec![];

    for filepath in example_files().chain(test_example_files()) {
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
    let entries = std::fs::read_dir(&*EXAMPLES_DIR).unwrap();
    for entry in entries {
        let entry = entry.unwrap();
        let filename = entry.file_name().to_string_lossy().into_owned();
        if filename.ends_with(".vir") {
            results.push(entry.path())
        }
    }
    results.into_iter()
}

fn test_example_files() -> impl Iterator<Item = std::path::PathBuf> {
    let mut results = vec![];
    let entries = std::fs::read_dir(&*EXAMPLES_DIR).unwrap();
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
fn test_top() {
    let mut virdant = Virdant::new(&[
        ("builtin", LIB_DIR.join("builtin.vir")),
        ("top", TEST_EXAMPLES_DIR.join("top.vir")),
    ]);

    virdant.check().unwrap();
}

#[test]
fn test_check_syntax_error() {
    let mut virdant = Virdant::new(&[
        ("top", ERROR_EXAMPLES_DIR.join("syntax_error.vir")),
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
    let mut virdant = Virdant::new(&[
        ("top", ERROR_EXAMPLES_DIR.join("no_such_package.vir")),
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
    let mut virdant = Virdant::new(&[
        ("top", ERROR_EXAMPLES_DIR.join("dup_import.vir")),
        ("bar", ERROR_EXAMPLES_DIR.join("bar.vir")),
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
    let mut virdant = Virdant::new(&[
        ("top", ERROR_EXAMPLES_DIR.join("duplicate_item.vir")),
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
fn test_check_missing_dependency() {
    let mut virdant = Virdant::new(&[
        ("builtin", LIB_DIR.join("builtin.vir")),
        ("top", ERROR_EXAMPLES_DIR.join("missing_dependency.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            for error in errors.iter() {
                if let VirErr::UnresolvedIdent(_) = &error {
                    // OK
                } else {
                    panic!();
                }
            }
            eprintln!("{errors:?}");
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_item_dep_cycle() {
    let mut virdant = Virdant::new(&[
        ("builtin", LIB_DIR.join("builtin.vir")),
        ("top", ERROR_EXAMPLES_DIR.join("item_dep_cycle.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            eprintln!("{errors:?}");
            assert_eq!(errors.len(), 1);
            if let VirErr::ItemDepCycle(_) = &errors[0] {
                ()
            } else {
                panic!()
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_kind_error() {
    let mut virdant = Virdant::new(&[
        ("builtin", LIB_DIR.join("builtin.vir")),
        ("top", ERROR_EXAMPLES_DIR.join("kind_error.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            eprintln!("{errors:?}");
            assert_eq!(errors.len(), 3);
            for error in errors.into_iter() {
                if let VirErr::KindError(_) = &error {
                    ()
                } else {
                    panic!()
                }
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_wrong_drivertype() {
    let mut virdant = Virdant::new(&[
        ("builtin", LIB_DIR.join("builtin.vir")),
        ("top", ERROR_EXAMPLES_DIR.join("wrong_drivertype.vir")),
    ]);

    match virdant.check() {
        Err(errors) => {
            eprintln!("{errors:?}");
            assert_eq!(errors.len(), 2);
            for error in errors.into_iter() {
                if let VirErr::WrongDriverType(_) = &error {
                    ()
                } else {
                    panic!()
                }
            }
        },
        _ => panic!(),
    }
}

#[test]
fn test_typecheck() {
    let mut virdant = Virdant::new(&[
        ("top", TEST_EXAMPLES_DIR.join("typecheck.vir")),
    ]);

    virdant.check().unwrap();
}

#[test]
fn test_top_design() {
    let mut virdant = Virdant::new(&[
        ("top", EXAMPLES_DIR.join("uart.vir")),
    ]);

    let design = virdant.check().unwrap();

    let packages = design.packages();
    assert_eq!(packages.len(), 2);

    let package_names: HashSet<_> = packages.iter().map(|p| p.name()).collect();
    assert_eq!(package_names, vec!["builtin", "top"].into_iter().collect());

    let top_package: design::Package = packages.into_iter()
        .filter(|p| p.name() == "top")
        .collect::<Vec<_>>()
        .first()
        .unwrap()
        .clone();

    assert_eq!(
        top_package.items()
            .into_iter()
            .map(|item| item.name().to_owned())
            .collect::<Vec<_>>(),
        vec!["UartState", "UartSender", "UartReceiver"],
    );

    let uart_state_item = top_package.items()[0].clone();
    if let design::ItemKind::UnionDef(uniondef) = uart_state_item.kind() {
        let ctors = uniondef.ctors();
        let ctor_names = ctors.into_iter().map(|ctor| ctor.name().to_string()).collect::<Vec<String>>();
        assert_eq!(ctor_names, vec!["Idle", "Start", "Bit", "Stop"]);
    } else {
        panic!("Expected Uniondef");
    }
}
