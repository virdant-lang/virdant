use paste::paste;
use std::sync::LazyLock;
use std::time::Instant;

use crate::{tokenizer::TokenKind, *};

use self::{parser::Parser, tokenizer::Tokenizer};

const CHECK: char = '✅';
const BATSU: char = '❌';

const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap());
const TEST_EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::path::PathBuf::from("examples"));
const ERROR_EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| std::path::PathBuf::from("examples/errors"));


#[test]
fn test_top() {
    let mut virdant = Virdant::new(TEST_EXAMPLES_DIR.join("top.vir"));
    virdant.check().unwrap();
}

#[test]
fn test_check_syntax_error() {
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("syntax_error.vir"));

    match virdant.check() {
        Err(errors) => {
            assert_eq!(errors.len(), 1);
            if let VirErr::Parse(_source, _err) = &errors[0] {
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
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("no_such_package.vir"));

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
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("dup_import.vir"));

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
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("duplicate_item.vir"));

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
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("missing_dependency.vir"));

    match virdant.check() {
        Err(errors) => {
            let mut count = 0;
            for error in errors.iter() {
                if let VirErr::UnresolvedIdent(_) = &error {
                    count += 1;
                }
            }
            if count == 0 {
                panic!();
            }
            eprintln!("{errors:?}");
        },
        _ => panic!(),
    }
}

#[test]
fn test_check_item_dep_cycle() {
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("item_dep_cycle.vir"));

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
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("kind_error.vir"));

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
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("wrong_drivertype.vir"));

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
fn test_check_no_drivers() {
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("no_drivers.vir"));

    match virdant.check() {
        Err(errors) => {
            eprintln!("{errors:?}");
            assert_eq!(errors.len(), 1);
            for error in errors.into_iter() {
                if let VirErr::NoDriver(_) = &error {
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
fn test_check_multiple_drivers() {
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("multiple_drivers.vir"));

    match virdant.check() {
        Err(errors) => {
            eprintln!("{errors:?}");
            assert_eq!(errors.len(), 1);
            for error in errors.into_iter() {
                if let VirErr::MultipleDrivers(_) = &error {
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
    let mut virdant = Virdant::new(TEST_EXAMPLES_DIR.join("typecheck.vir"));
    virdant.check().unwrap();
}

#[test]
fn test_verilog() {
    let mut virdant = Virdant::new(TEST_EXAMPLES_DIR.join("top.vir"));

    let design = virdant.check().unwrap();
    eprintln!("{virdant:?}");
    design.verilog("build/").unwrap();
}

#[test]
fn test_ext() {
    let mut virdant = Virdant::new(TEST_EXAMPLES_DIR.join("ext.vir"));

    let design = virdant.check().unwrap();
    eprintln!("{virdant:?}");
    design.verilog("build/").unwrap();
}

#[test]
fn test_no_clock() {
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("no_clock.vir"));

    match virdant.check() {
        Err(errors) => {
            let mut count = 0;
            for error in errors.iter() {
                if let VirErr::NoClock(_) = &error {
                    count += 1;
                }
            }
            if count == 0 {
                panic!();
            }
            eprintln!("{errors:?}");
        },
        _ => panic!(),
    }
}

#[test]
fn test_read_from_sink() {
    let mut virdant = Virdant::new(ERROR_EXAMPLES_DIR.join("read_from_sink.vir"));

    match virdant.check() {
        Err(errors) => {
            let mut count = 0;
            for error in errors.iter() {
                if let VirErr::ReadFromSink(_) = &error {
                    count += 1;
                }
            }
            if count == 0 {
                panic!();
            }
            eprintln!("{errors:?}");
        },
        _ => panic!(),
    }
}

/*
#[test]
fn test_tokenize() {
    let mut errors = vec![];

    for filepath in example_files().chain(test_example_files()) {
        let text = std::fs::read_to_string(filepath.clone()).unwrap();

        let filename = filepath
            .file_name()
            .unwrap()
            .to_owned()
            .to_string_lossy()
            .into_owned();

        let result = std::panic::catch_unwind(|| {
            let start = Instant::now();

            let text: Arc<[u8]> = Arc::from(text.into_bytes().into_boxed_slice());
            let tokenizer = Tokenizer::new(text.clone());
            let stop = Instant::now();

            for token in tokenizer.tokens() {
                if let TokenKind::Unknown = token.kind() {
                    let start = usize::from(token.pos());
                    let end = start + token.len() as usize;
                    panic!(
                        "Unexpected token: {:?} in {filename} starting at byte {start}",
                        String::from_utf8_lossy(&text.as_ref()[start..end]),
                    );
                }
            }

            stop - start
        });

        match result {
            Err(_error) => {
                eprintln!("    {BATSU} {filename}");
                errors.push(filename);
            }, 
            Ok(duration) => {
                eprintln!("    {CHECK} {filename:<20}{:>6} us", duration.as_micros());
            },
        }
    }
    if errors.len() > 0 {
        panic!("Errors in examples:\n  - {}", errors.join("\n  - "))
    }
}
*/

macro_rules! test_example {
    ($filename:ident) => {
        paste!(
            #[test]
            fn [< test_parse_ $filename >]() {
                let filename = format!("{}.vir", stringify!($filename));
                let filepath = EXAMPLES_DIR.join(&filename);
                let text = std::fs::read_to_string(&filepath).unwrap();

                let start = Instant::now();

                let text: Arc<[u8]> = Arc::from(text.into_bytes().into_boxed_slice());
                let mut parser = Parser::new(text.clone());
                parser.ast();
                let stop = Instant::now();
                let duration = stop - start;

                if parser.errors().len() > 0 {
                    eprintln!("    {BATSU} {filename:<20}{:>6} us", duration.as_micros());

                    for error in parser.errors() {
                        eprintln!("        {error:?}");
                    }
                    panic!("{} errors while parsing {filepath:?}", parser.errors().len());

                } else {
                    eprintln!("    {CHECK} {filename:<20}{:>6} us", duration.as_micros());
                }
            }
        );
    }
}

test_example!(blink);
test_example!(buffer);
test_example!(delay);
test_example!(echo);
test_example!(edge);
test_example!(enums);
test_example!(extensions);
test_example!(fns);
test_example!(gcd);
test_example!(lfsr);
test_example!(lights);
test_example!(literals);
test_example!(matches);
test_example!(passthrough);
test_example!(queue);
test_example!(random);
test_example!(resetter);
test_example!(rf);
test_example!(sockets);
test_example!(structs);
test_example!(top);
test_example!(uart);
