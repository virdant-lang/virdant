use paste::paste;
use std::sync::LazyLock;
use std::time::Instant;
use std::io::Read;

use crate::common::source::Source;
use crate::syntax::parsing::parse;

const CHECK: char = '✅';
const BATSU: char = '❌';

const EXAMPLES_DIR: LazyLock<std::path::PathBuf> = LazyLock::new(|| {
    std::fs::canonicalize(std::path::PathBuf::from("../examples")).unwrap()
});

macro_rules! test_example {
    ($filename:ident) => {
        paste!(
            #[test]
            fn [< test_parse_ $filename >]() {
                let filename = format!("{}.vir", stringify!($filename));
                let filepath = EXAMPLES_DIR.join(&filename);
                let mut file = std::fs::File::open(filepath).unwrap();
                let mut text = vec![];
                file.read_to_end(&mut text).unwrap();

                let source = Source::new(crate::fqn::PackageFqn::new("top".into()), text.into());

                let start = Instant::now();
                let parsing = parse(&source);
                let stop = Instant::now();

                let duration = stop - start;

                let errors = parsing.errors();
                if errors.len() > 0 {
                    eprintln!("    {BATSU} {filename:<20}{:>6} us", duration.as_micros());
                    eprintln!("        ERRORS:");
                    for error in &errors {
                        eprintln!("            {:?}  {}", error, error.0.region());
                    }
                } else {
                    eprintln!("    {CHECK} {filename:<20}{:>6} us", duration.as_micros());
                }

                assert!(errors.is_empty());
            }
        );
    }
}

test_example!(basic);
test_example!(blink);
test_example!(buffer);
test_example!(delay);
test_example!(echo);
test_example!(edge);
test_example!(enums);
test_example!(extensions);
test_example!(fns);
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
test_example!(it);
//test_example!(conditional_statements);

#[cfg(test)]
mod test_docstrings {
    use bstr::ByteSlice;
    use crate::common::source::Source;
    use crate::fqn::PackageFqn;
    use crate::syntax::parsing::parse;
    use crate::syntax::payload::AstNodePayload;

    fn test_source(text: &[u8]) -> Source {
        Source::new(PackageFqn::new("test".into()), text.into())
    }

    #[test]
    fn test_package_docstring() {
        let source = test_source(
            b"//! A top-level package docstring.\n\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        match root.payload() {
            AstNodePayload::Package(p) => {
                assert!(p.doc_string.is_some(),
                    "Expected package docstring, got None");
                let doc = parsing.string(p.doc_string.unwrap());
                assert!(doc.find("top-level package").is_some(),
                    "Docstring should contain 'top-level package', got: {:?}", doc);
            }
            other => panic!("Expected Package, got {:?}", other.kind()),
        }
    }

    #[test]
    fn test_moddef_docstring() {
        let source = test_source(
            b"/// A module with a docstring.\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_some(),
                    "Expected ModDef docstring, got None");
                let doc = parsing.string(m.doc_string.unwrap());
                assert!(doc.find("A module").is_some(),
                    "Docstring should contain 'A module', got: {:?}", doc);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_component_docstring() {
        let source = test_source(
            b"mod Foo {\n    /// An input signal.\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                for stmt in child.children() {
                    if let AstNodePayload::Component(c) = stmt.payload() {
                        assert!(c.doc_string.is_some(),
                            "Expected Component docstring, got None");
                        let doc = parsing.string(c.doc_string.unwrap());
                        assert!(doc.find("An input signal").is_some(),
                            "Docstring should contain 'An input signal', got: {:?}", doc);
                    }
                }
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_no_docstring_is_none() {
        let source = test_source(
            b"mod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_none(),
                    "Expected no docstring, got Some");
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_plain_comment_does_not_become_docstring() {
        let source = test_source(
            b"// This is a plain comment.\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_none(),
                    "Plain comment should not become docstring");
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_docstring_without_space_does_not_match() {
        let source = test_source(
            b"///no space here\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_none(),
                    "Docstring without space should not match, got: {:?}",
                    m.doc_string);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_struct_field_docstring() {
        let source = test_source(
            b"struct type Foo {\n    /// The foo field.\n    bar : Word[1]\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::StructDef(s) = child.payload() {
                for field in child.children() {
                    if let AstNodePayload::Field(f) = field.payload() {
                        assert!(f.doc_string.is_some(),
                            "Expected Field docstring, got None");
                        let doc = parsing.string(f.doc_string.unwrap());
                        assert!(doc.find("The foo field").is_some(),
                            "Docstring should contain 'The foo field', got: {:?}", doc);
                        return;
                    }
                }
            }
        }
        panic!("No StructDef found");
    }

    #[test]
    fn test_ast_node_doc_string_accessor() {
        let source = test_source(
            b"/// Documented mod.\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            let doc = child.doc_string();
            assert!(doc.is_some(),
                "Expected doc_string() to return Some");
            return;
        }
        panic!("No children found");
    }

    #[test]
    fn test_multiple_docstrings() {
        let source = test_source(
            b"//! Package doc.\n\n/// Module doc.\nmod Foo {\n    /// Incoming doc.\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();

        // Check package docstring
        match root.payload() {
            AstNodePayload::Package(p) => {
                assert!(p.doc_string.is_some(),
                    "Expected package docstring");
                let doc = parsing.string(p.doc_string.unwrap());
                assert!(doc.find("Package doc").is_some(),
                    "Package doc mismatch: {:?}", doc);
            }
            other => panic!("Expected Package, got {:?}", other.kind()),
        }

        // Check ModDef docstring
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_some(),
                    "Expected ModDef docstring");
                let doc = parsing.string(m.doc_string.unwrap());
                assert!(doc.find("Module doc").is_some(),
                    "ModDef doc mismatch: {:?}", doc);

                // Check component docstring
                for stmt in child.children() {
                    if let AstNodePayload::Component(c) = stmt.payload() {
                        assert!(c.doc_string.is_some(),
                            "Expected Component docstring");
                        let doc = parsing.string(c.doc_string.unwrap());
                        assert!(doc.find("Incoming doc").is_some(),
                            "Component doc mismatch: {:?}", doc);
                        return;
                    }
                }
            }
        }
        panic!("Did not find all expected docstrings");
    }
}
