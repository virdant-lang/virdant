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
test_example!(valid);
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
    fn test_docstring_without_space_matches() {
        // Space after /// is NOT required -- ///anything is a valid docstring.
        let source = test_source(
            b"///no space here\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_some(),
                    "Docstring without space should match, got None");
                let doc = parsing.string(m.doc_string.unwrap());
                assert!(doc.find("no space").is_some(),
                    "Docstring should contain 'no space', got: {:?}", doc);
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

    #[test]
    fn test_docbang_without_space_matches() {
        let source = test_source(
            b"//!Package doc without space.\n\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        match root.payload() {
            AstNodePayload::Package(p) => {
                assert!(p.doc_string.is_some(),
                    "Expected package docstring, got None");
                let doc = parsing.string(p.doc_string.unwrap());
                assert!(doc.find("without space").is_some(),
                    "Docbang should contain 'without space', got: {:?}", doc);
            }
            other => panic!("Expected Package, got {:?}", other.kind()),
        }
    }

    #[test]
    fn test_docstring_strip_prefix() {
        // Verify the docstring includes the `///` prefix (3 bytes).
        // Callers strip it.
        let source = test_source(
            b"/// hello\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                let doc = parsing.string(m.doc_string.unwrap());
                // doc includes the "///" prefix
                assert_eq!(&doc[..3], b"///",
                    "Docstring should start with '///', got: {:?}", &doc[..3]);
                // the rest is the content (with optional leading space)
                assert!(doc.find("hello").is_some(),
                    "Docstring should contain 'hello', got: {:?}", doc);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_invalid_docstring_diagnostic() {
        // `///foo` (no space) should produce an InvalidDocstring diagnostic.
        let source = test_source(
            b"///foo\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let diags = parsing.diagnostics();
        // At minimum there should be 1 diagnostic (the InvalidDocstring)
        assert!(diags.len() >= 1,
            "Expected at least one diagnostic for invalid docstring, got {}",
            diags.len());
        // The diagnostic message should mention "Invalid docstring"
        let message = diags[0].message();
        assert!(message.find("Invalid docstring").is_some(),
            "Expected 'Invalid docstring' in message, got: {:?}", message);
    }

    #[test]
    fn test_invalid_docbang_diagnostic() {
        // `//!bar` (no space) should produce an InvalidDocstring diagnostic.
        let source = test_source(
            b"//!bar\n\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let diags = parsing.diagnostics();
        assert!(diags.len() >= 1,
            "Expected at least one diagnostic for invalid docbang, got {}",
            diags.len());
        let message = diags[0].message();
        assert!(message.find("Invalid docstring").is_some(),
            "Expected 'Invalid docstring' in message, got: {:?}", message);
    }

    #[test]
    fn test_valid_docstring_no_diagnostic() {
        // `/// foo` (with space) should produce NO InvalidDocstring diagnostic.
        let source = test_source(
            b"/// foo\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let diags = parsing.diagnostics();
        let invalid_diags: Vec<_> = diags.iter().filter(|d| {
            let m = d.message();
            m.find("Invalid docstring").is_some()
        }).collect();
        assert!(invalid_diags.is_empty(),
            "Expected no InvalidDocstring diagnostic for valid docstring, got {}: {:?}",
            invalid_diags.len(), invalid_diags);
    }

    #[test]
    fn test_multiline_strips_prefix_from_each_line() {
        // Multi-line docstring with an empty `///` line in the middle.
        let source = test_source(
            b"/// A RISC-V CPU core\n///\n/// This implements RV32I.\nmod Cpu {\n    incoming clock : Clock\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                let doc = m.doc_string.unwrap();
                let stripped = parsing.doc_string(&doc);
                // The empty `///` line should become an empty line.
                // All lines should have the `///` prefix removed.
                let expected: &[u8] = b" A RISC-V CPU core\n\n This implements RV32I.";
                assert_eq!(stripped.as_bstr(), expected.as_bstr(),
                    "Stripped docstring mismatch\n  got:      {:?}\n  expected: {:?}",
                    stripped, expected);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_docbang_empty_line() {
        // Multi-line package docstring with an empty `//!` line.
        let source = test_source(
            b"//! Top-level package.\n//!\n//! Contains the CPU core.\n\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        if let AstNodePayload::Package(p) = root.payload() {
            let doc = p.doc_string.unwrap();
            let stripped = parsing.doc_bang(&doc);
            let expected: &[u8] = b" Top-level package.\n\n Contains the CPU core.";
            assert_eq!(stripped.as_bstr(), expected.as_bstr(),
                "Stripped docbang mismatch\n  got:      {:?}\n  expected: {:?}",
                stripped, expected);
        } else {
            panic!("Expected Package");
        }
    }

    #[test]
    fn test_docbang_exact_cpu_scenario() {
        // Exact content from cpu.vir top: three //! lines, middle one is bare `//!`.
        let source = test_source(
            b"//! This is the CPU core tile\n//!\n//! This needs to be integrated into a SoC subsystem\n\nimport rf\n\nmod Cpu {\n    incoming clock : Clock\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        match root.payload() {
            AstNodePayload::Package(p) => {
                let doc = p.doc_string.unwrap();
                let raw = parsing.string(doc.clone());
                // Raw interned text should include the middle bare `//!` line
                assert!(raw.find(b"//!").is_some(),
                    "Raw docstring should contain '//!', got: {:?}", raw);

                let stripped = parsing.doc_bang(&doc);
                let expected: &[u8] = b" This is the CPU core tile\n\n This needs to be integrated into a SoC subsystem";
                assert_eq!(stripped.as_bstr(), expected.as_bstr(),
                    "Stripped docbang mismatch\n  got:      {:?}\n  expected: {:?}",
                    stripped, expected);
            }
            other => panic!("Expected Package, got {:?}", other.kind()),
        }
    }

    #[test]
    fn test_docstring_exact_cpu_scenario() {
        // Same but with /// for a ModDef: middle line is bare `///`.
        let source = test_source(
            b"/// A RISC-V CPU core\n///\n/// This implements RV32I and passes all rv32iu tests in riscv-tests.\nmod Cpu {\n    incoming clock : Clock\n}\n"
        );
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                let doc = m.doc_string.unwrap();
                let raw = parsing.string(doc.clone());
                // Raw interned text should include the middle bare `///` line
                assert!(raw.find(b"///").is_some(),
                    "Raw docstring should contain '///', got: {:?}", raw);

                let stripped = parsing.doc_string(&doc);
                eprintln!("RAW:       {:?}", raw);
                eprintln!("STRIPPED:  {:?}", stripped);

                let expected: &[u8] = b" A RISC-V CPU core\n\n This implements RV32I and passes all rv32iu tests in riscv-tests.";
                assert_eq!(stripped.as_bstr(), expected.as_bstr(),
                    "Stripped docstring mismatch\n  got:      {:?}\n  expected: {:?}",
                    stripped, expected);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_valid_docbang_no_diagnostic() {
        // `//! bar` (with space) should produce NO InvalidDocstring diagnostic.
        let source = test_source(
            b"//! bar\n\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n"
        );
        let parsing = parse(&source);
        let diags = parsing.diagnostics();
        let invalid_diags: Vec<_> = diags.iter().filter(|d| {
            let m = d.message();
            m.find("Invalid docstring").is_some()
        }).collect();
        assert!(invalid_diags.is_empty(),
            "Expected no InvalidDocstring diagnostic for valid docbang, got {}: {:?}",
            invalid_diags.len(), invalid_diags);
    }
}
