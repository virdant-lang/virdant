#[cfg(test)]
mod test_docstrings {
    use crate::common::source::{Source, Span};
    use crate::fqn::PackageFqn;
    use crate::syntax::parsing::parse;
    use crate::syntax::ast::AstNode;
    use crate::syntax::payload::AstNodePayload;

    fn test_source(text: &[u8]) -> Source {
        Source::new(PackageFqn::new("test".into()), text.into())
    }

    #[test]
    fn test_package_docstring() {
        let source = test_source(b"//! A top-level package docstring.\n\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        match root.payload() {
            AstNodePayload::Package(p) => {
                assert!(p.doc_string.is_some(), "Expected package docstring, got None");
                let doc = parsing.string(p.doc_string.unwrap());
                assert!(doc.contains("top-level package"), "Docstring should contain 'top-level package', got: {:?}", doc);
            }
            other => panic!("Expected Package, got {:?}", other.kind()),
        }
    }

    #[test]
    fn test_moddef_docstring() {
        let source = test_source(b"/// A module with a docstring.\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        // Walk children to find the ModDef
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_some(), "Expected ModDef docstring, got None");
                let doc = parsing.string(m.doc_string.unwrap());
                assert!(doc.contains("A module"), "Docstring should contain 'A module', got: {:?}", doc);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_component_docstring() {
        let source = test_source(b"mod Foo {\n    /// An input signal.\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                // Find the component
                for stmt in child.children() {
                    if let AstNodePayload::Component(c) = stmt.payload() {
                        assert!(c.doc_string.is_some(), "Expected Component docstring, got None");
                        let doc = parsing.string(c.doc_string.unwrap());
                        assert!(doc.contains("An input signal"), "Docstring should contain 'An input signal', got: {:?}", doc);
                    }
                }
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_no_docstring_is_none() {
        let source = test_source(b"mod Foo {\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_none(), "Expected no docstring, got Some");
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_plain_comment_does_not_become_docstring() {
        // Plain // comment should not produce a DocComment token
        let source = test_source(b"// This is a plain comment.\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_none(), "Plain comment should not become docstring");
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_docstring_without_space_does_not_match() {
        // /// without a following space should NOT match as DocComment
        let source = test_source(b"///no space here\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::ModDef(m) = child.payload() {
                assert!(m.doc_string.is_none(), "Docstring without space should not match, got: {:?}", m.doc_string);
                return;
            }
        }
        panic!("No ModDef found");
    }

    #[test]
    fn test_struct_field_docstring() {
        let source = test_source(b"struct type Foo {\n    /// The foo field.\n    bar : Word[1]\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            if let AstNodePayload::StructDef(s) = child.payload() {
                for field in child.children() {
                    if let AstNodePayload::Field(f) = field.payload() {
                        assert!(f.doc_string.is_some(), "Expected Field docstring, got None");
                        let doc = parsing.string(f.doc_string.unwrap());
                        assert!(doc.contains("The foo field"), "Docstring should contain 'The foo field', got: {:?}", doc);
                        return;
                    }
                }
            }
        }
        panic!("No StructDef found");
    }

    #[test]
    fn test_ast_node_doc_string_accessor() {
        let source = test_source(b"/// Documented mod.\nmod Foo {\n    incoming inp : Bit\n    out := inp\n}\n");
        let parsing = parse(&source);
        let root = parsing.root();
        for child in root.children() {
            let doc = child.doc_string();
            assert!(doc.is_some(), "Expected doc_string() to return Some");
            return;
        }
        panic!("No children found");
    }
}
