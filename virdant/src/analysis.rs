use bstr::BStr;
use bstr::BString;
use std::collections::HashMap;

use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNode;
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::parsing::parse;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct PackageAnalysis<'p> {
    parsing: &'p Parsing,
    imports: Vec<PackageFqn>,
    items: HashMap<BString, Vec<AstNodeId>>,
}

impl<'p> PackageAnalysis<'p> {
    pub fn new(parsing: &Parsing) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            parsing,
            imports: vec![PackageFqn::new("builtin".into())],
            items: HashMap::new(),
        };

        analysis.add_imports(&parsing);
        analysis.add_items(&parsing);

        analysis
    }

    pub fn package(&self) -> PackageFqn {
        self.parsing.package()
    }

//    pub fn errors<'p>(&self, parsing: &'p Parsing, item_name: &BStr) -> Option<AstNode<'p>> {

    pub fn item_ast(&self, item_name: &BStr) -> Option<AstNode<'_>> {
        if let Some(items) = self.items.get(item_name) {
            if items.len() == 1 {
                let item_ast_id = &items[0];
                return Some(self.parsing.ast_node(*item_ast_id));
            }
        }

        None
    }

    fn add_imports(&mut self, parsing: &Parsing) {
        let root = parsing.root();
        for child_node in root.children() {
            eprintln!("child: {}", child_node.summary());
            if let AstNodePayload::Import(import) = child_node.payload() {
                let package = PackageFqn::new(parsing.string(import.package).into());
                self.imports.push(package);
            }
        }
    }

    fn add_items(&mut self, parsing: &Parsing) {
        let root = parsing.root();
        for child_node in root.children() {
            eprintln!("child: {}", child_node.summary());
            if child_node.is_item() {
                let name = parsing.string(child_node.name().unwrap()).to_owned();
                if !self.items.contains_key(&name) {
                    self.items.insert(name.clone(), vec![]);
                }
                let mut items = self.items.get_mut(&name).unwrap();
                items.push(child_node.id());
            }
        }
    }
}

#[cfg(test)]
#[test]
fn tests_package_analysis() {
    use crate::tests::EXAMPLES_DIR;
    use crate::source::Source;

    let source = Source::load_file(EXAMPLES_DIR.join("basic.vir"));
    let parsing = parse(&source);
    let analysis = PackageAnalysis::new(&parsing);

    dbg!(&analysis);

    eprintln!("Top AST:");
    analysis.item_ast("Top".into()).unwrap().dump();
    eprintln!();

    eprintln!("Foo AST:");
    analysis.item_ast("Foo".into()).unwrap().dump();
}
