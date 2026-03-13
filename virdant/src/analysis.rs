mod db;

use bstr::BStr;
use bstr::BString;
use std::collections::HashMap;
use std::sync::Arc;

use crate::analysis::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNode;
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::parsing::parse;
use crate::syntax::payload::AstNodePayload;

pub struct Location(PackageFqn, AstNodeId);

pub struct PackageAnalysis {
    parsing: Arc<Parsing>,
    imports: Vec<PackageFqn>,
    items: HashMap<BString, Vec<AstNodeId>>,
}

impl Location {
    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn ast_node_id(&self) -> AstNodeId {
        self.1.clone()
    }
}

impl std::fmt::Debug for PackageAnalysis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PackageAnalysis")
            .field("parsing", &"[Parsing]")
            .field("imports", &self.imports)
            .field("items", &self.items)
            .finish()
    }
}

fn build_package_analysis(builder: &mut Builder, package: PackageFqn) -> Arc<PackageAnalysis> {
    let parsing = builder.get_parsing(package);
    Arc::new(PackageAnalysis::new(parsing))
}

impl PackageAnalysis {
    pub fn new(parsing: Arc<Parsing>) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            parsing: parsing.clone(),
            imports: vec![PackageFqn::new("builtin".into())],
            items: HashMap::new(),
        };

        analysis.add_imports();
        analysis.add_items();

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

    fn add_imports(&mut self) {
        let root = self.parsing.root();
        for child_node in root.children() {
            eprintln!("child: {}", child_node.summary());
            if let AstNodePayload::Import(import) = child_node.payload() {
                let package = PackageFqn::new(self.parsing.string(import.package).into());
                self.imports.push(package);
            }
        }
    }

    fn add_items(&mut self) {
        let root = self.parsing.root();
        for child_node in root.children() {
            eprintln!("child: {}", child_node.summary());
            if child_node.is_item() {
                let name = self.parsing.string(child_node.name().unwrap()).to_owned();
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
fn test_package_analysis() {
    use crate::tests::EXAMPLES_DIR;
    use crate::source::Source;

    let source = Source::load_file(EXAMPLES_DIR.join("basic.vir"));
    let parsing = parse(&source);
    let analysis = PackageAnalysis::new(Arc::new(parsing));

    dbg!(&analysis);

    eprintln!("Top AST:");
    analysis.item_ast("Top".into()).unwrap().dump();
    eprintln!();

    eprintln!("Foo AST:");
    analysis.item_ast("Foo".into()).unwrap().dump();
}
