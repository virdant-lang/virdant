pub mod db;

use bstr::BStr;
use bstr::BString;
use hashbrown::HashSet;
use std::collections::HashMap;
use std::sync::Arc;

use crate::analysis::db::Builder;
use crate::common::json::ToJson;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::source::Region;
use crate::syntax::ast::AstNode;
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::parsing::parse;
use crate::syntax::payload::AstNodePayload;

pub struct Location(PackageFqn, AstNodeId);

#[derive(Debug)]
pub struct PackageAnalysis {
    package: PackageFqn,
    imports: HashSet<PackageFqn>,
    items: HashMap<BString, Vec<AstNodeId>>,
    diagnostics: Vec<Diagnostic>,
}

impl Location {
    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn ast_node_id(&self) -> AstNodeId {
        self.1.clone()
    }
}

fn build_package_analysis(builder: &mut Builder, package: PackageFqn) -> Arc<PackageAnalysis> {
    let parsing = builder.get_parsing(package);
    Arc::new(PackageAnalysis::new(parsing))
}

impl PackageAnalysis {
    pub fn new(parsing: Arc<Parsing>) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            package: parsing.package(),
            imports: vec![PackageFqn::new("builtin".into())].into_iter().collect(),
            items: HashMap::new(),
            diagnostics: vec![],
        };

        analysis.add_imports(parsing.clone());
        analysis.add_items(parsing.clone());

        analysis
    }

    pub fn package(&self) -> PackageFqn {
        self.package.clone()
    }

//    pub fn errors<'p>(&self, parsing: &'p Parsing, item_name: &BStr) -> Option<AstNode<'p>> {

    pub fn item_ast_node_id(&self, item_name: &BStr) -> Option<AstNodeId> {
        if let Some(items) = self.items.get(item_name) {
            if items.len() == 1 {
                let item_ast_id = &items[0];
                return Some(*item_ast_id);
            }
        }

        None
    }

    fn add_imports(&mut self, parsing: Arc<Parsing>) {
        let root = parsing.root();
        for child_node in root.children() {
            eprintln!("child: {}", child_node.summary());
            if let AstNodePayload::Import(import) = child_node.payload() {
                let package = PackageFqn::new(parsing.string(import.package).into());
                if !self.imports.insert(package) {
                    let imported_package = PackageFqn::new(parsing.string(child_node.package().unwrap()).to_owned());
                    self.diagnostics.push(diagnostics::DuplicateImport {
                        region: Region::new(self.package(), child_node.span()),
                        imported_package,
                    }.into());
                }
            }
        }
    }

    fn add_items(&mut self, parsing: Arc<Parsing>) {
        let root = parsing.root();
        for child_node in root.children() {
            eprintln!("child: {}", child_node.summary());
            if child_node.is_item() {
                let name = parsing.string(child_node.name().unwrap()).to_owned();
                if !self.items.contains_key(&name) {
                    self.items.insert(name.clone(), vec![]);
                } else {
                    let item = parsing.string(child_node.name().unwrap()).to_owned();
                    self.diagnostics.push(diagnostics::DuplicateItem {
                        region: Region::new(self.package(), child_node.span()),
                        item,
                    }.into());
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
    let parsing = Arc::new(parse(&source));
    let analysis = PackageAnalysis::new(parsing.clone());

    dbg!(&analysis);

    eprintln!("Top AST:");
    let node_id = analysis.item_ast_node_id("Top".into()).unwrap();
    parsing.ast_node(node_id).dump();
    eprintln!();

    eprintln!("Foo AST:");
    let node_id = analysis.item_ast_node_id("Foo".into()).unwrap();
    parsing.ast_node(node_id).dump();
}

impl ToJson for PackageAnalysis {
    fn to_json(&self) -> json::JsonValue {
        json::object!(
            "imports": self.imports.to_json(),
            "items": self.items.to_json(),
            "diagnostics": self.diagnostics.to_json(),
        )
    }
}
