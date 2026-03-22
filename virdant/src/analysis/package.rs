use bstr::BStr;
use bstr::BString;
use hashbrown::HashSet;
use std::collections::HashMap;
use std::sync::Arc;

use crate::common::ComponentKind;
use crate::common::json::ToJson;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::source::Region;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

use super::location::Location;

#[derive(Debug)]
pub struct PackageAnalysis {
    package: PackageFqn,
    imports: HashSet<PackageFqn>,
    items: HashMap<BString, Vec<AstNodeId>>,
    expr_roots: Vec<AstNodeId>,
    diagnostics: Vec<Diagnostic>,
}

impl PackageAnalysis {
    pub fn new(parsing: Arc<Parsing>) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            package: parsing.package(),
            imports: vec![PackageFqn::new("builtin".into())].into_iter().collect(),
            items: HashMap::new(),
            expr_roots: vec![],
            diagnostics: vec![],
        };

        analysis.propagate_diagnostics(parsing.clone());
        analysis.add_imports(parsing.clone());
        analysis.add_items(parsing.clone());

        analysis
    }

    pub fn package(&self) -> PackageFqn {
        self.package.clone()
    }

    pub fn imports(&self) -> Vec<PackageFqn> {
        self.imports.iter().cloned().collect()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn item_names(&self) -> Vec<&BString> {
        self.items.keys().collect()
    }

    pub fn expr_roots_node_ids(&self) -> Vec<AstNodeId> {
        self.expr_roots.clone()
    }

    pub fn item_ast_node_id(&self, item_name: &BStr) -> AstNodeId {
        if let Some(items) = self.items.get(item_name) {
            let item_ast_id = &items[0];
            return *item_ast_id;
        }

        panic!("No such item: {item_name}")
    }

    fn propagate_diagnostics(&mut self, parsing: Arc<Parsing>) {
        self.diagnostics = parsing.diagnostics();
    }

    fn add_imports(&mut self, parsing: Arc<Parsing>) {
        let root = parsing.root();
        for child_node in root.children() {
            if let AstNodePayload::Import(import) = child_node.payload() {
                let package = PackageFqn::new(parsing.string(import.package).into());
                if !self.imports.insert(package) {
                    let imported_package = PackageFqn::new(parsing.string(child_node.import_package().unwrap()).to_owned());
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

                self.add_item_expr_roots(parsing.clone(), child_node);
            }
        }
    }



    fn add_item_expr_roots(&mut self, parsing: Arc<Parsing>, node: AstNode<'_>) {
        if matches!(node.payload(), AstNodePayload::ModDef(_)) {
            if node.contains_errors() {
                return;
            }
            for child_node in node.children() {
                match child_node.payload() {
                    AstNodePayload::Component(component) => {
                        if component.kind == ComponentKind::Reg {
                            let node_id = child_node.clock().unwrap().id();
                            self.expr_roots.push(node_id);
                        }
                    }
                    AstNodePayload::Driver(driver) => {
                        let node_id = child_node.driver().unwrap().id();
                        self.expr_roots.push(node_id);
                    }
                    AstNodePayload::Module(_module) => (),
                    AstNodePayload::Socket(_socket) => (),
                    AstNodePayload::BidirectionalDriver => (),
                    AstNodePayload::ModDefStmtOn => {
                        let node_id = child_node.clock().unwrap().id();
                        self.expr_roots.push(node_id);
                        for command_node in child_node.children().into_iter().skip(1) {
                            if matches!(command_node.payload(), AstNodePayload::CommandAssert) {
                                self.expr_roots.push(command_node.child(0).id());
                            } else if matches!(command_node.payload(), AstNodePayload::CommandDisplay) {
                                self.expr_roots.push(command_node.child(0).id());
                            }
                        }
                    }
                    AstNodePayload::Error => (), // TODO should we even have error nodes at this point?
                    _ => unreachable!("{:?}", child_node.summary()),
                }
            }
        } else {
            // TODO handle FnDefs
        }
    }
}

#[cfg(test)]
#[test]
fn test_package_analysis() {
    use crate::tests::EXAMPLES_DIR;
    use crate::source::Source;
    use crate::syntax::parsing::parse;

    let source = Source::load_file(EXAMPLES_DIR.join("basic.vir"));
    let parsing = Arc::new(parse(&source));
    let analysis = PackageAnalysis::new(parsing.clone());

    dbg!(&analysis);

    eprintln!("Top AST:");
    let node_id = analysis.item_ast_node_id("Top".into());
    parsing.ast_node(node_id).dump();
    eprintln!();

    eprintln!("Foo AST:");
    let node_id = analysis.item_ast_node_id("Foo".into());
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
