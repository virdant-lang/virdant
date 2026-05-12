use bstr::BStr;
use bstr::BString;
use indexmap::IndexSet;
use indexmap::IndexMap;
use std::sync::Arc;

use crate::common::ComponentKind;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::common::source::Region;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;


#[derive(Debug)]
pub struct PackageAnalysis {
    package: PackageFqn,
    imports: IndexSet<PackageFqn>,
    items: IndexMap<BString, Vec<AstNodeId>>,
    expr_roots: Vec<AstNodeId>,
    diagnostics: Vec<Diagnostic>,
}

impl PackageAnalysis {
    pub fn new(parsing: Arc<Parsing>) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            package: parsing.package(),
            imports: vec![PackageFqn::new("builtin".into())].into_iter().collect(),
            items: IndexMap::new(),
            expr_roots: vec![],
            diagnostics: vec![],
        };

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
                let items = self.items.get_mut(&name).unwrap();
                items.push(child_node.id());

                // Skip adding exprroots for items which contain syntax errors
                // This suppresses typechecking for the item.
                let has_syntax_errors = child_node.contains_errors();
                if !has_syntax_errors {
                    self.add_item_expr_roots(parsing.clone(), child_node);
                }
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
                            if let Some(node) = child_node.clock() {
                                self.expr_roots.push(node.id());
                            } else {
                                self.diagnostics.push(
                                    diagnostics::MissingOnClause {
                                        region: node.region(),
                                        component: parsing.string(component.name).into(),
                                    }.into()
                                );
                            }
                        }
                    }
                    AstNodePayload::Driver(_driver) => {
                        let node_id = child_node.driver().unwrap().id();
                        self.expr_roots.push(node_id);
                    }
                    AstNodePayload::Submodule(_module) => (),
                    AstNodePayload::Socket(_socket) => (),
                    AstNodePayload::BidirectionalDriver => (),

                    AstNodePayload::ModDefStmtIf => {
                        self.add_moddefstmtif_expr_roots(child_node);
                    }
                    AstNodePayload::ModDefStmtMatch => {
                        self.add_moddefstmtmatch_expr_roots(child_node);
                    }
                    AstNodePayload::Error => (), // TODO should we even have error nodes at this point?
                    _ => unreachable!("{:?}", child_node.summary()),
                }
            }
        } else if matches!(node.payload(), AstNodePayload::EnumDef(_)) {
            if node.contains_errors() {
                return;
            }
            for child_node in node.children() {
                if let AstNodePayload::Enumerant(_) = child_node.payload() {
                    let node_id = child_node.child(0).id();
                    self.expr_roots.push(node_id);
                }
            }
        } else {
            // TODO handle FnDefs
        }
    }

    fn add_moddefstmtif_expr_roots(&mut self, if_node: AstNode<'_>) {
        // Children: [cond_0, block_0, cond_1, block_1, ..., (else_block?)]
        // An else block is present when the total number of children is odd.
        let children = if_node.children();
        let has_else = children.len() % 2 == 1;
        let num_cond_block_pairs = children.len() / 2;

        for i in 0..num_cond_block_pairs {
            // Each condition expression is an expr root.
            self.expr_roots.push(children[2 * i].id());
            // Recurse into the corresponding block.
            self.add_moddefstmt_block_expr_roots(&children[2 * i + 1]);
        }

        // Recurse into the else block only if one exists.
        if has_else {
            self.add_moddefstmt_block_expr_roots(children.last().unwrap());
        }
    }

    fn add_moddefstmtmatch_expr_roots(&mut self, match_node: AstNode<'_>) {
        // Children: [subject, pat_0, block_0, pat_1, block_1, ..., pat_N, block_N]
        let children = match_node.children();
        let num_arms = (children.len() - 1) / 2;
        // The subject expression is an expr root.
        self.expr_roots.push(children[0].id());
        // Recurse into each arm's block.
        for i in 0..num_arms {
            self.add_moddefstmt_block_expr_roots(&children[2 * i + 2]);
        }
    }

    fn add_moddefstmt_block_expr_roots(&mut self, block_node: &AstNode<'_>) {
        for stmt in block_node.children() {
            match stmt.payload() {
                AstNodePayload::Driver(_) => {
                    self.expr_roots.push(stmt.driver().unwrap().id());
                }
                AstNodePayload::ModDefStmtIf => {
                    self.add_moddefstmtif_expr_roots(stmt);
                }
                AstNodePayload::ModDefStmtMatch => {
                    self.add_moddefstmtmatch_expr_roots(stmt);
                }
                _ => {}
            }
        }
    }


}

