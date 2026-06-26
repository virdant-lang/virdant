use bstr::BStr;
use bstr::BString;
use indexmap::IndexSet;
use indexmap::IndexMap;
use std::sync::Arc;

use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::common::source::Region;
use crate::syntax::ast::{AstNode, AstNodeId, match_arm_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_package_analysis(builder: &mut Builder, package: PackageFqn) -> Arc<PackageAnalysis> {
    let parsing = builder.get_parsing(package);
    let packages = builder.get_packages();
    let analysis = PackageAnalysis::new(&packages, parsing.clone());
    Arc::new(analysis)
}


#[derive(Debug)]
pub struct PackageAnalysis {
    package: PackageFqn,
    imports: IndexSet<PackageFqn>,
    items: IndexMap<BString, Vec<AstNodeId>>,
    expr_roots: Vec<AstNodeId>,
    diagnostics: Vec<Diagnostic>,
}

impl PackageAnalysis {
    pub fn new(packages: &[PackageFqn], parsing: Arc<Parsing>) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            package: parsing.package(),
            imports: vec![PackageFqn::new("builtin".into())].into_iter().collect(),
            items: IndexMap::new(),
            expr_roots: vec![],
            diagnostics: vec![],
        };

        analysis.add_imports(parsing.clone());
        analysis.add_items(parsing.clone());
        analysis.validate_imports(packages, parsing);
        analysis
    }

    fn validate_imports(&mut self, packages: &[PackageFqn], parsing: Arc<Parsing>) {
        let root = parsing.root();
        for child_node in root.children() {
            if let AstNodePayload::Import(import) = child_node.payload() {
                let package = PackageFqn::new(parsing.string(import.package).into());
                if !packages.contains(&package) {
                    self.diagnostics.push(diagnostics::UnresolvedImportError {
                        region: Region::new(self.package(), child_node.span()),
                        imported_package: package,
                    }.into());
                }
            }
        }
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
                        if component.kind == ComponentKind::Reg || component.kind == ComponentKind::OutgoingReg {
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
                        for child in child_node.children() {
                            if matches!(child.payload(), AstNodePayload::It) {
                                let block = &child.children()[0];
                                self.add_moddefstmt_block_expr_roots(block);
                            }
                        }
                    }
                    AstNodePayload::Driver(_driver) => {
                        let node_id = child_node.driver().unwrap().id();
                        self.expr_roots.push(node_id);
                    }
                    AstNodePayload::Submodule(_module) => {
                        let children = child_node.children();
                        if children.len() == 3 {
                            let it_block = &children[2];
                            if matches!(it_block.payload(), AstNodePayload::It) {
                                let block = &it_block.children()[0];
                                self.add_moddefstmt_block_expr_roots(block);
                            }
                        }
                    }
                    AstNodePayload::Socket(_socket) => {
                        for child in child_node.children() {
                            if matches!(child.payload(), AstNodePayload::It) {
                                let block = &child.children()[0];
                                self.add_moddefstmt_block_expr_roots(block);
                            }
                        }
                    }
                    AstNodePayload::BidirectionalDriver => (),

                    AstNodePayload::ModDefStmtWhen => {
                        self.add_moddefstmtwhen_expr_roots(child_node);
                    }
                    AstNodePayload::ModDefStmtMatch => {
                        self.add_moddefstmtmatch_expr_roots(child_node);
                    }
                    AstNodePayload::ModDefStmtUnused => (),
                    AstNodePayload::Error => (), // TODO should we even have error nodes at this point?
                    AstNodePayload::Annotations(_) => (),
                    _ => unreachable!("{:?}", child_node.summary()),
                }
            }
        } else if matches!(node.payload(), AstNodePayload::EnumDef(_)) {
            if node.contains_errors() {
                return;
            }
            for child_node in node.children() {
                if let AstNodePayload::Enumerant(_) = child_node.payload() {
                    let node_id = child_node.child(1).id();
                    self.expr_roots.push(node_id);
                }
            }
        } else {
            // TODO handle FnDefs
        }
    }

    fn add_moddefstmtwhen_expr_roots(&mut self, when_node: AstNode<'_>) {
        // Children: [guard_0?, body_0, guard_1?, body_1, ...]
        // case arms have guard + body (2 children), else arms have only body (1 child)
        let children = when_node.children();
        let mut idx = 0;
        while let Some(first) = children.get(idx) {
            if first.is_expr() {
                // case arm: guard + body
                self.expr_roots.push(first.id());
                if let Some(body) = children.get(idx + 1) {
                    if matches!(body.payload(), AstNodePayload::ModDefStmtBlock) {
                        self.add_moddefstmt_block_expr_roots(body);
                    } else {
                        // nested when/match - recurse
                        if matches!(body.payload(), AstNodePayload::ModDefStmtWhen) {
                            self.add_moddefstmtwhen_expr_roots(body.clone());
                        } else if matches!(body.payload(), AstNodePayload::ModDefStmtMatch) {
                            self.add_moddefstmtmatch_expr_roots(body.clone());
                        }
                        // bare expr arms don't add expr roots (they are not driver exprs)
                    }
                }
                idx += 2;
            } else {
                // else arm: body only
                if matches!(first.payload(), AstNodePayload::ModDefStmtBlock) {
                    self.add_moddefstmt_block_expr_roots(first);
                } else if matches!(first.payload(), AstNodePayload::ModDefStmtWhen) {
                    self.add_moddefstmtwhen_expr_roots(first.clone());
                } else if matches!(first.payload(), AstNodePayload::ModDefStmtMatch) {
                    self.add_moddefstmtmatch_expr_roots(first.clone());
                }
                idx += 1;
            }
        }
    }

    fn add_moddefstmtmatch_expr_roots(&mut self, match_node: AstNode<'_>) {
        // Children: [subject, arm_0, arm_1, ...] where each `case` arm contributes
        // (pattern, body) and each `else` arm contributes (body) only.
        // Body can be a block, a when, or a match.
        let children = match_node.children();
        // The subject expression is an expr root.
        self.expr_roots.push(children[0].id());
        // Recurse into each arm's body (case or else).
        for (_pat_opt, body) in match_arm_children(&children) {
            match body.payload() {
                AstNodePayload::ModDefStmtBlock => {
                    self.add_moddefstmt_block_expr_roots(body);
                }
                AstNodePayload::ModDefStmtWhen => {
                    self.add_moddefstmtwhen_expr_roots(body.clone());
                }
                AstNodePayload::ModDefStmtMatch => {
                    self.add_moddefstmtmatch_expr_roots(body.clone());
                }
                _ => {}
            }
        }
    }

    fn add_moddefstmt_block_expr_roots(&mut self, block_node: &AstNode<'_>) {
        for stmt in block_node.children() {
            match stmt.payload() {
                AstNodePayload::Driver(_) => {
                    let driver_expr = stmt.driver().unwrap();
                    self.expr_roots.push(driver_expr.id());
                    // Trace into ExprWhen/ExprMatch nodes inside the driver expression
                    self.add_expr_when_match_roots(driver_expr);
                }
                AstNodePayload::ModDefStmtWhen => {
                    self.add_moddefstmtwhen_expr_roots(stmt);
                }
                AstNodePayload::ModDefStmtMatch => {
                    self.add_moddefstmtmatch_expr_roots(stmt);
                }
                _ => {}
            }
        }
    }

    fn add_expr_when_match_roots(&mut self, expr_node: AstNode<'_>) {
        match expr_node.payload() {
            AstNodePayload::ExprWhen => {
                // ExprWhen children: [guard_0?, body_0, guard_1?, body_1, ...]
                let children = expr_node.children();
                let mut idx = 0;
                while let Some(first) = children.get(idx) {
                    if first.is_expr() {
                        // case arm: guard + body
                        self.expr_roots.push(first.id());
                        self.add_expr_when_match_roots(first.clone());
                        if let Some(body) = children.get(idx + 1) {
                            if body.is_expr() {
                                self.add_expr_when_match_roots(body.clone());
                            }
                        }
                        idx += 2;
                    } else {
                        // else arm: body only
                        if first.is_expr() {
                            self.add_expr_when_match_roots(first.clone());
                        }
                        idx += 1;
                    }
                }
            }
            AstNodePayload::ExprMatch => {
                // ExprMatch children: [subject, arm_0, arm_1, ...]
                let children = expr_node.children();
                // The subject expression is an expr root
                self.expr_roots.push(children[0].id());
                self.add_expr_when_match_roots(children[0].clone());
                // Each arm is either (pattern, body) or (body) for else
                for (_pat_opt, body) in match_arm_children(&children) {
                    if body.is_expr() {
                        self.add_expr_when_match_roots(body.clone());
                    }
                }
            }
            _ => {
                // Recurse into all children to find nested ExprWhen/ExprMatch
                for child in expr_node.children() {
                    if child.is_expr() {
                        self.add_expr_when_match_roots(child);
                    }
                }
            }
        }
    }


}

