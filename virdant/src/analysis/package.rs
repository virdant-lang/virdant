//! Builds `PackageAnalysis`, recording a package's imports, top-level
//! item names, expression roots, and unresolved-import diagnostics,
//! implicitly seeding the builtin import.

use bstr::BStr;
use bstr::BString;
use indexmap::IndexSet;
use indexmap::IndexMap;
use std::sync::Arc;

use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::package::{PackageId, PackageTable};
use crate::common::source::Region;
use crate::syntax::ast::{AstNode, AstNodeId, match_arm_children, if_arm_children};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_package_analysis(builder: &mut Builder, package: PackageId) -> Arc<PackageAnalysis> {
    let parsing = builder.get_parsing(package);
    let packages = builder.get_packages();
    let analysis = PackageAnalysis::new(&packages, parsing.clone());
    Arc::new(analysis)
}

/// Resolve an item reference written in `package`.  A qualified name
/// (`pkg::Item`) resolves in the named package only; a bare name
/// resolves in the defining package first, then across the package's
/// import set (the same fallback platform references use).  Returns
/// `None` when the name resolves nowhere.
pub(crate) fn resolve_item_reference(
    builder: &mut Builder,
    name: &BStr,
    package: PackageId,
    qualifier: Option<PackageId>,
) -> Option<crate::analysis::symbols::Symbol> {
    let symboltable = builder.get_symboltable();
    match qualifier {
        Some(pkg) => symboltable.resolve_item(name, pkg).cloned(),
        None => symboltable
            .resolve_item(name, package)
            .cloned()
            .or_else(|| {
                let package_analysis = builder.get_package_analysis(package);
                for import in package_analysis.imports() {
                    if import == package {
                        continue;
                    }
                    if let Some(symbol) = symboltable.resolve_item_in_package(name, import) {
                        return Some(symbol.clone());
                    }
                }
                None
            }),
    }
}


#[derive(Debug)]
pub struct PackageAnalysis {
    package: PackageId,
    imports: IndexSet<PackageId>,
    items: IndexMap<BString, Vec<AstNodeId>>,
    expr_roots: Vec<AstNodeId>,
    diagnostics: Vec<Diagnostic>,
}

impl PackageAnalysis {
    pub fn new(packages: &PackageTable, parsing: Arc<Parsing>) -> PackageAnalysis {
        let mut analysis = PackageAnalysis {
            package: parsing.package(),
            imports: vec![packages.builtin()].into_iter().collect(),
            items: IndexMap::new(),
            expr_roots: vec![],
            diagnostics: vec![],
        };

        analysis.add_and_validate_imports(packages, parsing.clone());
        analysis.add_items(parsing.clone());
        analysis
    }

    fn add_and_validate_imports(&mut self, packages: &PackageTable, parsing: Arc<Parsing>) {
        let root = parsing.root();
        for child_node in root.children() {
            if let AstNodePayload::Import(import) = child_node.payload() {
                let name = parsing.string(import.package);
                match packages.id(name) {
                    Some(package_id) => {
                        if !self.imports.insert(package_id) {
                            self.diagnostics.push(Diagnostic::new(
                                Region::new(self.package(), child_node.span()),
                                diagnostics::DuplicateImport {
                                    imported_package: name.to_owned(),
                                },
                            ));
                        }
                    }
                    None => {
                        self.diagnostics.push(Diagnostic::new(
                            Region::new(self.package(), child_node.span()),
                            diagnostics::UnresolvedImportError {
                                imported_package: name.to_owned(),
                            },
                        ));
                    }
                }
            }
        }
    }

    pub fn package(&self) -> PackageId {
        self.package
    }

    pub fn imports(&self) -> Vec<PackageId> {
        self.imports.iter().copied().collect()
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

    fn add_items(&mut self, parsing: Arc<Parsing>) {
        let root = parsing.root();
        for child_node in root.children() {
            if child_node.is_item() {
                let name = parsing.string(child_node.name().unwrap()).to_owned();
                if !self.items.contains_key(&name) {
                    self.items.insert(name.clone(), vec![]);
                } else {
                    let item = parsing.string(child_node.name().unwrap()).to_owned();
                    self.diagnostics.push(Diagnostic::new(
                        Region::new(self.package(), child_node.span()),
                        diagnostics::DuplicateItem {
                            item,
                        },
                    ));
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



    fn add_item_expr_roots(&mut self, _parsing: Arc<Parsing>, node: AstNode<'_>) {
        if matches!(node.payload(), AstNodePayload::ModDef(_)) {
            if node.contains_errors() {
                return;
            }
            // The `on <CLOCK>` clause of a reg is optional when the module
            // declares exactly one Clock-typed component (the implicit
            // clock); whether it is missing is decided by the component
            // analysis, which resolves the component types.
            for child_node in node.children() {
                match child_node.payload() {
                    AstNodePayload::Component(component) => {
                        if component.kind == ComponentKind::Reg || component.kind == ComponentKind::OutgoingReg {
                            if let Some(clock_expr) = child_node.clock() {
                                self.expr_roots.push(clock_expr.id());
                            }
                        }
                        for child in child_node.children() {
                            if matches!(child.payload(), AstNodePayload::It) {
                                let block = &child.children()[0];
                                self.add_moddefstmt_block_expr_roots(block);
                            }
                        }
                    }
                    AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
                        let node_id = child_node.driver().unwrap().id();
                        self.expr_roots.push(node_id);
                    }
                    AstNodePayload::Submodule(_module) => {
                        // The It-block is found by payload rather than by
                        // index, so the optional GenericsParams child of a
                        // type-argument instantiation does not shift it
                        // out from under us.
                        if let Some(it_block) = child_node
                            .children()
                            .iter()
                            .find(|c| matches!(c.payload(), AstNodePayload::It))
                        {
                            let block = &it_block.children()[0];
                            self.add_moddefstmt_block_expr_roots(block);
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
                    AstNodePayload::Ofness(_) => (),

                    AstNodePayload::If => {
                        self.add_moddefstmtif_expr_roots(child_node);
                    }
                    AstNodePayload::Match => {
                        self.add_moddefstmtmatch_expr_roots(child_node);
                    }
                    AstNodePayload::ModDefStmtUnused => (),
                    AstNodePayload::DependsonList => (),
                    AstNodePayload::Error => (), // TODO should we even have error nodes at this point?
                    AstNodePayload::Annotations(_) => (),
                    // A generic parameter declaration list on a mod def.
                    AstNodePayload::Generics => (),
                    // A `let` statement: each binding's RHS expression
                    // is an expr root (it types the binding — for untyped
                    // bindings the expected type is derived from the
                    // binding's first usage in the body, see
                    // `build_expected_type`), and the body's statements
                    // contribute their own expr roots like any block.
                    AstNodePayload::Let => {
                        for binding in child_node.let_bindings() {
                            if let Some(rhs) = binding.let_binding_expr() {
                                self.expr_roots.push(rhs.id());
                            }
                        }
                        if let Some(body) = child_node.let_body() {
                            match body.payload() {
                                AstNodePayload::ArmBlock => {
                                    self.add_moddefstmt_block_expr_roots(&body);
                                }
                                AstNodePayload::If => {
                                    self.add_moddefstmtif_expr_roots(body);
                                }
                                AstNodePayload::Match => {
                                    self.add_moddefstmtmatch_expr_roots(body);
                                }
                                _ => {
                                    // The `=>`-form body is a bare
                                    // expression: it is an expr root of
                                    // its own (unconstrained — a statement
                                    // `let` discards its value).
                                    if body.is_expr() {
                                        self.expr_roots.push(body.id());
                                    }
                                }
                            }
                        }
                    }
                    _ => unreachable!("{:?}", child_node.summary()),
                }
            }
        } else if matches!(node.payload(), AstNodePayload::Platform(_)) {
            if node.contains_errors() {
                return;
            }
            for child_node in node.children() {
                // A platform `outgoing ... default <EXPR>` clause: the
                // default expression is type-checked in the platform's
                // own context. The driver synthesis in the consuming
                // module lowers it verbatim; a type mismatch against
                // the port type is reported separately (see
                // `check_platform_default_types`).
                if let Some(expr_node) = child_node.default_expr() {
                    self.expr_roots.push(expr_node.id());
                }
            }
        } else if matches!(node.payload(), AstNodePayload::EnumDef(_)) {
            if node.contains_errors() {
                return;
            }
            for child_node in node.children() {
                if let AstNodePayload::Enumerant(_) = child_node.payload() {
                    // Enumerants without an explicit value have no expr
                    // child; the value expression, if any, is the first
                    // expression child (after the Annotations wrapper and
                    // any ctor Param children).
                    if let Some(expr_node) =
                        child_node.children().into_iter().find(|c| c.is_expr())
                    {
                        self.expr_roots.push(expr_node.id());
                    }
                }
            }
        } else {
            // TODO handle FnDefs
        }
    }

    fn add_moddefstmtif_expr_roots(&mut self, if_node: AstNode<'_>) {
        // Only the guard expressions are expr roots (each must be a
        // Bit).  Arm bodies may be blocks (recurse into their
        // statements) or nested if/match (recurse); bare `=>`-arm
        // expression bodies are style errors flagged by the if/match
        // style pass and are not expr roots.
        let children = if_node.children();
        for (guard, body) in if_arm_children(&children) {
            if let Some(guard) = guard {
                self.expr_roots.push(guard.id());
            }
            match body.payload() {
                AstNodePayload::ArmBlock => {
                    self.add_moddefstmt_block_expr_roots(body);
                }
                AstNodePayload::If => {
                    self.add_moddefstmtif_expr_roots(body.clone());
                }
                AstNodePayload::Match => {
                    self.add_moddefstmtmatch_expr_roots(body.clone());
                }
                AstNodePayload::Let => {
                    // A `let` as an arm body: its bindings' RHSs are
                    // expr roots, and its body block is processed like
                    // any other statement block.
                    for binding in body.let_bindings() {
                        if let Some(rhs) = binding.let_binding_expr() {
                            self.expr_roots.push(rhs.id());
                        }
                    }
                    if let Some(let_body) = body.let_body() {
                        match let_body.payload() {
                            AstNodePayload::ArmBlock => {
                                self.add_moddefstmt_block_expr_roots(&let_body);
                            }
                            AstNodePayload::If => {
                                self.add_moddefstmtif_expr_roots(let_body);
                            }
                            AstNodePayload::Match => {
                                self.add_moddefstmtmatch_expr_roots(let_body);
                            }
                            _ => {
                                if let_body.is_expr() {
                                    self.expr_roots.push(let_body.id());
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn add_moddefstmtmatch_expr_roots(&mut self, match_node: AstNode<'_>) {
        // Children: [subject, arm_0, arm_1, ...] where each `with` arm contributes
        // (pattern, body) and each `else` arm contributes (body) only.
        // Body can be a block, an if, or a match.
        let children = match_node.children();
        // The subject expression is an expr root.
        self.expr_roots.push(children[0].id());
        // Recurse into each arm's body (case or else).
        for (_pat_opt, body) in match_arm_children(&children) {
            match body.payload() {
                AstNodePayload::ArmBlock => {
                    self.add_moddefstmt_block_expr_roots(body);
                }
                AstNodePayload::If => {
                    self.add_moddefstmtif_expr_roots(body.clone());
                }
                AstNodePayload::Match => {
                    self.add_moddefstmtmatch_expr_roots(body.clone());
                }
                AstNodePayload::Let => {
                    // A `let` as an arm body: its bindings' RHSs are
                    // expr roots, and its body block is processed like
                    // any other statement block.
                    for binding in body.let_bindings() {
                        if let Some(rhs) = binding.let_binding_expr() {
                            self.expr_roots.push(rhs.id());
                        }
                    }
                    if let Some(let_body) = body.let_body() {
                        match let_body.payload() {
                            AstNodePayload::ArmBlock => {
                                self.add_moddefstmt_block_expr_roots(&let_body);
                            }
                            AstNodePayload::If => {
                                self.add_moddefstmtif_expr_roots(let_body);
                            }
                            AstNodePayload::Match => {
                                self.add_moddefstmtmatch_expr_roots(let_body);
                            }
                            _ => {
                                if let_body.is_expr() {
                                    self.expr_roots.push(let_body.id());
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn add_moddefstmt_block_expr_roots(&mut self, block_node: &AstNode<'_>) {
        for stmt in block_node.children() {
            match stmt.payload() {
                AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
                    let driver_expr = stmt.driver().unwrap();
                    self.expr_roots.push(driver_expr.id());
                    // Trace into When/Match nodes inside the driver expression
                    self.add_expr_if_match_roots(driver_expr);
                }
                AstNodePayload::If => {
                    self.add_moddefstmtif_expr_roots(stmt);
                }
                AstNodePayload::Match => {
                    self.add_moddefstmtmatch_expr_roots(stmt);
                }
                AstNodePayload::Let => {
                    // A `let` nested inside a driver block or if/match
                    // arm: same treatment as at moddef level.
                    for binding in stmt.let_bindings() {
                        if let Some(rhs) = binding.let_binding_expr() {
                            self.expr_roots.push(rhs.id());
                        }
                    }
                    if let Some(body) = stmt.let_body() {
                        match body.payload() {
                            AstNodePayload::ArmBlock => {
                                self.add_moddefstmt_block_expr_roots(&body);
                            }
                            AstNodePayload::If => {
                                self.add_moddefstmtif_expr_roots(body);
                            }
                            AstNodePayload::Match => {
                                self.add_moddefstmtmatch_expr_roots(body);
                            }
                            _ => {
                                if body.is_expr() {
                                    self.expr_roots.push(body.id());
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn add_expr_if_match_roots(&mut self, expr_node: AstNode<'_>) {
        match expr_node.payload() {
            AstNodePayload::If => {
                // When children: [guard_0?, body_0, guard_1?, body_1, ...].
                // Use the positional arm iterator: an `else` arm's body is
                // also an expression, so "first child is an expr" cannot
                // distinguish a case guard from an else body (a trailing
                // `else if {...}`/`else match {...}` body would
                // otherwise be misclassified as a guard and pushed as an
                // expr root with a guard's expected type).
                use crate::syntax::ast::if_arm_children;
                let children = expr_node.children();
                for (guard, body) in if_arm_children(&children) {
                    if let Some(guard) = guard {
                        self.expr_roots.push(guard.id());
                        self.add_expr_if_match_roots(guard.clone());
                    }
                    if body.is_expr() {
                        self.add_expr_if_match_roots(body.clone());
                    }
                }
            }
            AstNodePayload::Match => {
                // Match children: [subject, arm_0, arm_1, ...]
                let children = expr_node.children();
                // The subject expression is an expr root
                self.expr_roots.push(children[0].id());
                self.add_expr_if_match_roots(children[0].clone());
                // Each arm is either (pattern, body) or (body) for else
                for (_pat_opt, body) in match_arm_children(&children) {
                    if body.is_expr() {
                        self.add_expr_if_match_roots(body.clone());
                    }
                }
            }
            _ => {
                // Recurse into all children to find nested When/Match
                for child in expr_node.children() {
                    if child.is_expr() {
                        self.add_expr_if_match_roots(child);
                    }
                }
            }
        }
    }


}

