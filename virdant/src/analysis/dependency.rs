//! Builds the `DependencyGraph`, a directed graph over a module's
//! components whose edges record combinational vs. sequential
//! dependencies, derived from component and driver analysis by walking
//! the AST.

use std::sync::Arc;

use bstr::BStr;
use bstr::BString;
use bstr::ByteSlice;
use indexmap::IndexMap;
use indexmap::IndexSet;

use crate::analysis::component::Component;
use crate::analysis::component::ComponentAnalysis;
use crate::analysis::component::ComponentId;
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType};
use crate::db::Builder;
use crate::syntax::ast::{match_arm_children, if_arm_children, AstNode};
use crate::syntax::payload::AstNodePayload;

// ---------------------------------------------------------------------------
// Data structures
// ---------------------------------------------------------------------------

/// A `let` environment: bound name -> the components referenced by the
/// binding's RHS (beta-reduced through earlier bindings).  References
/// to a bound name inside the `let`'s body contribute edges to the
/// binding's references instead of the name itself (which is not a
/// component).
pub(crate) type LetEnv = IndexMap<BString, Vec<ComponentId>>;

/// A dependency graph over the signals of a single module.
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    /// All components in the module.
    nodes: IndexSet<ComponentId>,

    /// For each component, the components it depends on.
    edges: IndexMap<ComponentId, Vec<DependencyEdge>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependencyEdge {
    pub dependee: ComponentId,
    pub kind: EdgeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EdgeKind {
    /// Combinational: source changes in the same cycle when dependee
    /// changes.
    Combinational,

    /// Sequential: source reflects dependee's value at the next clock
    /// tick.
    Sequential,
}

impl DependencyGraph {
    pub fn nodes(&self) -> &IndexSet<ComponentId> {
        &self.nodes
    }

    pub fn edges(&self) -> &IndexMap<ComponentId, Vec<DependencyEdge>> {
        &self.edges
    }

    pub fn dependencies_of(&self, component: ComponentId) -> Option<&[DependencyEdge]> {
        self.edges.get(&component).map(|v| v.as_slice())
    }
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

pub(crate) fn build_dependency_graph(
    builder: &mut Builder,
    symbol_id: SymbolId,
) -> Arc<DependencyGraph> {
    let component_analysis = builder.get_component_analysis(symbol_id);
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(symbol_id).location();
    let parsing = builder.get_parsing(location.package());
    let item_ast = parsing.ast_node(location.ast_node_id());

    // Collect all components as nodes.
    let mut nodes: IndexSet<ComponentId> = IndexSet::new();
    for (_, component) in component_analysis.components() {
        nodes.insert(component.id());
    }

    // Collect edges.
    let mut edges: IndexMap<ComponentId, Vec<DependencyEdge>> = IndexMap::new();

    // 1. Edges from drivers.
    collect_driver_edges(
        &item_ast.children(),
        &component_analysis,
        &mut edges,
        None,
        &[],
        None,
    );

    // 2. Edges from platform `default <EXPR>` drivers: a synthesized
    // outgoing port has no driver statement in the module's own AST;
    // its driver is the platform's default expression, which may
    // reference components.
    for (_path, component) in component_analysis.components() {
        if !component.synthetic() {
            continue;
        }
        if component.kind() != Some(ComponentKind::Outgoing) {
            continue;
        }
        let Some(expr_location) = component.default_expr() else { continue };
        let expr_parsing = builder.get_parsing(expr_location.package());
        let expr_node = expr_parsing.ast_node(expr_location.ast_node_id());
        collect_references_in_expr(
            &expr_node,
            &component_analysis,
            &mut edges,
            component.id(),
            EdgeKind::Combinational,
            None,
            None,
        );
    }

    Arc::new(DependencyGraph { nodes, edges })
}

// ---------------------------------------------------------------------------
// Collect edges from drivers
// ---------------------------------------------------------------------------

/// Walk all driver statements and their sub-structures (if, match) to
/// record edges from the target component to each component referenced
/// in the RHS expression.
///
/// `cond_refs` carries the components referenced by the guards/subjects
/// of the enclosing `if`/`match` statements: every driver inside an
/// arm depends not only on its RHS but also on the arm's condition, so
/// a driver target that appears in its own guard forms a
/// combinational loop.
#[allow(clippy::too_many_arguments)]
fn collect_driver_edges(
    stmts: &[AstNode<'_>],
    component_analysis: &ComponentAnalysis,
    edges: &mut IndexMap<ComponentId, Vec<DependencyEdge>>,
    it_context: Option<&BStr>,
    cond_refs: &[ComponentId],
    let_env: Option<&LetEnv>,
) {
    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::Driver(driver) => {
                let Some(target) = stmt.target() else { continue };
                let mut target_str = stmt.parsing.string(target).to_owned();
                resolve_it_path(&mut target_str, it_context);
                let Some(target_component) =
                    component_analysis.resolve(target_str.as_bstr())
                else {
                    continue;
                };

                let edge_kind = match driver.driver_type {
                    DriverType::Continuous => EdgeKind::Combinational,
                    DriverType::Latched => EdgeKind::Sequential,
                };

                // Walk the RHS expression for ExprReference nodes.
                if let Some(expr_node) = stmt.driver() {
                    collect_references_in_expr(
                        &expr_node,
                        component_analysis,
                        edges,
                        target_component.id(),
                        edge_kind,
                        it_context,
                        let_env,
                    );
                }

                // The target also depends on every guard/subject of the
                // enclosing `if`/`match` statements.
                for cond in cond_refs {
                    add_edge(edges, target_component.id(), *cond, edge_kind);
                }
            }
            AstNodePayload::BidirectionalDriver => {
                let parsing = stmt.parsing;
                let lhs_node = stmt.child(0);
                let rhs_node = stmt.child(1);
                let Some(lhs_path) = lhs_node.path() else { continue };
                let Some(rhs_path) = rhs_node.path() else { continue };
                let mut lhs_str = parsing.string(lhs_path).to_owned();
                let mut rhs_str = parsing.string(rhs_path).to_owned();
                resolve_it_path(&mut lhs_str, it_context);
                resolve_it_path(&mut rhs_str, it_context);

                let lhs_prefix = format!("{}.", lhs_str.to_str_lossy());
                let rhs_prefix = format!("{}.", rhs_str.to_str_lossy());

                for (lhs_full_path, lhs_component) in component_analysis.components() {
                    let lhs_path_str = lhs_full_path.to_str_lossy();
                    if !lhs_path_str.starts_with(&lhs_prefix) {
                        continue;
                    }
                    let suffix = &lhs_path_str[lhs_prefix.len()..];
                    let rhs_full_path = BString::from(
                        format!("{}{}", rhs_prefix, suffix).into_bytes(),
                    );
                    let Some(rhs_component) =
                        component_analysis.resolve(rhs_full_path.as_bstr())
                    else {
                        continue;
                    };
                    // A socket link transfers values along each channel:
                    // the receiving side combinationally depends on the
                    // driving side.  The driving side is the one whose
                    // *owner* drives the channel (client for `cosi`, server
                    // for `soci`), which is exactly the side with component
                    // kind `Outgoing`; the receiving side has kind
                    // `Incoming`.  `Flow` cannot decide the direction: it is
                    // module-centric (Source = the value enters this
                    // module's scope), so for a link between a module's own
                    // sockets the receiver is the `Source` side, while for a
                    // link between two submodule instances' sockets the
                    // receiver is the `Sink` side.  The kinds agree in both
                    // cases.  Degenerate pairs (both sides driving or both
                    // reading the channel) get no edge.
                    match (lhs_component.kind(), rhs_component.kind()) {
                        (Some(ComponentKind::Outgoing), Some(ComponentKind::Incoming)) => {
                            add_edge(
                                edges,
                                rhs_component.id(),
                                lhs_component.id(),
                                EdgeKind::Combinational,
                            );
                        }
                        (Some(ComponentKind::Incoming), Some(ComponentKind::Outgoing)) => {
                            add_edge(
                                edges,
                                lhs_component.id(),
                                rhs_component.id(),
                                EdgeKind::Combinational,
                            );
                        }
                        _ => {}
                    }
                }
            }
            AstNodePayload::Submodule(submodule) => {
                // Drivers of the instance's ports written in its it-block
                // (`mod chain of RegChain { it.clock := clock }`).  Locate
                // the block by payload: the node's children also carry
                // the annotations and the `of` clause.
                let name = stmt.parsing.string(submodule.name);
                let it_block = stmt
                    .children()
                    .into_iter()
                    .find(|c| matches!(c.payload(), AstNodePayload::It));
                if let Some(it_block) = it_block {
                    let block = &it_block.children()[0];
                    let name_with_context = if let Some(ctx) = it_context {
                        let mut new_name = BString::from(ctx);
                        new_name.push(b'.');
                        new_name.extend_from_slice(name);
                        new_name
                    } else {
                        BString::from(name)
                    };
                    collect_driver_edges(
                        &block.children(),
                        component_analysis,
                        edges,
                        Some(name_with_context.as_bstr()),
                        cond_refs,
                        let_env,
                    );
                }
            }
            AstNodePayload::Socket(socket) => {
                let name = stmt.parsing.string(socket.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];
                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };
                        collect_driver_edges(
                            &block.children(),
                            component_analysis,
                            edges,
                            Some(name_with_context.as_bstr()),
                            cond_refs,
                            let_env,
                        );
                    }
                }
            }
            AstNodePayload::Component(component) => {
                let name = stmt.parsing.string(component.name);
                for child in stmt.children() {
                    if matches!(child.payload(), AstNodePayload::It) {
                        let block = &child.children()[0];
                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };
                        collect_driver_edges(
                            &block.children(),
                            component_analysis,
                            edges,
                            Some(name_with_context.as_bstr()),
                            cond_refs,
                            let_env,
                        );
                    }
                }
            }
            AstNodePayload::If => {
                let children = stmt.children();
                for (guard, body) in if_arm_children(&children) {
                    // A `when` arm's drivers also depend on the guard's
                    // referenced components; an `else` arm only on the
                    // conditions of enclosing `if`/`match` statements.
                    let mut arm_cond_refs: Vec<ComponentId> =
                        cond_refs.to_vec();
                    if let Some(guard_node) = guard {
                        arm_cond_refs.extend(collect_cond_refs_in_expr(
                            guard_node,
                            component_analysis,
                            it_context,
                            let_env,
                        ));
                    }
                    let body_stmts = if matches!(
                        body.payload(),
                        AstNodePayload::ArmBlock
                    ) {
                        body.children()
                    } else {
                        vec![body.clone()]
                    };
                    collect_driver_edges(
                        &body_stmts,
                        component_analysis,
                        edges,
                        it_context,
                        &arm_cond_refs,
                        let_env,
                    );
                }
            }
            AstNodePayload::Match => {
                let children = stmt.children();
                // The subject (child 0) conditions every arm's drivers.
                let mut arm_cond_refs: Vec<ComponentId> = cond_refs.to_vec();
                if let Some(subject) = children.first() {
                    arm_cond_refs.extend(collect_cond_refs_in_expr(
                        subject,
                        component_analysis,
                        it_context,
                        let_env,
                    ));
                }
                for (_pat_opt, body) in match_arm_children(&children) {
                    let body_stmts = if matches!(
                        body.payload(),
                        AstNodePayload::ArmBlock
                    ) {
                        body.children()
                    } else {
                        vec![body.clone()]
                    };
                    collect_driver_edges(
                        &body_stmts,
                        component_analysis,
                        edges,
                        it_context,
                        &arm_cond_refs,
                        let_env,
                    );
                }
            }
            AstNodePayload::ModDefStmtBlock => {
                collect_driver_edges(
                    &stmt.children(),
                    component_analysis,
                    edges,
                    it_context,
                    cond_refs,
                    let_env,
                );
            }
            AstNodePayload::Let => {
                // A `let` is a pure scoping construct: it contributes
                // no drivers itself, and its body's drivers belong to
                // the enclosing context (the block is not
                // conditional).  References to the bound names are
                // beta-reduced to the binding's RHS references, so
                // drivers inside the body record their dependencies on
                // the RHS's components (transitively, through chained
                // bindings).
                let bindings = stmt.let_bindings();
                let mut env: LetEnv = IndexMap::new();
                for binding in &bindings {
                    let Some(rhs) = binding.let_binding_expr() else { continue };
                    let refs = collect_cond_refs_in_expr(
                        &rhs,
                        component_analysis,
                        it_context,
                        Some(&env),
                    );
                    if let Some(name) = binding.let_binding_name() {
                        let name: BString = binding.parsing.string(name).to_owned();
                        env.insert(name, refs);
                    }
                }
                let Some(body) = stmt.let_body() else { return };
                let body_stmts = if matches!(body.payload(), AstNodePayload::ArmBlock) {
                    body.children()
                } else {
                    vec![body]
                };
                collect_driver_edges(
                    &body_stmts,
                    component_analysis,
                    edges,
                    it_context,
                    cond_refs,
                    Some(&env),
                );
            }
            _ => {}
        }
    }
}

/// Walk an expression subtree and collect every `ExprReference` that
/// resolves to a known component, without adding edges. Used to record
/// the components referenced by `when` guards and `match` subjects,
/// which condition every driver inside their arms.
fn collect_cond_refs_in_expr(
    expr: &AstNode<'_>,
    component_analysis: &ComponentAnalysis,
    it_context: Option<&BStr>,
    let_env: Option<&LetEnv>,
) -> Vec<ComponentId> {
    let parsing = expr.parsing;
    let mut out: Vec<ComponentId> = vec![];
    let mut stack: Vec<crate::syntax::ast::AstNodeId> = vec![expr.id()];
    while let Some(node_id) = stack.pop() {
        let node = parsing.ast_node(node_id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(path) = node.path() {
                let mut name = parsing.string(path).to_owned();
                resolve_it_path(&mut name, it_context);
                // A `let`-bound name beta-reduces to its binding's
                // references.  Checked first: the binding is the
                // innermost declaration of the name.
                if let Some(env) = let_env {
                    if let Some(refs) = env.get(name.as_bstr()) {
                        out.extend(refs.iter().copied());
                        continue;
                    }
                }
                // References shadowed by a match-arm pattern binding name
                // the pattern variable, not a component.
                if crate::analysis::component::is_shadowed_by_pattern(
                    parsing,
                    node_id,
                    name.as_bstr(),
                ) {
                    continue;
                }
                if let Some(component) =
                    component_analysis.resolve(name.as_bstr())
                {
                    out.push(component.id());
                }
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
    out
}

/// Walk an expression subtree and add edges for every `ExprReference`
/// that resolves to a known component.
fn collect_references_in_expr(
    expr: &AstNode<'_>,
    component_analysis: &ComponentAnalysis,
    edges: &mut IndexMap<ComponentId, Vec<DependencyEdge>>,
    target: ComponentId,
    edge_kind: EdgeKind,
    it_context: Option<&BStr>,
    let_env: Option<&LetEnv>,
) {
    let parsing = expr.parsing;
    let mut stack: Vec<crate::syntax::ast::AstNodeId> = vec![expr.id()];
    while let Some(node_id) = stack.pop() {
        let node = parsing.ast_node(node_id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(path) = node.path() {
                let mut name = parsing.string(path).to_owned();
                resolve_it_path(&mut name, it_context);
                // A `let`-bound name beta-reduces to its binding's
                // references.  Checked first: the binding is the
                // innermost declaration of the name (it also shadows
                // module components of the same name, and any pattern
                // binding the name is bound under).
                if let Some(env) = let_env {
                    if let Some(refs) = env.get(name.as_bstr()) {
                        for dependee in refs {
                            add_edge(edges, target, *dependee, edge_kind);
                        }
                        continue;
                    }
                }
                // References shadowed by a match-arm pattern binding name
                // the pattern variable, not a component.
                if crate::analysis::component::is_shadowed_by_pattern(
                    parsing,
                    node_id,
                    name.as_bstr(),
                ) {
                    continue;
                }
                if let Some(dependee) =
                    component_analysis.resolve(name.as_bstr())
                {
                    add_edge(edges, target, dependee.id(), edge_kind);
                }
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn add_edge(
    edges: &mut IndexMap<ComponentId, Vec<DependencyEdge>>,
    source: ComponentId,
    dependee: ComponentId,
    kind: EdgeKind,
) {
    // Self-loops are allowed: a driver whose target appears in its own
    // RHS or in the guard of an enclosing `if`/`match` genuinely
    // depends on itself, and the combinational cycle check uses those
    // self-loops to report one-component loops.
    edges
        .entry(source)
        .or_default()
        .push(DependencyEdge { dependee, kind });
}

/// Resolve `it` references in a path string using the enclosing statement
/// context.
pub(crate) fn resolve_it_path(
    path: &mut BString,
    it_context: Option<&BStr>,
) {
    if path.starts_with(b"it.") || path == b"it" {
        if let Some(ctx) = it_context {
            if path == b"it" {
                *path = ctx.to_owned();
            } else {
                let suffix: BString = path[3..].to_owned().into();
                path.clear();
                path.extend_from_slice(ctx);
                path.push(b'.');
                path.extend_from_slice(&suffix);
            }
        }
    }
}

/// Resolve `it` references in a borrowed path, returning an owned copy.
pub(crate) fn resolve_it_path_to_owned(
    path: &BStr,
    it_context: Option<&BStr>,
) -> BString {
    let mut resolved = BString::from(path);
    resolve_it_path(&mut resolved, it_context);
    resolved
}

/// Find the nearest enclosing it-context (Submodule / Component /
/// Socket) by walking up the AST from `node`.  This is the context
/// against which bare `it` / `it.*` paths are resolved when no explicit
/// it-context has been threaded down from the caller.
pub(crate) fn find_it_context(node: &AstNode<'_>) -> Option<BString> {
    let parsing = node.parsing();
    let mut current_id = node.id();
    while let Some(parent_id) = parsing.ast_node(current_id).parent {
        let parent = parsing.ast_node(parent_id);
        match parent.payload() {
            AstNodePayload::Submodule(module) => {
                return Some(parsing.string(module.name).to_owned());
            }
            AstNodePayload::Component(component) => {
                return Some(parsing.string(component.name).to_owned());
            }
            AstNodePayload::Socket(socket) => {
                return Some(parsing.string(socket.name).to_owned());
            }
            _ => {}
        }
        current_id = parent_id;
    }
    None
}

/// Resolve a Path node to a Component, handling `it` resolution.
pub(crate) fn resolve_path_component(
    node: &AstNode<'_>,
    parsing: &crate::syntax::parsing::Parsing,
    component_analysis: &ComponentAnalysis,
    it_context: Option<&BStr>,
) -> Option<Component> {
    if let Some(path_interned) = node.path() {
        let mut resolved = parsing.string(path_interned).to_owned();
        if resolved.starts_with(b"it.") || resolved == b"it" {
            if let Some(ctx) = it_context {
                if resolved == b"it" {
                    resolved = ctx.to_owned();
                } else {
                    let suffix: BString = resolved[3..].to_owned().into();
                    resolved = ctx.to_owned();
                    resolved.push(b'.');
                    resolved.extend_from_slice(&suffix);
                }
            }
        }
        return component_analysis.resolve(resolved.as_bstr());
    }
    None
}