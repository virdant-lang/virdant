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
use crate::common::{DriverType, Flow};
use crate::db::Builder;
use crate::syntax::ast::{match_arm_children, AstNode};
use crate::syntax::payload::AstNodePayload;

// ---------------------------------------------------------------------------
// Data structures
// ---------------------------------------------------------------------------

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

    #[allow(unused)]
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
    );

    // 2. Edges from `dependson` statements.
    collect_dependson_edges(
        &item_ast.children(),
        &component_analysis,
        &mut edges,
        None,
    );

    Arc::new(DependencyGraph { nodes, edges })
}

// ---------------------------------------------------------------------------
// Collect edges from drivers
// ---------------------------------------------------------------------------

/// Walk all driver statements and their sub-structures (when, match) to
/// record edges from the target component to each component referenced
/// in the RHS expression.
fn collect_driver_edges(
    stmts: &[AstNode<'_>],
    component_analysis: &ComponentAnalysis,
    edges: &mut IndexMap<ComponentId, Vec<DependencyEdge>>,
    it_context: Option<&BStr>,
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
                    );
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
                    match (lhs_component.flow(), rhs_component.flow()) {
                        (Flow::Source, Flow::Sink) => {
                            // Cosi: server := client, so server depends on client.
                            add_edge(
                                edges,
                                rhs_component.id(),
                                lhs_component.id(),
                                EdgeKind::Combinational,
                            );
                        }
                        (Flow::Sink, Flow::Source) => {
                            // Soci: client := server, so client depends on server.
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
                let name = stmt.parsing.string(submodule.name);
                let children = stmt.children();
                if children.len() == 2 {
                    let it_block = &children[1];
                    if matches!(it_block.payload(), AstNodePayload::It) {
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
                        );
                    }
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
                        );
                    }
                }
            }
            AstNodePayload::ModDefStmtWhen => {
                let children = stmt.children();
                let mut idx = 0;
                while let Some(first) = children.get(idx) {
                    if first.is_expr() {
                        // case arm: guard + body
                        if let Some(body) = children.get(idx + 1) {
                            let body_stmts = if matches!(
                                body.payload(),
                                AstNodePayload::ModDefStmtBlock
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
                            );
                        }
                        idx += 2;
                    } else {
                        // else arm: body only
                        let body_stmts = if matches!(
                            first.payload(),
                            AstNodePayload::ModDefStmtBlock
                        ) {
                            first.children()
                        } else {
                            vec![first.clone()]
                        };
                        collect_driver_edges(
                            &body_stmts,
                            component_analysis,
                            edges,
                            it_context,
                        );
                        idx += 1;
                    }
                }
            }
            AstNodePayload::ModDefStmtMatch => {
                let children = stmt.children();
                for (_pat_opt, body) in match_arm_children(&children) {
                    let body_stmts = if matches!(
                        body.payload(),
                        AstNodePayload::ModDefStmtBlock
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
                    );
                }
            }
            AstNodePayload::ModDefStmtBlock => {
                collect_driver_edges(
                    &stmt.children(),
                    component_analysis,
                    edges,
                    it_context,
                );
            }
            _ => {}
        }
    }
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
) {
    let parsing = expr.parsing;
    let mut stack: Vec<crate::syntax::ast::AstNodeId> = vec![expr.id()];
    while let Some(node_id) = stack.pop() {
        let node = parsing.ast_node(node_id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(path) = node.path() {
                let mut name = parsing.string(path).to_owned();
                resolve_it_path_simple(&mut name, it_context);
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
// Collect edges from `dependson`
// ---------------------------------------------------------------------------

/// Walk all `dependson` statements and add combinational edges.
fn collect_dependson_edges(
    stmts: &[AstNode<'_>],
    component_analysis: &ComponentAnalysis,
    edges: &mut IndexMap<ComponentId, Vec<DependencyEdge>>,
    it_context: Option<&BStr>,
) {
    for stmt in stmts {
        match stmt.payload() {
            AstNodePayload::ModDefStmtDependsOn => {
                let parsing = stmt.parsing;
                let lhs_node = stmt.child(0);
                let rhs_node = stmt.child(1);

                // Resolve LHS (the dependent).
                let lhs_component =
                    resolve_path_component(&lhs_node, parsing, component_analysis, it_context);
                // Resolve RHS (the dependee).
                let rhs_component =
                    resolve_path_component(&rhs_node, parsing, component_analysis, it_context);

                if let (Some(dependent), Some(dependee)) = (lhs_component, rhs_component) {
                    add_edge(
                        edges,
                        dependent.id(),
                        dependee.id(),
                        EdgeKind::Combinational,
                    );
                }
            }
            AstNodePayload::Submodule(submodule) => {
                let name = stmt.parsing.string(submodule.name);
                let children = stmt.children();
                if children.len() == 2 {
                    let it_block = &children[1];
                    if matches!(it_block.payload(), AstNodePayload::It) {
                        let block = &it_block.children()[0];
                        let name_with_context = if let Some(ctx) = it_context {
                            let mut new_name = BString::from(ctx);
                            new_name.push(b'.');
                            new_name.extend_from_slice(name);
                            new_name
                        } else {
                            BString::from(name)
                        };
                        collect_dependson_edges(
                            &block.children(),
                            component_analysis,
                            edges,
                            Some(name_with_context.as_bstr()),
                        );
                    }
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
                        collect_dependson_edges(
                            &block.children(),
                            component_analysis,
                            edges,
                            Some(name_with_context.as_bstr()),
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
                        collect_dependson_edges(
                            &block.children(),
                            component_analysis,
                            edges,
                            Some(name_with_context.as_bstr()),
                        );
                    }
                }
            }
            AstNodePayload::ModDefStmtWhen => {
                let children = stmt.children();
                let mut idx = 0;
                while let Some(first) = children.get(idx) {
                    if first.is_expr() {
                        if let Some(body) = children.get(idx + 1) {
                            let body_stmts = if matches!(
                                body.payload(),
                                AstNodePayload::ModDefStmtBlock
                            ) {
                                body.children()
                            } else {
                                vec![body.clone()]
                            };
                            collect_dependson_edges(
                                &body_stmts,
                                component_analysis,
                                edges,
                                it_context,
                            );
                        }
                        idx += 2;
                    } else {
                        let body_stmts = if matches!(
                            first.payload(),
                            AstNodePayload::ModDefStmtBlock
                        ) {
                            first.children()
                        } else {
                            vec![first.clone()]
                        };
                        collect_dependson_edges(
                            &body_stmts,
                            component_analysis,
                            edges,
                            it_context,
                        );
                        idx += 1;
                    }
                }
            }
            AstNodePayload::ModDefStmtMatch => {
                let children = stmt.children();
                for (_pat_opt, body) in match_arm_children(&children) {
                    let body_stmts = if matches!(
                        body.payload(),
                        AstNodePayload::ModDefStmtBlock
                    ) {
                        body.children()
                    } else {
                        vec![body.clone()]
                    };
                    collect_dependson_edges(
                        &body_stmts,
                        component_analysis,
                        edges,
                        it_context,
                    );
                }
            }
            AstNodePayload::ModDefStmtBlock => {
                collect_dependson_edges(
                    &stmt.children(),
                    component_analysis,
                    edges,
                    it_context,
                );
            }
            _ => {}
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
    // Skip self-loops - a component depending on itself is not meaningful
    // for dependency tracking (the value of a component doesn't depend on
    // itself in a way that affects scheduling or cycle detection).
    if source == dependee {
        return;
    }
    edges
        .entry(source)
        .or_default()
        .push(DependencyEdge { dependee, kind });
}

/// Resolve `it` references in a path string using the enclosing statement
/// context.
fn resolve_it_path(
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

/// Simple `it` resolution without parent-walking (used inside expressions
/// where the it-context is already known).
fn resolve_it_path_simple(path: &mut BString, it_context: Option<&BStr>) {
    if (path.starts_with(b"it.") || path == b"it") && it_context.is_some() {
        let ctx = it_context.unwrap();
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

/// Resolve a Path node to a Component, handling `it` resolution.
fn resolve_path_component(
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