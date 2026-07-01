use std::sync::Arc;

use bstr::ByteSlice;
use indexmap::IndexMap;

use crate::analysis::dependency::EdgeKind;
use crate::analysis::symbols::{SymbolId, SymbolKind, SymbolTable};
use crate::common::graph::{Graph, VertIndex};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;

/// A module's dependency graph stitched together with its immediate
/// submodule instances' already-built stitched graphs.
///
/// All paths are local to the module's own namespace (no `top.` prefix).
/// A submodule instance `inst of S` contributes `S`'s stitched graph with
/// every path prefixed by `inst.`, which unifies with the parent's
/// `inst.port` port components at the instance boundary.
struct StitchedGraph {
    /// All edges (local + imported), keyed by source path -> dependee paths.
    edges: IndexMap<bstr::BString, Vec<bstr::BString>>,
    /// Edges local to this module (a subset of `edges`). A cycle is
    /// "created" by this module iff it uses at least one local edge;
    /// cycles living entirely inside a submodule are reported there.
    local_edges: Vec<(bstr::BString, bstr::BString)>,
}

/// Detect combinational loops by checking each module bottom-up in
/// submodule-inclusion order.
///
/// For each module `M`, build a stitched graph over `M`'s local component
/// paths: `M`'s own combinational edges plus each immediate submodule
/// instance's already-built stitched graph (paths prefixed by the instance
/// name). A cycle is reported at `M` iff it uses at least one edge local
/// to `M`, so each loop is reported exactly once, at the module that
/// closes it.
pub(crate) fn build_combinational_cycle_check(
    builder: &mut Builder,
) -> Arc<Vec<Diagnostic>> {
    let symboltable = builder.get_symboltable();
    let moddefs: Vec<SymbolId> = symboltable
        .items()
        .iter()
        .filter(|item| item.kind() == SymbolKind::ModDef)
        .map(|item| item.id())
        .collect();

    // Submodule-inclusion order: leaves before parents. If the
    // instantiation graph has a cycle, fall back to symboltable order
    // (best-effort; `check_mod_cycles` diagnoses the instantiation cycle).
    let order = inclusion_order(builder, &moddefs);

    let mut stitched: IndexMap<SymbolId, StitchedGraph> = IndexMap::new();
    let mut diagnostics: Vec<Diagnostic> = vec![];

    for &moddef in &order {
        let graph = build_stitched(builder, moddef, &stitched);
        check_module(builder, moddef, &graph, &mut diagnostics);
        stitched.insert(moddef, graph);
    }

    Arc::new(diagnostics)
}

/// Build the stitched combinational graph for `moddef`, using the
/// already-built stitched graphs of its immediate submodule instances.
fn build_stitched(
    builder: &mut Builder,
    moddef: SymbolId,
    stitched: &IndexMap<SymbolId, StitchedGraph>,
) -> StitchedGraph {
    let dep_graph = builder.get_dependency_graph(moddef);
    let component_analysis = builder.get_component_analysis(moddef);
    let symboltable = builder.get_symboltable();

    let mut edges: IndexMap<bstr::BString, Vec<bstr::BString>> = IndexMap::new();
    let mut local_edges: Vec<(bstr::BString, bstr::BString)> = vec![];

    // 1. M's own combinational edges (local).
    for (source, edge_list) in dep_graph.edges() {
        let Some(src_comp) = component_analysis.component(*source) else {
            continue;
        };
        let src_path = src_comp.path();
        for edge in edge_list {
            if edge.kind != EdgeKind::Combinational {
                continue;
            }
            let Some(dep_comp) = component_analysis.component(edge.dependee) else {
                continue;
            };
            let dep_path = dep_comp.path();
            if src_path == dep_path {
                continue;
            }
            edges
                .entry(src_path.clone())
                .or_default()
                .push(dep_path.clone());
            local_edges.push((src_path.clone(), dep_path.clone()));
        }
    }

    // 2. Imported edges from immediate submodule instances.
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let moddef_node_id = builder.get_symbol_ast(moddef);
    let moddef_node = parsing.ast_node(moddef_node_id);

    for stmt in moddef_node.children() {
        let AstNodePayload::Submodule(submodule) = stmt.payload() else {
            continue;
        };
        let inst_name = parsing.string(submodule.name);
        let Some(child_id) =
            resolve_submodule_moddef(&stmt, &parsing, &location, &symboltable)
        else {
            continue;
        };
        let Some(child_graph) = stitched.get(&child_id) else {
            // Submodule not yet built (instantiation cycle). Best-effort.
            continue;
        };
        for (src, deps) in &child_graph.edges {
            let src_full: bstr::BString =
                format!("{}.{}", inst_name.to_str_lossy(), src.to_str_lossy())
                    .into();
            for dep in deps {
                let dep_full: bstr::BString = format!(
                    "{}.{}",
                    inst_name.to_str_lossy(),
                    dep.to_str_lossy()
                )
                .into();
                edges
                    .entry(src_full.clone())
                    .or_default()
                    .push(dep_full);
            }
        }
    }

    StitchedGraph { edges, local_edges }
}

/// Report a cycle at `moddef` iff one of its local edges closes a cycle.
fn check_module(
    builder: &mut Builder,
    moddef: SymbolId,
    graph: &StitchedGraph,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Build a Graph<BString> over all edges (local + imported) for the
    // reachability check.
    let mut comb_graph: Graph<bstr::BString> = Graph::new();
    let mut vert_map: IndexMap<bstr::BString, VertIndex> = IndexMap::new();

    for (src, deps) in &graph.edges {
        let sv = *vert_map
            .entry(src.clone())
            .or_insert_with(|| comb_graph.add_vert(src.clone()));
        for dep in deps {
            let dv = *vert_map
                .entry(dep.clone())
                .or_insert_with(|| comb_graph.add_vert(dep.clone()));
            if sv != dv {
                comb_graph.add_edge(sv, dv);
            }
        }
    }

    // A local edge u -> v is in a cycle iff v can reach u. Such a cycle
    // uses a local edge, so it is "created" by this module.
    for (u, v) in &graph.local_edges {
        let Some(&uv) = vert_map.get(u) else { continue };
        let Some(&vv) = vert_map.get(v) else { continue };
        if let Some(path) = comb_graph.path(vv, uv) {
            // path is exclusive of v, inclusive of u: [n1, ..., u].
            let mut comps: Vec<bstr::BString> = vec![u.clone(), v.clone()];
            for vi in &path {
                comps.push(comb_graph[*vi].clone());
            }
            // Drop the trailing duplicate u.
            if comps.last().map(|last| last == u).unwrap_or(false) {
                comps.pop();
            }

            let region = moddef_region(builder, moddef);
            diagnostics.push(
                diagnostics::CombinationalLoop {
                    region,
                    components: comps,
                }
                .into(),
            );
            return;
        }
    }
}

/// Return the ModDefs in submodule-inclusion order (leaves before parents).
///
/// Uses a DFS post-order traversal so that modules not in any instantiation
/// cycle are processed after their submodules. Back-edges (instantiation
/// cycles) are skipped: a cyclic submodule won't be in `stitched` when its
/// parent is processed, so the parent imports it best-effort.
/// `check_mod_cycles` diagnoses instantiation cycles separately.
fn inclusion_order(builder: &mut Builder, moddefs: &[SymbolId]) -> Vec<SymbolId> {
    let symboltable = builder.get_symboltable();

    // Adjacency: parent -> child ModDefs it instantiates.
    let mut adj: IndexMap<SymbolId, Vec<SymbolId>> = IndexMap::new();
    for &m in moddefs {
        adj.entry(m).or_default();
    }
    for &parent in moddefs {
        let location = symboltable.symbol(parent).location();
        let parsing = builder.get_parsing(location.package());
        let node_id = builder.get_symbol_ast(parent);
        let node = parsing.ast_node(node_id);
        for stmt in node.children() {
            if let AstNodePayload::Submodule(_) = stmt.payload() {
                if let Some(child) =
                    resolve_submodule_moddef(&stmt, &parsing, &location, &symboltable)
                {
                    adj.entry(parent).or_default().push(child);
                }
            }
        }
    }

    // DFS post-order: children before parents. State: 0 = unvisited,
    // 1 = visiting (on the current stack), 2 = done.
    let mut state: IndexMap<SymbolId, u8> = IndexMap::new();
    let mut order: Vec<SymbolId> = vec![];
    for &start in moddefs {
        visit(start, &adj, &mut state, &mut order);
    }
    order
}

/// DFS visitor for `inclusion_order`. Skips back-edges (state == 1), so
/// instantiation cycles don't cause infinite recursion.
fn visit(
    node: SymbolId,
    adj: &IndexMap<SymbolId, Vec<SymbolId>>,
    state: &mut IndexMap<SymbolId, u8>,
    order: &mut Vec<SymbolId>,
) {
    match state.get(&node).copied().unwrap_or(0) {
        2 => return, // already done
        1 => return, // on the current stack: back-edge (instantiation cycle)
        _ => {}
    }
    state.insert(node, 1);
    if let Some(children) = adj.get(&node) {
        for &child in children {
            visit(child, adj, state, order);
        }
    }
    state.insert(node, 2);
    order.push(node);
}

/// Resolve a `Submodule` statement's `of` clause to the instantiated
/// ModDef's `SymbolId`, if it is a ModDef.
fn resolve_submodule_moddef(
    stmt: &AstNode<'_>,
    parsing: &crate::syntax::parsing::Parsing,
    location: &crate::analysis::Location,
    symboltable: &SymbolTable,
) -> Option<SymbolId> {
    let ofness_node = stmt.child(1);
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        return None;
    };
    let target_package = ofness
        .package
        .map(|pkg| PackageFqn::new(parsing.string(pkg).into()))
        .unwrap_or_else(|| location.package());
    let target_name = parsing.string(ofness.name);
    let target_symbol = symboltable.resolve_item(target_name, target_package)?;
    if target_symbol.kind() != SymbolKind::ModDef {
        return None;
    }
    Some(target_symbol.id())
}

/// The region of `moddef`'s ModDef node, for the diagnostic.
fn moddef_region(builder: &mut Builder, moddef: SymbolId) -> crate::common::source::Region {
    let symboltable = builder.get_symboltable();
    let location = symboltable.symbol(moddef).location();
    let parsing = builder.get_parsing(location.package());
    let node_id = builder.get_symbol_ast(moddef);
    let node = parsing.ast_node(node_id);
    node.region()
}
