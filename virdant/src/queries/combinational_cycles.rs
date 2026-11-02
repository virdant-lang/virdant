//! Detects combinational loops over each module's *potentiality graph*.
//!
//! The potentiality graph of a module contains:
//!
//! * *Local actual edges*: the module's own driver-derived
//!   combinational edges (including socket-link edges from `:=:`).
//! * *Local declared edges*: the module's own `dependson` lists (on
//!   `outgoing`/`outgoing wire` components) and its socket-obligation
//!   edges (from socketdef channel lists, in the driving role).
//! * *Imported declared edges*: each immediate submodule instance's
//!   declared edges and socket obligations, prefixed with the instance
//!   name. The parent never looks inside a child: the child's
//!   declarations are its boundary contract.
//!
//! Sequential (`reg`) edges are never traversed: a clock edge breaks
//! potentiality.
//!
//! A cycle is an error regardless of whether the actual comb path
//! exists today: `dependson` declares *potential*, and a designer must
//! be free to make any declared dependency actual at any time without
//! breaking clients.
//!
//! Each loop is reported once, at the module whose *local* edge closes
//! the cycle: a cycle living entirely inside a submodule is reported
//! there, and a cycle that spans a parent and its children is reported
//! at the parent (which is the only module that sees all of its edges).

use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};

use crate::analysis::dependency::EdgeKind;
use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::graph::{Graph, VertIndex};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};

/// A module's potentiality graph: all edges (local + imported), keyed
/// by source path -> dependee paths, plus the subset of edges that are
/// local to the module.
struct PotentialityGraph {
    /// All edges (local + imported), keyed by source path -> dependee paths.
    edges: IndexMap<bstr::BString, Vec<bstr::BString>>,
    /// Edges local to this module (a subset of `edges`). A cycle is
    /// "created" by this module iff it uses at least one local edge;
    /// cycles living entirely inside a submodule are reported there.
    local_edges: Vec<(bstr::BString, bstr::BString)>,
    /// The local *actual* edges (driver-derived): a cycle all of whose
    /// edges are actual is a genuine combinational loop
    /// (`CombinationalLoop`); a cycle containing any declared edge
    /// (local declared, socket obligation, or imported) is a cycle in
    /// the *potentiality* of the design (`DependsonCycle`).
    actual_edges: IndexSet<(bstr::BString, bstr::BString)>,
}

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

    let mut diagnostics: Vec<Diagnostic> = vec![];

    // Order does not affect correctness: each module's potentiality
    // graph is self-contained (local edges plus children's
    // declarations), and the local-edge reporting policy below
    // deduplicates cycles across the module tree. Moddefs are checked
    // in symboltable order.
    for &moddef in &moddefs {
        let graph = build_potentiality(builder, moddef);
        check_module(builder, moddef, &graph, &mut diagnostics);
    }

    Arc::new(diagnostics)
}

/// Build the potentiality graph for `moddef`.
fn build_potentiality(
    builder: &mut Builder,
    moddef: SymbolId,
) -> PotentialityGraph {
    let dep_graph = builder.get_dependency_graph(moddef);
    let component_analysis = builder.get_component_analysis(moddef);
    let dependson_analysis = builder.get_dependson_analysis(moddef);

    let mut edges: IndexMap<bstr::BString, Vec<bstr::BString>> = IndexMap::new();
    let mut local_edges: Vec<(bstr::BString, bstr::BString)> = vec![];
    let mut actual_edges: IndexSet<(bstr::BString, bstr::BString)> = IndexSet::new();

    let add_local = |edges: &mut IndexMap<bstr::BString, Vec<bstr::BString>>,
                         local_edges: &mut Vec<(bstr::BString, bstr::BString)>,
                         actual_edges: &mut IndexSet<(bstr::BString, bstr::BString)>,
                         source: &bstr::BString,
                         target: &bstr::BString,
                         actual: bool| {
        edges
            .entry(source.clone())
            .or_default()
            .push(target.clone());
        let key = (source.clone(), target.clone());
        if actual {
            actual_edges.insert(key.clone());
        }
        local_edges.push(key);
    };

    // 1. M's own driver-derived combinational edges (local actual).
    // Self-loops (src_path == dep_path) are kept: a driver whose
    // target appears in its own RHS or in an enclosing guard is a
    // genuine one-component combinational loop.
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
            add_local(
                &mut edges,
                &mut local_edges,
                &mut actual_edges,
                &src_path,
                &dep_path,
                true,
            );
        }
    }

    // 2. M's own declared edges: `dependson` lists and socket
    //    obligations (local declared).
    for (source, target) in dependson_analysis
        .declared_edges()
        .iter()
        .chain(dependson_analysis.socket_edges())
    {
        add_local(
            &mut edges,
            &mut local_edges,
            &mut actual_edges,
            source,
            target,
            false,
        );
    }

    // 3. Imported declared edges from immediate submodule instances.
    for (source, target) in dependson_analysis.imported_edges() {
        edges
            .entry(source.clone())
            .or_default()
            .push(target.clone());
        // Imported edges are never local: a cycle closed only by
        // imported edges belongs to the child (which reports it).
    }

    PotentialityGraph { edges, local_edges, actual_edges }
}

/// Report a cycle at `moddef` iff one of its local edges closes a cycle.
fn check_module(
    builder: &mut Builder,
    moddef: SymbolId,
    graph: &PotentialityGraph,
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
            comb_graph.add_edge(sv, dv);
        }
    }

    // A local edge u -> v is in a cycle iff v can reach u. Such a cycle
    // uses a local edge, so it is "created" by this module.
    for (u, v) in &graph.local_edges {
        let Some(&uv) = vert_map.get(u) else { continue };
        let Some(&vv) = vert_map.get(v) else { continue };
        if let Some(path) = comb_graph.path(vv, uv) {
            // path is exclusive of v, inclusive of u: [n1, ..., u].
            let comps: Vec<bstr::BString> = if u == v {
                // Self-loop: the cycle is the single component alone.
                vec![u.clone()]
            } else {
                let mut comps: Vec<bstr::BString> = vec![u.clone(), v.clone()];
                for vi in &path {
                    comps.push(comb_graph[*vi].clone());
                }
                // Drop the trailing duplicate u.
                if comps.last().map(|last| last == u).unwrap_or(false) {
                    comps.pop();
                }
                comps
            };

            // The cycle is an actual combinational loop iff every one
            // of its edges is driver-derived; any declared edge makes
            // it a cycle in the *potentiality* of the design.
            let mut edges_of_cycle: Vec<(bstr::BString, bstr::BString)> = vec![];
            if u == v {
                edges_of_cycle.push((u.clone(), v.clone()));
            } else {
                let mut nodes: Vec<&bstr::BString> = vec![u, v];
                for vi in &path {
                    nodes.push(&comb_graph[*vi]);
                }
                for window in nodes.windows(2) {
                    edges_of_cycle.push((window[0].clone(), window[1].clone()));
                }
            }
            let all_actual = edges_of_cycle
                .iter()
                .all(|edge| graph.actual_edges.contains(edge));

            let region = moddef_region(builder, moddef);
            if all_actual {
                diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::CombinationalLoop {
                        components: comps,
                    },
                ));
            } else {
                diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::DependsonCycle {
                        components: comps,
                    },
                ));
            }
            return;
        }
    }
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
