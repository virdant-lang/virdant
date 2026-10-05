//! Top-level diagnostic aggregator running all front-end checks over the
//! database (syntax errors, package-name keyword collisions, per-item
//! driver/match-coverage checks, typechecking, enum checks,
//! instantiation cycles), sorted by source region.

use std::sync::Arc;

use bstr::BString;
use bstr::ByteSlice;
use indexmap::IndexMap;

use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::graph::{Graph, VertIndex};
use crate::common::source::Region;
use crate::common::WordValue;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticPayload};
use crate::package::PackageId;
use crate::syntax::payload::AstNodePayload;
use crate::syntax::token::KEYWORDS;

/// A directed edge in the module instantiation graph:
/// `parent_id` has a `mod ... of (target_vert)` statement at `region`.
struct ModEdge {
    parent_id: SymbolId,
    region: Region,
    parent_vert: VertIndex,
    target_vert: VertIndex,
}

fn package_name_is_keyword(name: &str) -> bool {
    KEYWORDS.contains(&name)
}

/// Check that no package name (except `builtin`) collides with a keyword.
fn check_package_name_not_keyword(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let packages = builder.get_packages();
    for package in packages.ids() {
        let package_name = packages.name(package).to_str_lossy().into_owned();
        if package_name == "builtin" {
            continue;
        }
        if package_name_is_keyword(&package_name) {
            let parsing = builder.get_parsing(package);
            let root_node = parsing.root();
            diagnostics.push(Diagnostic::new(
                root_node.region(),
                diagnostics::Unknown {
                    message: format!(
                        "Package name '{}' is a keyword",
                        package_name,
                    ).into(),
                },
            ));
        }
    }
}

pub(crate) fn check(builder: &mut Builder) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors().iter().cloned());

    check_package_name_not_keyword(builder, &mut diagnostics);

    let symboltable = builder.get_symboltable();
    diagnostics.extend(symboltable.diagnostics.clone());

    let type_index = builder.get_type_index();
    diagnostics.extend(type_index.diagnostics());

    for item in symboltable.items() {
        // Platform items are unvalidated data (they have no drivers
        // and no match arms); running module checks on them would
        // falsely flag their `outgoing` ports as undriven.
        if item.kind == SymbolKind::Platform {
            continue;
        }
        diagnostics.extend(builder.check_drivers(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        if item.kind == SymbolKind::Platform {
            continue;
        }
        diagnostics.extend(builder.get_match_coverage(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        if item.kind == SymbolKind::ModDef {
            let component_analysis = builder.get_component_analysis(item.id());
            diagnostics.extend(component_analysis.diagnostics());
        }
        diagnostics.extend(builder.typecheck(item.id()).iter().cloned());

//        let driver_analysis = builder.get_driver_analysis(item.id());
//        diagnostics.extend(driver_analysis); // TODO
    }

    // Check for duplicate enumerant values in enum types.
    // Only the first duplicate per value is reported (as an Error).
    check_duplicate_enum_values(builder, &mut diagnostics);

    // Check that enum types have an inferrable width from the first enumerant.
    check_enum_unknown_width(builder, &mut diagnostics);

    // Check for recursive module instantiations.
    check_mod_cycles(builder, &mut diagnostics);

    // Check for combinational loops in module dependency graphs.
    // The check runs bottom-up per module in submodule-inclusion order,
    // stitching per-module graphs along the instance tree.
    diagnostics.extend(builder.get_combinational_cycle_check().iter().cloned());

    let packages = builder.get_packages();
    diagnostics.sort_by_key(|d| {
        let region = d.region();
        let package_name = packages.name(region.package()).to_owned();
        let start = region.start();
        let end = region.end();
        (package_name, start.line(), start.col(), end.line(), end.col())
    });

    Arc::new(diagnostics)
}

/// Check for enumerants of the same enum type that share the same value.
/// Only the first duplicate per value is reported (as an Error);
/// further duplicates for the same value are silently ignored.
fn check_duplicate_enum_values(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::EnumDef {
            continue;
        }
        let typedef = builder.get_typedef(item.id());
        // Map value -> (name, region) of the first occurrence.
        let mut first: IndexMap<WordValue, (BString, crate::common::source::Region)> = IndexMap::new();
        let mut already_reported: std::collections::HashSet<WordValue> = std::collections::HashSet::new();
        for (enumerant_id, value) in &typedef.enumerant_values {
            let enumerant_symbol = symboltable.symbol(*enumerant_id);
            if let Some((_prev_name, prev_region)) = first.get(value) {
                if already_reported.insert(*value) {
                    diagnostics.push(Diagnostic::new(
                        prev_region.clone(),
                        diagnostics::DuplicateEnumValue {
                            enum_name: item.name().to_owned(),
                            value: *value,
                        },
                    ));
                }
            } else {
                let parsing = builder.get_parsing(enumerant_symbol.package());
                let enumerant_node = parsing.ast_node(enumerant_symbol.location().ast_node_id());
                first.insert(*value, (enumerant_symbol.name().to_owned(), enumerant_node.region()));
            }
        }
    }
}

/// Check that every enum type has an inferrable width from its first enumerant.
/// When the first enumerant has no explicit width (e.g. `= 0` instead of `= 0w8`),
/// the enum width cannot be determined and an error is reported.
fn check_enum_unknown_width(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind != SymbolKind::EnumDef {
            continue;
        }
        let typedef = builder.get_typedef(item.id());
        if typedef.width.is_none() {
            let parsing = builder.get_parsing(item.package());
            let enumdef_node = parsing.ast_node(item.location().ast_node_id());
            diagnostics.push(Diagnostic::new(
                enumdef_node.region(),
                DiagnosticPayload::EnumUnknownWidth,
            ));
        }
    }
}

/// Build a graph of ModDef items, with edges for each `mod X of Y` statement.
/// For each edge `parent -> target`, check whether `target` can reach `parent`
/// (i.e. a back-edge).  When it can, we flag the `mod` statement as part of a cycle.
fn check_mod_cycles(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();

    // Graph vertices are SymbolIds of ModDef items.
    let mut graph: Graph<SymbolId> = Graph::new();
    let mut edges: Vec<ModEdge> = vec![];

    // Map SymbolId -> VertIndex so we can reuse existing vertices.
    let mut vertex_map: IndexMap<SymbolId, VertIndex> = IndexMap::new();
    let mut get_or_add_vert = |g: &mut Graph<SymbolId>, id: SymbolId| -> VertIndex {
        *vertex_map.entry(id).or_insert_with(|| g.add_vert(id))
    };

    for item in symboltable.items() {
        if item.kind() != SymbolKind::ModDef {
            continue;
        }
        let parent_id = item.id();
        let parent_vert = get_or_add_vert(&mut graph, parent_id);

        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let ast_node = parsing.ast_node(location.ast_node_id());

        for stmt in ast_node.children() {
            let AstNodePayload::Submodule(_submodule) = stmt.payload() else {
                continue;
            };
            let ofness_node = stmt.child(1);
            let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
                continue;
            };
            let packages = builder.get_packages();
            let target_package: Option<PackageId> = match ofness.package {
                Some(pkg) => packages.id(parsing.string(pkg)),
                None => Some(location.package()),
            };
            let target_name = parsing.string(ofness.name);
            let Some(target_symbol) =
                target_package.and_then(|pkg| symboltable.resolve_item(target_name, pkg))
            else {
                continue;
            };
            if target_symbol.kind() != SymbolKind::ModDef {
                continue;
            }
            let target_id = target_symbol.id();
            let target_vert = get_or_add_vert(&mut graph, target_id);

            graph.add_edge(parent_vert, target_vert);
            edges.push(ModEdge {
                parent_id,
                region: stmt.region(),
                parent_vert,
                target_vert,
            });
        }
    }

    // For each edge, check if `target` can reach `parent` (back-edge detection).
    for edge in &edges {
        let Some(back_path) = graph.path(edge.target_vert, edge.parent_vert) else {
            continue;
        };
        // back_path goes target -> ... -> parent (inclusive of parent).
        // The cycle is: parent, target, ..., parent.
        // Drop the duplicate trailing parent for the FQN list.
        let mut cycle_ids: Vec<SymbolId> = vec![edge.parent_id];
        for vi in back_path.iter().take(back_path.len().saturating_sub(1)) {
            cycle_ids.push(graph[*vi]);
        }
        let cycle_fqns: Vec<BString> = cycle_ids
            .iter()
            .map(|id| symboltable.symbol(*id).fqn().to_owned().into())
            .collect();

        diagnostics.push(Diagnostic::new(
            edge.region.clone(),
            diagnostics::ModuleCycle {
                module_cycle: cycle_fqns,
            },
        ));
    }
}
