use std::sync::Arc;

use bstr::BString;
use indexmap::IndexMap;

use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::graph::{Graph, VertIndex};
use crate::common::source::Region;
use crate::common::WordValue;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
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
    for package in builder.get_packages().iter() {
        let package_name: &str = &package.to_string();
        if package_name == "builtin" {
            continue;
        }
        if package_name_is_keyword(package_name) {
            let parsing = builder.get_parsing(package.clone());
            let root_node = parsing.root();
            diagnostics.push(
                diagnostics::Unknown {
                    region: root_node.region(),
                    message: format!(
                        "Package name '{}' is a keyword",
                        package_name,
                    ).into(),
                }.into(),
            );
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
        if item.kind == SymbolKind::Platform {
            // Platforms do not participate in typechecking or driver checks.
            // Their validation lives in PlatformAnalysis.
            continue;
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

    // Run platform analysis for any package that declares a platform item.
    for package in builder.get_packages().iter() {
        let package_analysis = builder.get_package_analysis(package.clone());
        if package_analysis.platform().is_some() {
            let platform_analysis = builder.get_platform_analysis(package.clone());
            diagnostics.extend(platform_analysis.diagnostics.iter().cloned());
        }
    }

    diagnostics.sort_by_key(|d| {
        let region = d.region();
        let start = region.start();
        let end = region.end();
        (region.package(), start.line(), start.col(), end.line(), end.col())
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
                    diagnostics.push(
                        diagnostics::DuplicateEnumValue {
                            region: prev_region.clone(),
                            enum_name: item.name().to_owned(),
                            value: *value,
                        }.into(),
                    );
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
            diagnostics.push(
                diagnostics::EnumUnknownWidth {
                    region: enumdef_node.region(),
                }.into(),
            );
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
            let target_package = ofness
                .package
                .map(|pkg| PackageFqn::new(parsing.string(pkg).into()))
                .unwrap_or_else(|| location.package());
            let target_name = parsing.string(ofness.name);
            let Some(target_symbol) =
                symboltable.resolve_item(target_name, target_package)
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

        diagnostics.push(
            diagnostics::ModuleCycle {
                region: edge.region.clone(),
                module_cycle: cycle_fqns,
            }.into(),
        );
    }
}
