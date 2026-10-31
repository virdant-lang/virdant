//! Builds the signal-level dependency map of an elaborated design:
//! `deps[signal]` is the set of elaborated components whose values the
//! signal's driver reads.
//!
//! A component A depends on component B if B's path appears as a reference
//! inside A's driver expression(s).  References in drivers are relative to
//! the owning module's scope (see `ElaboratedComponent::driver_scope`);
//! this module converts them to absolute elaborated paths before resolving.
//!
//! Shared by the simulator (`sim::circuit`) and the cone analysis
//! (`analysis::cones`), so both agree on what "reads" means.

use bstr::BString;
use bstr::ByteSlice;
use indexmap::{IndexMap, IndexSet};

use crate::analysis::drivers::Driver;
use crate::analysis::elaboration::{Elaboration, SignalId};
use crate::analysis::Location;
use crate::db::Builder;
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

/// Build the data-flow dependency map for all elaborated components.
pub fn build_dependencies(
    builder: &mut Builder,
    elaboration: &Elaboration,
) -> IndexMap<SignalId, IndexSet<SignalId>> {
    let mut dep_map: IndexMap<SignalId, IndexSet<SignalId>> =
        IndexMap::new();

    for component in elaboration.components() {
        let mut deps: IndexSet<SignalId> = IndexSet::new();

        if let Some(driver) = component.driver() {
            let prefix = component.driver_scope().to_str_lossy().into_owned();
            for loc in collect_locations(driver) {
                let parsing = builder.get_parsing(loc.package());
                for name in walk_expr_refs(&parsing, loc.ast_node_id()) {
                    let full_path: BString = format!("{prefix}.{name}").into();
                    if let Some(dep) = elaboration.resolve(&full_path) {
                        deps.insert(dep.id());
                    }
                    // If resolve returns None the name is a pattern-bound
                    // variable (e.g. from a match arm) or otherwise not a
                    // component; either way it contributes no dependency.
                }
            }
        }

        dep_map.insert(component.id(), deps);
    }

    dep_map
}

/// Recursively collect every expression `Location` from a `Driver`.
///
/// For `Driver::Expr` this is the single expression location.
/// For `Driver::When` this includes every condition location plus all
/// locations from every sub-driver and the else clause.
fn collect_locations(driver: &Driver) -> Vec<Location> {
    let mut locs = Vec::new();
    collect_locations_inner(driver, &mut locs);
    locs
}

fn collect_locations_inner(driver: &Driver, locs: &mut Vec<Location>) {
    match driver {
        Driver::Expr(_, loc) => locs.push(loc.clone()),
        Driver::Bidirectional(_) => {}
        Driver::Absent => {}
        Driver::When(driver_when) => {
            for (cond_loc, sub_driver) in &driver_when.clauses {
                locs.push(cond_loc.clone());
                collect_locations_inner(sub_driver, locs);
            }
            if let Some(else_driver) = &driver_when.else_clause {
                collect_locations_inner(else_driver, locs);
            }
        }
        Driver::Match(driver_match) => {
            locs.push(driver_match.subject.clone());
            for (pat_loc, sub_driver) in &driver_match.arms {
                locs.push(pat_loc.clone());
                collect_locations_inner(sub_driver, locs);
            }
            if let Some(else_driver) = &driver_match.else_clause {
                collect_locations_inner(else_driver, locs);
            }
        }
    }
}

/// DFS over the AST subtree rooted at `root_id`, collecting the name string
/// of every `ExprReference` node encountered.  Each name is returned with
/// any enclosing `it`-block prefix already substituted for the binding name
/// of its surrounding `Submodule` / `Component` / `Socket`, matching the
/// rewriting performed by `sim::expr::convert_ast_expr`.
fn walk_expr_refs(parsing: &Parsing, root_id: AstNodeId) -> Vec<BString> {
    let mut result = Vec::new();
    let mut stack = vec![root_id];
    while let Some(id) = stack.pop() {
        let node = parsing.ast_node(id);
        if matches!(node.payload(), AstNodePayload::ExprReference) {
            if let Some(interned) = node.path() {
                let raw = parsing.string(interned);
                result.push(rewrite_it_in_name(parsing, id, raw));
            }
        }
        for child in node.children() {
            stack.push(child.id());
        }
    }
    result
}

/// Rewrite a path of the form `it` or `it.<rest>` to use the binding name
/// of the nearest enclosing `Submodule` / `Component` / `Socket`.  Returns
/// the path unchanged when it does not start with `it`.
fn rewrite_it_in_name(parsing: &Parsing, ast_id: AstNodeId, raw: &bstr::BStr) -> BString {
    if !raw.starts_with(b"it.") && raw != b"it" {
        return raw.to_owned();
    }
    let mut current_id = ast_id;
    loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            return raw.to_owned();
        }
        let target: Option<BString> = match current.payload() {
            AstNodePayload::Submodule(m) => Some(parsing.string(m.name).to_owned()),
            AstNodePayload::Component(c) => Some(parsing.string(c.name).to_owned()),
            AstNodePayload::Socket(s) => Some(parsing.string(s.name).to_owned()),
            _ => None,
        };
        if let Some(target) = target {
            if raw == b"it" { return target; }
            let mut out = target;
            out.push(b'.');
            out.extend_from_slice(&raw[3..]);
            return out;
        }
        current_id = match current.parent() {
            Some(p) => p.id(),
            None => return raw.to_owned(),
        };
    }
}

