use std::sync::Arc;

use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::Location;
use crate::analysis::component::ComponentAnalysis;
use crate::analysis::drivers::{Driver, DriverAnalysis};
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::parsing::Parsing;
use crate::syntax::payload::AstNodePayload;

/// Validates all driver-related rules for the module identified by `symbol_id`.
///
/// Checks that driver types (continuous vs. latched) match their component kinds,
/// that sink-incompatible components have no drivers, and that sink-capable
/// components have exactly one driver (not zero and not multiple).
/// Skips external modules.
///
/// Emits these diagnostics:
/// - [`WrongDriverType`]: driver type does not match the component kind.
/// - [`DriverForSink`]: a driver targets a component that cannot sink.
/// - [`NoDrivers`]: a sink-capable (non-reg) component has zero drivers.
/// - [`NoRegDrivers`]: a reg component has zero drivers.
/// - [`MultipleDrivers`]: a sink-capable component has more than one driver.
/// Also forwards any diagnostics from [`DriverAnalysis`].
pub(crate) fn check_drivers(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let symbol = symboltable.symbol(symbol_id);
    let moddef_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let moddef_node = parsing.ast_node(moddef_node_id);

    if let AstNodePayload::ModDef(mod_def) = moddef_node.payload() {
        if mod_def.is_ext {
            return Arc::new(diagnostics);
        }
    }

    let component_analysis = builder.get_component_analysis(symbol_id);
    let driver_analysis = builder.get_driver_analysis(symbol_id);

    check_wrong_driver_types(builder, &driver_analysis, &component_analysis, &mut diagnostics);

    let driver_locations = get_all_driver_locations(&parsing, &moddef_node);
    for (path, driver_entries) in driver_locations.iter() {
        if let Some(component) = component_analysis.resolve(path.as_bstr()) {
            if !component.can_sink() && driver_entries.len() > 0 {
                for (_driver_type, location) in driver_entries {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::DriverForSink {
                        region,
                        target: path.clone(),
                    }.into());
                }
            }
        }
    }

    for (path, component) in component_analysis.components() {
        let drivers = driver_analysis.drivers().get(&component.id());
        if component.can_sink() {
            match drivers {
                None => {
                    let region = builder.get_location_region(component.location());
                    let is_reg = matches!(
                        component.kind(),
                        Some(ComponentKind::Reg) | Some(ComponentKind::OutgoingReg)
                    );
                    if is_reg {
                        diagnostics.push(diagnostics::NoRegDrivers {
                            region,
                            target: path.clone(),
                        }.into());
                    } else {
                        diagnostics.push(diagnostics::NoDrivers {
                            region,
                            target: path.clone(),
                        }.into());
                    }
                }
                Some(drivers) if drivers.len() > 1 => {
                    for driver in drivers {
                        if let Some(location) = driver.location() {
                            let region = builder.get_location_region(location);
                            diagnostics.push(diagnostics::MultipleDrivers {
                                region,
                                target: path.clone(),
                            }.into());
                        }
                    }
                }
                _ => {}
            }
        }
    }

    Arc::new(diagnostics)
}

fn check_wrong_driver_types(
    builder: &mut Builder,
    driver_analysis: &Arc<DriverAnalysis>,
    component_analysis: &ComponentAnalysis,
    diagnostics: &mut Vec<Diagnostic>,
) {
    diagnostics.extend_from_slice(driver_analysis.diagnostics());
    for (comp_id, drivers) in driver_analysis.drivers() {
        let Some(component) = component_analysis.component(*comp_id) else { continue };
        let expected = match component.kind() {
            Some(ComponentKind::Reg) | Some(ComponentKind::OutgoingReg) => DriverType::Latched,
            Some(ComponentKind::Wire) | Some(ComponentKind::Incoming)
            | Some(ComponentKind::Outgoing) | Some(ComponentKind::OutgoingWire) => DriverType::Continuous,
            None => continue,
        };
        let path = component.path();
        for driver in drivers {
            collect_wrong_driver_type_errors(builder, driver, expected, &path, diagnostics);
        }
    }
}

fn collect_wrong_driver_type_errors(
    builder: &mut Builder,
    driver: &Driver,
    expected: DriverType,
    path: &BString,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match driver {
        Driver::Expr(driver_type, location) => {
            if *driver_type != expected {
                let region = builder.get_location_region(location.clone());
                diagnostics.push(diagnostics::WrongDriverType {
                    region,
                    target: path.clone(),
                    expected_driver_type: expected,
                }.into());
            }
        }
        Driver::Bidirectional(_) => {}
        Driver::When(driver_when) => {
            for (_, sub_driver) in &driver_when.clauses {
                collect_wrong_driver_type_errors(builder, sub_driver, expected, path, diagnostics);
            }
            if let Some(else_driver) = &driver_when.else_clause {
                collect_wrong_driver_type_errors(builder, else_driver, expected, path, diagnostics);
            }
        }
        Driver::Match(driver_match) => {
            for (_, sub_driver) in &driver_match.arms {
                collect_wrong_driver_type_errors(builder, sub_driver, expected, path, diagnostics);
            }
            if let Some(else_driver) = &driver_match.else_clause {
                collect_wrong_driver_type_errors(builder, else_driver, expected, path, diagnostics);
            }
        }
    }
}

fn get_all_driver_locations(
    parsing: &Parsing,
    moddef_node: &AstNode<'_>,
) -> IndexMap<BString, Vec<(DriverType, Location)>> {
    let mut driver_locations: IndexMap<BString, Vec<(DriverType, Location)>> = IndexMap::new();
    let mut stack: Vec<AstNodeId> = vec![moddef_node.id()];

    while let Some(node_id) = stack.pop() {
        let node = parsing.ast_node(node_id);
        for child in &node.children() {
            match child.payload() {
                AstNodePayload::Driver(driver) => {
                    if let Some(target) = child.target() {
                        let target_str = parsing.string(target);
                        let entry = (driver.driver_type, child.location());
                        driver_locations
                            .entry(target_str.to_owned())
                            .or_default()
                            .push(entry);
                    }
                }
                AstNodePayload::Submodule(_) => {
                    let sub_children = child.children();
                    // If the submodule has an ItBlock (child index 1),
                    // recurse into its children.
                    if sub_children.len() > 1 {
                        stack.push(sub_children[1].id());
                    }
                }
                AstNodePayload::ModDefStmtBlock
                | AstNodePayload::ModDefStmtWhen
                | AstNodePayload::ModDefStmtMatch => {
                    stack.push(child.id());
                }
                _ => {}
            }
        }
    }

    driver_locations
}
