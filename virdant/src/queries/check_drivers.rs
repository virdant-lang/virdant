use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::Location;
use crate::analysis::component::ComponentAnalysis;
use crate::analysis::drivers::Driver;
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::payload::AstNodePayload;

pub(crate) fn check_drivers(builder: &mut Builder, symbol_id: SymbolId) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let symbol = symboltable.symbol(symbol_id);
    let moddef_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let moddef_node = parsing.ast_node(moddef_node_id);

    if let AstNodePayload::ModDef(mod_def) = moddef_node.payload() {
        if mod_def.is_ext {
            return diagnostics;
        }
    }

    let component_analysis = builder.get_component_analysis(symbol_id);

    check_wrong_driver_types(builder, symbol_id, &component_analysis, &mut diagnostics);

    let driver_locations = get_all_driver_locations(builder, symbol_id, &mut diagnostics);
    for (path, driver_entries) in driver_locations.iter() {
        if let Some(component) = component_analysis.resolve(path.as_bstr()) {
            if !component.can_sink() && driver_entries.len() > 0 {
                for (_driver_type, location) in driver_entries {
                    let region = builder.get_location_region(location.clone());
                    // TODO is this error properly named?
                    diagnostics.push(diagnostics::DriverForSink {
                        region,
                        target: path.clone(),
                    }.into());
                }
            } else if component.can_sink() && driver_entries.len() > 1 {
                for (_driver_type, location) in driver_entries {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::MultipleDrivers {
                        region,
                        target: path.clone(),
                    }.into());
                }
            }
        } else {
            for (_driver_type, location) in driver_entries {
                let region = builder.get_location_region(location.clone());
                diagnostics.push(diagnostics::UnresolvedComponent {
                    region,
                    path: path.clone(),
                }.into());
            }
        }
    }

    let driver_analysis = builder.get_driver_analysis(symbol_id);
    for (path, component) in component_analysis.components() {
        if component.can_sink() && !driver_analysis.drivers().contains_key(&component.id()) {
            let location = component.location();
            let region = builder.get_location_region(location);
            diagnostics.push(diagnostics::NoDrivers {
                region,
                target: path.clone(),
            }.into());
        }
    }

    diagnostics
}

fn check_wrong_driver_types(
    builder: &mut Builder,
    symbol_id: SymbolId,
    component_analysis: &ComponentAnalysis,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let driver_analysis = builder.get_driver_analysis(symbol_id);
    for (comp_id, drivers) in driver_analysis.drivers() {
        let Some(component) = component_analysis.component(*comp_id) else { continue };
        let expected = match component.kind() {
            Some(ComponentKind::Reg) => DriverType::Latched,
            Some(ComponentKind::Wire) | Some(ComponentKind::Incoming) | Some(ComponentKind::Outgoing) => DriverType::Continuous,
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
        Driver::If(driver_if) => {
            for (_, sub_driver) in &driver_if.clauses {
                collect_wrong_driver_type_errors(builder, sub_driver, expected, path, diagnostics);
            }
            if let Some(else_driver) = &driver_if.else_clause {
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

fn get_all_driver_locations(builder: &mut Builder, symbol_id: SymbolId, _diagnostics: &mut Vec<Diagnostic>) -> IndexMap<BString, Vec<(DriverType, Location)>> {
    let mut driver_locations: IndexMap<BString, Vec<(DriverType, Location)>> = IndexMap::new();

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    let item_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let moddef_node = parsing.ast_node(item_node_id);

    for child in moddef_node.children() {
        if let AstNodePayload::Driver(driver) = child.payload() {
            if let Some(target) = child.target() {
                let target_str = parsing.string(target);
                let entry = (driver.driver_type, child.location());
                if let Some(driver_list) = driver_locations.get_mut(target_str) {
                    driver_list.push(entry);
                } else {
                    driver_locations.insert(target_str.to_owned(), vec![entry]);
                }
            }
        }
    }
    driver_locations
}
