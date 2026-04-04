use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

//use crate::analysis::Location;
//use crate::analysis::symbols::SymbolId;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::payload::AstNodePayload;

/*
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

    let driver_locations = get_all_driver_locations(builder, symbol_id, &mut diagnostics);
    for (path, driver_locations) in driver_locations.iter() {
        if let Some(component) = component_analysis.resolve(path.as_bstr()) {
            if !component.can_sink() && driver_locations.len() > 0 {
                for location in driver_locations {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::DriverForSink {
                        region,
                        target: path.clone(),
                    }.into());
                }
            } else if component.can_sink() && driver_locations.len() > 1 {
                for location in driver_locations {
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(diagnostics::MultipleDrivers {
                        region,
                        target: path.clone(),
                    }.into());
                }
            }
        } else {
            for location in driver_locations {
                let region = builder.get_location_region(location.clone());
                diagnostics.push(diagnostics::UnresolvedComponent {
                    region,
                    path: path.clone(),
                }.into());
            }
        }
    }

    /*
     * TODO Make this work with ModDefStmtIf statements
    for (path, component) in component_analysis.components() {
        if component.can_sink() && !driver_locations.contains_key(&path) {
            let location = component.location();
            let region = builder.get_location_region(location.clone());
            diagnostics.push(diagnostics::NoDrivers {
                region,
                target: path.clone(),
            }.into());
        }
    }
    */

    diagnostics
}

fn get_all_driver_locations(builder: &mut Builder, symbol_id: SymbolId, _diagnostics: &mut Vec<Diagnostic>) -> IndexMap<BString, Vec<Location>> {
    let mut driver_locations: IndexMap<BString, Vec<Location>> = IndexMap::new();

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    let item_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let moddef_node = parsing.ast_node(item_node_id);

    for child in moddef_node.children() {
        if let AstNodePayload::Driver(_driver) = child.payload() {
            if let Some(target) = child.target() {
                let target_str = parsing.string(target);
                if let Some(driver_list) = driver_locations.get_mut(target_str) {
                    driver_list.push(child.location());
                } else {
                    driver_locations.insert(target_str.to_owned(), vec![child.location()]);
                }
            }
        }
    }
    driver_locations
}
*/
