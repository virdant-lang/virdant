use bstr::{BStr, BString, ByteSlice};
use hashbrown::HashMap;

use crate::analysis::Location;
use crate::analysis::symbols::{Symbol, SymbolId, SymbolTable};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::fqn::PackageFqn;
use crate::source::Region;
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

    let mut driver_locations = get_all_driver_locations(builder, symbol_id);

    for child in moddef_node.children() {
        if let AstNodePayload::Driver(_driver) = child.payload() {
            if let Some(target) = child.target() {
                let target_str = parsing.string(target);
                if let Some(driver_list) = driver_locations.get_mut(target_str) {
                    driver_list.push(child.location());
                } else {
                    diagnostics.push(diagnostics::UnresolvedComponent {
                        region: child.region(),
                        path: target_str.into(),
                    }.into());
                }
            }
        }
    }

    for (component_path, locations) in driver_locations {
        let slot_symbol = get_slot_symbol(builder, &symboltable, &symbol, &component_path);
        let location = slot_symbol.location();
        let node = parsing.ast_node(location.ast_node_id());

        if let AstNodePayload::Component(component) = node.payload() {
            if component.kind == ComponentKind::Incoming {
                if !locations.is_empty() {
                    diagnostics.push(diagnostics::IncomingDriver {
                        region: node.region(),
                        target: component_path,
                    }.into());
                }
                continue;
            }
        }
        let region = builder.get_location_region(location.clone());

        if locations.is_empty() {
            diagnostics.push(diagnostics::NoDrivers {
                region: node.region(),
                target: component_path,
            }.into());
        } else if locations.len() > 1 {
            diagnostics.push(diagnostics::MultipleDrivers {
                region: region.clone(),
                target: component_path.clone(),
            }.into());
            for location in locations.into_iter().skip(1) {
                let diag: Diagnostic = diagnostics::MultipleDrivers {
                    region: region.clone(),
                    target: component_path.clone(),
                }.into();
                diagnostics.push(diag.to_info());
            }
        }
    }

    diagnostics
}

fn get_all_driver_locations(builder: &mut Builder, symbol_id: SymbolId) -> HashMap<BString, Vec<Location>> {
    let mut driver_locations: HashMap<BString, Vec<Location>> = HashMap::new();

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    let item_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let item_node = parsing.ast_node(item_node_id);

    // Seed driver_locations with only the components that need drivers from within this module:
    //   - own Outgoing, Wire, Reg  → driven internally
    //   - submodule Incoming ports → driven from the parent
    // Excluded: own Incoming (driven from outside) and submodule Outgoing (driven by the submodule).
    for child in item_node.children() {
        match child.payload() {
            AstNodePayload::Component(component) => match component.kind {
                ComponentKind::Outgoing | ComponentKind::Wire | ComponentKind::Reg => {
                    let name = parsing.string(component.name).to_owned();
                    driver_locations.insert(name, vec![]);
                }
                ComponentKind::Incoming => {}
            },
            AstNodePayload::Module(module) => {
                let instance_name = parsing.string(module.name);
                let ofness_node = child.child(0);
                if let AstNodePayload::Ofness(ofness) = ofness_node.payload() {
                    let submodule_package = ofness
                        .package
                        .map(|pkg| PackageFqn::new(BString::from(parsing.string(pkg).to_vec())))
                        .unwrap_or_else(|| symbol.package());
                    let submodule_name = parsing.string(ofness.name);
                    if let Some(sub) = symboltable.resolve_item_in_package(submodule_name, submodule_package) {
                        let sub = sub.clone();
                        let sub_parsing = builder.get_parsing(sub.package());
                        let sub_node = sub_parsing.ast_node(sub.location().ast_node_id());
                        for sub_child in sub_node.children() {
                            if let AstNodePayload::Component(component) = sub_child.payload() {
                                if component.kind == ComponentKind::Incoming {
                                    let port_name = sub_parsing.string(component.name);
                                    let qualified = BString::from(
                                        format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes(),
                                    );
                                    driver_locations.insert(qualified, vec![]);
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
    driver_locations
}

fn get_slot_symbol(builder: &mut Builder, symboltable: &SymbolTable, in_item: &Symbol, path: &BString) -> Symbol {
    use bstr::ByteSlice;
    let parts = path.split(|c| *c == b'.').collect::<Vec<_>>();
    let name = BStr::new(&parts[0]);
    let item_fqn = in_item.fqn();
    let slot_fqn: BString = format!("{item_fqn}::{name}").into();
    let slot = symboltable.resolve(slot_fqn.as_bstr()).unwrap();

    slot.clone()
}
