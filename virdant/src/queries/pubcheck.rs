
use bstr::BString;
use indexmap::IndexSet;

use crate::analysis::symbols::{Symbol, SymbolId};
use crate::common::ComponentKind;
use crate::common::source::Region;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::package::PackageId;
use crate::syntax::ast::AstNode;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

pub(crate) fn item_is_pub(builder: &mut Builder, symbol: &Symbol) -> bool {
    let location = symbol.location();
    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    node.payload().is_pub()
}

pub(crate) fn check_item_reference(
    builder: &mut Builder,
    symbol: &Symbol,
    referencing_package: PackageId,
    region: Region,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let symbol_package = symbol.location().package();
    if symbol_package == referencing_package {
        return;
    }
    if item_is_pub(builder, symbol) {
        return;
    }
    let packages = builder.get_packages();
    let item = symbol.name().to_owned();
    let package = packages.name(symbol_package).to_owned();
    diagnostics.push(Diagnostic::new(
        region,
        diagnostics::NotPub {
            item: item.clone(),
            package: package.clone(),
        },
    ));
    // Also warn at the definition site, so the author of the non-`pub`
    // item learns that another package depends on it.
    diagnostics.push(Diagnostic::new(
        builder.get_location_region(symbol.location()),
        diagnostics::ReferencedWhileNotPub {
            item,
            package,
            referencing_package: packages.name(referencing_package).to_owned(),
        },
    ));
}

pub(crate) fn check_pub_visibility(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let packages = builder.get_packages();

    for package in packages.ids() {
        let type_index = builder.get_type_index();
        let symboltable = builder.get_symboltable();
        let parsing = builder.get_parsing(package);
        for location in walk_types(&parsing.root()) {
            let Some(typ) = type_index.type_at(location.clone()) else {
                continue;
            };
            let Type::Usual(symbol_id, _) = typ else {
                continue;
            };
            let symbol = symboltable.symbol(*symbol_id);
            let region = builder.get_location_region(location.clone());
            check_item_reference(
                builder,
                &symbol,
                package,
                region,
                diagnostics,
            );
        }
    }
}

fn walk_types<'p>(root: &'p AstNode<'p>) -> Vec<crate::analysis::Location> {
    let mut locations = vec![];
    walk_types_rec(root, &mut locations);
    locations
}

fn walk_types_rec<'p>(node: &AstNode<'p>, locations: &mut Vec<crate::analysis::Location>) {
    if matches!(node.payload(), AstNodePayload::Type(_)) {
        locations.push(node.location());
    }
    for child in node.children() {
        walk_types_rec(&child, locations);
    }
}

/// A mod with a `for` clause implements a platform, which only top
/// modules do: every such mod must be `pub`.
pub(crate) fn check_for_mods_pub(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if item.kind() != crate::analysis::symbols::SymbolKind::ModDef {
            continue;
        }
        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        let Some(for_node) = node.platform_for() else {
            continue;
        };
        if node.payload().is_pub() {
            continue;
        }
        diagnostics.push(Diagnostic::new(
            for_node.region(),
            diagnostics::TopNotPub {
                item: item.fqn().to_owned(),
            },
        ));
    }
}


pub(crate) fn check_pub_surface(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        if !item.kind().is_item() {
            continue;
        }
        let location = item.location();
        let parsing = builder.get_parsing(location.package());
        let node = parsing.ast_node(location.ast_node_id());
        if !node.payload().is_pub() {
            continue;
        }
        let owner: BString = item.fqn().to_owned();

        if let AstNodePayload::ModDef(_) = node.payload() {
            if let Some(for_node) = node.platform_for() {
                if let Some(platform) = resolve_ofness_target(builder, &for_node, &node) {
                    check_item_reference(
                        builder,
                        &platform,
                        location.package(),
                        for_node.region(),
                        diagnostics,
                    );
                }
            }
        }

        let mut visited: IndexSet<SymbolId> = IndexSet::new();
        for surface_location in surface_type_locations(builder, location.clone()) {
            check_type_node(builder, surface_location, &owner, &mut visited, diagnostics);
        }
    }
}

fn surface_type_locations(builder: &mut Builder, item_location: crate::analysis::Location) -> Vec<crate::analysis::Location> {
    let parsing = builder.get_parsing(item_location.package());
    let node = parsing.ast_node(item_location.ast_node_id());
    let mut locations = vec![];
    match node.payload() {
        AstNodePayload::ModDef(_) | AstNodePayload::Platform(_) => {
            if let AstNodePayload::ModDef(_) = node.payload() {
                if let Some(for_node) = node.platform_for() {
                    if let Some(platform) = resolve_ofness_target(builder, &for_node, &node) {
                        locations.extend(surface_type_locations(builder, platform.location()));
                    }
                }
            }
            for child in node.children() {
                let AstNodePayload::Component(component) = child.payload() else {
                    continue;
                };
                if !matches!(
                    component.kind,
                    ComponentKind::Incoming
                        | ComponentKind::Outgoing
                        | ComponentKind::OutgoingWire
                        | ComponentKind::OutgoingReg
                        | ComponentKind::Bus
                ) {
                    continue;
                }
                if let Some(typ) = child.typ() {
                    locations.push(typ.location());
                }
            }
        }
        AstNodePayload::StructDef(_) => {
            for child in node.children() {
                if matches!(child.payload(), AstNodePayload::Field(_)) {
                    if let Some(typ) = typ_of_field(&child) {
                        locations.push(typ.location());
                    }
                }
            }
        }
        AstNodePayload::UnionDef(_) | AstNodePayload::EnumDef(_) => {
            for child in node.children() {
                match child.payload() {
                    AstNodePayload::Ctor(_) | AstNodePayload::Enumerant(_) => {
                        for param in child.children() {
                            if matches!(param.payload(), AstNodePayload::Param(_)) {
                                if let Some(typ) = typ_of_field(&param) {
                                    locations.push(typ.location());
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        AstNodePayload::SocketDef(_) => {
            for child in node.children() {
                if matches!(child.payload(), AstNodePayload::Channel(_)) {
                    if let Some(typ) = child.typ() {
                        locations.push(typ.location());
                    }
                }
            }
        }
        _ => {}
    }
    locations
}

fn typ_of_field<'p>(node: &AstNode<'p>) -> Option<AstNode<'p>> {
    node.children()
        .into_iter()
        .find(|c| matches!(c.payload(), AstNodePayload::Type(_)))
}

fn resolve_ofness_target(
    builder: &mut Builder,
    ofness_node: &AstNode<'_>,
    context: &AstNode<'_>,
) -> Option<Symbol> {
    let AstNodePayload::Ofness(ofness) = ofness_node.payload() else {
        return None;
    };
    let parsing = builder.get_parsing(context.package());
    let packages = builder.get_packages();
    let target_package = match ofness.package {
        Some(pkg) => packages.id(parsing.string(pkg)),
        None => Some(context.package()),
    };
    let name = parsing.string(ofness.name);
    let symboltable = builder.get_symboltable();
    target_package
        .and_then(|pkg| symboltable.resolve_item(name, pkg))
        .cloned()
}

fn check_type_node(
    builder: &mut Builder,
    location: crate::analysis::Location,
    owner: &BString,
    visited: &mut IndexSet<SymbolId>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let parsing = builder.get_parsing(location.package());
    let node = parsing.ast_node(location.ast_node_id());
    let type_index = builder.get_type_index();
    let Some(typ) = type_index.type_at(location.clone()).cloned() else {
        return;
    };

    let symboltable = builder.get_symboltable();
    let symbol_id = match &typ {
        Type::Bit | Type::Clock | Type::Reset | Type::Word(_) | Type::Param(_) => return,
        Type::Usual(symbol_id, _) => *symbol_id,
    };
    let symbol = symboltable.symbol(symbol_id);

    if !item_is_pub(builder, &symbol) {
        diagnostics.push(Diagnostic::new(
            node.region(),
            diagnostics::TypeNotPub {
                typ: symbol.name().to_owned(),
                owner: owner.clone(),
            },
        ));
        return;
    }

    let arg_locations: Vec<crate::analysis::Location> = node
        .children()
        .into_iter()
        .find(|c| matches!(c.payload(), AstNodePayload::GenericsParams(_)))
        .map(|args| {
            args.children()
                .into_iter()
                .filter(|c| matches!(c.payload(), AstNodePayload::Type(_)))
                .map(|c| c.location())
                .collect()
        })
        .unwrap_or_default();
    for arg_location in arg_locations {
        check_type_node(builder, arg_location, owner, visited, diagnostics);
    }

    if visited.insert(symbol.id()) {
        for surface_location in surface_type_locations(builder, symbol.location()) {
            check_type_node(builder, surface_location, owner, visited, diagnostics);
        }
    }
}
