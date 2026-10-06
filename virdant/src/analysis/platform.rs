//! Platform items: accessors for reading platform data (`@fpga`,
//! `@part`, `@pin`, `@period_ns`), resolution of a module's `for`
//! clause, and the per-module exact port match.  Platforms are
//! unvalidated data: these are plain functions over the platform
//! item's AST, not queries, and nothing here validates annotation
//! values or cardinalities (OVERVIEW Decision 13).

use bstr::BString;
use bstr::ByteSlice;

use crate::analysis::symbols::{SymbolId, SymbolKind};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::syntax::ast::item_children;
use crate::syntax::ast::AnnotationValue;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

pub fn platform_fpga(builder: &mut Builder, symbol_id: SymbolId) -> Option<BString> {
    first_str(builder, symbol_id, "fpga")
}

pub fn platform_part(builder: &mut Builder, symbol_id: SymbolId) -> Option<BString> {
    first_str(builder, symbol_id, "part")
}

fn first_str(builder: &mut Builder, symbol_id: SymbolId, name: &str) -> Option<BString> {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    debug_assert_eq!(symbol.kind(), SymbolKind::Platform);

    let package = symbol.location().package();
    let parsing = builder.get_parsing(package);
    let node = parsing.ast_node(symbol.location().ast_node_id());
    match node.annotation_values(&parsing, name).into_iter().next() {
        Some(AnnotationValue::Str(value)) => Some(value),
        _ => None,
    }
}

/// (port name, pin string) for every `Incoming`/`Outgoing` port
/// carrying a `@pin` annotation.
pub fn platform_pins(builder: &mut Builder, symbol_id: SymbolId) -> Vec<(BString, BString)> {
    platform_ports(builder, symbol_id, |parsing, child, port_name| {
        let Some(value) = child
            .annotation_values(parsing, "pin")
            .into_iter()
            .next()
        else { return None; };
        Some((port_name, pin_value_to_string(value)))
    })
}

/// (port name, pin string, period_ns) for every Clock-typed incoming
/// port carrying both a `@pin` and a `@period_ns` annotation.
pub fn platform_clocks(builder: &mut Builder, symbol_id: SymbolId)
    -> Vec<(BString, BString, f64)>
{
    platform_ports(builder, symbol_id, |parsing, child, port_name| {
        if !is_clock_port(parsing, child) {
            return None;
        }
        let Some(pin) = child
            .annotation_values(parsing, "pin")
            .into_iter()
            .next()
        else { return None; };
        let Some(value) = child
            .annotation_values(parsing, "period_ns")
            .into_iter()
            .next()
        else { return None; };
        let Some(period_ns) = period_value_to_f64(value) else { return None; };
        Some((port_name, pin_value_to_string(pin), period_ns))
    })
}

/// Walks the platform item's `Incoming`/`Outgoing` ports, mapping each
/// through `f`; `None` drops the port from the result.
fn platform_ports<T>(
    builder: &mut Builder,
    symbol_id: SymbolId,
    mut f: impl FnMut(&crate::syntax::parsing::Parsing, &crate::syntax::ast::AstNode<'_>, BString)
        -> Option<T>,
) -> Vec<T> {
    let mut result = vec![];
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    debug_assert_eq!(symbol.kind(), SymbolKind::Platform);

    let package = symbol.location().package();
    let parsing = builder.get_parsing(package);
    let node = parsing.ast_node(symbol.location().ast_node_id());

    for child in item_children(&node) {
        let AstNodePayload::Component(component) = child.payload() else { continue };
        if !matches!(component.kind,
            ComponentKind::Incoming | ComponentKind::Outgoing)
        {
            continue;
        }
        let port_name: BString = parsing.string(component.name).to_owned();
        if let Some(entry) = f(&parsing, &child, port_name) {
            result.push(entry);
        }
    }
    result
}

/// Is this component node an `incoming` port of type `Clock`?
fn is_clock_port(
    parsing: &crate::syntax::parsing::Parsing,
    child: &crate::syntax::ast::AstNode<'_>,
) -> bool {
    let Some(typ_node) = child.typ() else { return false };
    let AstNodePayload::Ofness(ofness) = typ_node.child(0).payload() else {
        return false;
    };
    parsing.string(ofness.name.clone()) == b"Clock"
}

fn pin_value_to_string(value: AnnotationValue) -> BString {
    match value {
        AnnotationValue::Nat(nat) => nat.to_string().into(),
        AnnotationValue::Str(s) => s,
    }
}

fn period_value_to_f64(value: AnnotationValue) -> Option<f64> {
    match value {
        AnnotationValue::Nat(nat) => Some(nat as f64),
        AnnotationValue::Str(s) => s.to_str_lossy().parse::<f64>().ok(),
    }
}

/// Resolve a module's `for` clause to the platform item's symbol.
///
/// - `Ok(None)`: no `for` clause, or the module is an `ext` mod (no
///   ports to match, no meaningful binding).
/// - `Err(diagnostic)`: the clause names an unresolvable item
///   (`UnresolvedItem`) or a non-platform item (`ExpectedPlatform`).
/// - `Ok(Some(platform_symbol_id))`: the clause resolved to a
///   platform.
pub fn resolve_platform_for(
    builder: &mut Builder,
    moddef_symbol_id: SymbolId,
) -> Result<Option<SymbolId>, Diagnostic> {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(moddef_symbol_id);
    debug_assert_eq!(symbol.kind(), SymbolKind::ModDef);

    let package = symbol.location().package();
    let parsing = builder.get_parsing(package);
    let node = parsing.ast_node(symbol.location().ast_node_id());

    if let AstNodePayload::ModDef(mod_def) = node.payload() {
        if mod_def.is_ext {
            return Ok(None);
        }
    }

    let Some(for_node) = node.platform_for() else {
        return Ok(None);
    };
    let AstNodePayload::Ofness(ofness) = for_node.payload() else {
        return Ok(None);
    };

    let symboltable = builder.get_symboltable();

    let name: BString = parsing.string(ofness.name).to_owned();
    let platform_symbol = match ofness.package {
        Some(pkg) => {
            let packages = builder.get_packages();
            let pkg_id = packages.id(parsing.string(pkg));
            pkg_id.and_then(|pkg_id| symboltable.resolve_item(name.as_bstr(), pkg_id).cloned())
        }
        None => {
            // Bare name: the defining package first, then the import set.
            symboltable
                .resolve_item(name.as_bstr(), package)
                .cloned()
                .or_else(|| {
                    let package_analysis = builder.get_package_analysis(package);
                    for import in package_analysis.imports() {
                        if import == package {
                            continue;
                        }
                        if let Some(symbol) =
                            symboltable.resolve_item_in_package(name.as_bstr(), import)
                        {
                            return Some(symbol.clone());
                        }
                    }
                    None
                })
        }
    };

    let Some(platform_symbol) = platform_symbol else {
        return Err(Diagnostic::new(
            for_node.region(),
            diagnostics::UnresolvedItem {
                item: name,
            },
        ));
    };

    if platform_symbol.kind() != SymbolKind::Platform {
        return Err(Diagnostic::new(
            for_node.region(),
            diagnostics::ExpectedPlatform {
                item: platform_symbol.name().to_owned(),
            },
        ));
    }

    Ok(Some(platform_symbol.id()))
}

/// The exact port match for one module (Decision 5): every module port
/// must have a same-name, same-direction, same-type platform port, and
/// vice versa.
pub fn check_platform_ports_for(
    builder: &mut Builder,
    moddef_symbol_id: SymbolId,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let platform_id = match resolve_platform_for(builder, moddef_symbol_id) {
        Ok(Some(platform_id)) => platform_id,
        Ok(None) => return,
        Err(diagnostic) => {
            diagnostics.push(diagnostic);
            return;
        }
    };

    let module_ports = builder.get_ports_of(moddef_symbol_id);
    let platform_ports = builder.get_ports_of(platform_id);

    let symboltable = builder.get_symboltable();
    let platform_name: BString = symboltable.symbol(platform_id).name().to_owned().into();
    let module_package = symboltable.symbol(moddef_symbol_id).location().package();
    let module_parsing = builder.get_parsing(module_package);
    let moddef_node =
        module_parsing.ast_node(symboltable.symbol(moddef_symbol_id).location().ast_node_id());

    for module_port in module_ports.iter() {
        let Some(platform_port) = platform_ports
            .iter()
            .find(|p| p.path == module_port.path)
        else {
            // The module declares a port the platform does not have.
            diagnostics.push(Diagnostic::new(
                module_port_region(builder, moddef_symbol_id, &module_port.path),
                diagnostics::PlatformExtraPort {
                    platform: platform_name.clone(),
                    port: module_port.path.clone(),
                },
            ));
            continue;
        };

        if module_port.dir != platform_port.dir {
            diagnostics.push(Diagnostic::new(
                module_port_region(builder, moddef_symbol_id, &module_port.path),
                diagnostics::PlatformPortDirMismatch {
                    port: module_port.path.clone(),
                    expected: platform_port.dir,
                    actual: module_port.dir,
                },
            ));
        }

        if module_port.typ != platform_port.typ {
            diagnostics.push(Diagnostic::new(
                module_port_region(builder, moddef_symbol_id, &module_port.path),
                diagnostics::PlatformPortTypeMismatch {
                    port: module_port.path.clone(),
                    expected: platform_port.typ.clone().unwrap_or(Type::Bit),
                    actual: module_port.typ.clone().unwrap_or(Type::Bit),
                },
            ));
        }
    }

    for platform_port in platform_ports.iter() {
        if !module_ports.iter().any(|p| p.path == platform_port.path) {
            // The platform requires a port the module does not declare.
            diagnostics.push(Diagnostic::new(
                moddef_node.region(),
                diagnostics::PlatformMissingPort {
                    platform: platform_name.clone(),
                    port: platform_port.path.clone(),
                },
            ));
        }
    }
}

fn module_port_region(builder: &mut Builder, moddef_id: SymbolId, port_name: &BString) -> crate::common::source::Region {
    let symboltable = builder.get_symboltable();
    match symboltable.slot(moddef_id, port_name.as_bstr()) {
        Some(slot) => {
            let location = slot.location();
            builder.get_location_region(location)
        }
        None => {
            let symbol = symboltable.symbol(moddef_id);
            let location = symbol.location();
            builder.get_location_region(location)
        }
    }
}
