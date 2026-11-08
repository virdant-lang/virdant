//! Validates driver rules per module: driver type (continuous vs.
//! latched) matches component kind, non-sinking components get no
//! drivers, sink-capable components have exactly one driver. Emits
//! `WrongDriverType`, `DriverForSink`, `NoDrivers`, `NoRegDrivers`,
//! `MultipleDrivers`.

use std::sync::Arc;

use bstr::{BString, ByteSlice};
use indexmap::IndexMap;

use crate::analysis::Location;
use crate::analysis::component::ComponentAnalysis;
use crate::analysis::drivers::{Driver, DriverAnalysis};
use crate::analysis::symbols::SymbolId;
use crate::common::{ComponentKind, DriverType};
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticPayload};
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
                    diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::DriverForSink {
                            target: path.clone(),
                        },
                    ));
                }
            }
        }
    }

    for (path, component) in component_analysis.components() {
        if matches!(
            component.kind(),
            Some(ComponentKind::Bus) | Some(ComponentKind::Net)
        ) {
            continue;
        }
        let drivers = driver_analysis.drivers().get(&component.id());
        if component.can_sink() {
            match drivers {
                None => {
                    let region = builder.get_location_region(component.location());
                    let no_drivers_diag = match component.kind() {
                        Some(ComponentKind::Reg) | Some(ComponentKind::OutgoingReg) => {
                            Some(DiagnosticPayload::NoRegDrivers(diagnostics::NoRegDrivers {
                                target: path.clone(),
                            }))
                        }
                        // A memory with an image annotation is a ROM: it
                        // legitimately has no write site. Any other
                        // memory requires exactly one write site.
                        Some(ComponentKind::Memory) => {
                            let decl = parsing.ast_node(component.location().ast_node_id());
                            if memory_has_image(&parsing, &decl) {
                                continue;
                            }
                            Some(DiagnosticPayload::NoRegDrivers(diagnostics::NoRegDrivers {
                                target: path.clone(),
                            }))
                        }
                        _ => Some(DiagnosticPayload::NoDrivers(diagnostics::NoDrivers {
                            target: path.clone(),
                        })),
                    };
                    if let Some(payload) = no_drivers_diag {
                        diagnostics.push(Diagnostic::new(region, payload));
                    }
                }
                Some(drivers) if drivers.len() > 1 => {
                    for driver in drivers {
                        if let Some(location) = driver.location() {
                            let region = builder.get_location_region(location);
                            diagnostics.push(Diagnostic::new(
                                region,
                                diagnostics::MultipleDrivers {
                                    target: path.clone(),
                                },
                            ));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    check_overlapping_memory_writes(builder, &driver_analysis, &component_analysis, &mut diagnostics);

    Arc::new(diagnostics)
}

/// Whether a memory declaration carries an `@image`/`@image_hex`/
/// `@image_inline` annotation (its contents come from the image;
/// writes are not allowed and a missing write site is not an error).
fn memory_has_image(parsing: &Parsing, decl: &AstNode<'_>) -> bool {
    let children = decl.children();
    let Some(annotations) = children.first() else { return false };
    if !matches!(annotations.payload(), AstNodePayload::Annotations(_)) {
        return false;
    }
    annotations.children().iter().any(|annotation| {
        matches!(
            annotation.payload(),
            AstNodePayload::Annotation(annotation)
                if matches!(
                    parsing.string(annotation.name).as_bytes(),
                    b"image" | b"image_hex" | b"image_inline"
                ),
        )
    })
}

/// The syntactic conflicting-write check: in one
/// write site, two sibling `when` clauses whose guards are not
/// related by `else` (match arms are disjoint by construction) may
/// both fire in one cycle, which a single-port memory cannot honor.
/// The driver tree already pairs sibling clauses per component, so the
/// check is a walk over each memory's driver tree counting the
/// non-`Absent` `when` clauses of every `Driver::If`.
fn check_overlapping_memory_writes(
    builder: &mut Builder,
    driver_analysis: &Arc<DriverAnalysis>,
    component_analysis: &ComponentAnalysis,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (comp_id, drivers) in driver_analysis.drivers() {
        let Some(component) = component_analysis.component(*comp_id) else { continue };
        if component.kind() != Some(ComponentKind::Memory) {
            continue;
        }
        let path = component.path();
        // A memory initialized from an image is a ROM: its contents
        // come from the file and writes are not supported.
        let location = component.location();
        let decl_parsing = builder.get_parsing(location.package());
        let decl = decl_parsing.ast_node(location.ast_node_id());
        if memory_has_image(&decl_parsing, &decl) {
            for driver in drivers {
                if let Some(location) = driver.location() {
                    let region = builder.get_location_region(location);
                    diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::MemoryImageWrite {
                            component: path.clone(),
                        },
                    ));
                }
            }
            continue;
        }
        for driver in drivers {
            collect_overlapping_memory_writes(builder, driver, &path, diagnostics);
        }
    }
}

fn collect_overlapping_memory_writes(
    builder: &mut Builder,
    driver: &Driver,
    path: &BString,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match driver {
        Driver::If(driver_if) => {
            let writing_clauses = driver_if
                .clauses
                .iter()
                .filter(|(_, sub)| !matches!(sub.as_ref(), Driver::Absent))
                .count();
            // The `else` arm is exclusive with every clause; only
            // `when` clauses can conflict with each other.
            if writing_clauses > 1 {
                for (location, sub) in &driver_if.clauses {
                    if matches!(sub.as_ref(), Driver::Absent) {
                        continue;
                    }
                    let region = builder.get_location_region(location.clone());
                    diagnostics.push(Diagnostic::new(
                        region,
                        diagnostics::OverlappingMemoryWrites {
                            component: path.clone(),
                        },
                    ));
                }
            }
            for (_, sub_driver) in &driver_if.clauses {
                collect_overlapping_memory_writes(builder, sub_driver, path, diagnostics);
            }
            if let Some(else_driver) = &driver_if.else_clause {
                collect_overlapping_memory_writes(builder, else_driver, path, diagnostics);
            }
        }
        Driver::Match(driver_match) => {
            for (_, sub_driver) in &driver_match.arms {
                collect_overlapping_memory_writes(builder, sub_driver, path, diagnostics);
            }
            if let Some(else_driver) = &driver_match.else_clause {
                collect_overlapping_memory_writes(builder, else_driver, path, diagnostics);
            }
        }
        _ => {}
    }
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
            // Memory writes commit on the clock edge; `:=` is accepted
            // with a `MemoryAssignInsteadOfLassign` warning instead of a
            // `WrongDriverType` error.
            Some(ComponentKind::Memory) => DriverType::Latched,
            Some(ComponentKind::Wire) | Some(ComponentKind::Incoming)
            | Some(ComponentKind::Outgoing) | Some(ComponentKind::OutgoingWire)
            | Some(ComponentKind::Bus) | Some(ComponentKind::Net) => DriverType::Continuous,
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
                diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::WrongDriverType {
                        target: path.clone(),
                        expected_driver_type: expected,
                    },
                ));
            }
        }
        Driver::Bidirectional(_) => {}
        Driver::Absent => {}
        Driver::Write { driver_type, stmt, .. } => {
            // Only memories take `Write` drivers.  `<=` is the required
            // spelling; a `:=` warns (`MemoryAssignInsteadOfLassign`)
            // but still writes, so fixing the warning never changes
            // whether the program is legal.
            if *driver_type == DriverType::Continuous && expected == DriverType::Latched {
                let region = builder.get_location_region(stmt.clone());
                diagnostics.push(Diagnostic::new(
                    region,
                    diagnostics::MemoryAssignInsteadOfLassign {
                        component: path.clone(),
                    },
                ));
            }
        }
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
                    // If the submodule has an ItBlock, recurse into its
                    // children.  The ItBlock is found by payload rather
                    // than by index, so the optional GenericsParams
                    // child of a type-argument instantiation does not
                    // shift it out from under us.
                    if let Some(it_block) = sub_children
                        .iter()
                        .find(|c| matches!(c.payload(), AstNodePayload::It))
                    {
                        stack.push(it_block.id());
                    }
                }
                AstNodePayload::ModDefStmtBlock
                | AstNodePayload::If
                | AstNodePayload::Match => {
                    stack.push(child.id());
                }
                _ => {}
            }
        }
    }

    driver_locations
}
