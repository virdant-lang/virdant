//! Validates `initial` statements per module: the target must be a
//! register (`reg` / `outgoing reg`), and each register may have at most
//! one `initial` statement. Emits `InitialForNonReg`, `InitialMultiple`.
//!
//! Placement (`InitialInsideWhenMatch`) and resolution
//! (`UnresolvedComponent`) errors are reported during driver analysis.

use std::sync::Arc;

use crate::analysis::Location;
use crate::analysis::symbols::SymbolId;
use crate::common::ComponentKind;
use crate::common::source::Region;
use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};
use crate::syntax::payload::AstNodePayload;

/// Validates all `initial` statements in the module identified by `symbol_id`.
/// Skips external modules.
///
/// Emits these diagnostics:
/// - [`InitialForNonReg`]: the target is not a `reg` or `outgoing reg`.
/// - [`InitialMultiple`]: a register has more than one `initial` statement
///   (reported on every statement after the first).
pub(crate) fn check_initial(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];
    let symboltable = builder.get_symboltable();

    let symbol = symboltable.symbol(symbol_id);
    let moddef_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let moddef_node = parsing.ast_node(moddef_node_id);

    match moddef_node.payload() {
        AstNodePayload::ModDef(mod_def) if !mod_def.is_ext => (),
        _ => return Arc::new(diagnostics),
    }

    let component_analysis = builder.get_component_analysis(symbol_id);
    let driver_analysis = builder.get_driver_analysis(symbol_id);

    for (comp_id, locs) in driver_analysis.initials() {
        let Some(component) = component_analysis.component(*comp_id) else { continue };
        let is_reg = matches!(
            component.kind(),
            Some(ComponentKind::Reg | ComponentKind::OutgoingReg),
        );

        if !is_reg {
            for loc in locs {
                diagnostics.push(Diagnostic::new(
                    initial_stmt_region(builder, loc),
                    diagnostics::InitialForNonReg {
                        target: component.path(),
                    },
                ));
            }
            continue;
        }

        for loc in locs.iter().skip(1) {
            diagnostics.push(Diagnostic::new(
                initial_stmt_region(builder, loc),
                diagnostics::InitialMultiple {
                    target: component.path(),
                },
            ));
        }
    }

    Arc::new(diagnostics)
}

/// Region of the whole `initial` statement, given the location of its RHS expression.
fn initial_stmt_region(builder: &mut Builder, rhs_loc: &Location) -> Region {
    let parsing = builder.get_parsing(rhs_loc.package());
    let rhs_node = parsing.ast_node(rhs_loc.ast_node_id());
    match rhs_node.parent() {
        Some(stmt) => stmt.region(),
        None => rhs_node.region(),
    }
}
