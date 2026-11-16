//! The clock checking query: builds a [`Clocking`] — the clock-domain
//! engine for a single module — and runs it against the module.  See
//! `clocks` for the checked rules.

use std::sync::Arc;

use crate::clocks::Clocking;
use crate::db::Builder;
use crate::analysis::symbols::SymbolId;

/// Clockcheck one mod def, producing its `Clocking`.  See the module
/// docs for the rules.
pub(crate) fn clockcheck(builder: &mut Builder, symbol_id: SymbolId) -> Arc<Clocking> {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id).clone();
    let component_analysis = builder.get_component_analysis(symbol_id);

    let mut clocking = Clocking::new(symbol, component_analysis);
    clocking.check(builder);
    Arc::new(clocking)
}
