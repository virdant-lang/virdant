use std::sync::Arc;

use crate::analysis::symbols::SymbolKind;
use crate::db::Builder;
use crate::diagnostics::Diagnostic;

pub(crate) fn check(builder: &mut Builder) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors().iter().cloned());

    let symboltable = builder.get_symboltable();
    diagnostics.extend(symboltable.diagnostics.clone());

    let type_index = builder.get_type_index();
    diagnostics.extend(type_index.diagnostics());

    for item in symboltable.items() {
        diagnostics.extend(builder.check_drivers(item.id()).iter().cloned());
    }

    for item in symboltable.items() {
        if item.kind == SymbolKind::ModDef {
            let component_analysis = builder.get_component_analysis(item.id());
            diagnostics.extend(component_analysis.diagnostics());
        }
        diagnostics.extend(builder.typecheck(item.id()).iter().cloned());

//        let driver_analysis = builder.get_driver_analysis(item.id());
//        diagnostics.extend(driver_analysis); // TODO
    }

    Arc::new(diagnostics)
}
