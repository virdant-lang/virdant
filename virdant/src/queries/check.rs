use crate::db::Builder;
use crate::diagnostics::Diagnostic;

pub(crate) fn check(builder: &mut Builder) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors());

    let symboltable = builder.get_symboltable();
    diagnostics.extend(symboltable.diagnostics.clone());

    let type_index = builder.get_type_index();
    diagnostics.extend(type_index.diagnostics());

    for item in symboltable.items() {
        diagnostics.extend(builder.check_drivers(item.id()));
    }

    for item in symboltable.items() {
        diagnostics.extend(builder.typecheck(item.id()));
    }

    diagnostics
}
