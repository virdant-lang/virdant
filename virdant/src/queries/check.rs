use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic};

pub(crate) fn check(builder: &mut Builder) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors());
    diagnostics.extend(builder.get_symboltable().diagnostics.clone());
    diagnostics.extend(builder.typecheck());
    check_all_exprs_have_types(builder, &mut diagnostics);

    diagnostics
}

fn check_all_exprs_have_types(builder: &mut Builder, diagnostics: &mut Vec<Diagnostic>) {
    for (location, opt_typ) in builder.get_typeof_all().iter() {
        if opt_typ.is_none() {
            let region = builder.get_location_region(location.clone());
            diagnostics.push(
                diagnostics::Unknown {
                    region,
                    message: "Missing type in AST".into(),
                }.into()
            );
        }
    }
}
