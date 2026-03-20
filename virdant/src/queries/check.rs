use crate::db::Builder;
use crate::diagnostics::{self, Diagnostic, DiagnosticLevel};

pub(crate) fn check(builder: &mut Builder) -> Result<Vec<Diagnostic>, Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors());
    diagnostics.extend(builder.typecheck());

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

    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
