define_queries! {
    input packages from crate::queries::packages;
    input source from crate::queries::source;
    query ast from crate::queries::ast;
    query diagnostics from crate::queries::diagnostics;
}
