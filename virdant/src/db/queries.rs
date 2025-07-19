define_queries! {
    input packages from crate::queries::packages;
    input source from crate::queries::source;
    query ast from crate::queries::ast;
    query importorder from crate::queries::importorder;
    query itemresolution from crate::queries::itemresolution;
    query diagnostics from crate::queries::diagnostics;
}
