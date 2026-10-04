//! Dispatches per-query-kind HTML rendering to specialized renderers.

pub(crate) mod common;

mod components;
mod diagnostics;
mod symbols;
mod typing;

use common::Ctx;

use serde_json::Value as JsonValue;

/// Renders the body HTML for one cached result of the given query
/// kind. `result` is the externally tagged result value (for example
/// `{"Typing": {...}}`). Returns None when no specialized renderer
/// exists, in which case the caller falls back to raw JSON.
pub fn render_result(ctx: &Ctx, kind: &str, result: &JsonValue) -> Option<String> {
    let inner = result.get(kind)?;
    let body = match kind {
        "Check" => diagnostics::render_check(ctx, inner),
        "CheckDrivers" => diagnostics::render_check_drivers(ctx, inner),
        "CombinationalCycleCheck" => diagnostics::render_combinational_cycle_check(ctx, inner),
        "MatchCoverage" => diagnostics::render_match_coverage(ctx, inner),
        "SyntaxErrors" => diagnostics::render_syntax_errors(ctx, inner),
        "TypeCheck" => diagnostics::render_type_check(ctx, inner),

        "AllExprs" => symbols::render_all_exprs(ctx, inner),
        "ExprRootFor" => symbols::render_expr_root_for(ctx, inner),
        "ExprRoots" => symbols::render_expr_roots(ctx, inner),
        "LocationRegion" => symbols::render_location_region(ctx, inner),
        "PackageAnalysis" => symbols::render_package_analysis(ctx, inner),
        "Packages" => symbols::render_packages(ctx, inner),
        "SymbolAst" => symbols::render_symbol_ast(ctx, inner),
        "SymbolTable" => symbols::render_symbol_table(ctx, inner),
        "TypeDef" => symbols::render_typedef(ctx, inner),
        "TypeDefs" => symbols::render_typedefs(ctx, inner),
        "TypeIndex" => symbols::render_type_index(ctx, inner),

        "Component" => components::render_component(ctx, inner),
        "ComponentAnalysis" => components::render_component_analysis(ctx, inner),
        "CtorSignature" => components::render_ctor_signature(ctx, inner),
        "DependencyGraph" => components::render_dependency_graph(ctx, inner),
        "DriverAnalysis" => components::render_driver_analysis(ctx, inner),
        "PortsOf" => components::render_ports_of(ctx, inner),
        "StructFields" => components::render_struct_fields(ctx, inner),
        "TypingContext" => components::render_typing_context(ctx, inner),

        "Elaboration" => typing::render_elaboration(ctx, inner),
        "ExpectedType" => typing::render_expected_type(ctx, inner),
        "Parsing" => typing::render_parsing(ctx, inner),
        "Source" => typing::render_source(ctx, inner),
        "String" => typing::render_string(ctx, inner),
        "TypeAt" => typing::render_type_at(ctx, inner),
        "Typeof" => typing::render_typeof(ctx, inner),
        "TypeofAll" => typing::render_typeof_all(ctx, inner),
        "Typing" => typing::render_typing(ctx, inner),

        _ => return None,
    };
    Some(body)
}
