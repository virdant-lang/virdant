use crate::analysis::location::Location;
use crate::db::Builder;
use crate::diagnostics::Diagnostic;
use crate::syntax::ast::AstNode;

pub(crate) fn build_syntax_errors(builder: &mut Builder) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package);
        diagnostics.extend(parsing.diagnostics());
    }

    diagnostics
}

pub(crate) fn build_all_exprs(builder: &mut Builder) -> Vec<Location> {
    let mut exprs = vec![];

    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package);
        collect_expr_locations(parsing.root(), &mut exprs);
    }

    exprs
}

fn collect_expr_locations(node: AstNode<'_>, exprs: &mut Vec<Location>) {
    if node.is_expr() {
        exprs.push(node.location());
    }

    for child in node.children() {
        collect_expr_locations(child, exprs);
    }
}
