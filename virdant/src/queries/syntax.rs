use std::sync::Arc;

use crate::analysis::location::Location;
use crate::types::ExprRoot;
use crate::db::Builder;
use crate::diagnostics::Diagnostic;
use crate::syntax::ast::AstNode;

pub(crate) fn find_exprroots(builder: &mut Builder) -> Arc<Vec<ExprRoot>> {
    let mut exprroots = vec![];
    let packages = builder.get_packages();
    for package in packages.iter() {
        let analysis = builder.get_package_analysis(package.clone());

        for ast_node_id in analysis.expr_roots_node_ids() {
            let location = Location::new(analysis.package(), ast_node_id);
            exprroots.push(ExprRoot::new(location));
        }
    }
    Arc::new(exprroots)
}

pub(crate) fn build_syntax_errors(builder: &mut Builder) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics = vec![];
    let packages = builder.get_packages();
    for package in packages.iter() {
        let parsing = builder.get_parsing(package.clone());
        diagnostics.extend(parsing.diagnostics());
    }

    Arc::new(diagnostics)
}

pub(crate) fn build_all_exprs(builder: &mut Builder) -> Arc<Vec<Location>> {
    let mut exprs = vec![];

    let packages = builder.get_packages();
    for package in packages.iter() {
        let parsing = builder.get_parsing(package.clone());
        collect_expr_locations(parsing.root(), &mut exprs);
    }

    Arc::new(exprs)
}

fn collect_expr_locations(node: AstNode<'_>, exprs: &mut Vec<Location>) {
    if node.is_expr() {
        exprs.push(node.location());
    }

    for child in node.children() {
        collect_expr_locations(child, exprs);
    }
}
