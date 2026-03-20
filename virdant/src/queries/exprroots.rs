use crate::analysis::location::Location;
use crate::analysis::types::ExprRoot;
use crate::db::Builder;

pub(crate) fn build_exprroots(builder: &mut Builder) -> Vec<ExprRoot> {
    let mut exprroots = vec![];
    for package in builder.get_packages() {
        let analysis = builder.get_package_analysis(package);

        for ast_node_id in analysis.expr_roots_node_ids() {
            let location = Location::new(analysis.package(), ast_node_id);
            exprroots.push(ExprRoot::new(location));
        }
    }
    exprroots
}
