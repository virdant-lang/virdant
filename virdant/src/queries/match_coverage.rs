use std::sync::Arc;

use crate::analysis::symbols::SymbolId;
use crate::db::Builder;
use crate::diagnostics::Diagnostic;
use crate::syntax::ast::{AstNode, AstNodeId};
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;
use crate::types::match_coverage::check_match_coverage;

pub(crate) fn build_match_coverage(
    builder: &mut Builder,
    symbol_id: SymbolId,
) -> Arc<Vec<Diagnostic>> {
    let mut diagnostics: Vec<Diagnostic> = Vec::new();

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    let item_node_id = builder.get_symbol_ast(symbol.id());
    let parsing = builder.get_parsing(symbol.package());
    let item_node = parsing.ast_node(item_node_id);

    if item_node.contains_errors() {
        return Arc::new(diagnostics);
    }

    let mut match_node_ids: Vec<AstNodeId> = Vec::new();
    collect_match_nodes(&item_node, &mut match_node_ids);

    for node_id in match_node_ids {
        let node = parsing.ast_node(node_id);
        let children = node.children();
        if children.is_empty() {
            continue;
        }
        let subject = &children[0];
        let subject_typ: Type = match builder.get_typeof(subject.location()) {
            Ok(typ) => typ,
            Err(_) => continue,
        };
        check_match_coverage(builder, &node, &subject_typ, &mut diagnostics);
    }

    Arc::new(diagnostics)
}

fn collect_match_nodes(node: &AstNode<'_>, out: &mut Vec<AstNodeId>) {
    match node.payload() {
        AstNodePayload::ExprMatch | AstNodePayload::ModDefStmtMatch => {
            out.push(node.id());
        }
        _ => {}
    }
    for child in node.children() {
        collect_match_nodes(&child, out);
    }
}
