//! Per-item query collecting every `match` node, determining the
//! subject's type, and delegating to
//! `types::match_coverage::check_match_coverage` to report
//! non-exhaustive or unreachable arms.

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

    let mut match_node_ids: Vec<(AstNodeId, Option<bstr::BString>)> = Vec::new();
    collect_match_nodes(&item_node, None, &mut match_node_ids);

    for (node_id, it_context) in match_node_ids {
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
        check_match_coverage(
            builder,
            &node,
            it_context.as_deref().map(bstr::BStr::new),
            &subject_typ,
            &mut diagnostics,
        );
    }

    Arc::new(diagnostics)
}

/// Collects every `match` node with the `it`-context of its enclosing
/// it-block (the dotted instance name, threaded through Submodule,
/// Component, and Socket bodies).
fn collect_match_nodes(
    node: &AstNode<'_>,
    it_context: Option<&bstr::BStr>,
    out: &mut Vec<(AstNodeId, Option<bstr::BString>)>,
) {
    if matches!(node.payload(), AstNodePayload::Match) {
        out.push((node.id(), it_context.map(bstr::BString::from)));
    }
    let instantiation_name = match node.payload() {
        AstNodePayload::Submodule(submodule) => Some(node.parsing.string(submodule.name)),
        AstNodePayload::Component(component) => Some(node.parsing.string(component.name)),
        AstNodePayload::Socket(socket) => Some(node.parsing.string(socket.name)),
        _ => None,
    };
    if let Some(name) = instantiation_name {
        let it_block = node
            .children()
            .into_iter()
            .find(|c| matches!(c.payload(), AstNodePayload::It));
        if let Some(it_block) = it_block {
            let block = &it_block.children()[0];
            let ctx = match it_context {
                Some(ctx) => {
                    let mut joined = bstr::BString::from(ctx);
                    joined.push(b'.');
                    joined.extend_from_slice(name);
                    joined
                }
                None => bstr::BString::from(name),
            };
            collect_match_nodes(block, Some(bstr::BStr::new(&ctx)), out);
            for child in node.children() {
                if matches!(child.payload(), AstNodePayload::It) {
                    continue;
                }
                collect_match_nodes(&child, it_context, out);
            }
            return;
        }
    }
    for child in node.children() {
        collect_match_nodes(&child, it_context, out);
    }
}
