use bstr::ByteSlice;
use crate::analysis::symbols::SymbolId;
use crate::types::{ExprRoot, Type, TypingContext};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::syntax::payload::AstNodePayload;

#[virdant_db::query(get = get_typing_context)]
pub(crate) fn build_typing_context(builder: &mut Builder, item: SymbolId) -> TypingContext {
    let component_analysis = builder.get_component_analysis(item);

    // TODO How does this need to get used?
    let _typedefs = builder.get_typedefs();

    let mut context = TypingContext::new();
    for (path, component) in component_analysis.components() {
        context = context.push_component(path, component.id(), component.typ());
    }

    context
}

#[virdant_db::query(get = get_expected_type)]
pub(crate) fn build_expected_type(builder: &mut Builder, exprroot: ExprRoot) -> Option<Type> {
    let location = exprroot.location();
    let parsing = builder.get_parsing(location.package());
    let symboltable = builder.get_symboltable();

    let node = parsing.ast_node(location.ast_node_id());
    let parent_node = node.parent().unwrap();

    match parent_node.payload() {
        AstNodePayload::Component(component) if component.kind == ComponentKind::Reg
            || component.kind == ComponentKind::OutgoingReg => Some(Type::Clock),
        AstNodePayload::Driver(_) => {
            let mut lhs_path = parsing.string(parent_node.child(0).path().unwrap()).to_owned();

            // Walk up through any enclosing ModDefStmtBlocks and ModDefStmtIfs
            // to find the containing item (ModDef). Loop over AstNodeId (Copy)
            // to avoid holding overlapping borrows on the AstNode.
            let mut ancestor_id = parent_node.parent().unwrap().id();
            let mut resolved_it_name = None;
            loop {
                let ancestor = parsing.ast_node(ancestor_id);
                if ancestor.is_item() { break; }
                if lhs_path.starts_with(b"it.") || lhs_path == b"it" {
                    if let AstNodePayload::Submodule(module) = ancestor.payload() {
                        resolved_it_name = Some(parsing.string(module.name));
                    } else if let AstNodePayload::Component(component) = ancestor.payload() {
                        resolved_it_name = Some(parsing.string(component.name));
                    } else if let AstNodePayload::Socket(socket) = ancestor.payload() {
                        resolved_it_name = Some(parsing.string(socket.name));
                    }
                }
                ancestor_id = ancestor.parent().unwrap().id();
            }

            if let Some(name) = resolved_it_name {
                if lhs_path.starts_with(b"it.") {
                    let suffix = lhs_path[3..].to_owned();
                    lhs_path.clear();
                    lhs_path.extend_from_slice(name);
                    lhs_path.push(b'.');
                    lhs_path.extend_from_slice(&suffix);
                } else {
                    lhs_path = name.to_owned();
                }
            }

            let moddef_node = parsing.ast_node(ancestor_id);
            let moddef_name = parsing.string(moddef_node.name().unwrap());
            let moddef = symboltable
                .resolve_item_in_package(moddef_name, location.package())
                .unwrap();
            let component_analysis = builder.get_component_analysis(moddef.id());
            component_analysis.type_of(lhs_path.as_bstr())
        }
        AstNodePayload::ModDefStmtIf => {
            // Children of ModDefStmtIf: [cond_0, block_0, cond_1, block_1, ..., else_block]
            // Only the condition expressions (even-indexed children) are registered as
            // expr roots with ModDefStmtIf as their parent. Each condition must be a Bit.
            Some(Type::Bit)
        }
        AstNodePayload::ModDefStmtMatch => {
            // The only expr root whose parent is ModDefStmtMatch is the subject expression
            // (child[0]).  Its expected type is unconstrained; let the expression itself
            // determine its own type.
            None
        }

        AstNodePayload::Enumerant(_) => {
            let enumdef_node = parent_node.parent().unwrap();
            let AstNodePayload::EnumDef(enum_def) = enumdef_node.payload() else { unreachable!() };
            Some(Type::Word(enum_def.width))
        }
        _ => {
            todo!("Can't build expected type for: {:?} at {:?} (parent node is {:?})",
                exprroot, node.region(), parent_node.summary());
        }
    }
}
