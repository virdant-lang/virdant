use crate::analysis::symbols::SymbolId;
use crate::types::{ExprRoot, Type, TypingContext};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_typing_context(builder: &mut Builder, item: SymbolId) -> TypingContext {
    let component_analysis = builder.get_component_analysis(item);

    // TODO How does this need to get used?
    let _typedefs = builder.get_typedefs();

    let mut context = TypingContext {
        context: vec![],
    };
    for (path, component) in component_analysis.components() {
        if let Some(typ) = component.typ() {
            context.context.push((path, typ));
        }
    }

    context
}

pub(crate) fn build_expected_type(builder: &mut Builder, exprroot: ExprRoot) -> Option<Type> {
    let location = exprroot.location();
    let parsing = builder.get_parsing(location.package());
    let symboltable = builder.get_symboltable();

    let node = parsing.ast_node(location.ast_node_id());
    let parent_node = node.parent().unwrap();

    match parent_node.payload() {
        AstNodePayload::Component(component) if component.kind == ComponentKind::Reg => Some(Type::Clock),
        AstNodePayload::Driver(_) => {
            let lhs_path = parsing.string(parent_node.child(0).path().unwrap());

            // Walk up through any enclosing ModDefStmtBlocks and ModDefStmtIfs
            // to find the containing item (ModDef). Loop over AstNodeId (Copy)
            // to avoid holding overlapping borrows on the AstNode.
            let mut ancestor_id = parent_node.parent().unwrap().id();
            loop {
                let ancestor = parsing.ast_node(ancestor_id);
                if ancestor.is_item() { break; }
                ancestor_id = ancestor.parent().unwrap().id();
            }
            let moddef_node = parsing.ast_node(ancestor_id);
            let moddef_name = parsing.string(moddef_node.name().unwrap());
            let moddef = symboltable
                .resolve_item_in_package(moddef_name, location.package())
                .unwrap();
            let component_analysis = builder.get_component_analysis(moddef.id());
            component_analysis.type_of(lhs_path)
        }
        AstNodePayload::ModDefStmtIf => {
            // Children of ModDefStmtIf: [cond_0, block_0, cond_1, block_1, ..., else_block]
            // Only the condition expressions (even-indexed children) are registered as
            // expr roots with ModDefStmtIf as their parent. Each condition must be a Bit.
            Some(Type::Bit)
        }
        AstNodePayload::ModDefStmtOn => {
            Some(Type::Clock)
        }
        AstNodePayload::CommandAssert => {
            Some(Type::Bit)
        }
        AstNodePayload::CommandIf => {
            Some(Type::Bit)
        }
        AstNodePayload::CommandDisplay(_) => {
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
