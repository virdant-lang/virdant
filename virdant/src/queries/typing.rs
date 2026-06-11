use bstr::ByteSlice;
use crate::analysis::symbols::SymbolId;
use crate::types::{ExprRoot, Type, TypingContext};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::syntax::payload::AstNodePayload;

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

            // Walk up through any enclosing ModDefStmtBlocks and ModDefStmtWhens
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
        AstNodePayload::ModDefStmtWhen => {
            // Children of ModDefStmtWhen: [guard_0?, body_0, guard_1?, body_1, ...]
            // Only the guard expressions are registered as expr roots.
            // Each guard must be a Bit.
            Some(Type::Bit)
        }
        AstNodePayload::ModDefStmtMatch => {
            // The only expr root whose parent is ModDefStmtMatch is the subject expression
            // (child[0]).  Its expected type is unconstrained; let the expression itself
            // determine its own type.
            None
        }

        AstNodePayload::ExprWhen => {
            // Children of ExprWhen: [guard_0?, body_0, guard_1?, body_1, ...]
            // case arms: guard at even index, body at odd index
            // else arms: body only at even index (no preceding guard)
            // Guard expressions must be Bit.
            // Body expressions inherit the expected type from the ExprWhen itself.
            let children = parent_node.children();
            for (i, child) in children.iter().enumerate() {
                if child.id() == node.id() {
                    // Check if this is a guard or a body
                    let is_guard = if i > 0 && i % 2 == 0 {
                        // Even index > 0: could be a guard or an else body
                        // It's a guard if the previous child (i-1) is also a guard or body
                        // Actually, simpler: it's a guard if this node is followed by another expr
                        i + 1 < children.len() && children[i + 1].is_expr()
                    } else if i == 0 {
                        // First child: must be a guard (case arms start with guard)
                        true
                    } else {
                        // Odd index: this is a body (follows a guard)
                        false
                    };

                    if is_guard {
                        return Some(Type::Bit);
                    } else {
                        // Body expression - inherit expected type from parent ExprWhen
                        return build_expected_type(builder,
                            ExprRoot::new(parent_node.location()));
                    }
                }
            }
            None
        }

        AstNodePayload::ExprMatch => {
            // The subject expression (child[0]) has unconstrained expected type.
            // Body expressions inherit the expected type from the ExprMatch itself.
            let children = parent_node.children();
            if children[0].id() == node.id() {
                // This is the subject expression
                None
            } else {
                // This is a body expression - inherit expected type from parent ExprMatch
                build_expected_type(builder, ExprRoot::new(parent_node.location()))
            }
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
