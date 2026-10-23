//! Builds the per-item `TypingContext` (component path/type bindings)
//! and computes the expected type for an `ExprRoot` from its parent
//! context (`Bit` for `when` guards, `Clock` for reg drivers, LHS
//! component type for drivers and `initial` statements, inherited through `when`/`match` arms).

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
        AstNodePayload::DefaultClause => {
            // A platform `outgoing ... default <EXPR>` clause: the
            // default expression is typed against the port's declared
            // type. (In a mod definition the clause is a semantic
            // error, but the expression still type-checks here.)
            let component_node = parent_node.parent().unwrap();
            let typ_node = component_node
                .typ()
                .expect("default clause on a non-component statement");
            builder.get_type_at(typ_node.location()).ok()
        }
        // An `initial` statement's RHS is typed against its target, exactly like a driver.
        AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
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
        AstNodePayload::When => {
            // Guard expressions must be Bit.
            // Body expressions inherit the expected type from the When
            // itself (only reachable when the When is in expression
            // position; in statement position only guards are expr
            // roots).
            let children = parent_node.children();
            let is_guard = crate::syntax::ast::when_arm_children(&children)
                .into_iter()
                .any(|(guard, _)| guard.is_some_and(|g| g.id() == node.id()));
            if is_guard {
                Some(Type::Bit)
            } else {
                build_expected_type(builder,
                    ExprRoot::new(parent_node.location()))
            }
        }

        AstNodePayload::Match => {
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
            let enum_name = parsing.string(enum_def.name);
            let Some(enum_symbol) = symboltable.resolve_item_in_package(enum_name, location.package()) else {
                return None;
            };
            let typedef = builder.get_typedef(enum_symbol.id());
            typedef.width.map(Type::Word)
        }
        _ => {
            todo!("Can't build expected type for: {:?} at {:?} (parent node is {:?})",
                exprroot, node.region(), parent_node.summary());
        }
    }
}
