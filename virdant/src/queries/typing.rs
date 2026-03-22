use std::sync::Arc;

use bstr::ByteSlice;

use crate::analysis::location::Location;
use crate::analysis::symbols::SymbolId;
use crate::analysis::types::{ExprRoot, Type, Typing, TypingContext};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics;
use crate::syntax::ast::AstNodeId;
use crate::syntax::payload::AstNodePayload;
use hashbrown::HashMap;

pub(crate) fn build_typing_context(builder: &mut Builder, item: SymbolId) -> TypingContext {
    let component_analysis = builder.get_component_analysis(item);

    // TODO How does this need to get used?
    let _typedefs = builder.get_typedefs();

    let mut context = TypingContext(vec![]);
    for (path, opt_typ) in component_analysis.components() {
        if let Some(typ) = opt_typ {
            context.0.push((path, typ));
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

            let moddef_node = parent_node.parent().unwrap();
            let moddef_name = parsing.string(moddef_node.name().unwrap());
            let moddef = symboltable
                .resolve_item_in_package(moddef_name, location.package())
                .unwrap();
            let component_analysis = builder.get_component_analysis(moddef.id());
            component_analysis.type_of(lhs_path)
        }
        AstNodePayload::ModDefStmtOn => {
            Some(Type::Clock)
        }
        AstNodePayload::CommandAssert => {
            Some(Type::Bit)
        }
        AstNodePayload::CommandDisplay => {
            None
        }
        _ => {
            todo!("Can't build expected type for: {:?} at {:?} (parent node is {:?})",
                exprroot, node.region(), parent_node.summary());
        }
    }
}

pub(crate) fn build_typing(builder: &mut Builder, exprroot: ExprRoot) -> Arc<Typing> {
    let location = exprroot.location();
    let parsing = builder.get_parsing(location.package());

    let mut current_id = location.ast_node_id();
    // REVIEW walk up until you find the containing item ast node.
    let item_name = loop {
        let current = parsing.ast_node(current_id);
        if current.is_item() {
            break parsing
                .string(current.name().expect("expected containing item to have a name"))
                .to_owned();
        }
        current_id = current
            .parent()
            .expect("expected expr root to be contained in an item")
            .id();
    };
    let symboltable = builder.get_symboltable();
    let item = symboltable
        .resolve_item_in_package(item_name.as_bstr(), location.package())
        .unwrap();
    let context = builder.get_typing_context(item.id());

    let node = parsing.ast_node(location.ast_node_id());
    let expected_typ = builder.get_expected_type(exprroot.clone());

    let diagnostics = vec![];
    let mut typing = Typing {
        exprroot: exprroot.clone(),
        context,
        typs: HashMap::new(),
        diagnostics,
        expected_typ: expected_typ.clone().unwrap_or_else(|| builtin_bit_type()),
    };

    // if there is no expected type, you can't type check the expression.
    if let Some(expected_typ) = expected_typ {
        typing.check(&node, &expected_typ);
    } else {
        match typing.infer(&node) {
            Err(diags) => {
                typing.diagnostics.extend(diags);
            }
            Ok(None) => {
                typing.diagnostics.push(diagnostics::Todo {
                    region: node.region(),
                    message: format!("Can't typecheck expression because we don't know what type it should have").into(),
                }.into());
            }
            Ok(Some(typ)) => {
                typing.typs.insert(node.id(), typ);
            }
        }
    }

    Arc::new(typing)
}

fn builtin_bit_type() -> Type {
    Type::Bit
}

