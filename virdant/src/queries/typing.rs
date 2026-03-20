use std::sync::Arc;

use bstr::{BString, ByteSlice};

use crate::analysis::location::Location;
use crate::analysis::symbols::SymbolId;
use crate::analysis::types::{ExprRoot, Type, Typing, TypingContext};
use crate::common::ComponentKind;
use crate::db::Builder;
use crate::diagnostics;
use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNodeId;
use crate::syntax::payload::AstNodePayload;
use hashbrown::HashMap;

pub(crate) fn build_typing_context(builder: &mut Builder, item: SymbolId) -> TypingContext {
    let symboltable = builder.get_symboltable();

    let symbol = symboltable.symbol(item);
    let location = symbol.location();
    let parsing = builder.get_parsing(location.package());

    let item_ast = parsing.ast_node(location.ast_node_id());
    let component_analysis = builder.get_component_analysis(item);

    // TODO How does this need to get used?
    let _typedefs = builder.get_typedefs();

    // TODO HACK this isn't complete
    let mut context = TypingContext(vec![]);
    for (path, opt_typ) in component_analysis.components() {
        if let Some(typ) = opt_typ {
            context.0.push((path, typ));
        }
    }

    for stmt in item_ast.children() {
        let AstNodePayload::Module(module) = stmt.payload() else {
            continue;
        };

        let instance_name = parsing.string(module.name);
        let module = match stmt.child(0).payload() {
            AstNodePayload::Ofness(ofness) => {
                let module_package = ofness
                    .package
                    .map(|package| PackageFqn::new(BString::from(parsing.string(package).to_vec())))
                    .unwrap_or_else(|| location.package());
                let module_name = parsing.string(ofness.name);
                let module = symboltable.resolve_item_in_package(module_name, module_package);
                if module.is_none() {
                    continue;
                }
                module.unwrap()
            }
            _ => todo!(),
        };

        let module_component_analysis = builder.get_component_analysis(module.id());
        for (port_name, opt_typ) in module_component_analysis.components() {
            use bstr::ByteSlice;
            let qualified_name =
                BString::from(format!("{}.{}", instance_name.to_str_lossy(), port_name.to_str_lossy()).into_bytes());
            if let Some(typ) = opt_typ {
                context.0.push((qualified_name, typ));
            }
        }
    }

    context
}

pub(crate) fn build_expected_type(builder: &mut Builder, location: Location) -> Option<Type> {
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
        _ => todo!("Can't build expected type for: {:?}", parent_node.summary()),
    }
}

pub(crate) fn build_typing(builder: &mut Builder, expr_root: ExprRoot) -> Arc<Typing> {
    let location = expr_root.location();
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
    let expected_typ = builder.get_expected_type(location.clone());
    let fallback_expected_typ = expected_typ.clone();
    let expected_typ = expected_typ.unwrap_or_else(|| builtin_bit_type());

    let diagnostics = vec![];
    let mut typing = Typing {
        expr_root: expr_root.clone(),
        context,
        typs: HashMap::new(),
        diagnostics,
        expected_typ: expected_typ.clone(),
    };

    // if there is no expected type, you can't type check the expression.
    if fallback_expected_typ.is_some() {
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

