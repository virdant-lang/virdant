//! Builds the per-item `TypingContext` (component path/type bindings)
//! and computes the expected type for an `ExprRoot` from its parent
//! context (`Bit` for `when` guards, `Clock` for reg drivers, LHS
//! component type for drivers and `initial` statements, inherited through `if`/`match` arms).

use bstr::{BStr, ByteSlice};
use crate::analysis::symbols::SymbolId;
use crate::types::{ExprRoot, Type, TypingContext};
use crate::db::Builder;
use crate::syntax::ast::AstNode;
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
        // A component's `on <CLOCK>` clause: the expression must
        // evaluate to the builtin `Clock` type, whatever the component
        // kind (only `reg`/`outgoing reg` are actually clocked by it).
        AstNodePayload::Component(_) => Some(Type::Clock),
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

            // Walk up through any enclosing ModDefStmtBlocks and If/Match nodes
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
        AstNodePayload::LetBinding(_) => {
            // A `let` binding's RHS: typed against the binding's
            // ascribed type when there is one.  For an untyped binding,
            // the expected type flows back from the binding's first
            // usage in the let's body (beta-reduction: the usage site's
            // expected type is the binding's type).
            let bindings_children = parent_node.children();
            if bindings_children.len() == 2 {
                let typ_node = &bindings_children[0];
                return builder.get_type_at(typ_node.location()).ok();
            }
            let Some(binding_name) = parent_node.let_binding_name() else { return None };
            let name = parsing.string(binding_name).to_owned();
            let Some(let_node) = parent_node.parent() else { return None };
            let Some(body) = let_node.let_body() else { return None };
            let first_use = find_first_reference(&body, name.as_bstr())?;
            expected_type_at_reference(builder, &first_use)
        }
        AstNodePayload::Let => {
            // A statement `let`'s `=>`-form body is a discarded value
            // expression with no expected type from its parent.
            None
        }
        AstNodePayload::If => {
            // Guard expressions must be Bit.
            // Body expressions inherit the expected type from the When
            // itself (only reachable when the When is in expression
            // position; in statement position only guards are expr
            // roots).
            let children = parent_node.children();
            let is_guard = crate::syntax::ast::if_arm_children(&children)
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

/// Find the first `ExprReference` to `name` in a subtree (pre-order).
fn find_first_reference<'p>(node: &AstNode<'p>, name: &BStr) -> Option<AstNode<'p>> {
    if matches!(node.payload(), AstNodePayload::ExprReference) {
        if let Some(path) = node.path() {
            if node.parsing().string(path) == name {
                return Some(node.clone());
            }
        }
    }
    for child in node.children() {
        if let Some(found) = find_first_reference(&child, name) {
            return Some(found);
        }
    }
    None
}

/// The expected type of an expression-position scope form — an `if`,
/// a `match`, or a `let` — derived purely from the syntax of its
/// ancestors, without querying any `Typing` result.
///
/// This backs the ctor-payload branch of [`expected_type_at_reference`]:
/// when the expected type of an untyped `let` binding flows from its
/// first use, that use can sit inside the very expression root whose
/// typing is still being built, so any query that re-enters `Typing`
/// here would recurse forever (the query cache only stores finished
/// results).  Walking the ancestors instead terminates: a scope's arm
/// body inherits the scope's expected type, and the scope's own
/// expected type is ultimately fixed by the driver, initial statement,
/// or platform default clause it feeds.
fn scope_expected_type(builder: &mut Builder, node: &AstNode<'_>) -> Option<Type> {
    let parent = node.parent()?;
    match parent.payload() {
        // `node` is an arm body of an enclosing `if`: a guard must be
        // `Bit`; a body inherits the `if`'s own expected type.
        AstNodePayload::If => {
            let children = parent.children();
            let is_guard = crate::syntax::ast::if_arm_children(&children)
                .into_iter()
                .any(|(guard, _)| guard.is_some_and(|g| g.id() == node.id()));
            if is_guard {
                Some(Type::Bit)
            } else {
                scope_expected_type(builder, &parent)
            }
        }
        // `node` is an arm body of an enclosing `match`: the subject is
        // unconstrained; a body inherits the match's expected type.
        AstNodePayload::Match => {
            if parent.child(0).id() == node.id() {
                None
            } else {
                scope_expected_type(builder, &parent)
            }
        }
        // `node` is the body of an enclosing `let`: an
        // expression-position `let` inherits its own expected type; a
        // statement `let`'s body is not a value expression, so stop.
        AstNodePayload::Let => {
            if parent.is_expression_position() {
                scope_expected_type(builder, &parent)
            } else {
                None
            }
        }
        // Parentheses are transparent for expected types.
        AstNodePayload::ExprParen => scope_expected_type(builder, &parent),
        // An ascription fixes the expected type of what it wraps.
        AstNodePayload::ExprAs => {
            let typ_node = parent.child(1);
            builder.get_type_at(typ_node.location()).ok()
        }
        // The scope feeds a driver, an initial statement, or a platform
        // `default` clause: its expected type is fixed there, which
        // `build_expected_type` computes (the scope node itself is that
        // clause's expression root).
        AstNodePayload::Driver(_)
        | AstNodePayload::InitialDriver
        | AstNodePayload::DefaultClause => {
            build_expected_type(builder, ExprRoot::new(node.location()))
        }
        _ => None,
    }
}

/// The type a reference node must have at its usage site: the expected
/// type flows through parentheses and constructor payloads (the ctor's
/// parameter type at the reference's position); a reference that is
/// itself a driver RHS inherits the driven component's type.
fn expected_type_at_reference(builder: &mut Builder, reference: &AstNode<'_>) -> Option<Type> {
    let parent = reference.parent()?;
    match parent.payload() {
        AstNodePayload::Driver(_) | AstNodePayload::InitialDriver => {
            // The reference is the whole driver RHS: its expected type
            // is the driven component's type.
            build_expected_type(builder, ExprRoot::new(reference.location()))
        }
        AstNodePayload::ExprCtor(expr_ctor) => {
            // `@Ctor(payload, ...)`: the payload's expected type is the
            // ctor signature's parameter type at its position.
            //
            // The ctor's type is derived from the syntax of its
            // ancestors (see `scope_expected_type`), not from a
            // `get_typeof` query: that query re-enters `Typing` for the
            // enclosing expression root, which — when this expected
            // type is being computed for an untyped `let` binding
            // while that same root's typing is still being built — is
            // an infinite query recursion (the cache only stores
            // finished results).
            let ctor_typ = scope_expected_type(builder, &parent)?;
            let Type::Usual(typedef_id, args) = ctor_typ else { return None };
            let ctor_name = parent.parsing().string(expr_ctor.ctor);
            let symboltable = builder.get_symboltable();
            let ctor_symbol = symboltable.slot(typedef_id, ctor_name)?;
            let sig = builder.get_ctor_signature(ctor_symbol.id());
            let idx = parent
                .children()
                .iter()
                .position(|c| c.id().0 == reference.id().0)?;
            let (_param_name, param_typ) = sig.parameters.get(idx)?;
            Some(crate::types::substitute::substitute(param_typ, &args))
        }
        AstNodePayload::ExprAs => {
            // `expr : Type`: the ascription's type is the expected
            // type of the expression it wraps.
            let typ_node = parent.child(1);
            builder.get_type_at(typ_node.location()).ok()
        }
        AstNodePayload::ExprParen => expected_type_at_reference(builder, &parent),
        _ => None,
    }
}
