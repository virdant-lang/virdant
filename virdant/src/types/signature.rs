//! Defines `Signature`, the type signature of a constructor (a union
//! ctor or an enum enumerant carrying payload parameters: named
//! parameters and return type), with `build_ctor_signature`
//! constructing it from the constructor's AST node and the
//! `TypeIndex`.
//!
//! Parameter types are stored *as written*: inside a generic def they
//! reference the def's parameters via `Type::Param`.  Consumers
//! substitute them with the type arguments of the concrete
//! instantiation via `types::substitute::substitute`.

use std::sync::Arc;

use bstr::BString;

use crate::analysis::symbols::SymbolId;
use crate::db::Builder;
use crate::syntax::payload::AstNodePayload;
use crate::types::Type;

/// The signature of a constructor: its named arguments and return type.
#[derive(Debug, Clone)]
pub struct Signature {
    pub parameters: Vec<(BString, Type)>,
    pub ret_typ: Type,
}

pub(crate) fn build_ctor_signature(
    builder: &mut Builder,
    ctor_symbol_id: SymbolId,
) -> Arc<Signature> {
    let symboltable = builder.get_symboltable();
    let ctor_symbol = symboltable.symbol(ctor_symbol_id);

    // The direct parent of a Ctor symbol is the containing UnionDef;
    // the direct parent of a payload Enumerant symbol is the containing
    // EnumDef.
    let parent_id = ctor_symbol.parent_id().unwrap();
    let ret_typ = Type::Usual(parent_id, vec![]);

    // Navigate to the constructor's AST node.
    let location = ctor_symbol.location();
    let parsing = builder.get_parsing(location.package());
    let ctor_node = parsing.ast_node(location.ast_node_id());

    // Use the pre-built TypeIndex to resolve each parameter's type without
    // re-running type inference.
    let type_index = builder.get_type_index();

    // Each child of a Ctor (or payload Enumerant) node is a Param node
    // that carries a name and a single Type child.  Enumerant nodes
    // may also carry a value expression child, which is skipped here.
    let mut arguments: Vec<(BString, Type)> = vec![];
    for param_node in ctor_node.children() {
        let AstNodePayload::Param(param) = param_node.payload() else { continue };
        let name = parsing.string(param.name).to_owned();
        let type_node = param_node.child(1);
        let typ = type_index
            .type_at(type_node.location())
            .cloned()
            .expect("type of ctor param must be in TypeIndex");
        arguments.push((name, typ));
    }

    Arc::new(Signature { parameters: arguments, ret_typ })
}
