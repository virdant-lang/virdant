use crate::analysis::symbols::SymbolKind;
use crate::types::TypeDef;
use crate::common::TypeScheme;
use crate::db::Builder;

pub(crate) fn build_typedefs(builder: &mut Builder) -> Vec<TypeDef> {
    let mut typedefs = vec![];
    let symboltable = builder.get_symboltable();
    for item_symbol in symboltable.typedefs() {
        let kind = match item_symbol.kind() {
            SymbolKind::UnionDef => TypeScheme::UnionDef,
            SymbolKind::StructDef => TypeScheme::StructDef,
            SymbolKind::EnumDef => TypeScheme::EnumDef,
            SymbolKind::BuiltinDef => TypeScheme::BuiltinDef,
            _ => unreachable!(),
        };
        typedefs.push(TypeDef {
            fqn: item_symbol.fqn().into(),
            location: item_symbol.location(),
            kind,
        });
    }
    typedefs
}

pub(crate) fn build_type_monomorphizations(builder: &mut Builder) -> Vec<crate::types::Type> {
    use hashbrown::HashSet;

    let mut typs = HashSet::new();

    let symboltable = builder.get_symboltable();
    for item in symboltable.items() {
        builder.typecheck(item.id());
    }

    for exprroot in builder.get_exprroots() {
        let typing = builder.get_typing(exprroot);
        for typ in typing.typs.values() {
            typs.insert(typ.clone());
        }
    }

    typs.into_iter().collect()
}
