use bstr::BString;

use crate::analysis::symbols::SymbolId;
use crate::db::Builder;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct StructField {
    pub field_symbol_id: SymbolId,
    pub name: BString,
    pub typ: Option<Type>,
}

pub(crate) fn build_struct_fields(builder: &mut Builder, symbol_id: SymbolId) -> Vec<StructField> {
    let mut fields = vec![];

    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);

    // Only StructDef items have fields
    if !matches!(symbol.kind(), crate::analysis::symbols::SymbolKind::StructDef) {
        return fields;
    }

    let struct_location = symbol.location();
    let parsing = builder.get_parsing(struct_location.package());
    let struct_ast = parsing.ast_node(struct_location.ast_node_id());

    // Iterate through field nodes (direct children of StructDef)
    for field_node in struct_ast.children() {
        use crate::syntax::payload::AstNodePayload;

        let AstNodePayload::Field(field) = field_node.payload() else {
            continue;
        };

        let field_name = parsing.string(field.name);

        // Get the field symbol from the symbol table
        let Some(field_symbol) = symboltable.slot(symbol_id, field_name) else {
            continue;
        };

        // Get the type from the type annotation (first child of Field node)
        let typ_node = field_node.child(0);
        let typ = match builder.get_type_at(typ_node.location()) {
            Ok(typ) => Some(typ),
            Err(_) => None,
        };

        fields.push(StructField {
            field_symbol_id: field_symbol.id(),
            name: field_name.to_owned(),
            typ,
        });
    }

    fields
}
