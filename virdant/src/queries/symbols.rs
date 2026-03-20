use std::sync::Arc;

use bstr::BString;
use hashbrown::HashSet;
use indexmap::IndexMap;

use crate::analysis::location::Location;
use crate::analysis::symbols::{Symbol, SymbolId, SymbolKind, SymbolTable};
use crate::db::Builder;
use crate::syntax::payload::AstNodePayload;

pub(crate) fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics = vec![];
    let mut symbols = vec![];
    let mut builtin_names = vec![];

    for package in packages {
        let analysis = builder.get_package_analysis(package.clone());
        diagnostics.extend(analysis.diagnostics());
        let parsing = builder.get_parsing(package.clone());

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let node = parsing.ast_node(ast_node_id);
            let location = Location::new(package.clone(), ast_node_id);
            let fqn: BString = format!("{}::{}", package, item_name.clone()).into();
            let payload = node.payload();
            let kind = match &payload {
                AstNodePayload::ModDef(_mod_def) => SymbolKind::ModDef,
                AstNodePayload::StructDef(_struct_def) => SymbolKind::StructDef,
                AstNodePayload::UnionDef(_union_def) => SymbolKind::UnionDef,
                AstNodePayload::EnumDef(_enum_def) => SymbolKind::EnumDef,
                AstNodePayload::BuiltinDef(_builtin_def) => SymbolKind::BuiltinDef,
                AstNodePayload::FnDef(_fn_def) => SymbolKind::FnDef,
                AstNodePayload::SocketDef(_socket_def) => SymbolKind::SocketDef,
                _ => unreachable!(),
            };

            if let AstNodePayload::BuiltinDef(_builtin_def) = payload {
                builtin_names.push(item_name.to_owned());
            }

            let id = SymbolId(symbols.len().try_into().unwrap());

            symbols.push((
                fqn.clone(),
                Symbol {
                    id,
                    fqn,
                    location,
                    kind,
                },
            ));
        }
    }

    Arc::new(SymbolTable {
        symbols: symbols.into_iter().collect(),
        diagnostics,
        builtin_names: builtin_names.into_iter().collect(),
    })
}
