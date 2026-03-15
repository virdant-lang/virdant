use std::sync::Arc;

use bstr::{BStr, BString};
use hashbrown::HashMap;
use indexmap::IndexMap;

use crate::analysis::Location;
use crate::syntax::ast::AstNodeId;
use crate::analysis::db::Builder;
use crate::fqn::PackageFqn;
use crate::diagnostics::Diagnostic;
use crate::common::json::ToJson;
use crate::syntax::payload::AstNodePayload;

#[derive(Debug)]
pub struct SymbolTable {
    symbols: IndexMap<BString, Symbol>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    fqn: BString,
    location: Location,
    kind: SymbolKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    ModDef,
    UnionDef,
    StructDef,
    EnumDef,
    BuiltinDef,
    FnDef,
    SocketDef,
}

impl Symbol {
    pub fn fqn(&self) -> &BStr {
        use bstr::ByteSlice;
        self.fqn.as_bstr()
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn kind(&self) -> SymbolKind {
        self.kind.clone()
    }
}

pub fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics = vec![];
    let mut symbols = vec![];

    for package in packages {
        let analysis = builder.get_package_analysis(package.clone());
        diagnostics.extend(analysis.diagnostics());
        let parsing = builder.get_parsing(package.clone());

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let node = parsing.ast_node(ast_node_id);
            let location = Location::new(package.clone(), ast_node_id);
            let fqn: BString = format!("{}::{}", package, item_name.clone()).into();
            let kind = match node.payload() {
                AstNodePayload::ModDef(mod_def) => SymbolKind::ModDef,
                AstNodePayload::StructDef(struct_def) => SymbolKind::StructDef,
                AstNodePayload::UnionDef(union_def) => SymbolKind::UnionDef,
                AstNodePayload::EnumDef(enum_def) => SymbolKind::EnumDef,
                AstNodePayload::BuiltinDef(builtin_def) => SymbolKind::BuiltinDef,
                AstNodePayload::FnDef(fn_def) => SymbolKind::FnDef,
                AstNodePayload::SocketDef(socket_def) => SymbolKind::SocketDef,
                _ => unreachable!(),
            };
            symbols.push((
                fqn.clone(),
                Symbol {
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
    })
}

impl SymbolTable {
    pub fn symbols(&self) -> Vec<Symbol> {
        self.symbols.iter().map(|(key, val)| val.clone()).collect()
    }

    pub fn items(&self) -> Vec<Symbol> {
        self.symbols.values()
            .cloned()
            .filter(|val| val.kind.is_item())
            .collect()
    }

    pub fn typedefs(&self) -> Vec<Symbol> {
        self.symbols.values()
            .cloned()
            .filter(|val| val.kind.is_typedef())
            .collect()
    }
}

impl SymbolKind {
    pub fn is_item(&self) -> bool {
        match self {
            SymbolKind::ModDef => true,
            SymbolKind::UnionDef => true,
            SymbolKind::StructDef => true,
            SymbolKind::EnumDef => true,
            SymbolKind::BuiltinDef => true,
            SymbolKind::FnDef => true,
            SymbolKind::SocketDef => true,
            _ => false
        }
    }

    pub fn is_typedef(&self) -> bool {
        match self {
            SymbolKind::UnionDef => true,
            SymbolKind::StructDef => true,
            SymbolKind::EnumDef => true,
            SymbolKind::BuiltinDef => true,
            _ => false
        }
    }
}

impl ToJson for SymbolTable {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
