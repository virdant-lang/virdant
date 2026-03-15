use std::sync::Arc;

use bstr::{BStr, BString};
use hashbrown::{HashMap, HashSet};
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
    builtin_names: HashSet<BString>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    id: SymbolId,
    fqn: BString,
    location: Location,
    kind: SymbolKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

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
    pub fn id(&self) -> SymbolId {
        self.id
    }

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
                AstNodePayload::ModDef(mod_def) => SymbolKind::ModDef,
                AstNodePayload::StructDef(struct_def) => SymbolKind::StructDef,
                AstNodePayload::UnionDef(union_def) => SymbolKind::UnionDef,
                AstNodePayload::EnumDef(enum_def) => SymbolKind::EnumDef,
                AstNodePayload::BuiltinDef(builtin_def) => SymbolKind::BuiltinDef,
                AstNodePayload::FnDef(fn_def) => SymbolKind::FnDef,
                AstNodePayload::SocketDef(socket_def) => SymbolKind::SocketDef,
                _ => unreachable!(),
            };

            if let AstNodePayload::BuiltinDef(builtin_def) = payload {
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

impl SymbolTable {
    pub fn symbol(&self, symbol_id: SymbolId) -> Symbol {
        self.symbols[symbol_id.0 as usize].clone()
    }

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

    pub fn resolve_item_fqn(&self, item_fqn: &BStr) -> Option<&Symbol> {
        self.lookup_item_by_fqn(item_fqn)
    }

    pub fn resolve_item_in_package(&self, name: &BStr, package: PackageFqn) -> Option<&Symbol> {
        use bstr::ByteSlice;

        let item_fqn: BString = format!("{package}::{name}").into();
        self.lookup_item_by_fqn(item_fqn.as_bstr())
    }

    pub fn resolve_item(&self, name: &BStr, in_package: PackageFqn) -> Option<&Symbol> {
        if let Some((_package_name, _item_name)) = try_split_qualification(name) {
            self.resolve_item_fqn(name)
        } else if self.builtin_names.contains(name) {
            self.resolve_item_in_package(name, PackageFqn::new("builtin".into()))
        } else{
            self.resolve_item_in_package(name, in_package)
        }
    }

    fn lookup_item_by_fqn(&self, item_fqn: &BStr) -> Option<&Symbol> {
        for (fqn, symbol) in &self.symbols {
            if symbol.kind.is_item() && fqn == item_fqn {
                return Some(symbol);
            }
        }
        None
    }
}

// Tries to split "foo::Bar" into Some(("foo", "bar")).
// Returns None if it can't.
fn try_split_qualification(item_fqn: &BStr) -> Option<(BString, BString)> {
    if let Some(split_at) = item_fqn.windows(2).position(|window| window == b"::") {
        let first = BString::from(item_fqn[..split_at].to_vec());
        let second = BString::from(item_fqn[(split_at + 2)..].to_vec());
        Some((first, second))
    } else {
        None
    }
}

#[cfg(test)]
#[test]
fn test_try_split_qualification() {
    assert_eq!(try_split_qualification("foo::Bar".into()), Some(("foo".into(), "Bar".into())));
    assert_eq!(try_split_qualification("Bar".into()), None);
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

impl ToJson for SymbolId {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
