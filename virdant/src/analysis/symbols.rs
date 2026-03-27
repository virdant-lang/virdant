use std::sync::Arc;

use hashbrown::{HashMap, HashSet};

use crate::db::Builder;
use crate::diagnostics;
use crate::source::Region;
use crate::syntax::ast::AstNodeId;
use crate::syntax::payload::AstNodePayload;
use bstr::{BStr, BString};
use indexmap::IndexMap;

use crate::{analysis::Location, diagnostics::Diagnostic};
use crate::fqn::PackageFqn;
use crate::common::json::ToJson;

#[derive(Debug)]
pub struct SymbolTable {
    pub symbols: IndexMap<BString, Symbol>,
    pub diagnostics: Vec<Diagnostic>,
    pub builtin_names: HashSet<BString>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub id: SymbolId,
    pub fqn: BString,
    pub location: Location,
    pub kind: SymbolKind,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
    Component,
    Submodule,
    Socket,
}

impl SymbolTable {
    pub fn symbol(&self, symbol_id: SymbolId) -> Symbol {
        self.symbols[symbol_id.0 as usize].clone()
    }

    pub fn resolve(&self, fqn: &BStr) -> Option<&Symbol> {
        for (symbol_fqn, symbol) in &self.symbols {
            if symbol_fqn == fqn {
                return Some(symbol);
            }
        }
        None
    }

    pub fn symbols(&self) -> Vec<Symbol> {
        self.symbols.iter().map(|(_key, val)| val.clone()).collect()
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
        } else {
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

impl ToJson for SymbolTable {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
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

    pub fn package(&self) -> PackageFqn {
        self.location.package()
    }

    pub fn kind(&self) -> SymbolKind {
        self.kind.clone()
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

impl ToJson for SymbolId {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}

impl std::fmt::Debug for SymbolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SymbolId({})", self.0)
    }
}

pub(crate) fn build_symboltable(builder: &mut Builder) -> Arc<SymbolTable> {
    let packages = builder.get_packages();
    let mut diagnostics_vec = vec![];
    let mut symbols = vec![];
    let mut builtin_names = vec![];

    for package in packages {
        let analysis = builder.get_package_analysis(package.clone());
        diagnostics_vec.extend(analysis.diagnostics());
        let parsing = builder.get_parsing(package.clone());

        for item_name in analysis.item_names() {
            let ast_node_id = analysis.item_ast_node_id(item_name.as_ref());
            let node = parsing.ast_node(ast_node_id);
            let location = Location::new(package.clone(), ast_node_id);
            let fqn: BString = format!("{}::{}", package, item_name.clone()).into();
            let kind = match node.payload() {
                AstNodePayload::ModDef(_) => SymbolKind::ModDef,
                AstNodePayload::StructDef(_) => SymbolKind::StructDef,
                AstNodePayload::UnionDef(_) => SymbolKind::UnionDef,
                AstNodePayload::EnumDef(_) => SymbolKind::EnumDef,
                AstNodePayload::BuiltinDef(_) => SymbolKind::BuiltinDef,
                AstNodePayload::FnDef(_) => SymbolKind::FnDef,
                AstNodePayload::SocketDef(_) => SymbolKind::SocketDef,
                _ => unreachable!(),
            };

            if kind == SymbolKind::BuiltinDef {
                builtin_names.push(item_name.to_owned());
            }

            let id = SymbolId(symbols.len().try_into().unwrap());

            symbols.push((
                fqn.clone(),
                Symbol {
                    id,
                    fqn: fqn.clone(),
                    location,
                    kind: kind.clone(),
                },
            ));

            if kind == SymbolKind::ModDef {
                let mut seen: HashMap<BString, Region> = HashMap::new();

                for child in node.children() {
                    let (component_name, component_ast_node_id, kind) = match child.payload() {
                        AstNodePayload::Component(component) => {
                            let name: BString = parsing.string(component.name).to_owned();
                            (name, child.id(), SymbolKind::Component)
                        }
                        AstNodePayload::Module(module) => {
                            let name: BString = parsing.string(module.name).to_owned();
                            (name, child.id(), SymbolKind::Submodule)
                        }
                        AstNodePayload::Socket(socket) => {
                            let name: BString = parsing.string(socket.name).to_owned();
                            (name, child.id(), SymbolKind::Socket)
                        }
                        _ => continue,
                    };

                    let component_region = Region::new(package.clone(), child.span());

                    if seen.contains_key(&component_name) {
                        diagnostics_vec.push(diagnostics::DuplicateSlot {
                            item: item_name.to_owned().into(),
                            region: component_region,
                            slot: component_name,
                        }.into());
                        continue;
                    }

                    seen.insert(component_name.clone(), component_region);

                    let component_fqn: BString =
                        format!("{}::{}::{}", package, item_name, component_name).into();
                    let component_location = Location::new(package.clone(), component_ast_node_id);
                    let component_id = SymbolId(symbols.len().try_into().unwrap());

                    symbols.push((
                        component_fqn.clone(),
                        Symbol {
                            id: component_id,
                            fqn: component_fqn,
                            location: component_location,
                            kind,
                        },
                    ));
                }
            }
        }
    }

    Arc::new(SymbolTable {
        symbols: symbols.into_iter().collect(),
        diagnostics: diagnostics_vec,
        builtin_names: builtin_names.into_iter().collect(),
    })
}

pub(crate) fn build_symbol_ast(builder: &mut Builder, symbol_id: SymbolId) -> AstNodeId {
    let symboltable = builder.get_symboltable();
    let symbol = symboltable.symbol(symbol_id);
    symbol.location().ast_node_id()
}
