use bstr::{BStr, BString};
use hashbrown::HashSet;
use indexmap::IndexMap;

use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::common::json::ToJson;

use super::symbol::{Symbol, SymbolId, SymbolKind};

#[derive(Debug)]
pub struct SymbolTable {
    pub(crate) symbols: IndexMap<BString, Symbol>,
    pub(crate) diagnostics: Vec<Diagnostic>,
    pub(crate) builtin_names: HashSet<BString>,
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

