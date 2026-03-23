use bstr::BStr;
use bstr::BString;

use crate::analysis::location::Location;
use crate::common::json::ToJson;
use crate::fqn::PackageFqn;

#[derive(Debug, Clone)]
pub struct Symbol {
    pub(crate) id: SymbolId,
    pub(crate) fqn: BString,
    pub(crate) location: Location,
    pub(crate) kind: SymbolKind,
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

