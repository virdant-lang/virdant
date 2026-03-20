use crate::analysis::symbols::SymbolId;
use crate::common::Width;
use crate::common::json::ToJson;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bit,
    Clock,
    Word(Width),
    Usual(SymbolId), // TODO rename this
}

impl ToJson for Type {
    fn to_json(&self) -> json::JsonValue {
        self.to_string().into()
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Word(n) => write!(f, "Word[{n}]"),
            Type::Usual(_symbol_id) => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bit => write!(f, "Bit"),
            Type::Clock => write!(f, "Clock"),
            Type::Word(n) => write!(f, "Word[{n}]"),
            Type::Usual(symbol_id) => write!(f, "Usual({symbol_id:?})"),
        }
    }
}
