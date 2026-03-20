use bstr::BString;

use crate::analysis::location::Location;
use crate::common::TypeScheme;
use crate::common::json::ToJson;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    pub(crate) fqn: BString,
    pub(crate) location: Location,
    pub(crate) kind: TypeScheme,
}

impl ToJson for TypeDef {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
