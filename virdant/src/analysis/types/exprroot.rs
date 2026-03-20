use crate::analysis::location::Location;
use crate::common::json::ToJson;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprRoot {
    pub location: Location,
}

impl ExprRoot {
    pub fn new(location: Location) -> ExprRoot {
        ExprRoot { location }
    }

    pub fn location(&self) -> Location {
        self.location.clone()
    }
}

impl ToJson for ExprRoot {
    fn to_json(&self) -> json::JsonValue {
        self.location.to_json()
    }
}

