use bstr::BString;

use crate::common::json::ToJson;

use super::typ::Type;

#[derive(Debug, Clone)]
pub struct TypingContext(pub(crate) Vec<(BString, Type)>);

impl TypingContext {
    pub fn bindings(&self) -> &[(BString, Type)] {
        self.0.as_slice()
    }

    pub fn get(&self, name: BString) -> Option<Type> {
        for (name_, typ) in self.bindings().iter().rev() {
            if name == *name_ {
                return Some(typ.clone());
            }
        }
        None
    }
}

impl ToJson for TypingContext {
    fn to_json(&self) -> json::JsonValue {
        self.0.to_json()
    }
}

