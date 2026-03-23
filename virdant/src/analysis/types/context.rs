use bstr::BString;
use hashbrown::HashSet;

use crate::common::json::ToJson;

use super::typ::Type;

#[derive(Debug, Clone)]
pub struct TypingContext {
    pub(crate) context: Vec<(BString, Type)>,
    pub(crate) used: HashSet<BString>,
}

impl TypingContext {
    pub fn bindings(&self) -> &[(BString, Type)] {
        self.context.as_slice()
    }

    pub fn get(&mut self, name: BString) -> Option<Type> {
        for (name_, typ) in self.context.iter().rev() {
            if name == *name_ {
                self.used.insert(name);
                return Some(typ.clone());
            }
        }
        None
    }
}

impl ToJson for TypingContext {
    fn to_json(&self) -> json::JsonValue {
        self.context.to_json()
    }
}

