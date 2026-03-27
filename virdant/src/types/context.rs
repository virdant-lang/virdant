use bstr::BString;
use hashbrown::{HashMap, HashSet};

use crate::{analysis::Location, common::json::ToJson};

use super::typ::Type;

#[derive(Debug, Clone)]
pub struct TypingContext {
    pub(crate) context: Vec<(BString, Type)>,
}

impl TypingContext {
    pub fn bindings(&self) -> &[(BString, Type)] {
        self.context.as_slice()
    }

    pub fn get(&self, name: BString) -> Option<Type> {
        for (name_, typ) in self.context.iter().rev() {
            if name == *name_ {
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

