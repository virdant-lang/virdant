use bstr::BString;

use crate::analysis::component::ComponentId;
use crate::analysis::Location;
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct TypingContext {
    context: Vec<(BString, (Referent, Option<Type>))>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Referent {
    Component(ComponentId),
    Local(Location),
}

impl TypingContext {
    pub fn new() -> TypingContext {
        TypingContext { context: vec![] }
    }

    pub fn bindings(&self) -> &[(BString, (Referent, Option<Type>))] {
        self.context.as_slice()
    }

    pub fn get(&self, name: BString) -> Option<(Referent, Option<Type>)> {
        for (name_, (referent, typ)) in self.context.iter().rev() {
            if name == *name_ {
                return Some((referent.clone(), typ.clone()));
            }
        }
        None
    }

    pub fn push_component(mut self, name: BString, component_id: ComponentId, typ: Option<Type>) -> TypingContext {
        self.context.push((name, (Referent::Component(component_id), typ)));
        self
    }

    pub fn push_local(mut self, name: BString, location: Location, typ: Type) -> TypingContext {
        self.context.push((name, (Referent::Local(location), Some(typ))));
        self
    }
}

