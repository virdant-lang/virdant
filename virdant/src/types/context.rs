use bstr::BString;

use super::typ::Type;

#[derive(Debug, Clone)]
pub struct TypingContext {
    context: Vec<(BString, Type)>,
}

impl TypingContext {
    pub fn new() -> TypingContext {
        TypingContext { context: vec![] }
    }

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

    pub fn extend<I>(&self, bindings: I) -> TypingContext
        where I: IntoIterator<Item = (BString, Type)>
    {
        let mut new_context = self.clone();
        new_context.context.extend(bindings.into_iter());
        new_context
    }
}

