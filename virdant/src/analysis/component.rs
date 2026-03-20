use bstr::{BStr, BString};

use crate::analysis::symbols::SymbolId;
use crate::analysis::types::Type;
use crate::common::json::ToJson;
use crate::diagnostics::Diagnostic;

#[derive(Debug)]
pub struct ComponentAnalysis {
    // TODO reference to ModDef this is created for
    pub(crate) moddef: SymbolId,
    pub(crate) components: Vec<(BString, Option<Type>)>,
    pub(crate) diagnostics: Vec<Diagnostic>,
}

impl ComponentAnalysis {
    pub fn type_of(&self, path: &BStr) -> Option<Type> {
        for (path_, typ) in &self.components {
            if path == path_ {
                return typ.clone();
            }
        }
        None
    }

    pub fn components(&self) -> Vec<(BString, Option<Type>)> {
        self.components.clone()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }
}


impl ToJson for ComponentAnalysis {
    fn to_json(&self) -> json::JsonValue {
        format!("{self:?}").into()
    }
}
