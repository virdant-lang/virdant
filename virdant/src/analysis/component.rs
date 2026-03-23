use bstr::{BStr, BString};

use crate::analysis::symbols::SymbolId;
use crate::analysis::types::Type;
use crate::common::Flow;
use crate::common::json::ToJson;
use crate::diagnostics::Diagnostic;

#[derive(Debug)]
pub struct ComponentAnalysis {
    // TODO Remove pub(crate)
    pub(crate) moddef: SymbolId,
    pub(crate) components: Vec<(BString, Component)>,
    pub(crate) diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct Component {
    // TODO Remove pub(crate)
    pub(crate)path: BString,
    pub(crate)typ: Option<Type>,
    pub(crate)flow: Flow,
}

impl Component {
    pub fn path(&self) -> BString {
        self.path.clone()
    }

    pub fn typ(&self) -> Option<Type> {
        self.typ.clone()
    }

    pub fn flow(&self) -> Flow {
        self.flow.clone()
    }
}

impl ComponentAnalysis {
    pub fn type_of(&self, path: &BStr) -> Option<Type> {
        for (path_, component) in &self.components {
            if path == path_ {
                return component.typ.clone();
            }
        }
        None
    }

    pub fn components(&self) -> Vec<(BString, Component)> {
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
