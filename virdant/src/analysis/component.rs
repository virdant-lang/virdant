use bstr::{BStr, BString};

use crate::analysis::Location;
use crate::analysis::symbols::SymbolId;
use crate::types::Type;
use crate::common::Flow;
use crate::diagnostics::Diagnostic;

#[derive(Debug)]
pub struct ComponentAnalysis {
    pub moddef: SymbolId,
    pub components: Vec<(BString, Component)>,
    pub diagnostics: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
pub struct Component {
    pub path: BString,
    pub location: Location,
    pub typ: Option<Type>,
    pub flow: Flow,
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

    pub fn location(&self) -> Location {
        self.location.clone()
    }

    pub fn can_sink(&self) -> bool {
        matches!(self.flow, Flow::Sink | Flow::Duplex)
    }


    pub fn can_source(&self) -> bool {
        matches!(self.flow, Flow::Source | Flow::Duplex)
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

    pub fn component(&self, path: &BStr) -> Option<Component> {
        for (path_, component) in &self.components {
            if path_ == path {
                return Some(component.clone());
            }
        }
        None
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }
}


