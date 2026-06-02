// Import all the types needed by the generated database
use std::sync::Arc;
use indexmap::{IndexMap, IndexSet};
use bstr::BString;

use crate::analysis::component::{Component, ComponentId, ComponentAnalysis};
use crate::analysis::drivers::DriverAnalysis;
use crate::analysis::elaboration::Elaboration;
use crate::analysis::location::Location;
use crate::analysis::package::PackageAnalysis;
use crate::analysis::symbols::{SymbolId, SymbolTable};
use crate::analysis::ports::Port;
use crate::analysis::structs::StructField;
use crate::syntax::parsing::{InternedString, Parsing};
use crate::types::typedef::{TypeDef, TypeIndex};
use crate::types::signature::Signature;
use crate::types::{ExprRoot, Type, Typing, TypingContext};
use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::common::source::{Region, Source};
use crate::syntax::ast::AstNodeId;

// Include the generated database code from the build script
include!(concat!(env!("OUT_DIR"), "/db.rs"));

// Forwarding methods for the generated Db wrapper

impl Db {
    /// Dump the query execution trace
    pub fn dump(&self) {
        self.inner.dump_trace();
    }

    /// Save the query dependency graph as a GraphViz DOT file
    pub fn save_graphviz<P: AsRef<std::path::Path>>(&self, filepath: P) {
        let _ = self.inner.save_graphviz(filepath);
    }
}

impl<'d> Builder<'d> {
    /// Dump the current build stack
    pub fn dump(&self) {
        self.db.inner.dump_call_stack();
    }
}
