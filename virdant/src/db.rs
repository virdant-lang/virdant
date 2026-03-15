#[macro_use]
mod macros;
mod graphviz;
mod guts;

use std::fmt::Write;
use std::sync::Arc;
use std::sync::Mutex;

use bstr::BStr;
use bstr::BString;
use hashbrown::{HashMap, HashSet};

use crate::analysis::Location;
use crate::analysis::PackageAnalysis;
use crate::analysis::component::ComponentAnalysis;
use crate::analysis::symboltable::SymbolId;
use crate::analysis::symboltable::SymbolKind;
use crate::analysis::symboltable::SymbolTable;
use crate::analysis::typecheck::Type;
use crate::analysis::typecheck::TypeDef;
use crate::analysis::typecheck::{ExprRoot, Typing, TypingContext};
use crate::common::json::ToJson;
use crate::diagnostics::DiagnosticLevel;
use crate::syntax::parsing::parse;
use crate::syntax::payload::Component;
use crate::{diagnostics::Diagnostic, fqn::PackageFqn, source::Source, syntax::parsing::Parsing};

pub use guts::*;

queries! {
    Packages() -> Vec<PackageFqn>;
    Source(package: PackageFqn) -> Source;
    Parsing(package: PackageFqn) -> Arc<Parsing>;
    SyntaxErrors() -> Vec<Diagnostic>;
    PackageAnalysis(package: PackageFqn) -> Arc<PackageAnalysis>;
    ComponentAnalysis(symbol_id: SymbolId) -> Arc<ComponentAnalysis>;
    SymbolTable() -> Arc<SymbolTable>;
    TypeDefs() -> Vec<TypeDef>;
    ExprRoots() -> Vec<ExprRoot>;
    ExpectedType(location: Location) -> Option<Type>;
    TypingContext(symbol_id: SymbolId) -> TypingContext;
    Typing(exprroot: ExprRoot) -> Arc<Typing>;
    TypeCheck() -> Vec<Diagnostic>;
    Typeof(location: Location) -> Option<Type>;
    Check() -> Result<Vec<Diagnostic>, Vec<Diagnostic>>;
    TypeMonomorphizations() -> Vec<Type>;
}


impl Query {
    fn is_input(&self) -> bool {
        matches!(self, Query::Packages() | Query::Source(_))
    }

    fn build(&self, db: &Db) -> CachedVal {
        if self.is_input() {
            panic!("Can't build input");
        }

        let _self = self;

        // when invoked with a Query, dispatch to the given function with the arguments:
        // $buildfn(&mut builder, arg1, ..., argN)
        dispatch_build!(_self, db;
            build_parsing : Parsing(package);
            build_syntax_errors : SyntaxErrors();
            crate::analysis::build_package_analysis : PackageAnalysis(analysis);
            crate::analysis::component::build_component_analysis : ComponentAnalysis(symbol_id);
            crate::analysis::symboltable::build_symboltable : SymbolTable();
            crate::analysis::typecheck::build_exprroots : ExprRoots();
            crate::analysis::typecheck::build_expected_type : ExpectedType(location);
            crate::analysis::typecheck::build_typedefs : TypeDefs();
            crate::analysis::typecheck::build_typing_context : TypingContext(symbol_id);
            crate::analysis::typecheck::build_typing : Typing(expr_root);
            crate::analysis::typecheck::typecheck : TypeCheck();
            crate::analysis::typecheck::build_typeof : Typeof(location);
            crate::analysis::typecheck::build_type_monomorphizations : TypeMonomorphizations();
            check : Check();

        )
    }
}

// These methods are all defined on BOTH Db and on Builder.
// And service the given queries.
db_getter!(get_packages : Packages() -> Vec<PackageFqn>);
db_getter!(get_source : Source(package : PackageFqn) -> Source);
db_getter!(get_parsing : Parsing(package: PackageFqn) -> Arc<Parsing>);
db_getter!(get_syntax_errors : SyntaxErrors() -> Vec<Diagnostic>);
db_getter!(get_package_analysis : PackageAnalysis(package: PackageFqn) -> Arc<PackageAnalysis>);
db_getter!(get_component_analysis : ComponentAnalysis(moddef: SymbolId) -> Arc<ComponentAnalysis>);
db_getter!(get_symboltable : SymbolTable() -> Arc<SymbolTable>);
db_getter!(get_typing_context : TypingContext(item: SymbolId) -> TypingContext);
db_getter!(get_typedefs : TypeDefs() -> Vec<TypeDef>);
db_getter!(get_exprroots : ExprRoots() -> Vec<ExprRoot>);
db_getter!(get_expected_type : ExpectedType(location: Location) -> Option<Type>);
db_getter!(get_typing : Typing(expr_root: ExprRoot) -> Arc<Typing>);
db_getter!(typecheck : TypeCheck() -> Vec<Diagnostic>);
db_getter!(get_typeof : Typeof(location: Location) -> Option<Type>);
db_getter!(get_type_monomorphizations : TypeMonomorphizations() -> Vec<Type>);


fn build_parsing(builder: &mut Builder<'_>, package: PackageFqn) -> Arc<Parsing> {
    let source = cast!(builder.get(Query::Source(package)), Source);
    let parsing = parse(&source);
    Arc::new(parsing)
}

fn build_syntax_errors(builder: &mut Builder) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package);
        diagnostics.extend(parsing.diagnostics());
    }

    diagnostics
}

fn check(builder: &mut Builder) -> Result<Vec<Diagnostic>, Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors());
    diagnostics.extend(builder.typecheck());

    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
