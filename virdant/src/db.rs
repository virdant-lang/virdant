#[macro_use]
mod macros;
mod graphviz;
mod guts;

use std::fmt::Write;
use std::sync::Arc;
use std::sync::Mutex;

use hashbrown::{HashMap, HashSet};

use crate::analysis::location::Location;
use crate::analysis::package::PackageAnalysis;
use crate::analysis::component::ComponentAnalysis;
use crate::analysis::symbols::{SymbolId, SymbolTable};
use crate::types::{ExprRoot, Type, TypeDef, Typing, TypingContext};
use crate::diagnostics::Diagnostic;
use crate::fqn::PackageFqn;
use crate::common::source::{Region, Source};
use crate::syntax::ast::AstNodeId;
use crate::syntax::parsing::Parsing;

pub use guts::*;

queries! {
    Packages() -> Vec<PackageFqn>;
    Source(package: PackageFqn) -> Source;
    Parsing(package: PackageFqn) -> Arc<Parsing>;
    SyntaxErrors() -> Vec<Diagnostic>;
    LocationRegion(location: Location) -> Region;
    PackageAnalysis(package: PackageFqn) -> Arc<PackageAnalysis>;
    ComponentAnalysis(symbol_id: SymbolId) -> Arc<ComponentAnalysis>;
    SymbolTable() -> Arc<SymbolTable>;
    SymbolAst(symbol_id: SymbolId) -> AstNodeId;
    CheckDrivers(symbol_id: SymbolId) -> Vec<Diagnostic>;
    TypeDefs() -> Vec<TypeDef>;
    ExprRoots() -> Vec<ExprRoot>;
    AllExprs() -> Vec<Location>;
    ExpectedType(exprroot: ExprRoot) -> Option<Type>;
    ExprRootFor(location: Location) -> ExprRoot;
    TypingContext(symbol_id: SymbolId) -> TypingContext;
    Typing(exprroot: ExprRoot) -> Arc<Typing>;
    TypeCheck(symbol_id: SymbolId) -> Vec<Diagnostic>;
    Typeof(location: Location) -> Result<Type, Vec<Diagnostic>>;
    TypeofAll() -> HashMap<Location, Option<Type>>;
    Check() -> Vec<Diagnostic>;
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
            crate::queries::build_parsing : Parsing(package);
            crate::queries::build_syntax_errors : SyntaxErrors();
            crate::queries::build_package_analysis : PackageAnalysis(analysis);
            crate::queries::build_component_analysis : ComponentAnalysis(symbol_id);
            crate::analysis::symbols::build_symboltable : SymbolTable();
            crate::analysis::symbols::build_symbol_ast : SymbolAst(symbol_id);
            crate::queries::check_drivers : CheckDrivers(symbol_id);
            crate::queries::find_exprroots : ExprRoots();
            crate::queries::build_all_exprs : AllExprs();
            crate::queries::build_expected_type : ExpectedType(location);
            crate::queries::build_typedefs : TypeDefs();
            crate::queries::build_typing_context : TypingContext(symbol_id);
            crate::queries::build_typing : Typing(expr_root);
            crate::types::typing::typecheck : TypeCheck(symbol_id);
            crate::queries::typecheck::build_exprroot_for : ExprRootFor(location);
            crate::queries::build_typeof : Typeof(location);
            crate::queries::build_typeof_all : TypeofAll();
            crate::queries::build_type_monomorphizations : TypeMonomorphizations();
            crate::queries::build_location_region : LocationRegion(location);
            crate::queries::check : Check();

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
db_getter!(get_symbol_ast : SymbolAst(symbol_id: SymbolId) -> AstNodeId);
db_getter!(check_drivers : CheckDrivers(symbol_id: SymbolId) -> Vec<Diagnostic>);
db_getter!(get_typing_context : TypingContext(item: SymbolId) -> TypingContext);
db_getter!(get_typedefs : TypeDefs() -> Vec<TypeDef>);
db_getter!(get_exprroots : ExprRoots() -> Vec<ExprRoot>);
db_getter!(get_all_exprs : AllExprs() -> Vec<Location>);
db_getter!(get_expected_type : ExpectedType(exprroot: ExprRoot) -> Option<Type>);
db_getter!(get_exprroot_for : ExprRootFor(location: Location) -> ExprRoot);
db_getter!(get_typing : Typing(expr_root: ExprRoot) -> Arc<Typing>);
db_getter!(typecheck : TypeCheck(symbol_id: SymbolId) -> Vec<Diagnostic>);
db_getter!(get_typeof : Typeof(location: Location) -> Result<Type, Vec<Diagnostic>>);
db_getter!(get_typeof_all : TypeofAll() -> HashMap<Location, Option<Type>>);
db_getter!(get_type_monomorphizations : TypeMonomorphizations() -> Vec<Type>);
db_getter!(get_location_region : LocationRegion(location: Location) -> Region);
db_getter!(check : Check() -> Vec<Diagnostic>);
