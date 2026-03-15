mod graphviz;

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

macro_rules! cast {
    ($query_result:expr, $query:ident) => {{
        if let QueryResult::$query(value) = $query_result {
            value
        } else {
            panic!("Expected QueryResult::{}", stringify!($query))
        }
    }};
}

macro_rules! queries {
    ($(
            $query:ident($($arg:ident : $typ:path),*) -> $ret:path;)*
    ) => {
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        enum Query {
            $(
                $query($($typ),*),
            )*
        }

        #[derive(Clone, Debug)]
        enum QueryResult {
            $(
                $query($ret),
            )*
        }
    }
}

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
}

#[derive(Debug)]
pub struct Db {
    rev: usize,
    map: Mutex<HashMap<Query, CachedVal>>,
}

#[derive(Debug, Clone)]
pub struct CachedVal {
    val: QueryResult,
    rev: usize,
    deps: Vec<Query>,
}

impl<'d> Builder<'d> {
    fn new(db: &'d Db) -> Builder<'d> {
        Builder {
            db,
            deps: HashSet::new(),
        }
    }

    fn get(&mut self, query: Query) -> QueryResult {
        self.deps.insert(query.clone());
        let cached_val = self.db.get_or_build(query);
        cached_val.val
    }
}

impl Db {
    pub fn new() -> Self {
        Db {
            map: Mutex::new(HashMap::new()),
            rev: 0,
        }
    }

    pub fn check(&self) -> Result<Vec<Diagnostic>, Vec<Diagnostic>> {
        let query = Query::Check();
        cast!(self.get(query), Check)
    }

    fn get_or_build(&self, query: Query) -> CachedVal {
        if self.is_dirty(&query) {
            let cached_val = query.clone().build(self);
            let mut map = self.map.try_lock().unwrap();
            map.insert(query.clone(), cached_val.clone());
            cached_val
        } else {
            let map = self.map.try_lock().unwrap();
            let cached_val = map.get(&query)
                .unwrap_or_else(|| {
                    panic!("Couldn't find cached value: {query:?}");
                });
            cached_val.clone()
        }
    }

    fn is_dirty(&self, query: &Query) -> bool {
        // Input are never dirty
        if query.is_input() {
            return false;
        }

        // Get the last revision and deps list
        // A query which has never been built is dirty
        let (cached_rev, cached_deps) = {
            let map = self.map.try_lock().unwrap();

            if !map.contains_key(query) {
                return true;
            }

            let cached_val = &map[query];
            (cached_val.rev, cached_val.deps.clone())
        };

        for dep_query in &cached_deps {
            // If any dependency is dirty, the query itself is dirty
            if self.is_dirty(dep_query) {
                return true;
            }

            let dep_rev = {
                let map = self.map.try_lock().unwrap();
                let dep_cached_val = &map[dep_query];
                dep_cached_val.rev
            };

            // If the dependency value is newer than the current value, the query is dirty
            if dep_rev > cached_rev {
                return true;
            }
        }

        false
    }

    fn get(&self, query: Query) -> QueryResult {
        let cached_val = self.get_or_build(query);
        cached_val.val
    }
}

impl Db {
    pub fn set_packages(&mut self, packages: Vec<PackageFqn>) {
        self.rev += 1;

        let query = Query::Packages();
        let val = CachedVal {
            val: QueryResult::Packages(packages).into(),
            rev: self.rev,
            deps: vec![],
        };

        let mut map = self.map.lock().unwrap();
        if let Some(entry) = map.get_mut(&query) {
            *entry = val;
        } else {
            map.insert(query, val);
        }
    }

    pub fn set_source(&mut self, package: PackageFqn, source: Source) {
        self.rev += 1;

        let query = Query::Source(package);
        let val = CachedVal {
            val: QueryResult::Source(source).into(),
            rev: self.rev,
            deps: vec![],
        };

        let mut map = self.map.lock().unwrap();
        if let Some(entry) = map.get_mut(&query) {
            *entry = val;
        } else {
            map.insert(query, val);
        }
    }
}

pub struct Builder<'d> {
    db: &'d Db,
    deps: HashSet<Query>,
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
        macro_rules! dispatch_build {
            ($( $buildfn:path : $query:ident ($($args:tt),*); )*) => {
                match _self {
                    $(
                        Query::$query($( $args, )*) => {
                            let mut builder = Builder::new(db);
                            let built_val = $buildfn(&mut builder, $( $args.clone(), )*);
                            let val = QueryResult::$query(built_val).into();
                            CachedVal {
                                val,
                                rev: builder.db.rev,
                                deps: builder.deps.into_iter().collect(),
                            }
                        }
                    )*
                    _ => unreachable!("Tried to dispatch build for {_self:?}"),
                }
            }
        }

        // when invoked with a Query, dispatch to the given function with the arguments:
        // $buildfn(&mut builder, arg1, ..., argN)
        dispatch_build!(
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
            check : Check();

        )
    }
}

fn build_parsing(builder: &mut Builder<'_>, package: PackageFqn) -> Arc<Parsing> {
    let source = cast!(builder.get(Query::Source(package)), Source);
    let parsing = parse(&source);
    Arc::new(parsing)
}

#[test]
fn test_db() {
    let mut db = Db::new();

    let builtin_package = crate::fqn::PackageFqn::new(bstr::BString::new("builtin".as_bytes().to_vec()));
    let basic_package = crate::fqn::PackageFqn::new(bstr::BString::new("basic".as_bytes().to_vec()));
    db.set_packages(vec![builtin_package.clone(), basic_package.clone()]);

    let text = BString::new(include_bytes!("../../../lib/builtin.vir").to_vec());
    let builtin_source = Source::new(builtin_package.clone(), text);
    db.set_source(builtin_package.clone(), builtin_source);

    let text = BString::new(include_bytes!("../../../examples/basic.vir").to_vec());
    let basic_source = Source::new(basic_package.clone(), text);
    db.set_source(basic_package.clone(), basic_source);


    eprintln!("{}", db.to_json().pretty(4));
    eprintln!("CHECK");
    db.check();
    eprintln!("{}", db.to_json().pretty(4));


//    for _ in 0 .. 10 {
//        db.get_parsing(builtin_package.clone());
//    }
//    let ast = db.get_parsing(builtin_package.clone());
//    ast.root().dump();
//
//    db.get_package_analysis(builtin_package.clone());

    if let Err(diagnostics) = db.check() {
        println!("Diagnostics:");
        for diagnostic in diagnostics {
            println!("  {:?}", diagnostic);
        }
    }

//    eprintln!("{}", db.to_json().pretty(4));
}

impl ToJson for Db {
    fn to_json(&self) -> json::JsonValue {
        let mut pairs: Vec<json::JsonValue> = vec![];
        let guard = self.map.lock().unwrap();
        for (query, val) in guard.iter() {
            pairs.push(json::array!(query.to_json(), val.to_json()));
        }
        json::array!(pairs)
    }
}

fn escape_graphviz_label(label: &str) -> String {
    label
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
}

impl ToJson for Query {
    fn to_json(&self) -> json::JsonValue {
        match self {
            Query::Packages() => json::array!("Packages"),
            Query::Source(package) => json::array!("Source", package.to_json()),
            Query::Parsing(package) => json::array!("Parsing", package.to_json()),
            Query::SyntaxErrors() => json::array!("SyntaxErrors"),
            Query::PackageAnalysis(package) => json::array!("PackageAnalysis", package.to_json()),
            Query::ComponentAnalysis(name) => json::array!("Components", name.to_json()),
            Query::SymbolTable() => json::array!("SymbolTable"),
            Query::ExprRoots() => json::array!("ExprRoots"),
            Query::ExpectedType(location) => json::array!("ExpectedType", location.to_json()),
            Query::TypingContext(item_fqn) => json::array!("TypingContext", item_fqn.to_json()),
            Query::Typing(expr_root) => json::array!("Typecheck", expr_root.to_json()),
            Query::TypeCheck() => json::array!("TypeCheck"),
            Query::Check() => json::array!("Check"),
            Query::TypeDefs() => json::array!("TypeDefs"),
            Query::Typeof(location) => json::array!("Typeof", location.to_json()),
        }
    }
}

impl ToJson for CachedVal {
    fn to_json(&self) -> json::JsonValue {
        self.val.to_json()
    }
}

impl ToJson for QueryResult {
    fn to_json(&self) -> json::JsonValue {
        match &self {
            QueryResult::Packages(packages) => packages.to_json(),
            QueryResult::Source(source) => source.to_json(),
            QueryResult::Parsing(parsing) => format!("{self:?}").into(),
            QueryResult::SyntaxErrors(diagnostics) => diagnostics.to_json(),
            QueryResult::PackageAnalysis(package_analysis) => package_analysis.to_json(),
            QueryResult::ComponentAnalysis(component_analysis) => component_analysis.to_json(),
            QueryResult::SymbolTable(symboltable) => symboltable.to_json(),
            QueryResult::ExprRoots(expr_roots) => expr_roots.to_json(),
            QueryResult::ExpectedType(typ) => typ.to_json(),
            QueryResult::Typing(typecheck) => typecheck.to_json(),
            QueryResult::TypingContext(context) => context.to_json(),
            QueryResult::TypeCheck(diagnostics) => diagnostics.to_json(),
            QueryResult::Typeof(typ) => typ.to_json(),
            QueryResult::Check(result) => {
                if let Err(diagnostics) = &result {
                    json::value!(["Error", diagnostics.to_json()])
                } else {
                    json::value!(["Ok"])
                }
            }
            QueryResult::TypeDefs(type_defs) => type_defs.to_json(),
        }
    }
}

fn build_syntax_errors(builder: &mut Builder) -> Vec<Diagnostic> {
    let mut diagnostics = vec![];
    for package in builder.get_packages() {
        let parsing = builder.get_parsing(package);
        diagnostics.extend(parsing.diagnostics());
    }

    diagnostics
}

macro_rules! db_getter {
    ($getter_name:ident : $query:ident($($arg:ident : $argtyp:path),*) -> $rettyp:path) => {
        impl Db {
            pub fn $getter_name(&self, $($arg : $argtyp),*) -> $rettyp {
                let query = Query::$query($($arg),*);
                cast!(self.get(query), $query)
            }
        }
        impl<'d> Builder<'d> {
            pub(crate) fn $getter_name(&mut self, $($arg : $argtyp),*) -> $rettyp {
                let query = Query::$query($($arg),*);
                cast!(self.get(query), $query)
            }
        }
    };
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
db_getter!(get_typecheck : TypeCheck() -> Vec<Diagnostic>);
db_getter!(get_typeof : Typeof(location: Location) -> Option<Type>);


fn check(builder: &mut Builder) -> Result<Vec<Diagnostic>, Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    diagnostics.extend(builder.get_syntax_errors());
    diagnostics.extend(builder.get_typecheck());

    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
