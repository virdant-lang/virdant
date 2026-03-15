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
use crate::analysis::symboltable::SymbolKind;
use crate::analysis::symboltable::SymbolTable;
use crate::analysis::typecheck::TypeCheck as Typecheck;
use crate::common::json::ToJson;
use crate::diagnostics::DiagnosticLevel;
use crate::syntax::parsing::parse;
use crate::syntax::payload::Component;
use crate::{diagnostics::Diagnostic, fqn::PackageFqn, source::Source, syntax::parsing::Parsing};

macro_rules! cast {
    ($query_result:expr, $query:ident) => {{
        if let QueryResultPayload::$query(value) = $query_result.0 {
            value
        } else {
            panic!("Expected QueryResult::{}", stringify!($query))
        }
    }};
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Query {
    Packages(),
    Source(PackageFqn),
    Parsing(PackageFqn),
    PackageAnalysis(PackageFqn),
    ComponentAnalysis(BString),
    SymbolTable(),
    ExprRoots(),
    Typecheck(Location),
    Check(),
}

#[derive(Clone, Debug)]
pub struct QueryResult(QueryResultPayload);

#[derive(Clone, Debug)]
enum QueryResultPayload {
    Packages(Vec<PackageFqn>),
    Source(Source),
    Parsing(Arc<Parsing>),
    PackageAnalysis(Arc<PackageAnalysis>),
    ComponentAnalysis(Arc<ComponentAnalysis>),
    SymbolTable(Arc<SymbolTable>),
    ExprRoots(Vec<Location>),
    Typecheck(Arc<Typecheck>),
    Check(Result<Vec<Diagnostic>, Vec<Diagnostic>>),
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

impl From<QueryResultPayload> for QueryResult {
    fn from(payload: QueryResultPayload) -> Self {
        QueryResult(payload)
    }
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

    pub(crate) fn get_packages(&mut self) -> Vec<PackageFqn> {
        let query = Query::Packages();
        cast!(self.get(query), Packages)
    }

    pub(crate) fn get_source(&mut self, package: PackageFqn) -> Source {
        let query = Query::Source(package);
        cast!(self.get(query), Source)
    }

    pub(crate) fn get_parsing(&mut self, package: PackageFqn) -> Arc<Parsing> {
        let query = Query::Parsing(package);
        cast!(self.get(query), Parsing)
    }

    pub(crate) fn get_package_analysis(&mut self, package: PackageFqn) -> Arc<PackageAnalysis> {
        let query = Query::PackageAnalysis(package);
        cast!(self.get(query), PackageAnalysis)
    }

    pub(crate) fn get_component_analysis(&mut self, name: BString) -> Arc<ComponentAnalysis> {
        let query = Query::ComponentAnalysis(name);
        cast!(self.get(query), ComponentAnalysis)
    }

    pub(crate) fn get_symboltable(&mut self) -> Arc<SymbolTable> {
        let query = Query::SymbolTable();
        cast!(self.get(query), SymbolTable)
    }

    pub(crate) fn get_exprroots(&mut self) -> Vec<Location> {
        let query = Query::ExprRoots();
        cast!(self.get(query), ExprRoots)
    }

    pub(crate) fn get_typecheck(&mut self, location: Location) -> Arc<Typecheck> {
        let query = Query::Typecheck(location);
        cast!(self.get(query), Typecheck)
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
            val: QueryResultPayload::Packages(packages).into(),
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
            val: QueryResultPayload::Source(source).into(),
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

    pub fn get_packages(&self) -> Vec<PackageFqn> {
        let query = Query::Packages();
        cast!(self.get(query), Packages)
    }

    pub fn get_parsing(&self, package: PackageFqn) -> Arc<Parsing> {
        let query = Query::Parsing(package);
        cast!(self.get(query), Parsing)
    }

    pub fn get_package_analysis(&self, package: PackageFqn) -> Arc<PackageAnalysis> {
        let query = Query::PackageAnalysis(package);
        cast!(self.get(query), PackageAnalysis)
    }

    pub fn get_component_analysis(&self, name: BString) -> Arc<ComponentAnalysis> {
        let query = Query::ComponentAnalysis(name);
        cast!(self.get(query), ComponentAnalysis)
    }

    pub fn get_symboltable(&self) -> Arc<SymbolTable> {
        let query = Query::SymbolTable();
        cast!(self.get(query), SymbolTable)
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
                            let val = QueryResultPayload::$query(built_val).into();
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
            crate::analysis::build_package_analysis : PackageAnalysis(analysis);
            crate::analysis::component::build_component_analysis : ComponentAnalysis(name);
            crate::analysis::symboltable::build_symboltable : SymbolTable();
            crate::analysis::typecheck::build_exprroots : ExprRoots();
            crate::analysis::typecheck::typecheck : Typecheck(ast_node_id);
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
            Query::PackageAnalysis(package) => json::array!("PackageAnalysis", package.to_json()),
            Query::ComponentAnalysis(name) => json::array!("Components", name.to_json()),
            Query::SymbolTable() => json::array!("SymbolTable"),
            Query::ExprRoots() => json::array!("ExprRoots"),
            Query::Typecheck(location) => json::array!("Typecheck", location.to_json()),
            Query::Check() => json::array!("Check"),
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
        match &self.0 {
            QueryResultPayload::Packages(packages) => packages.to_json(),
            QueryResultPayload::Source(source) => source.to_json(),
            QueryResultPayload::Parsing(parsing) => format!("{self:?}").into(),
            QueryResultPayload::PackageAnalysis(package_analysis) => package_analysis.to_json(),
            QueryResultPayload::ComponentAnalysis(component_analysis) => component_analysis.to_json(),
            QueryResultPayload::SymbolTable(symboltable) => symboltable.to_json(),
            QueryResultPayload::ExprRoots(locations) => locations.to_json(),
            QueryResultPayload::Typecheck(typecheck) => typecheck.to_json(),
            QueryResultPayload::Check(result) => {
                if let Err(diagnostics) = &result {
                    json::value!(["Error", diagnostics.to_json()])
                } else {
                    json::value!(["Ok"])
                }
            }
        }
    }
}

fn check(builder: &mut Builder) -> Result<Vec<Diagnostic>, Vec<Diagnostic>> {
    let mut diagnostics = vec![];

    let symboltable = builder.get_symboltable();
    for symbol in symboltable.symbols() {
        if symbol.kind() == SymbolKind::ModDef {
            let component_analysis = builder.get_component_analysis(symbol.fqn().to_owned());
        }
    }

    for package in builder.get_packages() {
        let package_analysis = builder.get_package_analysis(package);
        diagnostics.extend(package_analysis.diagnostics());
    }

    for exprroot in builder.get_exprroots() {
        let typing = builder.get_typecheck(exprroot);
        diagnostics.extend(typing.diagnostics());
    }

    if diagnostics.iter().any(|diag| diag.level() == DiagnosticLevel::Error) {
        Err(diagnostics)
    } else {
        Ok(diagnostics)
    }
}
