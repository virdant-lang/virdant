use super::*;

#[derive(Debug)]
pub struct Db {
    pub(super) rev: usize,
    pub(super) map: Mutex<HashMap<Query, CachedVal>>,
    pub(super) call_stack: Mutex<Vec<Query>>,
}

#[derive(Debug, Clone)]
pub(super) struct CachedVal {
    pub(super) val: QueryResult,
    pub(super) rev: usize,
    pub(super) deps: Vec<Query>,
}

impl<'d> Builder<'d> {
    pub(crate) fn new(db: &'d Db) -> Builder<'d> {
        Builder {
            db,
            deps: HashSet::new(),
        }
    }

    pub(crate) fn get(&mut self, query: Query) -> QueryResult {
        self.deps.insert(query.clone());
        let cached_val = self.db.get_or_build(query);
        cached_val.val
    }

    pub fn dump(&self) {
        let stack = self.db.call_stack.lock().unwrap();
        eprintln!("=== Db build stack =====================");
        for (i, query) in stack.iter().enumerate() {
            eprintln!("{}{:?}", "  ".repeat(i), query);
        }
        eprintln!("========================================");
    }
}

impl Db {
    pub fn new() -> Self {
        Db {
            map: Mutex::new(HashMap::new()),
            rev: 0,
            call_stack: Mutex::new(vec![]),
        }
    }

    fn get_or_build(&self, query: Query) -> CachedVal {
        if self.is_dirty(&query) {
            self.call_stack.lock().unwrap().push(query.clone());
            let cached_val = query.clone().build(self);
            self.call_stack.lock().unwrap().pop();
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

    pub fn get(&self, query: Query) -> QueryResult {
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
    pub(super) db: &'d Db,
    pub(super) deps: HashSet<Query>,
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
            Query::AllExprs() => json::array!("AllExprs"),
            Query::ExpectedType(location) => json::array!("ExpectedType", location.to_json()),
            Query::TypingContext(item_fqn) => json::array!("TypingContext", item_fqn.to_json()),
            Query::Typing(expr_root) => json::array!("Typecheck", expr_root.to_json()),
            Query::TypeCheck() => json::array!("TypeCheck"),
            Query::Check() => json::array!("Check"),
            Query::TypeDefs() => json::array!("TypeDefs"),
            Query::Typeof(location) => json::array!("Typeof", location.to_json()),
            Query::TypeofAll() => json::array!("TypeofAll"),
            Query::TypeMonomorphizations() => json::array!("TypeMonomorphizations"),
            Query::LocationRegion(location) => json::array!("LocationRegion", location.to_json()),
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
            QueryResult::AllExprs(exprs) => exprs.to_json(),
            QueryResult::ExpectedType(typ) => typ.to_json(),
            QueryResult::Typing(typecheck) => typecheck.to_json(),
            QueryResult::TypingContext(context) => context.to_json(),
            QueryResult::TypeCheck(diagnostics) => diagnostics.to_json(),
            QueryResult::Typeof(typ) => typ.to_json(),
            QueryResult::TypeofAll(typs) => typs.to_json(),
            QueryResult::Check(diagnostics) => diagnostics.to_json(),
            QueryResult::TypeDefs(type_defs) => type_defs.to_json(),
            QueryResult::TypeMonomorphizations(items) => items.to_json(),
            QueryResult::LocationRegion(region) => region.to_json(),
        }
    }
}
