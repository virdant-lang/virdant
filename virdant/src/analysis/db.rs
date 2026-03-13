use std::sync::Arc;
use std::sync::Mutex;

use bstr::BString;
use hashbrown::{HashMap, HashSet};

use crate::analysis::PackageAnalysis;
use crate::syntax::parsing::parse;
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
    Source(PackageFqn),
    Parsing(PackageFqn),
    PackageAnalysis(PackageFqn),
}

#[derive(Clone, Debug)]
pub struct QueryResult(QueryResultPayload);

#[derive(Clone, Debug)]
enum QueryResultPayload {
    Source(Source),
    Parsing(Arc<Parsing>),
    PackageAnalysis(Arc<PackageAnalysis>),
}

pub struct Db {
    rev: usize,
    map: Mutex<HashMap<Query, CachedVal>>,
    context: DbContext,
}

#[derive(Clone)]
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

    pub(crate) fn get_parsing(&mut self, package: PackageFqn) -> Arc<Parsing> {
        let query = Query::Parsing(package);
        cast!(self.get(query), Parsing)
    }
}

impl Db {
    pub fn new(context: DbContext) -> Self {
        Db {
            map: Mutex::new(HashMap::new()),
            rev: 0,
            context,
        }
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

    pub fn get_parsing(&mut self, package: PackageFqn) -> Arc<Parsing> {
        let query = Query::Parsing(package);
        cast!(self.get(query), Parsing)
    }
}

pub struct DbContext {
}

pub struct Builder<'d> {
    db: &'d Db,
    deps: HashSet<Query>,
}

impl Query {
    fn is_input(&self) -> bool {
        matches!(self, Query::Source(_))
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
                    _ => unreachable!(),
                }
            }
        }

        // when invoked with a Query, dispatch to the given function with the arguments:
        // $buildfn(&mut builder, arg1, ..., argN)
        dispatch_build!(
            build_parsing : Parsing(package);
            crate::analysis::build_package_analysis : PackageAnalysis(analysis);
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
    let mut db = Db::new(DbContext { });
    let package = crate::fqn::PackageFqn::new(bstr::BString::new("builtin".as_bytes().to_vec()));
    let text = BString::new(include_bytes!("../../../lib/builtin.vir").to_vec());
    let builtin_source = Source::new(package.clone(), text);

    db.set_source(package.clone(), builtin_source);
    for _ in 0 .. 10 {
        db.get_parsing(package.clone());
    }
    let ast = db.get_parsing(package.clone());
    ast.root().dump();
}
