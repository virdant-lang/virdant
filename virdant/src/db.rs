#[macro_use]
mod macros;
mod queries;

use std::any::Any;
use std::sync::Mutex;

use hashbrown::{HashMap, HashSet};

use crate::error::Diagnostic;
use crate::stringtable::StringTable;
use self::queries::{Query, QueryResult};

pub struct Db {
    rev: usize,
    errors: Mutex<Vec<Diagnostic>>,
    map: Mutex<HashMap<Query, CachedVal>>,
    context: DbContext,
}

pub struct DbContext {
    pub stringtable: StringTable,
}

pub struct Builder<'d> {
    db: &'d Db,
    deps: HashSet<Query>,
}

#[derive(Clone)]
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

    fn get<T: Clone + 'static>(&mut self, query: Query) -> T {
        self.deps.insert(query.clone());
        let cached_val = self.db.get_or_build(query);
        cached_val.val.cast::<T>()
    }

    pub fn stringtable(&self) -> StringTable {
        self.db.context.stringtable.clone()
    }
}

impl Db {
    pub fn new(context: DbContext) -> Self {
        Db {
            map: Mutex::new(HashMap::new()),
            errors: Mutex::new(Vec::new()),
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

    fn try_get<T: Any + Clone>(&self, query: Query) -> Option<T> {
        let cached_val = self.get_or_build(query);
        let val = cached_val.val.try_cast::<T>();
        val
    }

    fn get<T: Any + Clone>(&self, query: Query) -> T {
        let cached_val = self.get_or_build(query);
        let val = cached_val.val.cast::<T>();
        val
    }

    fn clear_errors(&self) {
        self.errors.lock().unwrap().clear();
    }
}

#[test]
fn test_db() {
    let stringtable = StringTable::new();
    let mut db = Db::new(DbContext { stringtable });
    let package = crate::fqn::PackageFqn::new(bstr::BString::new("builtin".as_bytes().to_vec()));
    let builtin_source = bstr::BString::new(include_bytes!("../../lib/builtin.vir").to_vec());

    db.set_source(package.clone(), builtin_source.clone());
    for _ in 0 .. 10 {
        db.get_ast(package.clone());
    }
    let ast = db.get_ast(package.clone());
    ast.root().as_ast_node().dump();
}
