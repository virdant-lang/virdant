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

    pub(crate) fn get(&self, query: Query) -> QueryResult {
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
