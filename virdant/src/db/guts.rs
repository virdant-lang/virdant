use super::*;

#[derive(Debug)]
pub struct Db {
    pub(super) rev: usize,
    pub(super) map: Mutex<IndexMap<Query, CachedVal>>,
    pub(super) call_stack: Mutex<Vec<Query>>,
    trace: Mutex<Vec<TraceElement>>,
}

#[derive(Debug, Clone)]
pub(super) struct CachedVal {
    pub(super) val: QueryResult,
    pub(super) rev: usize,
    pub(super) deps: Vec<Query>,
}

#[derive(Debug)]
struct TraceElement {
    query: Query,
    level: usize,
    cached: bool,
    location: std::panic::Location<'static>,
}

impl<'d> Builder<'d> {
    pub(crate) fn new(db: &'d Db) -> Builder<'d> {
        Builder {
            db,
            deps: IndexSet::new(),
        }
    }

    pub(crate) fn get(&mut self, query: Query, location: std::panic::Location<'static>) -> QueryResult {
        self.deps.insert(query.clone());
        let cached_val = self.db.get_or_build(query, location);
        cached_val.val
    }

    pub fn dump(&self) {
        let stack = self.db.call_stack.lock().unwrap();
        eprintln!("=== Db build stack =====================");
        for (i, query) in stack.iter().enumerate() {
            eprintln!("{}{:?}", "    ".repeat(i), query);
        }
        eprintln!("========================================");
    }
}

impl Db {
    pub fn new() -> Self {
        Db {
            map: Mutex::new(IndexMap::new()),
            rev: 0,
            call_stack: Mutex::new(vec![]),
            trace: Mutex::new(vec![]),
        }
    }

    pub(super) fn get_or_build(&self, query: Query, location: std::panic::Location<'static>) -> CachedVal {
        if self.is_dirty(&query) {
            let mut call_stack = self.call_stack.lock().unwrap();
            let level = call_stack.len();
            call_stack.push(query.clone());
            self.trace.lock().unwrap().push(TraceElement {
                query: query.clone(),
                level,
                cached: false,
                location,
            });
            drop(call_stack);
            let cached_val = query.clone().build(self);
            self.call_stack.lock().unwrap().pop();
            let mut map = self.map.try_lock().unwrap();
            map.insert(query.clone(), cached_val.clone());
            cached_val
        } else {
            let level = self.call_stack.lock().unwrap().len();
            self.trace.lock().unwrap().push(TraceElement {
                query: query.clone(),
                level,
                cached: true,
                location,
            });
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

    pub fn dump(&self) {
        use colored::Colorize;
        let trace = self.trace.lock().unwrap();
        eprintln!("=== Db trace ===========================");
        for trace in trace.iter() {
            let indent = "    ".repeat(trace.level);
            let location = format!(
                "{}[{}:{}]",
                trace.location.file(),
                trace.location.line(),
                trace.location.column(),
            ).dimmed();
            let text = if trace.query.is_input() {
                format!("{:?}", trace.query).bright_blue()
            } else if trace.cached {
                format!("{:?}", trace.query).green()
            } else {
                format!("{:?}", trace.query).bright_yellow()
            };
            eprintln!("{indent}{text} {location}");
        }
        eprintln!("========================================");
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
    pub(super) deps: IndexSet<Query>,
}
