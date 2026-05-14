use std::collections::HashMap;

use super::*;

#[derive(Debug)]
pub struct Db {
    pub(super) rev: usize,
    pub(super) map: Mutex<IndexMap<Query, CachedVal>>,
    // Memoizes `is_dirty(query)` results for the current `rev`. Cleared whenever
    // an input changes (set_packages / set_source).
    pub(super) dirty_cache: Mutex<HashMap<Query, bool>>,
    pub(super) call_stack: Mutex<Vec<Query>>,
    trace: Mutex<Vec<TraceElement>>,
}

#[derive(Debug, Clone)]
pub(super) struct CachedVal {
    pub(super) val: QueryResult,
    pub(super) rev: usize,
    pub(super) deps: Vec<Query>,
    pub(super) duration: std::time::Duration, // How long the query took to build
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
            dirty_cache: Mutex::new(HashMap::new()),
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
            let start = std::time::Instant::now();
            let mut cached_val = query.clone().build(self);
            // Time elapsed building the query
            let duration = start.elapsed();
            cached_val.duration = duration;
            self.call_stack.lock().unwrap().pop();
            {
                let mut map = self.map.try_lock().unwrap();
                map.insert(query.clone(), cached_val.clone());
            }
            // The query has just been rebuilt at the current rev; it's now clean.
            self.dirty_cache.lock().unwrap().insert(query.clone(), false);
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
        let mut dirty_cache = self.dirty_cache.lock().unwrap();
        let map = self.map.try_lock().unwrap();
        Self::is_dirty_inner(query, &map, &mut dirty_cache)
    }

    fn is_dirty_inner(
        query: &Query,
        map: &IndexMap<Query, CachedVal>,
        dirty_cache: &mut HashMap<Query, bool>,
    ) -> bool {
        if let Some(&result) = dirty_cache.get(query) {
            return result;
        }

        let result = 'result: {
            let Some(cached_val) = map.get(query) else {
                // Never built ⇒ dirty.
                break 'result true;
            };
            let cached_rev = cached_val.rev;
            for dep_query in &cached_val.deps {
                if Self::is_dirty_inner(dep_query, map, dirty_cache) {
                    break 'result true;
                }
                if let Some(dep_cached_val) = map.get(dep_query) {
                    if dep_cached_val.rev > cached_rev {
                        break 'result true;
                    }
                }
            }
            false
        };

        dirty_cache.insert(query.clone(), result);
        result
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
            if trace.cached || trace.query.is_input() {
                eprintln!("{indent}{text} {location}");
            } else {
                let duration = {
                    let map = self.map.try_lock().unwrap();
                    map.get(&trace.query).map(|cv| cv.duration).unwrap_or_default()
                };
                let duration_text = format!("({duration:?})").dimmed();
                eprintln!("{indent}{text} {duration_text} {location}");
            }
        }
        eprintln!("========================================");
    }
}

impl Db {
    pub fn set_packages(&mut self, packages: Vec<PackageFqn>) {
        self.rev += 1;
        self.dirty_cache.lock().unwrap().clear();

        let query = Query::Packages();
        let val = CachedVal {
            val: QueryResult::Packages(Arc::new(packages)).into(),
            rev: self.rev,
            deps: vec![],
            duration: std::time::Duration::ZERO,
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
        self.dirty_cache.lock().unwrap().clear();

        let query = Query::Source(package);
        let val = CachedVal {
            val: QueryResult::Source(source).into(),
            rev: self.rev,
            deps: vec![],
            duration: std::time::Duration::ZERO,
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
