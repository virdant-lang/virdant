//! Core database struct for incremental computation

use std::collections::HashMap;
use std::sync::Mutex;
use indexmap::IndexMap;

/// Trait that all query enums must implement
pub trait QueryTrait: Clone + std::fmt::Debug + PartialEq + Eq + std::hash::Hash {
    /// Returns true if this is an input query (never dirty)
    fn is_input(&self) -> bool;
}

/// Trait for query result types
pub trait QueryResultTrait: Clone + std::fmt::Debug {}

/// Core database struct with caching and dependency tracking
#[derive(Debug)]
pub struct Db<Q, R>
where
    Q: QueryTrait,
    R: QueryResultTrait,
{
    /// Global revision counter, incremented when inputs change
    pub rev: usize,
    /// Cache of computed query results
    pub map: Mutex<IndexMap<Q, crate::runtime::cache::CachedVal<Q, R>>>,
    /// Memoizes `is_dirty(query)` results for the current revision
    pub dirty_cache: Mutex<HashMap<Q, bool>>,
    /// Stack of queries currently being built (for cycle detection)
    pub call_stack: Mutex<Vec<Q>>,
    /// Optional trace of all query invocations
    #[cfg(feature = "tracing")]
    pub trace: Mutex<Vec<TraceElement<Q>>>,
}

#[cfg(feature = "tracing")]
#[derive(Debug)]
pub struct TraceElement<Q> {
    pub query: Q,
    pub level: usize,
    pub cached: bool,
    pub location: std::panic::Location<'static>,
}

impl<Q, R> Db<Q, R>
where
    Q: QueryTrait,
    R: QueryResultTrait,
{
    /// Create a new empty database
    pub fn new() -> Self {
        Db {
            map: Mutex::new(IndexMap::new()),
            dirty_cache: Mutex::new(HashMap::new()),
            rev: 0,
            call_stack: Mutex::new(vec![]),
            #[cfg(feature = "tracing")]
            trace: Mutex::new(vec![]),
        }
    }

    /// Check if a query result is dirty (needs recomputation) - internal implementation
    /// This should be called while already holding the necessary locks
    pub fn is_dirty_internal(
        query: &Q,
        cached: &crate::runtime::CachedVal<Q, R>,
        map: &indexmap::IndexMap<Q, crate::runtime::CachedVal<Q, R>>,
        dirty_cache: &mut HashMap<Q, bool>,
        current_rev: usize,
    ) -> bool {
        // Check memoization cache
        if let Some(&is_dirty) = dirty_cache.get(query) {
            return is_dirty;
        }

        // Inputs are never dirty
        if query.is_input() {
            dirty_cache.insert(query.clone(), false);
            return false;
        }

        // If computed in an older revision, check dependencies
        let is_dirty = if cached.rev < current_rev {
            // Check if any dependency is dirty
            cached.deps.iter().any(|dep| {
                if let Some(dep_cached) = map.get(dep) {
                    Self::is_dirty_internal(dep, dep_cached, map, dirty_cache, current_rev)
                } else {
                    // Dependency not in cache, so we need to recompute
                    true
                }
            })
        } else {
            false
        };

        dirty_cache.insert(query.clone(), is_dirty);
        is_dirty
    }

    /// Increment the global revision number
    pub fn increment_revision(&mut self) {
        self.rev += 1;
        self.dirty_cache.lock().unwrap().clear();
    }

    /// Set a cached value directly (for inputs)
    pub fn set_value(&mut self, query: Q, val: crate::runtime::CachedVal<Q, R>) {
        self.map.lock().unwrap().insert(query, val);
    }

    /// Dump the query execution trace (requires "tracing" feature)
    pub fn dump_trace(&self) {
        #[cfg(feature = "tracing")]
        {
            let trace = self.trace.lock().unwrap();
            eprintln!("=== Db trace ===========================");
            for entry in trace.iter() {
                let indent = "    ".repeat(entry.level);
                let location = format!(
                    "{}[{}:{}]",
                    entry.location.file(),
                    entry.location.line(),
                    entry.location.column(),
                );
                let text = format!("{:?}", entry.query);
                eprintln!("{indent}{text} {location}");
            }
            eprintln!("========================================");
        }
        #[cfg(not(feature = "tracing"))]
        {
            eprintln!("Tracing not enabled (enable 'tracing' feature)");
        }
    }

    /// Dump the current build stack
    pub fn dump_call_stack(&self) {
        let stack = self.call_stack.lock().unwrap();
        eprintln!("=== Db build stack =====================");
        for (i, query) in stack.iter().enumerate() {
            eprintln!("{}{:?}", "    ".repeat(i), query);
        }
        eprintln!("========================================");
    }
}

/// Public interface for generated DB implementations
pub trait DatabaseExt {
    /// Increment the revision counter (called when inputs change)
    fn increment_revision(&mut self);
}
