//! Caching infrastructure for query results

use std::time::Duration;

/// A cached query result with metadata
#[derive(Debug, Clone)]
pub struct CachedVal<Q, R> {
    /// The cached result value
    pub val: R,
    /// The revision number when this was computed
    pub rev: usize,
    /// Dependencies tracked during building
    pub deps: Vec<Q>,
    /// How long the query took to build
    pub duration: Duration,
}