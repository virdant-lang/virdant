//! The Builder struct for tracking dependencies during query execution

use indexmap::IndexSet;

/// Builder for tracking query dependencies
/// 
/// This struct is passed as the first parameter to all query functions.
/// It tracks which queries are invoked during the build process.
pub struct Builder<'d, D, Q> {
    /// Reference to the database
    pub(crate) db: &'d D,
    /// Set of queries this build depends on
    pub(crate) deps: IndexSet<Q>,
}