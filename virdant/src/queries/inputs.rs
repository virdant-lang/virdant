//! Input query definitions for the Virdant database

use std::sync::Arc;
use crate::fqn::PackageFqn;
use crate::common::source::Source;
use virdant_db::input;

/// The list of packages configured in the database
#[input]
pub fn packages() -> Arc<Vec<PackageFqn>> {
    unimplemented!("Input functions are never called - use Db::set_packages() instead")
}

/// The source text for a package
#[input]
pub fn source(package: PackageFqn) -> Source {
    unimplemented!("Input functions are never called - use Db::set_source() instead")
}