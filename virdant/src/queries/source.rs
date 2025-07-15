//! Input for the textual source of a package.

use crate::fqn::PackageFqn;

pub type Params = PackageFqn;
pub type Response = bstr::BString;
