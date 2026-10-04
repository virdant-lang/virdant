//! Thin query implementations building the `Parsing` for a package
//! (tokenizing + LALRPOP parsing) and resolving `InternedString` to
//! owned `BString` text, cached so each package is parsed at most once.

use std::sync::Arc;

use bstr::BString;

use crate::db::Builder;
use crate::package::PackageId;
use crate::syntax::parsing::{InternedString, Parsing, parse};

pub(crate) fn build_parsing(builder: &mut Builder<'_>, package: PackageId) -> Arc<Parsing> {
    let source = builder.get_source(package);
    let parsing = parse(&source);
    Arc::new(parsing)
}

pub(crate) fn build_string(builder: &mut Builder<'_>, string: InternedString) -> Arc<BString> {
    let source = builder.get_source(string.package());
    let parsing = parse(&source);
    Arc::new(parsing.string(string).to_owned())
}
