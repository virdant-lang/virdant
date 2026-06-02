use std::sync::Arc;

use bstr::BString;
use virdant_db::query;

use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::parsing::{InternedString, Parsing, parse};

#[query(get = get_parsing)]
pub fn build_parsing(builder: &mut Builder<'_>, package: PackageFqn) -> Arc<Parsing> {
    let source = builder.get_source(package);
    let parsing = parse(&source);
    Arc::new(parsing)
}

#[query]
pub fn build_string(builder: &mut Builder<'_>, string: InternedString) -> Arc<BString> {
    let source = builder.get_source(string.package());
    let parsing = parse(&source);
    Arc::new(parsing.string(string).to_owned())
}
