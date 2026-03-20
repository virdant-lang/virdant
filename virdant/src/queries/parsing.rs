use std::sync::Arc;

use crate::db::Builder;
use crate::fqn::PackageFqn;
use crate::syntax::parsing::{parse, Parsing};

pub(crate) fn build_parsing(builder: &mut Builder<'_>, package: PackageFqn) -> Arc<Parsing> {
    let source = builder.get_source(package);
    let parsing = parse(&source);
    Arc::new(parsing)
}
