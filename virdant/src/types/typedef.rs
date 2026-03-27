use bstr::BString;

use crate::analysis::location::Location;
use crate::common::TypeScheme;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    pub(crate) fqn: BString,
    pub(crate) location: Location,
    pub(crate) kind: TypeScheme,
}

