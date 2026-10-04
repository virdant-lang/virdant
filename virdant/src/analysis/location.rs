//! Defines `Location`, a lightweight value pairing a `PackageId` with
//! an `AstNodeId` that uniquely identifies an AST node across the whole
//! compilation, serving as the canonical reference handle in the
//! analysis layer.

use crate::package::PackageId;
use crate::syntax::ast::AstNodeId;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location(PackageId, AstNodeId);

impl Location {
    pub fn new(package: PackageId, ast_node_id: AstNodeId) -> Location {
        Location(package, ast_node_id)
    }

    pub fn package(&self) -> PackageId {
        self.0
    }

    pub fn ast_node_id(&self) -> AstNodeId {
        self.1
    }
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location({:?}, {:?})", self.0, self.1)
    }
}
