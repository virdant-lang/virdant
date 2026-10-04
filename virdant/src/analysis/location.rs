//! Defines `Location`, a lightweight value pairing a `PackageId` with
//! an `AstNodeId` that uniquely identifies an AST node across the whole
//! compilation, serving as the canonical reference handle in the
//! analysis layer.

use crate::package::PackageId;
use crate::syntax::ast::AstNodeId;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Location(PackageId, AstNodeId);

// Serializes as the string "{package}:{ast_node_id}",
// so that `Location` can be used as a JSON map key.
impl serde::Serialize for Location {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&format!("{}:{}", self.0.id(), self.1.0))
    }
}

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
