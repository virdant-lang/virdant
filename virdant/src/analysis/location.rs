use crate::fqn::PackageFqn;
use crate::syntax::ast::AstNodeId;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Location(PackageFqn, AstNodeId);

impl Location {
    pub fn new(package: PackageFqn, ast_node_id: AstNodeId) -> Location {
        Location(package, ast_node_id)
    }

    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn ast_node_id(&self) -> AstNodeId {
        self.1
    }
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Location({}, {:?})", self.0, self.1)
    }
}
