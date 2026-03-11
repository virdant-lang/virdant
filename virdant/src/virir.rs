pub mod expr;
pub mod typ;

#[cfg(test)]
pub mod tests;

use std::sync::Arc;

use crate::common::PortDir;
use crate::source::Region;

use crate::virir::expr::Expr;
use crate::virir::typ::Type;

pub type Width = u16;
pub struct TypeId(u32);

impl TypeId {
    pub fn new(id: u32) -> Self {
        TypeId(id)
    }
}

pub struct VirIr {
    pub packages: Vec<Package>,
    pub types: Vec<Arc<Type>>,
}

pub struct Package {
    pub items: Vec<Item>,
}

pub enum Item {
    ModDef(ModDef),
}

pub struct ModDef {
    pub region: Region,
    pub ports: Vec<Port>,
    pub drivers: Vec<Driver>,
}

pub struct Port {
    pub region: Region,
    pub name: String,
    pub dir: PortDir,
    pub width: Width,
}

pub struct Driver {
    pub region: Region,
    pub name: String,
    pub expr: Arc<Expr>,
}
