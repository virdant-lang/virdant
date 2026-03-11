use crate::common::PortDir;

pub type Width = u16;

pub struct Design {
    packages: Vec<Package>,
}

pub struct Package {
    items: Vec<Item>,
}

pub enum Item {
    ModDef(ModDef),
}

pub struct ModDef {
    ports: Vec<Port>,

    drivers: Vec<Driver>,
}

pub struct Port {
    dir: PortDir,
    name: String,
    width: Width,
}

struct Driver;
