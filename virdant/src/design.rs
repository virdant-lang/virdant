use std::cell::OnceCell;
use std::sync::{Arc, Weak};

use indexmap::IndexMap;

use crate::expr::Width;
use crate::id::*;
use crate::info::*;
use crate::id;

#[derive(Clone, Debug)]
pub struct Design(pub(crate) Arc<DesignRoot>);

#[derive(Clone, Debug)]
pub(crate) struct DesignRoot {
    pub packages: IndexMap<Id<id::Package>, Package>,
    pub items: IndexMap<Id<id::Item>, Item>,
    pub components: IndexMap<Id<id::Component>, Component>,
    pub fields: IndexMap<Id<id::Field>, Field>,
}

impl Design {
    /// Returns a list of the packages in the design.
    pub fn packages(&self) -> impl Iterator<Item=&Package> {
        self.root().packages.values()
    }

    fn root(&self) -> &DesignRoot {
        &self.0
    }
}

/// `Package` is a representation of a given Virdant package.
/// It will correspond to a single `.vir` file.
#[derive(Clone, Debug)]
pub struct Package {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::Package>,
    pub(crate) info: PackageInfo,
}


#[derive(Clone, Debug)]
pub struct Item {
    pub root: OnceCell<Weak<DesignRoot>>,
    pub id: Id<id::Item>,
    pub info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct Component {
    pub root: OnceCell<Weak<DesignRoot>>,
    pub id: Id<id::Component>,
    pub info: ComponentInfo,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub root: OnceCell<Weak<DesignRoot>>,
    pub id: Id<id::Field>,
    pub info: FieldInfo,
}

impl Package {
    /// The name of the package.
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn items(&self) -> Vec<Item> {
        let mut items: Vec<Item> = vec![];
        for item in self.root().items.values() {
            if *item.info.package.unwrap() == self.id {
                items.push(item.clone());
            }
        }
        items
    }
}

impl Item {
    pub fn name(&self) -> &str {
        &self.info.name
    }

    pub fn kind(&self) -> ItemKind {
        match self.info.kind.unwrap() {
            crate::ItemKind::ModDef => ItemKind::ModDef(ModDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }),
            crate::ItemKind::UnionDef => ItemKind::UnionDef(UnionDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }),
            crate::ItemKind::StructDef => ItemKind::StructDef(StructDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }),
            crate::ItemKind::BuiltinDef => ItemKind::BuiltinDef(BuiltinDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }),
            crate::ItemKind::PortDef => ItemKind::PortDef(PortDef { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ModDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::ModDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct UnionDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::UnionDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct BuiltinDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}

#[derive(Clone, Debug)]
pub struct PortDef {
    pub(crate) root: OnceCell<Weak<DesignRoot>>,
    pub(crate) id: Id<id::StructDef>,
    pub(crate) info: ItemInfo,
}



impl ModDef {
    pub fn components(&self) -> Vec<Component> {
        let mut components = vec![];
        let component_ids = self.info.components.unwrap();
        for (component_id, component) in self.root().components.iter() {
            if component_ids.contains(component_id) {
                components.push(component.clone());
            }
        }
        components
    }
}


impl Component {
    pub fn path(&self) -> &[String] {
        &self.info.path
    }
}

impl StructDef {
    pub fn fields(&self) -> Vec<Field> {
        let mut fields = vec![];
        let field_ids = self.info.fields.unwrap();
        for (field_id, field) in self.root().fields.iter() {
            if field_ids.contains(field_id) {
                fields.push(field.clone());
            }
        }
        fields
    }
}

impl Field {
    pub fn name(&self) -> &str {
        &self.info.name
    }
}

/*
pub struct Type(TypeDef, Vec<TypeParam>);

enum TypeDef {
    Clock,
    Word,
    Union(UnionDef),
    Struct(StructDef),
}

pub enum TypeParam {
    Nat(Width),
    Type(Type),
}

pub enum Portlike {
    Incoming(String, Type),
    Outgoing(String, Type),
    Port(String, PortDef),
}


impl UnionDef {
    pub fn as_item(&self) -> Item {
        let item_id = self.id.as_item();
        self.root().items[&item_id].clone()
    }

    pub fn name(&self) -> String {
        self.as_item().info.name
    }
}


impl StructDef {
    pub fn as_item(&self) -> Item {
        let item_id = self.id.as_item();
        self.root().items[&item_id].clone()
    }
}

#[derive(Clone, Debug)]
pub struct PortDef {
    pub root: OnceCell<Weak<DesignRoot>>,
    pub id: Id<id::PortDef>,
    pub info: ItemInfo,
}

impl PortDef {
    pub fn as_item(&self) -> Item {
        let item_id = self.id.as_item();
        self.root().items[&item_id].clone()
    }
}

#[derive(Clone, Debug)]
pub struct BuiltinDef {
    pub root: OnceCell<Weak<DesignRoot>>,
    pub id: Id<id::BuiltinDef>,
    pub info: ItemInfo,
}

impl BuiltinDef {
    pub fn as_item(&self) -> Item {
        let item_id = self.id.as_item();
        self.root().items[&item_id].clone()
    }

    pub fn name(&self) -> String {
        self.as_item().info.name
    }
}
*/

trait HasRoot {
    fn root(&self) -> Arc<DesignRoot>;
}

macro_rules! impl_hasroot {
    ($struct:ident) => {
        impl HasRoot for $struct {
            fn root(&self) -> Arc<DesignRoot> {
                self.root.get().unwrap().upgrade().unwrap().clone()
            }
        }
    };
}

impl_hasroot!(Package);

impl_hasroot!(ModDef);
impl_hasroot!(UnionDef);
impl_hasroot!(StructDef);
impl_hasroot!(PortDef);
impl_hasroot!(BuiltinDef);

impl_hasroot!(Component);

#[derive(Clone, Debug)]
pub enum ItemKind {
    ModDef(ModDef),
    UnionDef(UnionDef),
    StructDef(StructDef),
    BuiltinDef(BuiltinDef),
    PortDef(PortDef),
}

macro_rules! item_fns {
    ($struct:ident) => {
        impl $struct {
            pub fn as_item(&self) -> Item {
                Item { root: self.root.clone(), id: self.id.cast(), info: self.info.clone() }
            }

            pub fn name(&self) -> String {
                self.as_item().name().to_string()
            }
        }
    };
}

item_fns!(ModDef);
item_fns!(UnionDef);
item_fns!(StructDef);
item_fns!(PortDef);
item_fns!(BuiltinDef);
