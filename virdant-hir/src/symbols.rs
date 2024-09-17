use std::marker::PhantomData;

use virdant_common::{ComponentClass, Flow};

#[derive(Hash, PartialEq, Eq)]
pub struct Id<T>(u32, PhantomData<T>);

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Id::new(self.0 as usize)
    }
}

impl<T> Copy for Id<T> {}

impl<T> Id<T> {
    pub(crate) fn new(id: usize) -> Id<T> {
        let id: u32 = id.try_into().unwrap();
        Id(id, PhantomData)
    }

    pub(crate) fn to_usize(&self) -> usize {
        let Id(id, PhantomData) = self;
        *id as usize
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name: &str = std::any::type_name::<T>().split("::").last().unwrap();
        write!(f, "{name}({})", self.to_usize())
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub id: Option<Id<TypeDef>>,
    pub qualname: String,
}

#[derive(Debug, Clone)]
pub struct ModDef {
    pub id: Option<Id<ModDef>>,
    pub qualname: String,
}

#[derive(Debug, Clone)]
pub struct FnDef {
    pub id: Option<Id<FnDef>>,
    pub qualname: String,
}

#[derive(Debug, Clone)]
pub struct SocketDef {
    pub id: Option<Id<SocketDef>>,
    pub qualname: String,
}

#[derive(Debug, Clone)]
pub struct Component {
    pub id: Option<Id<Component>>,
    pub qualname: String,
    pub moddef: Id<ModDef>,
    pub flow: Flow,
    pub class: ComponentClass,
//    pub path: Vec<String>,
//    pub typ: Arc<Type>,
//    pub clock: Option<Arc<Expr>>,
//    pub driver: None
}

#[derive(Debug, Clone)]
pub struct Submodule {
    pub id: Option<Id<Submodule>>,
    pub qualname: String,
    pub moddef: Id<ModDef>,
}

#[derive(Debug, Clone)]
pub struct Socket {
    pub id: Option<Id<Socket>>,
    pub qualname: String,
    pub socketdef: Id<SocketDef>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub id: Option<Id<Field>>,
    pub qualname: String,
    pub typedef: Id<TypeDef>,
}

#[derive(Debug, Clone)]
pub struct Ctor {
    pub id: Option<Id<Ctor>>,
    pub qualname: String,
    pub typedef: Id<TypeDef>,
}

#[derive(Debug, Clone)]
pub struct Enumerant {
    pub id: Option<Id<Enumerant>>,
    pub qualname: String,
    pub typedef: Id<TypeDef>,
}

#[derive(Debug, Clone)]
pub struct Channel {
    pub id: Option<Id<Channel>>,
    pub qualname: String,
    pub socketdef: Id<SocketDef>,
}

#[derive(Clone)]
pub enum ItemId {
    TypeDef(Id<TypeDef>),
    ModDef(Id<ModDef>),
    SocketDef(Id<SocketDef>),
}

impl std::fmt::Debug for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemId::TypeDef(id) => write!(f, "TypeDef({})", id.to_usize()),
            ItemId::ModDef(id) => write!(f, "ModDef({})", id.to_usize()),
            ItemId::SocketDef(id) => write!(f, "SocketDef({})", id.to_usize()),
        }
    }
}

macro_rules! symbol_methods {
    ($name:ident) => {
        impl $name {
            pub(crate) fn set_id(&mut self, id: Id<$name>) { self.id = Some(id); }
            pub fn id(&self) -> Id<$name> { self.id.clone().unwrap() }
            pub fn qualname(&self) -> &str { &self.qualname }
        }
    };
}

symbol_methods!(TypeDef);
symbol_methods!(ModDef);
symbol_methods!(SocketDef);
symbol_methods!(FnDef);
symbol_methods!(Component);
symbol_methods!(Submodule);
symbol_methods!(Socket);
symbol_methods!(Field);
symbol_methods!(Ctor);
symbol_methods!(Enumerant);
symbol_methods!(Channel);
