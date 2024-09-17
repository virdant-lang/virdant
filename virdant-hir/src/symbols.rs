use std::marker::PhantomData;

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct Id<T>(u32, PhantomData<T>);

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
        let parts: Vec<&str> = std::any::type_name::<T>().split("::").collect();
        write!(f, "{}({})", parts[1], self.to_usize())
    }
}

#[derive(Debug, Clone)]
pub struct TypeDef {
    pub(crate) id: Option<Id<TypeDef>>,
    pub(crate) qualname: String,
}

#[derive(Debug, Clone)]
pub struct ModDef {
    pub(crate) id: Option<Id<ModDef>>,
    pub(crate) qualname: String,
}

#[derive(Debug, Clone)]
pub struct FnDef {
    pub(crate) id: Option<Id<FnDef>>,
    pub(crate) qualname: String,
}

#[derive(Debug, Clone)]
pub struct SocketDef {
    pub(crate) id: Option<Id<SocketDef>>,
    pub(crate) qualname: String,
}

#[derive(Debug, Clone)]
pub struct Component {
    pub(crate) id: Option<Id<Component>>,
    pub(crate) qualname: String,
}

#[derive(Debug, Clone)]
pub struct Submodule {
    pub(crate) id: Option<Id<Submodule>>,
    pub(crate) qualname: String,
}

#[derive(Debug, Clone)]
pub struct Socket {
    pub(crate) id: Option<Id<Socket>>,
    pub(crate) qualname: String,
}

#[derive(Clone)]
pub enum ItemId {
    TypeDef(Id<TypeDef>),
    ModDef(Id<ModDef>),
    Component(Id<Component>),
}

impl std::fmt::Debug for ItemId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemId::TypeDef(id) => write!(f, "TypeDef({})", id.to_usize()),
            ItemId::ModDef(id) => write!(f, "ModDef({})", id.to_usize()),
            ItemId::Component(id) => write!(f, "Component({})", id.to_usize()),
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
