//! Defines the [`Id<T>`](Id) type.
//!
//! The [`Id<T>`](Id) type is a wrapper around an [`Intern<String>`](internment::Intern)
//! for use in [`HashMap`](std::collections::HashMap)'s in the [`Virdant`](crate::Virdant) struct.

use internment::Intern;
use std::marker::PhantomData;

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Id<T>(Intern<String>, PhantomData<T>);


impl<T> Id<T> {
    pub(crate) fn new<S: Into<String>>(s: S) -> Id<T> {
        Id(Intern::new(s.into()), PhantomData)
    }

    pub(crate) fn cast<S>(&self) -> Id<S> {
        Id(self.0, PhantomData)
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub mod types {
    macro_rules! id_type {
        ($name:ident) => {
            #[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
            pub struct $name(());
        };
    }

    id_type!(Package);

    id_type!(Item);

    id_type!(ModDef);
    id_type!(UnionDef);
    id_type!(BuiltinDef);
    id_type!(FnDef);
    id_type!(StructDef);
    id_type!(EnumDef);
    id_type!(SocketDef);

    id_type!(Ctor);
    id_type!(Field);
    id_type!(Enumerant);
    id_type!(Channel);

    id_type!(Component);
    id_type!(ExprRoot);
    id_type!(Submodule);
    id_type!(Socket);

    id_type!(Method);
}

pub use types::*;

impl Id<ModDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}

impl Id<UnionDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}

impl Id<StructDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}

impl Id<EnumDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}

impl Id<BuiltinDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}

impl Id<FnDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}

impl Id<SocketDef> {
    pub fn as_item(&self) -> Id<Item> {
        self.cast()
    }
}
