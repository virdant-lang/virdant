use std::marker::PhantomData;

pub trait Entity {
    fn typename() -> &'static str;
}

#[derive(Copy, Eq, PartialEq, Hash)]
pub struct Id<T: Entity>(u32, PhantomData<T>);

impl<T: Entity> From<usize> for Id<T> {
    fn from(id: usize) -> Self {
        #[cfg(debug_assertions)] usize::try_from(id).unwrap();
        Id(id as u32, PhantomData)
    }
}

impl<T: Entity> From<Id<T>> for usize {
    fn from(id: Id<T>) -> Self {
        let Id(id, _phantomdata) = id;
        #[cfg(debug_assertions)] u32::try_from(id).unwrap();
        id.try_into().unwrap()
    }
}

impl<T: Entity> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Id").field(&self.0).field(&self.1).finish()
    }
}

impl<T: Entity> Clone for Id<T> {
    fn clone(&self) -> Self {
        let Id(id, phantomdata) = *self;
        Id(id, phantomdata)
    }
}
