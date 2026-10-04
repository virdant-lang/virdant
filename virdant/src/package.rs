//! Provides the `PackageTable` mapping dense `PackageId`s to package names.

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct PackageId(u16);

impl serde::Serialize for PackageId {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_u16(self.0)
    }
}

impl std::fmt::Debug for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PackageId({})", self.0)
    }
}

impl PackageId {
    pub fn id(&self) -> u16 {
        self.0
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct PackageTable {
    names: Vec<BString>,
    by_name: IndexMap<BString, PackageId>,
    builtin: PackageId,
}

impl PackageTable {
    pub fn new(names: Vec<BString>) -> PackageTable {
        let by_name: IndexMap<BString, PackageId> = names
            .iter()
            .enumerate()
            .map(|(i, name)| (name.clone(), PackageId(i as u16)))
            .collect();
        let builtin = by_name[BStr::new("builtin")];
        PackageTable { names, by_name, builtin }
    }

    pub fn name(&self, id: PackageId) -> &BStr {
        self.names[id.0 as usize].as_bstr()
    }

    pub fn id(&self, name: &BStr) -> Option<PackageId> {
        self.by_name.get(name).copied()
    }

    pub fn ids(&self) -> impl Iterator<Item = PackageId> + '_ {
        (0..self.names.len() as u16).map(PackageId)
    }

    pub fn builtin(&self) -> PackageId {
        self.builtin
}
}
