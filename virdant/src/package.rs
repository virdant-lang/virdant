//! Provides the `PackageTable` mapping dense `PackageId`s to package
//! names, plus the legacy `PackageFqn`/`ItemFqn` types being phased out.

use bstr::{BStr, BString, ByteSlice};
use indexmap::IndexMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct PackageId(u16);

impl std::fmt::Debug for PackageId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "PackageId({})", self.0)
    }
}

#[derive(Debug, Clone)]
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

fn leak(s: BString) -> &'static BStr {
    BStr::new(Box::leak(Vec::from(s).into_boxed_slice()))
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PackageFqn(&'static BStr);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct ItemFqn(PackageFqn, &'static BStr);

impl From<&str> for PackageFqn {
    fn from(value: &str) -> Self {
        PackageFqn::new(value.into())
    }
}

impl From<String> for PackageFqn {
    fn from(value: String) -> Self {
        PackageFqn::new(value.into())
    }
}

impl std::fmt::Display for PackageFqn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl std::fmt::Display for ItemFqn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.0, self.1)
    }
}

impl AsRef<BStr> for PackageFqn {
    fn as_ref(&self) -> &BStr {
        self.0
    }
}

impl AsRef<BStr> for ItemFqn {
    fn as_ref(&self) -> &BStr {
        self.0.as_ref()
    }
}

impl PackageFqn {
    pub fn new(s: BString) -> PackageFqn {
        PackageFqn(leak(s))
    }
}

impl ItemFqn {
    pub fn new(s: &BStr) -> ItemFqn {
        let colon_index = s.iter().position(|ch| *ch == b':').unwrap();
        assert_eq!(s[colon_index + 1], b':');
        let package = PackageFqn::new(s[..colon_index].to_owned());
        let name = leak(s[colon_index + 2..].to_owned());
        ItemFqn(package, name)
    }

    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn name(&self) -> &BStr {
        self.1
    }
}

impl std::fmt::Debug for PackageFqn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.to_str_lossy().to_owned();
        write!(f, "{s}")
    }
}
