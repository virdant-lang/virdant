use bstr::{BStr, BString, ByteSlice};

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
