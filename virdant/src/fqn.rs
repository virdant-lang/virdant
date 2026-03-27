use bstr::{BStr, BString, ByteSlice};
use internment::ArcIntern;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PackageFqn(ArcIntern<BString>);

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct ItemFqn(PackageFqn, ArcIntern<BString>);

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
        self.0.as_bstr()
    }
}

impl AsRef<BStr> for ItemFqn {
    fn as_ref(&self) -> &BStr {
        self.0.as_ref()
    }
}

impl PackageFqn {
    pub fn new(s: BString) -> PackageFqn {
        PackageFqn(ArcIntern::from(s))
    }
}

impl ItemFqn {
    pub fn new(s: &BStr) -> ItemFqn {
        let colon_index = s.iter().position(|ch| *ch == b':').unwrap();
        assert_eq!(s[colon_index + 1], b':');
        let package = PackageFqn::new(s[..colon_index].to_owned());
        let name = ArcIntern::from_ref(BStr::new(&s[colon_index + 2..]));
        ItemFqn(package, name)
    }

    pub fn package(&self) -> PackageFqn {
        self.0.clone()
    }

    pub fn name(&self) -> &BStr {
        self.1.as_bstr()
    }
}

impl std::fmt::Debug for PackageFqn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use bstr::ByteSlice;
        let s = self.0.to_str_lossy().to_owned();
        write!(f, "{s}")
    }
}
