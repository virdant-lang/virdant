use internment::ArcIntern;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct PackageFqn(ArcIntern<str>);

impl std::fmt::Display for PackageFqn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl AsRef<str> for PackageFqn {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl PackageFqn {
    pub fn new(s: &[u8]) -> PackageFqn {
        PackageFqn(ArcIntern::from(String::from_utf8_lossy(&s)))
    }
}
