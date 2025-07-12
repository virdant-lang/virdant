use bstr::{BStr, BString, ByteSlice};
use internment::ArcIntern;

#[derive(Clone)]
pub struct StringTable;

#[derive(Clone)]
pub struct InternedString {
    data: ArcIntern<BString>,
}

impl std::fmt::Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", String::from_utf8_lossy(&self.data))
    }
}

impl StringTable {
    pub fn new() -> StringTable {
        StringTable
    }

    pub fn get<'s>(&self, string: &'s InternedString) -> &'s BStr {
        string.data.as_bstr()
    }

    pub fn intern(&self, string: &[u8]) -> InternedString {
        InternedString {
            data: ArcIntern::new(BString::from(string)),
        }
    }
}
