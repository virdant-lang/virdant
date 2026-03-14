pub trait ToJson {
    fn to_json(&self) -> json::JsonValue;
}

impl<T: ToJson> ToJson for &[T] {
    fn to_json(&self) -> json::JsonValue {
        json::array!(self.iter().map(|item| item.to_json()).collect::<Vec<_>>())
    }
}

impl<T: ToJson> ToJson for Vec<T> {
    fn to_json(&self) -> json::JsonValue {
        json::array!(self.iter().map(|item| item.to_json()).collect::<Vec<_>>())
    }
}

impl ToJson for bstr::BStr {
    fn to_json(&self) -> json::JsonValue {
        use bstr::ByteSlice;
        let s = self.clone().to_str_lossy();
        (*s).into()
    }
}

impl ToJson for bstr::BString {
    fn to_json(&self) -> json::JsonValue {
        use bstr::ByteSlice;
        let clone = self.clone();
        let s = clone.to_str_lossy();
        (*s).into()
    }
}

impl<K: ToJson, V: ToJson> ToJson for hashbrown::HashMap<K, V> {
    fn to_json(&self) -> json::JsonValue {
        json::array!(self.iter().map(|(key, val)| {
            json::array!(key.to_json(), val.to_json())
        }).collect::<Vec<_>>())
    }
}

impl<T: ToJson> ToJson for hashbrown::HashSet<T> {
    fn to_json(&self) -> json::JsonValue {
        json::array!(self.iter().map(|item| item.to_json()).collect::<Vec<_>>())
    }
}

impl<K: ToJson, V: ToJson> ToJson for std::collections::HashMap<K, V> {
    fn to_json(&self) -> json::JsonValue {
        json::array!(self.iter().map(|(key, val)| {
            json::array!(key.to_json(), val.to_json())
        }).collect::<Vec<_>>())
    }
}
