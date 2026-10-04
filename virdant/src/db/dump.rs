//! Adds `Db::dump_query_results_json`, serializing every cached query
//! result to JSON. Each entry records the query variant name, a
//! structured key, the serialized result, the query's dependencies,
//! the revision at which it was built, and its build duration.
//!
//! `BString` values serialize as lossy UTF-8 strings, so that byte
//! strings stay readable in JSON and work as map keys. Results which
//! cannot be serialized are recorded with an error note.

use super::*;
use serde_json::Value as JsonValue;

impl Db {
    /// Serializes all cached query results as a JSON array of entries.
    pub fn dump_query_results_json(&self) -> JsonValue {
        let map = self.map.lock().unwrap();
        let mut entries: Vec<JsonValue> = vec![];
        for (query, cached_val) in map.iter() {
            let name = query_debug_name(query);
            let debug = format!("{:?}", query);

            let mut object = serde_json::Map::new();
            object.insert("name".to_string(), JsonValue::String(name));
            object.insert("debug".to_string(), JsonValue::String(debug.clone()));
            object.insert("key".to_string(), to_json_value_or_error(query, &debug));
            object.insert(
                "result".to_string(),
                to_json_value_or_error(&cached_val.val, &debug),
            );
            object.insert(
                "deps".to_string(),
                to_json_value_or_error(&cached_val.deps, &debug),
            );
            object.insert("rev".to_string(), JsonValue::from(cached_val.rev));
            object.insert(
                "duration_secs".to_string(),
                JsonValue::from(cached_val.duration.as_secs_f64()),
            );
            entries.push(JsonValue::Object(object));
        }
        JsonValue::Array(entries)
    }
}

/// Serializes a value, or returns an error note if it cannot be
/// serialized.
fn to_json_value_or_error<T: serde::Serialize + ?Sized>(value: &T, debug: &str) -> JsonValue {
    match value.serialize(ValueSerializer) {
        Ok(json) => json,
        Err(e) => {
            let message = format!("<serialization error: {e} in {debug}>");
            JsonValue::String(message)
        }
    }
}

/// The query variant name, taken from the `Debug` representation up to
/// the first opening parenthesis.
fn query_debug_name(query: &Query) -> String {
    let debug = format!("{:?}", query);
    match debug.find('(') {
        Some(idx) => debug[..idx].to_string(),
        None => debug,
    }
}

// ---------------------------------------------------------------------------
// ValueSerializer
// ---------------------------------------------------------------------------

/// A serializer producing `serde_json::Value`s.
///
/// Unlike `serde_json::to_value`, byte strings serialize as lossy
/// UTF-8 strings (so `BString` values stay readable and work as map
/// keys), and map keys may be strings, numbers, or booleans.
struct ValueSerializer;

impl serde::Serializer for ValueSerializer {
    type Ok = JsonValue;
    type Error = serde_json::Error;
    type SerializeSeq = ValueSeq;
    type SerializeTuple = ValueSeq;
    type SerializeTupleStruct = ValueSeq;
    type SerializeTupleVariant = ValueTupleVariant;
    type SerializeMap = ValueMap;
    type SerializeStruct = ValueStruct;
    type SerializeStructVariant = ValueStructVariant;

    fn serialize_bool(self, v: bool) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<JsonValue, Self::Error> {
        self.serialize_i64(v as i64)
    }

    fn serialize_i16(self, v: i16) -> Result<JsonValue, Self::Error> {
        self.serialize_i64(v as i64)
    }

    fn serialize_i32(self, v: i32) -> Result<JsonValue, Self::Error> {
        self.serialize_i64(v as i64)
    }

    fn serialize_i64(self, v: i64) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Number(v.into()))
    }

    fn serialize_i128(self, v: i128) -> Result<JsonValue, Self::Error> {
        let number = serde_json::Number::from_i128(v)
            .ok_or_else(|| serde::ser::Error::custom("i128 out of JSON range"))?;
        Ok(JsonValue::Number(number))
    }

    fn serialize_u8(self, v: u8) -> Result<JsonValue, Self::Error> {
        self.serialize_u64(v as u64)
    }

    fn serialize_u16(self, v: u16) -> Result<JsonValue, Self::Error> {
        self.serialize_u64(v as u64)
    }

    fn serialize_u32(self, v: u32) -> Result<JsonValue, Self::Error> {
        self.serialize_u64(v as u64)
    }

    fn serialize_u64(self, v: u64) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Number(v.into()))
    }

    fn serialize_u128(self, v: u128) -> Result<JsonValue, Self::Error> {
        let number = serde_json::Number::from_u128(v)
            .ok_or_else(|| serde::ser::Error::custom("u128 out of JSON range"))?;
        Ok(JsonValue::Number(number))
    }

    fn serialize_f32(self, v: f32) -> Result<JsonValue, Self::Error> {
        self.serialize_f64(v as f64)
    }

    fn serialize_f64(self, v: f64) -> Result<JsonValue, Self::Error> {
        serde_json::Number::from_f64(v)
            .map(JsonValue::Number)
            .ok_or_else(|| serde::ser::Error::custom("not a finite float"))
    }

    fn serialize_char(self, v: char) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::String(v.to_string()))
    }

    fn serialize_str(self, v: &str) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::String(v.to_owned()))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::String(String::from_utf8_lossy(v).into_owned()))
    }

    fn serialize_none(self) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Null)
    }

    fn serialize_some<T: ?Sized + serde::Serialize>(
        self,
        value: &T,
    ) -> Result<JsonValue, Self::Error> {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Null)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<JsonValue, Self::Error> {
        Ok(serde_json::json!({ variant: null }))
    }

    fn serialize_newtype_struct<T: ?Sized + serde::Serialize>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<JsonValue, Self::Error> {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + serde::Serialize>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<JsonValue, Self::Error> {
        Ok(serde_json::json!({ variant: value.serialize(ValueSerializer)? }))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(ValueSeq(vec![]))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(ValueSeq(vec![]))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(ValueSeq(vec![]))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(ValueTupleVariant {
            variant,
            elements: vec![],
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(ValueMap {
            entries: vec![],
            next_key: None,
        })
    }

    fn serialize_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(ValueStruct {
            fields: serde_json::Map::new(),
        })
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(ValueStructVariant {
            variant,
            fields: serde_json::Map::new(),
        })
    }
}

// ---------------------------------------------------------------------------
// State machines
// ---------------------------------------------------------------------------

/// Accumulates seq/tuple/tuple-struct elements into a JSON array.
struct ValueSeq(Vec<JsonValue>);

impl serde::ser::SerializeSeq for ValueSeq {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_element<T: ?Sized + serde::Serialize>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.0.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Array(self.0))
    }
}

impl serde::ser::SerializeTuple for ValueSeq {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_element<T: ?Sized + serde::Serialize>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl serde::ser::SerializeTupleStruct for ValueSeq {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_field<T: ?Sized + serde::Serialize>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        serde::ser::SerializeSeq::end(self)
    }
}

/// Accumulates tuple-variant elements into `{ name: { variant: [...] } }`.
struct ValueTupleVariant {
    variant: &'static str,
    elements: Vec<JsonValue>,
}

impl serde::ser::SerializeTupleVariant for ValueTupleVariant {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_field<T: ?Sized + serde::Serialize>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.elements.push(value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        Ok(serde_json::json!({ self.variant: self.elements }))
    }
}

/// Accumulates map entries, coercing keys to strings.
struct ValueMap {
    entries: Vec<(String, JsonValue)>,
    next_key: Option<JsonValue>,
}

impl serde::ser::SerializeMap for ValueMap {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_key<T: ?Sized + serde::Serialize>(
        &mut self,
        key: &T,
    ) -> Result<(), Self::Error> {
        self.next_key = Some(key.serialize(ValueSerializer)?);
        Ok(())
    }

    fn serialize_value<T: ?Sized + serde::Serialize>(
        &mut self,
        value: &T,
    ) -> Result<(), Self::Error> {
        let key = self
            .next_key
            .take()
            .expect("serialize_value called before serialize_key");
        self.entries.push((map_key_to_string(key)?, value.serialize(ValueSerializer)?));
        Ok(())
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        let mut object = serde_json::Map::new();
        for (key, value) in self.entries {
            object.insert(key, value);
        }
        Ok(JsonValue::Object(object))
    }
}

/// Coerces a serialized map key into a string.
///
/// Strings and byte strings pass through, numbers and booleans use
/// their textual form.
fn map_key_to_string(key: JsonValue) -> Result<String, serde_json::Error> {
    match key {
        JsonValue::String(s) => Ok(s),
        JsonValue::Number(n) => Ok(n.to_string()),
        JsonValue::Bool(b) => Ok(b.to_string()),
        _ => Err(serde::ser::Error::custom("map key must be a string")),
    }
}

/// Accumulates struct fields into a JSON object.
struct ValueStruct {
    fields: serde_json::Map<String, JsonValue>,
}

impl serde::ser::SerializeStruct for ValueStruct {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_field<T: ?Sized + serde::Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.fields
            .insert(key.to_string(), value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        Ok(JsonValue::Object(self.fields))
    }
}

/// Accumulates struct-variant fields into `{ name: { variant: {...} } }`.
struct ValueStructVariant {
    variant: &'static str,
    fields: serde_json::Map<String, JsonValue>,
}

impl serde::ser::SerializeStructVariant for ValueStructVariant {
    type Ok = JsonValue;
    type Error = serde_json::Error;

    fn serialize_field<T: ?Sized + serde::Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.fields
            .insert(key.to_string(), value.serialize(ValueSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<JsonValue, Self::Error> {
        let mut object = serde_json::Map::new();
        object.insert(self.variant.to_string(), JsonValue::Object(self.fields));
        Ok(JsonValue::Object(object))
    }
}
