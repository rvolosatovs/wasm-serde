#![allow(unused)]

mod bindings {
    use super::Component;

    wit_bindgen::generate!();

    export!(Component);
}

use core::cell::RefCell;
use core::fmt;
use core::ops::{Deref, DerefMut};

use std::collections::VecDeque;
use std::io::Cursor;

use serde_core::Deserializer as _;
use serde_core::de::{self, Error as _};
use serde_json::de::IoRead;

use bindings::exports::rvolosatovs::serde::deserializer;
use bindings::exports::rvolosatovs::serde::deserializer::Payload;

struct Component;

impl deserializer::Guest for Component {
    type Deserializer = Deserializer;
    type Error = serde_json::Error;
}

impl deserializer::GuestError for serde_json::Error {
    fn to_string(&self) -> String {
        ToString::to_string(self)
    }
}

struct Deserializer {
    source: RefCell<serde_json::Deserializer<IoRead<Cursor<Vec<u8>>>>>,
    current: RefCell<Option<serde_json::Value>>,
    map: RefCell<Vec<VecDeque<(Option<serde_json::Value>, serde_json::Value)>>>,
}

impl deserializer::GuestDeserializer for Deserializer {
    fn from_list(buf: Vec<u8>) -> deserializer::Deserializer {
        deserializer::Deserializer::new(Deserializer {
            source: RefCell::new(serde_json::Deserializer::from_reader(Cursor::new(buf))),
            current: RefCell::default(),
            map: RefCell::default(),
        })
    }

    fn deserialize_u8(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_s8(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_u16(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_s16(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_u32(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_s32(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_u64(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_s64(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_f32(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_f64(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_char(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_string(&self) -> Result<Payload, deserializer::Error> {
        if let Some(v) = self.current.take() {
            let serde_json::Value::String(s) = v else {
                return Err(deserializer::Error::new(serde_json::Error::custom(
                    "invalid type",
                )));
            };
            Ok(Payload::ValString(s))
        } else {
            let s = self
                .source
                .borrow_mut()
                .deserialize_string(StringVisitor)
                .map_err(deserializer::Error::new)?;
            Ok(Payload::ValString(s))
        }
    }

    fn deserialize_record(&self, fields: Vec<String>) -> Result<Payload, deserializer::Error> {
        // name and fields are not used by `serde_json`
        self.source
            .borrow_mut()
            .deserialize_struct(
                "",
                &[],
                RecordVisitor {
                    fields: fields.as_slice(),
                    de: self,
                },
            )
            .map_err(deserializer::Error::new)
    }

    fn deserialize_variant(&self, cases: Vec<String>) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_list(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_tuple(&self, n: u32) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_flags(&self, cases: Vec<String>) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_enum(&self, cases: Vec<String>) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_option(&self) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn deserialize_result(&self, ok: bool, err: bool) -> Result<Payload, deserializer::Error> {
        todo!()
    }

    fn next_element(&self) -> bool {
        todo!()
    }

    fn next_key(&self) -> bool {
        let mut map = self.map.borrow_mut();
        let Some(mut entries) = map.pop() else {
            return false;
        };
        loop {
            let Some((k, v)) = entries.pop_back() else {
                return false;
            };
            let Some(k) = k else {
                continue;
            };
            *self.current.borrow_mut() = Some(k);
            entries.push_back((None, v));
            map.push(entries);
            return true;
        }
        return false;
    }

    fn next_value(&self) -> bool {
        let mut map = self.map.borrow_mut();
        let Some(mut entries) = map.pop() else {
            return false;
        };
        let Some((.., v)) = entries.pop_back() else {
            return false;
        };
        *self.current.borrow_mut() = Some(v);
        map.push(entries);
        return true;
    }
}

struct RecordVisitor<'a> {
    fields: &'a [String],
    de: &'a Deserializer,
}

impl<'de> de::Visitor<'de> for RecordVisitor<'_> {
    type Value = Payload;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a record")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let mut entries = if let Some(n) = map.size_hint() {
            VecDeque::with_capacity(n)
        } else {
            VecDeque::default()
        };
        while let Some((k, v)) = map.next_entry()? {
            entries.push_back(((Some(k), v)))
        }
        self.de.map.borrow_mut().push(entries);
        Ok(Payload::ValMap)
    }
}

struct StringVisitor;

impl<'de> de::Visitor<'de> for StringVisitor {
    type Value = String;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v.into())
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v)
    }
}
