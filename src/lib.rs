mod bindings;
mod num;

use core::fmt;
use core::iter::zip;

use std::collections::HashMap;
use std::io::Cursor;

use serde_core::Deserializer as _;
use serde_core::de::{self, Error as _};
use serde_json::de::IoRead;

use crate::bindings::exports::rvolosatovs::serde::deserializer::{
    self, GuestRecordDeserializer, GuestSizedListDeserializer, GuestTupleDeserializer,
    GuestUnsizedListDeserializer, ListDeserializer,
};
use crate::num::{
    F32Visitor, F64Visitor, S8Visitor, S16Visitor, S32Visitor, S64Visitor, U8Visitor, U16Visitor,
    U32Visitor, U64Visitor,
};

struct Component;

impl deserializer::Guest for Component {
    type Error = serde_json::Error;
    type Deserializer = Deserializer;
    type RecordDeserializer = RecordDeserializer;
    type TupleDeserializer = TupleDeserializer;
    type SizedListDeserializer = SizedListDeserializer;
    type UnsizedListDeserializer = UnsizedListDeserializer;
}

impl deserializer::GuestError for serde_json::Error {
    fn to_string(&self) -> String {
        ToString::to_string(self)
    }
}

impl From<serde_json::Error> for deserializer::Error {
    fn from(err: serde_json::Error) -> Self {
        deserializer::Error::new(err)
    }
}

fn invalid_type(v: serde_json::Value, exp: &dyn de::Expected) -> deserializer::Error {
    let err = match v {
        serde_json::Value::Null => serde_json::Error::invalid_type(de::Unexpected::Unit, exp),
        serde_json::Value::Bool(v) => serde_json::Error::invalid_type(de::Unexpected::Bool(v), exp),
        serde_json::Value::Number(v) => {
            if let Some(v) = v.as_u64() {
                serde_json::Error::invalid_type(de::Unexpected::Unsigned(v), exp)
            } else if let Some(v) = v.as_i64() {
                serde_json::Error::invalid_type(de::Unexpected::Signed(v), exp)
            } else if let Some(v) = v.as_f64() {
                serde_json::Error::invalid_type(de::Unexpected::Float(v), exp)
            } else if let Some(v) = v.as_u128() {
                serde_json::Error::invalid_type(de::Unexpected::Other(&format!("{v} as u128")), exp)
            } else if let Some(v) = v.as_i128() {
                serde_json::Error::invalid_type(de::Unexpected::Other(&format!("{v} as i128")), exp)
            } else {
                serde_json::Error::invalid_type(
                    de::Unexpected::Other(&format!("{v} as unclassified")),
                    exp,
                )
            }
        }
        serde_json::Value::String(v) => {
            serde_json::Error::invalid_type(de::Unexpected::Str(&v), exp)
        }
        serde_json::Value::Array(..) => serde_json::Error::invalid_type(de::Unexpected::Seq, exp),
        serde_json::Value::Object(..) => serde_json::Error::invalid_type(de::Unexpected::Map, exp),
    };
    deserializer::Error::new(err)
}

enum Deserializer {
    Source(serde_json::Deserializer<IoRead<Cursor<Vec<u8>>>>),
    Value(serde_json::Value),
}

impl Deserializer {
    fn deserialize_value<'a, T: de::Visitor<'a>>(
        self,
        visitor: T,
        f: impl FnOnce(
            serde_json::Deserializer<IoRead<Cursor<Vec<u8>>>>,
            T,
        ) -> Result<T::Value, serde_json::Error>,
    ) -> Result<T::Value, deserializer::Error> {
        match self {
            Self::Source(source) => f(source, visitor),
            Self::Value(serde_json::Value::Null) => visitor.visit_unit::<serde_json::Error>(),
            Self::Value(serde_json::Value::Bool(v)) => visitor.visit_bool::<serde_json::Error>(v),
            Self::Value(serde_json::Value::Number(v)) => {
                if let Some(v) = v.as_u64() {
                    visitor.visit_u64::<serde_json::Error>(v)
                } else if let Some(v) = v.as_i64() {
                    visitor.visit_i64::<serde_json::Error>(v)
                } else if let Some(v) = v.as_f64() {
                    visitor.visit_f64::<serde_json::Error>(v)
                } else if let Some(v) = v.as_u128() {
                    visitor.visit_u128::<serde_json::Error>(v)
                } else if let Some(v) = v.as_i128() {
                    visitor.visit_i128::<serde_json::Error>(v)
                } else {
                    return Err(deserializer::Error::new(serde_json::Error::custom(
                        "failed to classify number",
                    )));
                }
            }
            Self::Value(serde_json::Value::String(v)) => {
                visitor.visit_string::<serde_json::Error>(v)
            }
            Self::Value(serde_json::Value::Array(_vs)) => todo!(),
            Self::Value(serde_json::Value::Object(_obj)) => todo!(),
        }
        .map_err(deserializer::Error::new)
    }
}

#[expect(unused, reason = "incomplete")]
impl deserializer::GuestDeserializer for Deserializer {
    fn from_list(buf: Vec<u8>) -> deserializer::Deserializer {
        deserializer::Deserializer::new(Deserializer::Source(
            serde_json::Deserializer::from_reader(Cursor::new(buf)),
        ))
    }

    fn deserialize_bool(this: deserializer::Deserializer) -> Result<bool, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(BoolVisitor, |mut src, visitor| {
                src.deserialize_bool(visitor)
            })
    }

    fn deserialize_u8(this: deserializer::Deserializer) -> Result<u8, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(U8Visitor, |mut src, visitor| src.deserialize_u8(visitor))
    }

    fn deserialize_s8(this: deserializer::Deserializer) -> Result<i8, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(S8Visitor, |mut src, visitor| src.deserialize_i8(visitor))
    }

    fn deserialize_u16(this: deserializer::Deserializer) -> Result<u16, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(U16Visitor, |mut src, visitor| src.deserialize_u16(visitor))
    }

    fn deserialize_s16(this: deserializer::Deserializer) -> Result<i16, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(S16Visitor, |mut src, visitor| src.deserialize_i16(visitor))
    }

    fn deserialize_u32(this: deserializer::Deserializer) -> Result<u32, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(U32Visitor, |mut src, visitor| src.deserialize_u32(visitor))
    }

    fn deserialize_s32(this: deserializer::Deserializer) -> Result<i32, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(S32Visitor, |mut src, visitor| src.deserialize_i32(visitor))
    }

    fn deserialize_u64(this: deserializer::Deserializer) -> Result<u64, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(U64Visitor, |mut src, visitor| src.deserialize_u64(visitor))
    }

    fn deserialize_s64(this: deserializer::Deserializer) -> Result<i64, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(S64Visitor, |mut src, visitor| src.deserialize_i64(visitor))
    }

    fn deserialize_f32(this: deserializer::Deserializer) -> Result<f32, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(F32Visitor, |mut src, visitor| src.deserialize_f32(visitor))
    }

    fn deserialize_f64(this: deserializer::Deserializer) -> Result<f64, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(F64Visitor, |mut src, visitor| src.deserialize_f64(visitor))
    }

    fn deserialize_char(this: deserializer::Deserializer) -> Result<char, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(CharVisitor, |mut src, visitor| {
                src.deserialize_char(visitor)
            })
    }

    fn deserialize_bytes(this: deserializer::Deserializer) -> Result<Vec<u8>, deserializer::Error> {
        todo!()
    }

    fn deserialize_string(this: deserializer::Deserializer) -> Result<String, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_value(StringVisitor, |mut src, visitor| {
                src.deserialize_string(visitor)
            })
    }

    fn deserialize_record(
        this: deserializer::Deserializer,
        fields: Vec<String>,
    ) -> Result<
        (
            u32,
            deserializer::Deserializer,
            deserializer::RecordDeserializer,
        ),
        deserializer::Error,
    > {
        match this.into_inner::<Self>() {
            Self::Source(mut src) => {
                let mut vs = src.deserialize_struct("", &[], RecordVisitor(fields))?;
                let v = vs.pop().ok_or_else(|| {
                    deserializer::Error::new(serde_json::Error::custom("no fields found"))
                })?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: vs,
                    }),
                ))
            }
            Self::Value(serde_json::Value::Array(mut vs)) => {
                if vs.len() != fields.len() {
                    return Err(deserializer::Error::new(serde_json::Error::custom(
                        format!("expected {} fields, got {}", fields.len(), vs.len()),
                    )));
                }
                vs.reverse();
                let v = vs.pop().ok_or_else(|| {
                    deserializer::Error::new(serde_json::Error::custom("no fields found"))
                })?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: vs,
                    }),
                ))
            }
            Self::Value(serde_json::Value::Object(mut obj)) => {
                let mut vs = Vec::with_capacity(fields.len());
                for name in fields.iter().rev() {
                    let v = obj.remove(name).ok_or_else(|| {
                        deserializer::Error::new(serde_json::Error::custom(format!(
                            "field `{name}` not found"
                        )))
                    })?;
                    vs.push(v);
                }
                if !obj.is_empty() {
                    return Err(deserializer::Error::new(serde_json::Error::invalid_length(
                        obj.len().saturating_add(fields.len()),
                        &RecordVisitor(fields),
                    )));
                }
                let v = vs.pop().ok_or_else(|| {
                    deserializer::Error::new(serde_json::Error::custom("no fields found"))
                })?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: vs,
                    }),
                ))
            }
            Self::Value(v) => Err(invalid_type(v, &RecordVisitor(fields))),
        }
    }

    fn deserialize_variant(
        this: deserializer::Deserializer,
        cases: Vec<(String, bool)>,
    ) -> Result<(u32, deserializer::Deserializer), deserializer::Error> {
        todo!()
    }

    fn deserialize_list(
        this: deserializer::Deserializer,
    ) -> Result<ListDeserializer, deserializer::Error> {
        match this.into_inner::<Self>() {
            Self::Source(_src) => {
                todo!()
            }
            Self::Value(serde_json::Value::Array(_vs)) => {
                todo!()
            }
            Self::Value(v) => Err(invalid_type(v, todo!())),
        }
    }

    fn deserialize_tuple(
        this: deserializer::Deserializer,
        n: u32,
    ) -> Result<(deserializer::Deserializer, deserializer::TupleDeserializer), deserializer::Error>
    {
        let n = n as _;
        match this.into_inner::<Self>() {
            Self::Source(mut src) => {
                let mut vs = src.deserialize_tuple(n, TupleVisitor(n))?;
                let v = vs.pop().ok_or_else(|| {
                    deserializer::Error::new(serde_json::Error::custom("no elements found"))
                })?;
                Ok((
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer(vs)),
                ))
            }
            Self::Value(serde_json::Value::Array(mut vs)) => {
                if vs.len() != n {
                    return Err(deserializer::Error::new(serde_json::Error::custom(
                        format!("expected {n} elements, got {}", vs.len()),
                    )));
                }
                vs.reverse();
                let v = vs.pop().ok_or_else(|| {
                    deserializer::Error::new(serde_json::Error::custom("no fields found"))
                })?;
                Ok((
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer(vs)),
                ))
            }
            Self::Value(v) => Err(invalid_type(v, &TupleVisitor(n))),
        }
    }

    fn deserialize_flags(
        this: deserializer::Deserializer,
        cases: Vec<String>,
    ) -> Result<u32, deserializer::Error> {
        todo!()
    }

    fn deserialize_enum(
        this: deserializer::Deserializer,
        cases: Vec<String>,
    ) -> Result<u32, deserializer::Error> {
        todo!()
    }

    fn deserialize_option(
        this: deserializer::Deserializer,
        payload: bool,
    ) -> Result<Option<deserializer::Deserializer>, deserializer::Error> {
        todo!()
    }

    fn deserialize_result(
        this: deserializer::Deserializer,
        ok: bool,
        err: bool,
    ) -> Result<Result<deserializer::Deserializer, deserializer::Deserializer>, deserializer::Error>
    {
        todo!()
    }
}

struct RecordDeserializer {
    idx: u32,
    fields: Vec<serde_json::Value>,
}

impl RecordDeserializer {
    pub fn next(&mut self) -> serde_json::Result<(u32, serde_json::Value)> {
        // only n-1 fields are stored in the vec
        self.idx = self
            .idx
            .checked_add(1)
            .ok_or_else(|| serde_json::Error::custom("too many fields"))?;
        let v = self
            .fields
            .pop()
            .ok_or_else(|| serde_json::Error::custom("no fields left to iterate"))?;
        Ok((self.idx, v))
    }
}

impl GuestRecordDeserializer for RecordDeserializer {
    fn next(
        this: deserializer::RecordDeserializer,
    ) -> (
        u32,
        deserializer::Deserializer,
        deserializer::RecordDeserializer,
    ) {
        let mut this = this.into_inner::<Self>();
        let (idx, v) = this.next().expect("failed to iterate");
        (
            idx,
            deserializer::Deserializer::new(Deserializer::Value(v)),
            deserializer::RecordDeserializer::new(this),
        )
    }
}

struct TupleDeserializer(Vec<serde_json::Value>);

impl GuestTupleDeserializer for TupleDeserializer {
    fn next(
        this: deserializer::TupleDeserializer,
    ) -> (deserializer::Deserializer, deserializer::TupleDeserializer) {
        let mut this = this.into_inner::<Self>();
        let v = this.0.pop().expect("no fields left to iterate");
        (
            deserializer::Deserializer::new(Deserializer::Value(v)),
            deserializer::TupleDeserializer::new(this),
        )
    }
}

struct SizedListDeserializer;

impl GuestSizedListDeserializer for SizedListDeserializer {
    fn next(
        _this: deserializer::SizedListDeserializer,
    ) -> (
        deserializer::Deserializer,
        deserializer::SizedListDeserializer,
    ) {
        todo!()
    }
}

struct UnsizedListDeserializer;

impl GuestUnsizedListDeserializer for UnsizedListDeserializer {
    fn next(
        _this: deserializer::UnsizedListDeserializer,
    ) -> Option<(
        deserializer::Deserializer,
        deserializer::UnsizedListDeserializer,
    )> {
        todo!()
    }
}

pub struct BoolVisitor;

impl<'de> de::Visitor<'de> for BoolVisitor {
    type Value = bool;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a bool")
    }

    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v)
    }
}

pub struct CharVisitor;

impl<'de> de::Visitor<'de> for CharVisitor {
    type Value = char;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a char")
    }

    fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(v.into())
    }

    fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        u32::from(v).try_into().map_err(E::custom)
    }

    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        v.try_into().map_err(E::custom)
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

    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
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

struct TupleVisitor(usize);

impl<'de> de::Visitor<'de> for TupleVisitor {
    type Value = Vec<serde_json::Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a tuple with {:?} elements", self.0)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(self.0);
        while let Some(v) = seq.next_element()? {
            values.push(v)
        }
        if values.len() != self.0 {
            return Err(A::Error::invalid_length(values.len(), &self));
        }
        Ok(values)
    }
}

struct RecordVisitor(Vec<String>);

impl<'de> de::Visitor<'de> for RecordVisitor {
    type Value = Vec<serde_json::Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a record with fields: {:?}", self.0)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        if u32::try_from(self.0.len().saturating_sub(1)).is_err() {
            return Err(A::Error::custom("record has too many fields"));
        };
        let n = self.0.len();
        let mut values = vec![serde_json::Value::Null; self.0.len()];
        let mut indexes: HashMap<_, _> = zip(self.0.iter().map(String::as_str), 0..).collect();
        while let Some((k, v)) = map.next_entry::<String, _>()? {
            let idx = indexes.remove(k.as_str()).ok_or_else(|| {
                A::Error::custom(format!("unknown field `{k}` from `{indexes:?}`"))
            })?;
            // NOTE: we have already verified that `self.0.len()` fits in u32
            values[n - 1 - idx] = v;
        }
        if !indexes.is_empty() {
            return Err(A::Error::invalid_length(
                values.len().saturating_sub(indexes.len()),
                &self,
            ));
        }
        Ok(values)
    }
}
