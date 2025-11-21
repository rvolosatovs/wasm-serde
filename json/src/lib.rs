use core::fmt;
use core::iter::zip;
use core::ops::Deref;

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Cursor;
use std::rc::Rc;

use serde::de;
use serde::de::value::{MapDeserializer, SeqDeserializer};
use serde::de::{Error as _, VariantAccess as _};
use serde::{Deserialize, Deserializer as _};
use serde_json::de::IoRead;
use wasm_serde::bindings::exports::rvolosatovs::serde::deserializer::{
    GuestListDeserializer, GuestRecordDeserializer, GuestTupleDeserializer,
};
use wasm_serde::bindings::exports::rvolosatovs::serde::reflect::{
    EnumTypeBorrow, FlagsTypeBorrow, GuestEnumType, GuestFlagsType, GuestListType, GuestOptionType,
    GuestRecordType, GuestResultType, GuestTupleType, GuestVariantType, RecordTypeBorrow,
    TupleTypeBorrow, VariantTypeBorrow,
};
use wasm_serde::bindings::exports::rvolosatovs::serde::{deserializer, reflect};

struct Component;

wasm_serde::bindings::export!(Component with_types_in wasm_serde::bindings);

impl reflect::Guest for Component {
    type RecordType = RecordType;
    type VariantType = VariantType;
    type ListType = ListType;
    type TupleType = TupleType;
    type FlagsType = FlagsType;
    type EnumType = EnumType;
    type OptionType = OptionType;
    type ResultType = ResultType;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum Type {
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    F32,
    F64,
    Char,
    Bytes,
    String,
    Record(RecordType),
    Variant(VariantType),
    List(Box<ListType>),
    Tuple(TupleType),
    Flags(FlagsType),
    Enum(EnumType),
    Option(Box<OptionType>),
    Result(Box<ResultType>),
}

impl From<reflect::Type<'_>> for Type {
    fn from(ty: reflect::Type<'_>) -> Self {
        match ty {
            reflect::Type::Bool => Self::Bool,
            reflect::Type::U8 => Self::U8,
            reflect::Type::S8 => Self::S8,
            reflect::Type::U16 => Self::U16,
            reflect::Type::S16 => Self::S16,
            reflect::Type::U32 => Self::U32,
            reflect::Type::S32 => Self::S32,
            reflect::Type::U64 => Self::U64,
            reflect::Type::S64 => Self::S64,
            reflect::Type::F32 => Self::F32,
            reflect::Type::F64 => Self::F64,
            reflect::Type::Char => Self::Char,
            reflect::Type::Bytes => Self::Bytes,
            reflect::Type::String => Self::String,
            reflect::Type::Record(ty) => Self::Record(ty.get::<RecordType>().clone()),
            reflect::Type::Variant(ty) => Self::Variant(ty.get::<VariantType>().clone()),
            reflect::Type::List(ty) => Self::List(Box::new(ty.get::<ListType>().clone())),
            reflect::Type::Tuple(ty) => Self::Tuple(ty.get::<TupleType>().clone()),
            reflect::Type::Flags(ty) => Self::Flags(ty.get::<FlagsType>().clone()),
            reflect::Type::Enum(ty) => Self::Enum(ty.get::<EnumType>().clone()),
            reflect::Type::Option(ty) => Self::Option(Box::new(ty.get::<OptionType>().clone())),
            reflect::Type::Result(ty) => Self::Result(Box::new(ty.get::<ResultType>().clone())),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct RecordType(Rc<[(Box<str>, Type)]>);

impl Deref for RecordType {
    type Target = Rc<[(Box<str>, Type)]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestRecordType for RecordType {
    fn new(fields: Vec<(String, reflect::Type<'_>)>) -> Self {
        let fields = fields
            .into_iter()
            .map(|(name, ty)| (name.into(), ty.into()))
            .collect();
        Self(fields)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct VariantType(Rc<[(Box<str>, Option<Type>)]>);

impl GuestVariantType for VariantType {
    fn new(cases: Vec<(String, Option<reflect::Type<'_>>)>) -> Self {
        let cases = cases
            .into_iter()
            .map(|(name, ty)| (name.into(), ty.map(Into::into)))
            .collect();
        Self(cases)
    }
}

impl Deref for VariantType {
    type Target = Rc<[(Box<str>, Option<Type>)]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct ListType(Type);

impl GuestListType for ListType {
    fn new(ty: reflect::Type<'_>) -> Self {
        Self(ty.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct TupleType(Rc<[Type]>);

impl Deref for TupleType {
    type Target = Rc<[Type]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestTupleType for TupleType {
    fn new(types: Vec<reflect::Type<'_>>) -> Self {
        let types = types.into_iter().map(Into::into).collect();
        Self(types)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct FlagsType(Rc<[Box<str>]>);

impl Deref for FlagsType {
    type Target = Rc<[Box<str>]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestFlagsType for FlagsType {
    fn new(cases: Vec<String>) -> Self {
        let cases = cases.into_iter().map(Into::into).collect();
        Self(cases)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct EnumType(Rc<[Box<str>]>);

impl Deref for EnumType {
    type Target = Rc<[Box<str>]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestEnumType for EnumType {
    fn new(cases: Vec<String>) -> Self {
        let cases = cases.into_iter().map(Into::into).collect();
        Self(cases)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct OptionType(Type);

impl GuestOptionType for OptionType {
    fn new(ty: reflect::Type<'_>) -> Self {
        Self(ty.into())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct ResultType {
    ok: Option<Type>,
    err: Option<Type>,
}

impl GuestResultType for ResultType {
    fn new(ok: Option<reflect::Type<'_>>, err: Option<reflect::Type<'_>>) -> Self {
        let ok = ok.map(Into::into);
        let err = err.map(Into::into);
        Self { ok, err }
    }
}

#[repr(transparent)]
struct Error(serde_json::Error);

impl From<Error> for deserializer::Error {
    #[inline]
    fn from(err: Error) -> Self {
        deserializer::Error::new(err)
    }
}

impl From<Error> for serde_json::Error {
    #[inline]
    fn from(Error(err): Error) -> Self {
        err
    }
}

impl From<serde_json::Error> for Error {
    #[inline]
    fn from(err: serde_json::Error) -> Self {
        Self(err)
    }
}

impl Error {
    #[inline]
    fn wrap(err: impl Into<Error>) -> deserializer::Error {
        err.into().into()
    }
}

impl deserializer::Guest for Component {
    type Error = Error;
    type Deserializer = Deserializer;
    type RecordDeserializer = RecordDeserializer;
    type TupleDeserializer = TupleDeserializer;
    type ListDeserializer = ListDeserializer;
}

impl deserializer::GuestError for Error {
    fn to_string(&self) -> String {
        ToString::to_string(&self.0)
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
    Error::wrap(err)
}

enum Deserializer {
    Source(serde_json::Deserializer<IoRead<Cursor<Vec<u8>>>>),
    Value(serde_json::Value),
    Empty,
}

impl Deserializer {
    fn deserialize<'a, T: Deserialize<'a>>(self) -> Result<T, deserializer::Error> {
        match self {
            Self::Source(mut src) => T::deserialize(&mut src),
            Self::Value(v) => T::deserialize(v),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
    }

    fn deserialize_visitor<'a, T: de::Visitor<'a>>(
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
                    Err(serde_json::Error::custom("failed to classify number"))
                }
            }
            Self::Value(serde_json::Value::String(v)) => {
                visitor.visit_string::<serde_json::Error>(v)
            }
            Self::Value(serde_json::Value::Array(vs)) => {
                visitor.visit_seq(SeqDeserializer::new(vs.into_iter().rev()))
            }
            Self::Value(serde_json::Value::Object(obj)) => {
                visitor.visit_map(MapDeserializer::new(obj.into_iter()))
            }
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
    }
}

impl deserializer::GuestDeserializer for Deserializer {
    fn from_list(buf: Vec<u8>) -> deserializer::Deserializer {
        deserializer::Deserializer::new(Deserializer::Source(
            serde_json::Deserializer::from_reader(Cursor::new(buf)),
        ))
    }

    fn deserialize_bool(this: deserializer::Deserializer) -> Result<bool, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_u8(this: deserializer::Deserializer) -> Result<u8, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_s8(this: deserializer::Deserializer) -> Result<i8, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_u16(this: deserializer::Deserializer) -> Result<u16, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_s16(this: deserializer::Deserializer) -> Result<i16, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_u32(this: deserializer::Deserializer) -> Result<u32, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_s32(this: deserializer::Deserializer) -> Result<i32, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_u64(this: deserializer::Deserializer) -> Result<u64, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_s64(this: deserializer::Deserializer) -> Result<i64, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_f32(this: deserializer::Deserializer) -> Result<f32, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_f64(this: deserializer::Deserializer) -> Result<f64, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_char(this: deserializer::Deserializer) -> Result<char, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_bytes(this: deserializer::Deserializer) -> Result<Vec<u8>, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize::<serde_bytes::ByteBuf>()
            .map(serde_bytes::ByteBuf::into_vec)
    }

    fn deserialize_string(this: deserializer::Deserializer) -> Result<String, deserializer::Error> {
        this.into_inner::<Self>().deserialize()
    }

    fn deserialize_record(
        this: deserializer::Deserializer,
        ty: RecordTypeBorrow<'_>,
    ) -> Result<
        (
            u32,
            deserializer::Deserializer,
            deserializer::RecordDeserializer,
        ),
        deserializer::Error,
    > {
        let ty = ty.get();
        match this.into_inner::<Self>() {
            Self::Source(mut src) => {
                let mut vs = src
                    .deserialize_struct("", &[], RecordVisitor(ty))
                    .map_err(Error::wrap)?;
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
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
                if vs.len() != ty.len() {
                    return Err(Error::wrap(serde_json::Error::invalid_length(
                        vs.len(),
                        &RecordVisitor(ty),
                    )));
                }
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
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
                let mut vs = Vec::with_capacity(ty.len());
                for (name, _) in ty.iter().rev() {
                    let v = obj.remove(name.as_ref()).ok_or_else(|| {
                        Error::wrap(serde_json::Error::custom(format!(
                            "field `{name}` not found"
                        )))
                    })?;
                    vs.push(v);
                }
                if !obj.is_empty() {
                    return Err(Error::wrap(serde_json::Error::invalid_length(
                        obj.len().saturating_add(vs.len()),
                        &RecordVisitor(ty),
                    )));
                }
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: vs,
                    }),
                ))
            }
            Self::Value(v) => Err(invalid_type(v, &RecordVisitor(ty))),
            Self::Empty => Err(Error::wrap(serde_json::Error::custom("empty deserializer"))),
        }
    }

    fn deserialize_variant(
        this: deserializer::Deserializer,
        ty: VariantTypeBorrow<'_>,
    ) -> Result<(u32, deserializer::Deserializer), deserializer::Error> {
        let ty = ty.get();
        let (idx, v) = this
            .into_inner::<Self>()
            .deserialize_visitor(VariantVisitor(ty), |mut src, visitor| {
                src.deserialize_enum("", &[], visitor)
            })?;
        let de = v.map(Deserializer::Value).unwrap_or(Deserializer::Empty);
        Ok((idx, deserializer::Deserializer::new(de)))
    }

    fn deserialize_list(
        this: deserializer::Deserializer,
        _ty: reflect::Type<'_>,
    ) -> Result<deserializer::ListDeserializer, deserializer::Error> {
        let mut vs: Vec<serde_json::Value> = this.into_inner::<Self>().deserialize()?;
        vs.reverse();
        Ok(deserializer::ListDeserializer::new(ListDeserializer(vs)))
    }

    fn deserialize_tuple(
        this: deserializer::Deserializer,
        ty: TupleTypeBorrow<'_>,
    ) -> Result<(deserializer::Deserializer, deserializer::TupleDeserializer), deserializer::Error>
    {
        let ty = ty.get::<TupleType>();
        match this.into_inner::<Self>() {
            Self::Source(mut src) => {
                let mut vs = src
                    .deserialize_tuple(ty.len(), TupleVisitor(ty))
                    .map_err(Error::wrap)?;
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no elements found")))?;
                Ok((
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer(vs)),
                ))
            }
            Self::Value(serde_json::Value::Array(mut vs)) => {
                if vs.len() != ty.len() {
                    return Err(Error::wrap(serde_json::Error::custom(format!(
                        "expected {} elements, got {}",
                        ty.len(),
                        vs.len()
                    ))));
                }
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
                Ok((
                    deserializer::Deserializer::new(Deserializer::Value(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer(vs)),
                ))
            }
            Self::Value(v) => Err(invalid_type(v, &TupleVisitor(ty))),
            Self::Empty => Err(Error::wrap(serde_json::Error::custom("empty deserializer"))),
        }
    }

    fn deserialize_flags(
        this: deserializer::Deserializer,
        ty: FlagsTypeBorrow<'_>,
    ) -> Result<u32, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_visitor(FlagsVisitor(ty.get()), |mut src, visitor| {
                src.deserialize_struct("", &[], visitor)
            })
    }

    fn deserialize_enum(
        this: deserializer::Deserializer,
        ty: EnumTypeBorrow<'_>,
    ) -> Result<u32, deserializer::Error> {
        this.into_inner::<Self>()
            .deserialize_visitor(EnumVisitor(ty.get()), |mut src, visitor| {
                src.deserialize_enum("", &[], visitor)
            })
    }

    fn deserialize_option(
        this: deserializer::Deserializer,
        _ty: reflect::Type<'_>,
    ) -> Result<Option<deserializer::Deserializer>, deserializer::Error> {
        let v: Option<serde_json::Value> = this.into_inner::<Self>().deserialize()?;
        Ok(v.map(|v| deserializer::Deserializer::new(Deserializer::Value(v))))
    }

    fn deserialize_result(
        this: deserializer::Deserializer,
        ok: Option<reflect::Type<'_>>,
        err: Option<reflect::Type<'_>>,
    ) -> Result<Result<deserializer::Deserializer, deserializer::Deserializer>, deserializer::Error>
    {
        match (ok.is_some(), err.is_some()) {
            (true, true) => {
                #[derive(Deserialize)]
                #[serde(rename_all = "lowercase")]
                enum Result {
                    Ok(serde_json::Value),
                    Err(serde_json::Value),
                }
                match this.into_inner::<Self>().deserialize::<Result>()? {
                    Result::Ok(v) => {
                        Ok(Ok(deserializer::Deserializer::new(Deserializer::Value(v))))
                    }
                    Result::Err(v) => {
                        Ok(Err(deserializer::Deserializer::new(Deserializer::Value(v))))
                    }
                }
            }
            (true, false) => {
                #[derive(Deserialize)]
                #[serde(rename_all = "lowercase")]
                enum Result {
                    Ok(serde_json::Value),
                    Err,
                }
                match this.into_inner::<Self>().deserialize::<Result>()? {
                    Result::Ok(v) => {
                        Ok(Ok(deserializer::Deserializer::new(Deserializer::Value(v))))
                    }
                    Result::Err => Ok(Err(deserializer::Deserializer::new(Deserializer::Empty))),
                }
            }
            (false, true) => {
                #[derive(Deserialize)]
                #[serde(rename_all = "lowercase")]
                enum Result {
                    Ok,
                    Err(serde_json::Value),
                }
                match this.into_inner::<Self>().deserialize::<Result>()? {
                    Result::Ok => Ok(Ok(deserializer::Deserializer::new(Deserializer::Empty))),
                    Result::Err(v) => {
                        Ok(Err(deserializer::Deserializer::new(Deserializer::Value(v))))
                    }
                }
            }
            (false, false) => {
                #[derive(Deserialize)]
                #[serde(rename_all = "lowercase")]
                enum Result {
                    Ok,
                    Err,
                }
                match this.into_inner::<Self>().deserialize::<Result>()? {
                    Result::Ok => Ok(Ok(deserializer::Deserializer::new(Deserializer::Empty))),
                    Result::Err => Ok(Err(deserializer::Deserializer::new(Deserializer::Empty))),
                }
            }
        }
    }
}

struct RecordDeserializer {
    idx: u32,
    fields: Vec<serde_json::Value>,
}

impl RecordDeserializer {
    fn next(&mut self) -> serde_json::Result<(u32, serde_json::Value)> {
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

struct ListDeserializer(Vec<serde_json::Value>);

impl GuestListDeserializer for ListDeserializer {
    fn next(
        this: deserializer::ListDeserializer,
    ) -> Option<(deserializer::Deserializer, deserializer::ListDeserializer)> {
        let mut this = this.into_inner::<Self>();
        this.0.pop().map(|v| {
            (
                deserializer::Deserializer::new(Deserializer::Value(v)),
                deserializer::ListDeserializer::new(this),
            )
        })
    }
}

struct TupleVisitor<'a>(&'a TupleType);

impl<'de> de::Visitor<'de> for TupleVisitor<'_> {
    type Value = Vec<serde_json::Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a tuple of type {:?}", self.0.0)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(self.0.len());
        while let Some(v) = seq.next_element()? {
            values.push(v)
        }
        if values.len() != self.0.len() {
            return Err(A::Error::invalid_length(values.len(), &self));
        }
        Ok(values)
    }
}

struct RecordVisitor<'a>(&'a RecordType);

impl<'de> de::Visitor<'de> for RecordVisitor<'_> {
    type Value = Vec<serde_json::Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a record with fields: {:?}", self.0.0)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(self.0.len());
        while let Some(v) = seq.next_element()? {
            values.push(v);
        }
        if values.len() != self.0.len() {
            return Err(A::Error::invalid_length(
                values.len().saturating_sub(values.len()),
                &self,
            ));
        }
        Ok(values)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let mut values = vec![serde_json::Value::Null; self.0.len()];
        let mut indexes: HashMap<_, _> = zip(self.0.iter().map(|(k, _)| k.as_ref()), 0..).collect();
        while let Some((k, v)) = map.next_entry::<Cow<str>, _>()? {
            let idx = indexes.remove(k.as_ref()).ok_or_else(|| {
                A::Error::custom(format!("unknown field `{k}` from `{indexes:?}`"))
            })?;
            values[self.0.len() - 1 - idx] = v;
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

struct VariantVisitor<'a>(&'a VariantType);

impl<'de> de::Visitor<'de> for VariantVisitor<'_> {
    type Value = (u32, Option<serde_json::Value>);

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a variant with cases: {:?}", self.0.0)
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        for (idx, (name, payload)) in zip(0.., self.0.as_ref()) {
            if v == name.as_ref() {
                if payload.is_some() {
                    return Err(E::custom("missing payload"));
                }
                return Ok((idx, None));
            }
        }
        let expected: Vec<_> = self.0.iter().map(|(k, _)| k).collect();
        Err(E::custom(format!(
            "unknown variant case `{v}`, expected one of `{expected:?}`"
        )))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let (Some((k, v)), None) = (
            map.next_entry::<Cow<str>, serde_json::Value>()?,
            map.next_entry::<Cow<str>, serde_json::Value>()?,
        ) else {
            return Err(A::Error::custom("the object must have exactly one field"));
        };
        for (idx, (name, payload)) in zip(0.., self.0.as_ref()) {
            if k == name.as_ref() {
                let v = if payload.is_some() {
                    Some(v)
                } else {
                    if !v.is_null() {
                        return Err(A::Error::custom("unexpected payload received"));
                    }
                    None
                };
                return Ok((idx, v));
            }
        }
        let expected: Vec<_> = self.0.iter().map(|(k, _)| k).collect();
        Err(A::Error::custom(format!(
            "unknown variant case `{k}`, expected one of `{expected:?}`"
        )))
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: de::EnumAccess<'de>,
    {
        let (k, v): (Cow<str>, _) = data.variant()?;
        for (idx, (name, payload)) in zip(0.., self.0.as_ref()) {
            if k == name.as_ref() {
                let v = if payload.is_some() {
                    let v = v.newtype_variant()?;
                    Some(v)
                } else {
                    v.unit_variant()?;
                    None
                };
                return Ok((idx, v));
            }
        }
        let expected: Vec<_> = self.0.iter().map(|(k, _)| k).collect();
        Err(A::Error::custom(format!(
            "unknown variant case `{k}`, expected one of `{expected:?}`"
        )))
    }
}

struct EnumVisitor<'a>(&'a EnumType);

impl<'de> de::Visitor<'de> for EnumVisitor<'_> {
    type Value = u32;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "an enum with cases: {:?}", self.0.0)
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        for (idx, name) in zip(0.., self.0.as_ref()) {
            if v == name.as_ref() {
                return Ok(idx);
            }
        }
        Err(E::custom(format!(
            "unknown enum case `{v}`, expected one of `{:?}`",
            self.0.0
        )))
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let (Some((k, v)), None) = (
            map.next_entry::<Cow<str>, serde_json::Value>()?,
            map.next_entry::<Cow<str>, serde_json::Value>()?,
        ) else {
            return Err(A::Error::custom("the object must have exactly one field"));
        };
        for (idx, name) in zip(0.., self.0.as_ref()) {
            if k == name.as_ref() {
                if !v.is_null() {
                    return Err(A::Error::custom("unexpected payload received"));
                }
                return Ok(idx);
            }
        }
        Err(A::Error::custom(format!(
            "unknown enum case `{v}`, expected one of `{:?}`",
            self.0.0
        )))
    }

    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: de::EnumAccess<'de>,
    {
        let (k, v): (Cow<str>, _) = data.variant()?;
        for (idx, name) in zip(0.., self.0.as_ref()) {
            if k == name.as_ref() {
                v.unit_variant()?;
                return Ok(idx);
            }
        }
        Err(A::Error::custom(format!(
            "unknown enum case `{k}`, expected one of `{:?}`",
            self.0.0
        )))
    }
}

struct FlagsVisitor<'a>(&'a FlagsType);

impl<'de> de::Visitor<'de> for FlagsVisitor<'_> {
    type Value = u32;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a flags with cases: {:?}", self.0.0)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        debug_assert!(self.0.len() <= 32, "flags may contain at most 32 cases");
        let mut flags = 0;
        let mut i = 0u8;
        while let Some(v) = seq.next_element()? {
            if usize::from(i) == self.0.len() {
                return Err(A::Error::custom(format!(
                    "sequence must contain exactly {} elements",
                    self.0.len()
                )));
            }
            if v {
                flags |= 1 << i;
            }
            i += 1;
        }
        Ok(flags)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        debug_assert!(self.0.len() <= 32, "flags may contain at most 32 cases");
        let mut flags = 0;
        let mut cases: HashMap<&str, u8> = zip(self.0.iter().map(|s| s.as_ref()), 0..).collect();
        while let Some((k, v)) = map.next_entry::<Cow<str>, _>()? {
            let i = cases
                .remove(k.as_ref())
                .ok_or_else(|| A::Error::custom(format!("unknown case `{k}` received")))?;
            if v {
                flags |= 1 << i;
            }
        }
        Ok(flags)
    }
}
