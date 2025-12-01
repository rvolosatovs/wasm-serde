#![allow(unused)] // TODO: Remove

use core::fmt;
use core::iter::zip;
use core::ops::{Deref, DerefMut};

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Cursor;
use std::rc::Rc;

use serde::de;
use serde::de::{DeserializeSeed as _, Error as _, IntoDeserializer as _, VariantAccess as _};
use serde::{Deserialize, Deserializer as _, Serializer as _};
use serde_json::de::IoRead;
use wasm_serde::bindings::exports::rvolosatovs::serde::deserializer::{
    GuestListDeserializer, GuestRecordDeserializer, GuestTupleDeserializer,
};
use wasm_serde::bindings::exports::rvolosatovs::serde::reflect::{
    EnumTypeBorrow, FlagsTypeBorrow, GuestEnumType, GuestFlagsType, GuestListType, GuestOptionType,
    GuestRecordType, GuestResultType, GuestTupleType, GuestVariantType, RecordTypeBorrow,
    TupleTypeBorrow, VariantTypeBorrow,
};
use wasm_serde::bindings::exports::rvolosatovs::serde::{deserializer, reflect, serializer};

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

macro_rules! impl_serde_deserializer {
    ($t:ty) => {
        impl<'de> de::Deserializer<'de> for $t {
            type Error = serde_json::Error;

            fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_unit_struct<V>(
                self,
                _name: &'static str,
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_newtype_struct<V>(
                self,
                _name: &'static str,
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_tuple_struct<V>(
                self,
                _name: &'static str,
                _len: usize,
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_struct<V>(
                self,
                _name: &'static str,
                _fields: &'static [&'static str],
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_enum<V>(
                self,
                _name: &'static str,
                _variants: &'static [&'static str],
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }

            fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: de::Visitor<'de>,
            {
                self.visit(visitor)
            }
        }

        impl<'de> de::IntoDeserializer<'de, serde_json::Error> for $t {
            type Deserializer = $t;

            #[inline]
            fn into_deserializer(self) -> Self::Deserializer {
                self
            }
        }
    };
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

fn deserialize_byte_buf<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
where
    D: de::Deserializer<'de>,
{
    serde_bytes::ByteBuf::deserialize(deserializer).map(serde_bytes::ByteBuf::into_vec)
}

fn deserialize_record<'de, D>(deserializer: D, ty: &RecordType) -> Result<Vec<Value>, D::Error>
where
    D: de::Deserializer<'de>,
{
    deserializer.deserialize_struct("", &[], RecordVisitor::new(ty))
}

fn deserialize_variant<'de, D>(
    deserializer: D,
    ty: &VariantType,
) -> Result<(u32, Option<Value>), D::Error>
where
    D: de::Deserializer<'de>,
{
    deserializer.deserialize_enum("", &[], VariantVisitor(ty))
}

fn deserialize_list<'de, D>(deserializer: D, ty: &ListType) -> Result<List, D::Error>
where
    D: de::Deserializer<'de>,
{
    use de::DeserializeSeed;
    ty.deserialize(deserializer)
}

fn deserialize_tuple<'de, D>(deserializer: D, ty: &TupleType) -> Result<Vec<Value>, D::Error>
where
    D: de::Deserializer<'de>,
{
    deserializer.deserialize_tuple(ty.len(), TupleVisitor(ty))
}

fn deserialize_flags<'de, D>(deserializer: D, ty: &FlagsType) -> Result<u32, D::Error>
where
    D: de::Deserializer<'de>,
{
    deserializer.deserialize_struct("", &[], FlagsVisitor(ty))
}

fn deserialize_enum<'de, D>(deserializer: D, ty: &EnumType) -> Result<u32, D::Error>
where
    D: de::Deserializer<'de>,
{
    deserializer.deserialize_enum("", &[], EnumVisitor(ty))
}

fn deserialize_option<'de, D>(deserializer: D, ty: &OptionType) -> Result<Option<Value>, D::Error>
where
    D: de::Deserializer<'de>,
{
    deserializer.deserialize_option(OptionVisitor(ty))
}

impl<'de> de::DeserializeSeed<'de> for &Type {
    type Value = Value;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        match self {
            Type::Bool => bool::deserialize(deserializer).map(Value::from),
            Type::U8 => u8::deserialize(deserializer).map(Value::from),
            Type::S8 => i8::deserialize(deserializer).map(Value::from),
            Type::U16 => u16::deserialize(deserializer).map(Value::from),
            Type::S16 => i16::deserialize(deserializer).map(Value::from),
            Type::U32 => u32::deserialize(deserializer).map(Value::from),
            Type::S32 => i32::deserialize(deserializer).map(Value::from),
            Type::U64 => u64::deserialize(deserializer).map(Value::from),
            Type::S64 => i64::deserialize(deserializer).map(Value::from),
            Type::F32 => f32::deserialize(deserializer).map(Value::from),
            Type::F64 => f64::deserialize(deserializer).map(Value::from),
            Type::Char => char::deserialize(deserializer).map(Value::from),
            Type::Bytes => deserialize_byte_buf(deserializer).map(Value::from),
            Type::List(ty) if **ty == ListType(Type::U8) => {
                deserialize_byte_buf(deserializer).map(Value::from)
            }
            Type::String => String::deserialize(deserializer).map(Value::from),
            Type::Record(ty) => deserialize_record(deserializer, ty).map(Value::Record),
            Type::Variant(ty) => deserialize_variant(deserializer, ty)
                .map(|(case, payload)| Value::Variant(case, payload.map(Box::new))),
            Type::List(ty) => deserialize_list(deserializer, ty).map(Value::List),
            Type::Tuple(ty) => deserialize_tuple(deserializer, ty).map(Value::Tuple),
            Type::Flags(ty) => deserialize_flags(deserializer, ty).map(Value::Flags),
            Type::Enum(ty) => deserialize_enum(deserializer, ty).map(Value::Enum),
            Type::Option(ty) => {
                deserialize_option(deserializer, ty).map(|v| Value::Option(v.map(Box::new)))
            }
            // TODO: optimize
            Type::Result(..) => serde_json::Value::deserialize(deserializer).map(Value::from),
        }
    }
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

impl Deref for ListType {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'de> de::DeserializeSeed<'de> for &ListType {
    type Value = List;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        match self.0 {
            Type::Bool => Vec::deserialize(deserializer).map(List::Bool),
            Type::S8 => Vec::deserialize(deserializer).map(List::S8),
            Type::U8 => deserialize_byte_buf(deserializer).map(List::U8),
            Type::S16 => Vec::deserialize(deserializer).map(List::S16),
            Type::U16 => Vec::deserialize(deserializer).map(List::U16),
            Type::S32 => Vec::deserialize(deserializer).map(List::S32),
            Type::U32 => Vec::deserialize(deserializer).map(List::U32),
            Type::S64 => Vec::deserialize(deserializer).map(List::S64),
            Type::U64 => Vec::deserialize(deserializer).map(List::U64),
            Type::F32 => Vec::deserialize(deserializer).map(List::F32),
            Type::F64 => Vec::deserialize(deserializer).map(List::F64),
            Type::Char => Vec::deserialize(deserializer).map(List::Char),
            Type::Bytes => Vec::deserialize(deserializer).map(List::Bytes),
            Type::String => Vec::deserialize(deserializer).map(List::String),
            _ => Vec::deserialize(deserializer).map(List::Raw),
        }
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

impl Deref for OptionType {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

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

#[derive(Clone, Debug, PartialEq)]
enum Primitive {
    Bool(bool),
    S8(i8),
    U8(u8),
    S16(i16),
    U16(u16),
    S32(i32),
    U32(u32),
    S64(i64),
    U64(u64),
    F32(f32),
    F64(f64),
    Char(char),
    String(String),
}

impl_serde_deserializer!(Primitive);

macro_rules! impl_primitive_from {
    ($t:ty, $c:ident) => {
        impl From<$t> for Primitive {
            #[inline]
            fn from(v: $t) -> Self {
                Self::$c(v)
            }
        }
    };
}

impl_primitive_from!(bool, Bool);
impl_primitive_from!(u8, U8);
impl_primitive_from!(u16, U16);
impl_primitive_from!(u32, U32);
impl_primitive_from!(u64, U64);
impl_primitive_from!(i8, S8);
impl_primitive_from!(i16, S16);
impl_primitive_from!(i32, S32);
impl_primitive_from!(i64, S64);
impl_primitive_from!(f32, F32);
impl_primitive_from!(f64, F64);
impl_primitive_from!(char, Char);
impl_primitive_from!(String, String);

impl Primitive {
    fn deserialize<'a, T: Deserialize<'a>, E: de::Error>(self) -> Result<T, E> {
        match self {
            Self::Bool(v) => T::deserialize(v.into_deserializer()),
            Self::U8(v) => T::deserialize(v.into_deserializer()),
            Self::S8(v) => T::deserialize(v.into_deserializer()),
            Self::U16(v) => T::deserialize(v.into_deserializer()),
            Self::S16(v) => T::deserialize(v.into_deserializer()),
            Self::U32(v) => T::deserialize(v.into_deserializer()),
            Self::S32(v) => T::deserialize(v.into_deserializer()),
            Self::U64(v) => T::deserialize(v.into_deserializer()),
            Self::S64(v) => T::deserialize(v.into_deserializer()),
            Self::F32(v) => T::deserialize(v.into_deserializer()),
            Self::F64(v) => T::deserialize(v.into_deserializer()),
            Self::Char(v) => T::deserialize(v.into_deserializer()),
            Self::String(v) => T::deserialize(v.into_deserializer()),
        }
    }

    fn invalid_type<E: de::Error>(self, exp: &dyn de::Expected) -> E {
        match self {
            Self::Bool(v) => E::invalid_type(de::Unexpected::Bool(v), exp),
            Self::U8(v) => E::invalid_type(de::Unexpected::Unsigned(v.into()), exp),
            Self::U16(v) => E::invalid_type(de::Unexpected::Unsigned(v.into()), exp),
            Self::U32(v) => E::invalid_type(de::Unexpected::Unsigned(v.into()), exp),
            Self::U64(v) => E::invalid_type(de::Unexpected::Unsigned(v), exp),
            Self::S8(v) => E::invalid_type(de::Unexpected::Signed(v.into()), exp),
            Self::S16(v) => E::invalid_type(de::Unexpected::Signed(v.into()), exp),
            Self::S32(v) => E::invalid_type(de::Unexpected::Signed(v.into()), exp),
            Self::S64(v) => E::invalid_type(de::Unexpected::Signed(v), exp),
            Self::F32(v) => E::invalid_type(de::Unexpected::Float(v.into()), exp),
            Self::F64(v) => E::invalid_type(de::Unexpected::Float(v), exp),
            Self::Char(v) => E::invalid_type(de::Unexpected::Char(v), exp),
            Self::String(v) => E::invalid_type(de::Unexpected::Str(&v), exp),
        }
    }

    fn visit<'a, T: de::Visitor<'a>, E: de::Error>(self, visitor: T) -> Result<T::Value, E> {
        match self {
            Self::Bool(v) => visitor.visit_bool(v),
            Self::U8(v) => visitor.visit_u8(v),
            Self::S8(v) => visitor.visit_i8(v),
            Self::U16(v) => visitor.visit_u16(v),
            Self::S16(v) => visitor.visit_i16(v),
            Self::U32(v) => visitor.visit_u32(v),
            Self::S32(v) => visitor.visit_i32(v),
            Self::U64(v) => visitor.visit_u64(v),
            Self::S64(v) => visitor.visit_i64(v),
            Self::F32(v) => visitor.visit_f32(v),
            Self::F64(v) => visitor.visit_f64(v),
            Self::Char(v) => visitor.visit_char(v),
            Self::String(v) => visitor.visit_string(v),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum List {
    Bool(Vec<bool>),
    U8(Vec<u8>),
    S8(Vec<i8>),
    U16(Vec<u16>),
    S16(Vec<i16>),
    U32(Vec<u32>),
    S32(Vec<i32>),
    U64(Vec<u64>),
    S64(Vec<i64>),
    F32(Vec<f32>),
    F64(Vec<f64>),
    Char(Vec<char>),
    Bytes(Vec<Vec<u8>>),
    String(Vec<String>),
    Raw(Vec<serde_json::Value>),
}

impl_serde_deserializer!(List);

macro_rules! impl_list_from {
    ($t:ty, $c:ident) => {
        impl From<Vec<$t>> for List {
            #[inline]
            fn from(v: Vec<$t>) -> Self {
                Self::$c(v)
            }
        }
    };
}

impl_list_from!(bool, Bool);
impl_list_from!(u8, U8);
impl_list_from!(u16, U16);
impl_list_from!(u32, U32);
impl_list_from!(u64, U64);
impl_list_from!(i8, S8);
impl_list_from!(i16, S16);
impl_list_from!(i32, S32);
impl_list_from!(i64, S64);
impl_list_from!(f32, F32);
impl_list_from!(f64, F64);
impl_list_from!(char, Char);
impl_list_from!(Vec<u8>, Bytes);
impl_list_from!(String, String);

impl List {
    fn reverse(&mut self) {
        match self {
            Self::Bool(vs) => vs.reverse(),
            Self::U8(vs) => vs.reverse(),
            Self::S8(vs) => vs.reverse(),
            Self::U16(vs) => vs.reverse(),
            Self::S16(vs) => vs.reverse(),
            Self::U32(vs) => vs.reverse(),
            Self::S32(vs) => vs.reverse(),
            Self::U64(vs) => vs.reverse(),
            Self::S64(vs) => vs.reverse(),
            Self::F32(vs) => vs.reverse(),
            Self::F64(vs) => vs.reverse(),
            Self::Char(vs) => vs.reverse(),
            Self::Bytes(vs) => vs.reverse(),
            Self::String(vs) => vs.reverse(),
            Self::Raw(vs) => vs.reverse(),
        }
    }

    fn visit<'a, T: de::Visitor<'a>>(self, visitor: T) -> Result<T::Value, serde_json::Error> {
        match self {
            Self::Bool(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::U8(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::S8(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::U16(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::S16(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::U32(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::S32(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::U64(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::S64(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::F32(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::F64(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::Char(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::Bytes(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::String(vs) => visitor.visit_seq(vs.into_deserializer()),
            Self::Raw(vs) => visitor.visit_seq(vs.into_deserializer()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Primitive(Primitive),
    Record(Vec<Value>),
    Variant(u32, Option<Box<Value>>),
    List(List),
    Tuple(Vec<Value>),
    Flags(u32),
    Enum(u32),
    Option(Option<Box<Value>>),
    ResultEmpty(Result<(), ()>),
    ResultOnlyOk(Result<Box<Value>, ()>),
    ResultOnlyErr(Result<(), Box<Value>>),
    Result(Result<Box<Value>, Box<Value>>),
    Raw(serde_json::Value),
}

impl_serde_deserializer!(Value);

macro_rules! impl_value_from_primitive {
    ($t:ty) => {
        impl From<$t> for Value {
            #[inline]
            fn from(v: $t) -> Self {
                Self::Primitive(v.into())
            }
        }
    };
}

impl_value_from_primitive!(bool);
impl_value_from_primitive!(u8);
impl_value_from_primitive!(u16);
impl_value_from_primitive!(u32);
impl_value_from_primitive!(u64);
impl_value_from_primitive!(i8);
impl_value_from_primitive!(i16);
impl_value_from_primitive!(i32);
impl_value_from_primitive!(i64);
impl_value_from_primitive!(f32);
impl_value_from_primitive!(f64);
impl_value_from_primitive!(char);
impl_value_from_primitive!(String);

impl From<Vec<u8>> for Value {
    #[inline]
    fn from(v: Vec<u8>) -> Self {
        Self::List(List::U8(v))
    }
}

impl From<serde_bytes::ByteBuf> for Value {
    #[inline]
    fn from(v: serde_bytes::ByteBuf) -> Self {
        Self::List(List::U8(v.into_vec()))
    }
}

impl From<serde_json::Value> for Value {
    #[inline]
    fn from(v: serde_json::Value) -> Self {
        Self::Raw(v)
    }
}

fn invalid_number_type<E: de::Error>(v: serde_json::Number, exp: &dyn de::Expected) -> E {
    if let Some(v) = v.as_u64() {
        E::invalid_type(de::Unexpected::Unsigned(v), exp)
    } else if let Some(v) = v.as_i64() {
        E::invalid_type(de::Unexpected::Signed(v), exp)
    } else if let Some(v) = v.as_f64() {
        E::invalid_type(de::Unexpected::Float(v), exp)
    } else if let Some(v) = v.as_u128() {
        E::invalid_type(de::Unexpected::Other(&format!("{v} as u128")), exp)
    } else if let Some(v) = v.as_i128() {
        E::invalid_type(de::Unexpected::Other(&format!("{v} as i128")), exp)
    } else {
        E::invalid_type(de::Unexpected::Other(&format!("{v} as unclassified")), exp)
    }
}

impl Value {
    fn deserialize<'a, T: Deserialize<'a>>(self) -> Result<T, serde_json::Error> {
        match self {
            Self::Primitive(v) => v.deserialize(),
            Self::Record(vs) => T::deserialize(vs.into_deserializer()),
            Self::Variant(_, value) => todo!(),
            Self::List(vs) => T::deserialize(vs.into_deserializer()),
            Self::Tuple(vs) => T::deserialize(vs.into_deserializer()),
            Self::Flags(_) => todo!(),
            Self::Enum(_) => todo!(),
            Self::Option(value) => todo!(),
            Self::ResultEmpty(_) => todo!(),
            Self::ResultOnlyOk(value) => todo!(),
            Self::ResultOnlyErr(_) => todo!(),
            Self::Result(value) => todo!(),
            Self::Raw(v) => T::deserialize(v),
        }
    }

    fn invalid_type<E: de::Error>(self, exp: &dyn de::Expected) -> E {
        match self {
            Self::Primitive(v) => v.invalid_type(exp),
            Self::Record(..) => E::invalid_type(de::Unexpected::Other("record"), exp),
            Self::Variant(..) => E::invalid_type(de::Unexpected::Other("variant"), exp),
            Self::List(..) => E::invalid_type(de::Unexpected::Other("list"), exp),
            Self::Tuple(..) => E::invalid_type(de::Unexpected::Other("tuple"), exp),
            Self::Flags(..) => E::invalid_type(de::Unexpected::Other("flags"), exp),
            Self::Enum(..) => E::invalid_type(de::Unexpected::Other("enum"), exp),
            Self::Option(..) => E::invalid_type(de::Unexpected::Other("option"), exp),
            Self::ResultEmpty(..)
            | Self::ResultOnlyOk(..)
            | Self::ResultOnlyErr(..)
            | Self::Result(..) => E::invalid_type(de::Unexpected::Other("result"), exp),
            Self::Raw(serde_json::Value::Null) => E::invalid_type(de::Unexpected::Unit, exp),
            Self::Raw(serde_json::Value::Bool(v)) => E::invalid_type(de::Unexpected::Bool(v), exp),
            Self::Raw(serde_json::Value::Number(v)) => return invalid_number_type(v, exp),
            Self::Raw(serde_json::Value::String(v)) => {
                E::invalid_type(de::Unexpected::Str(&v), exp)
            }
            Self::Raw(serde_json::Value::Array(..)) => E::invalid_type(de::Unexpected::Seq, exp),
            Self::Raw(serde_json::Value::Object(..)) => E::invalid_type(de::Unexpected::Map, exp),
        }
    }

    fn visit<'a, T: de::Visitor<'a>>(self, visitor: T) -> Result<T::Value, serde_json::Error> {
        match self {
            Self::Primitive(v) => v.deserialize_any(visitor),
            Self::Record(vs) => vs.into_deserializer().deserialize_any(visitor),
            Self::Variant(..) => todo!(),
            Self::List(vs) => vs.deserialize_any(visitor),
            Self::Tuple(vs) => vs.into_deserializer().deserialize_any(visitor),
            Self::Flags(..) => todo!(),
            Self::Enum(..) => todo!(),
            Self::Option(..) => todo!(),
            Self::ResultEmpty(..)
            | Self::ResultOnlyOk(..)
            | Self::ResultOnlyErr(..)
            | Self::Result(..) => todo!(),
            Self::Raw(v) => v.deserialize_any(visitor),
        }
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
    fn wrap<E: Into<Error>>(err: E) -> deserializer::Error {
        err.into().into()
    }
}

impl deserializer::Guest for Component {
    type Error = Error;
    type Deserializer = Deserializer;
    type RecordDeserializer = RecordDeserializer;
    type TupleDeserializer = TupleDeserializer;
    type ListDeserializer = List;
}

impl deserializer::GuestError for Error {
    fn to_string(&self) -> String {
        ToString::to_string(&self.0)
    }
}

enum Deserializer {
    Source(serde_json::Deserializer<IoRead<Cursor<Vec<u8>>>>),
    Value(Value),
    Empty,
}

macro_rules! impl_deserializer_from_primitive {
    ($t:ty) => {
        impl From<$t> for Deserializer {
            #[inline]
            fn from(v: $t) -> Self {
                Self::Value(v.into())
            }
        }
    };
}

impl_deserializer_from_primitive!(bool);
impl_deserializer_from_primitive!(u8);
impl_deserializer_from_primitive!(u16);
impl_deserializer_from_primitive!(u32);
impl_deserializer_from_primitive!(u64);
impl_deserializer_from_primitive!(i8);
impl_deserializer_from_primitive!(i16);
impl_deserializer_from_primitive!(i32);
impl_deserializer_from_primitive!(i64);
impl_deserializer_from_primitive!(f32);
impl_deserializer_from_primitive!(f64);
impl_deserializer_from_primitive!(char);
impl_deserializer_from_primitive!(String);

impl From<Vec<u8>> for Deserializer {
    #[inline]
    fn from(v: Vec<u8>) -> Self {
        Self::Value(v.into())
    }
}

impl From<serde_json::Value> for Deserializer {
    #[inline]
    fn from(v: serde_json::Value) -> Self {
        Self::Value(v.into())
    }
}

impl From<Value> for Deserializer {
    #[inline]
    fn from(v: Value) -> Self {
        Self::Value(v)
    }
}

impl Deserializer {
    fn deserialize<'a, T: Deserialize<'a>>(self) -> Result<T, deserializer::Error> {
        match self {
            Self::Source(mut src) => T::deserialize(&mut src),
            Self::Value(v) => v.deserialize(),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
    }
}

macro_rules! impl_deserialize_primitive {
    ($name:ident, $t:ty, $c:ident, $exp:literal) => {
        fn $name(this: deserializer::Deserializer) -> Result<$t, deserializer::Error> {
            match this.into_inner::<Self>() {
                Self::Source(mut src) => <$t>::deserialize(&mut src),
                Self::Value(Value::Primitive(Primitive::$c(v))) => Ok(v),
                Self::Value(Value::Raw(v)) => <$t>::deserialize(v),
                Self::Value(v) => Err(v.invalid_type(&$exp)),
                Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
            }
            .map_err(Error::wrap)
        }
    };
}

impl deserializer::GuestDeserializer for Deserializer {
    fn from_list(buf: Vec<u8>) -> deserializer::Deserializer {
        deserializer::Deserializer::new(Deserializer::Source(
            serde_json::Deserializer::from_reader(Cursor::new(buf)),
        ))
    }

    impl_deserialize_primitive!(deserialize_bool, bool, Bool, "a boolean");
    impl_deserialize_primitive!(deserialize_u8, u8, U8, "a u8");
    impl_deserialize_primitive!(deserialize_u16, u16, U16, "a u16");
    impl_deserialize_primitive!(deserialize_u32, u32, U32, "a u32");
    impl_deserialize_primitive!(deserialize_u64, u64, U64, "a u64");
    impl_deserialize_primitive!(deserialize_s8, i8, S8, "a s8");
    impl_deserialize_primitive!(deserialize_s16, i16, S16, "a s16");
    impl_deserialize_primitive!(deserialize_s32, i32, S32, "a s32");
    impl_deserialize_primitive!(deserialize_s64, i64, S64, "a s64");
    impl_deserialize_primitive!(deserialize_f32, f32, F32, "a f32");
    impl_deserialize_primitive!(deserialize_f64, f64, F64, "a f64");
    impl_deserialize_primitive!(deserialize_char, char, Char, "a char");
    impl_deserialize_primitive!(deserialize_string, String, String, "a string");

    fn deserialize_bytes(this: deserializer::Deserializer) -> Result<Vec<u8>, deserializer::Error> {
        match this.into_inner::<Self>() {
            Self::Source(mut src) => deserialize_byte_buf(&mut src),
            Self::Value(Value::Primitive(Primitive::String(v))) => Ok(v.into()),
            Self::Value(Value::List(List::U8(v))) => Ok(v),
            Self::Value(v) => Err(v.invalid_type(&"a byte buffer")),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
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
                let mut vs = deserialize_record(&mut src, ty).map_err(Error::wrap)?;
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: RecordDeserializerFields::Typed(vs),
                    }),
                ))
            }
            Self::Value(Value::Record(mut vs)) => {
                if vs.len() != ty.len() {
                    return Err(Error::wrap(serde_json::Error::invalid_length(
                        vs.len(),
                        &RecordVisitor::new(ty),
                    )));
                }
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: RecordDeserializerFields::Typed(vs),
                    }),
                ))
            }
            Self::Value(Value::Raw(serde_json::Value::Array(mut vs))) => {
                if vs.len() != ty.len() {
                    return Err(Error::wrap(serde_json::Error::invalid_length(
                        vs.len(),
                        &RecordVisitor::new(ty),
                    )));
                }
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: RecordDeserializerFields::Raw(vs),
                    }),
                ))
            }
            Self::Value(Value::Raw(serde_json::Value::Object(mut obj))) => {
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
                        &RecordVisitor::new(ty),
                    )));
                }
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no fields found")))?;
                Ok((
                    0,
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::RecordDeserializer::new(RecordDeserializer {
                        idx: 0,
                        fields: RecordDeserializerFields::Raw(vs),
                    }),
                ))
            }
            Self::Value(v) => Err(Error::wrap(
                v.invalid_type::<serde_json::Error>(&RecordVisitor::new(ty)),
            )),
            Self::Empty => Err(Error::wrap(serde_json::Error::custom("empty deserializer"))),
        }
    }

    fn deserialize_variant(
        this: deserializer::Deserializer,
        ty: VariantTypeBorrow<'_>,
    ) -> Result<(u32, deserializer::Deserializer), deserializer::Error> {
        let (idx, v) = match this.into_inner::<Self>() {
            Self::Source(mut src) => deserialize_variant(&mut src, ty.get()),
            Self::Value(Value::Variant(idx, Some(v))) => Ok((idx, Some(*v))),
            Self::Value(Value::Variant(idx, None)) => Ok((idx, None)),
            Self::Value(v) => Err(v.invalid_type(&VariantVisitor(ty.get()))),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)?;
        let de = v.map(Deserializer::from).unwrap_or(Deserializer::Empty);
        Ok((idx, deserializer::Deserializer::new(de)))
    }

    fn deserialize_list(
        this: deserializer::Deserializer,
        ty: reflect::Type<'_>,
    ) -> Result<deserializer::ListDeserializer, deserializer::Error> {
        fn deserialize<'de, T, D>(
            deserializer: D,
            f: impl FnOnce(Vec<T>) -> List,
        ) -> Result<List, D::Error>
        where
            T: Deserialize<'de>,
            D: de::Deserializer<'de>,
        {
            let mut vs = Vec::deserialize(deserializer)?;
            vs.reverse();
            Ok(f(vs))
        }
        match this.into_inner::<Self>() {
            Self::Source(mut src) => match ty {
                reflect::Type::Bool => deserialize(&mut src, List::Bool),
                reflect::Type::U8 => deserialize(&mut src, List::U8),
                reflect::Type::S8 => deserialize(&mut src, List::S8),
                reflect::Type::U16 => deserialize(&mut src, List::U16),
                reflect::Type::S16 => deserialize(&mut src, List::S16),
                reflect::Type::U32 => deserialize(&mut src, List::U32),
                reflect::Type::S32 => deserialize(&mut src, List::S32),
                reflect::Type::U64 => deserialize(&mut src, List::U64),
                reflect::Type::S64 => deserialize(&mut src, List::S64),
                reflect::Type::F32 => deserialize(&mut src, List::F32),
                reflect::Type::F64 => deserialize(&mut src, List::F64),
                reflect::Type::Char => deserialize(&mut src, List::Char),
                reflect::Type::Bytes => deserialize(&mut src, List::Bytes),
                reflect::Type::String => deserialize(&mut src, List::String),
                reflect::Type::List(ty) if *ty.get::<ListType>() == ListType(Type::U8) => {
                    deserialize(&mut src, List::Bytes)
                }
                ty => deserialize_list(&mut src, &ListType(ty.into())).map(|mut vs| {
                    vs.reverse();
                    vs
                }),
            },
            Self::Value(Value::List(mut vs)) => {
                vs.reverse();
                Ok(vs)
            }
            Self::Value(v) => Err(v.invalid_type(&"a list")),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
        .map(deserializer::ListDeserializer::new)
    }

    fn deserialize_tuple(
        this: deserializer::Deserializer,
        ty: TupleTypeBorrow<'_>,
    ) -> Result<(deserializer::Deserializer, deserializer::TupleDeserializer), deserializer::Error>
    {
        let ty = ty.get::<TupleType>();
        match this.into_inner::<Self>() {
            Self::Source(mut src) => {
                let mut vs = deserialize_tuple(&mut src, ty).map_err(Error::wrap)?;
                vs.reverse();
                let v = vs
                    .pop()
                    .ok_or_else(|| Error::wrap(serde_json::Error::custom("no elements found")))?;
                Ok((
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer::Value(vs)),
                ))
            }
            Self::Value(Value::Tuple(mut vs)) => {
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
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer::Value(vs)),
                ))
            }
            Self::Value(Value::Raw(serde_json::Value::Array(mut vs))) => {
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
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::TupleDeserializer::new(TupleDeserializer::Raw(vs)),
                ))
            }
            Self::Value(v) => Err(Error::wrap(
                v.invalid_type::<serde_json::Error>(&TupleVisitor(ty)),
            )),
            Self::Empty => Err(Error::wrap(serde_json::Error::custom("empty deserializer"))),
        }
    }

    fn deserialize_flags(
        this: deserializer::Deserializer,
        ty: FlagsTypeBorrow<'_>,
    ) -> Result<u32, deserializer::Error> {
        match this.into_inner::<Self>() {
            Self::Source(mut src) => deserialize_flags(&mut src, ty.get()),
            Self::Value(Value::Flags(v)) => Ok(v),
            Self::Value(v) => Err(v.invalid_type(&FlagsVisitor(ty.get()))),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
    }

    fn deserialize_enum(
        this: deserializer::Deserializer,
        ty: EnumTypeBorrow<'_>,
    ) -> Result<u32, deserializer::Error> {
        match this.into_inner::<Self>() {
            Self::Source(mut src) => deserialize_enum(&mut src, ty.get()),
            Self::Value(Value::Enum(case)) => Ok(case),
            Self::Value(v) => Err(v.invalid_type(&EnumVisitor(ty.get()))),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)
    }

    fn deserialize_option(
        this: deserializer::Deserializer,
        ty: reflect::Type<'_>,
    ) -> Result<Option<deserializer::Deserializer>, deserializer::Error> {
        fn deserialize<'de, T, D>(deserializer: D) -> Result<Option<Value>, D::Error>
        where
            T: Deserialize<'de> + Into<Value>,
            D: de::Deserializer<'de>,
        {
            let v = Option::<T>::deserialize(deserializer)?;
            Ok(v.map(T::into))
        }

        let v = match this.into_inner::<Self>() {
            Self::Source(mut src) => match ty {
                reflect::Type::Bool => deserialize::<bool, _>(&mut src),
                reflect::Type::U8 => deserialize::<u8, _>(&mut src),
                reflect::Type::S8 => deserialize::<i8, _>(&mut src),
                reflect::Type::U16 => deserialize::<u16, _>(&mut src),
                reflect::Type::S16 => deserialize::<i16, _>(&mut src),
                reflect::Type::U32 => deserialize::<u32, _>(&mut src),
                reflect::Type::S32 => deserialize::<i32, _>(&mut src),
                reflect::Type::U64 => deserialize::<u64, _>(&mut src),
                reflect::Type::S64 => deserialize::<i64, _>(&mut src),
                reflect::Type::F32 => deserialize::<f32, _>(&mut src),
                reflect::Type::F64 => deserialize::<f64, _>(&mut src),
                reflect::Type::Char => deserialize::<char, _>(&mut src),
                reflect::Type::Bytes => deserialize::<serde_bytes::ByteBuf, _>(&mut src),
                reflect::Type::String => deserialize::<String, _>(&mut src),
                reflect::Type::List(ty) if *ty.get::<ListType>() == ListType(Type::U8) => {
                    deserialize::<serde_bytes::ByteBuf, _>(&mut src)
                }
                ty => deserialize_option(&mut src, &OptionType(ty.into())),
            },
            Self::Value(Value::Option(Some(v))) => Ok(Some(*v)),
            Self::Value(Value::Option(None)) => Ok(None),
            Self::Value(v) => Err(v.invalid_type(&"a list")),
            Self::Empty => Err(serde_json::Error::custom("empty deserializer")),
        }
        .map_err(Error::wrap)?;
        Ok(v.map(|v| deserializer::Deserializer::new(Self::from(v))))
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
                    Result::Ok(v) => Ok(Ok(deserializer::Deserializer::new(Deserializer::Value(
                        Value::Raw(v),
                    )))),
                    Result::Err(v) => Ok(Err(deserializer::Deserializer::new(
                        Deserializer::Value(Value::Raw(v)),
                    ))),
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
                    Result::Ok(v) => Ok(Ok(deserializer::Deserializer::new(Deserializer::Value(
                        Value::Raw(v),
                    )))),
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
                    Result::Err(v) => Ok(Err(deserializer::Deserializer::new(
                        Deserializer::Value(Value::Raw(v)),
                    ))),
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
    fields: RecordDeserializerFields,
}

enum RecordDeserializerFields {
    Typed(Vec<Value>),
    Raw(Vec<serde_json::Value>),
}

impl RecordDeserializer {
    fn next(&mut self) -> serde_json::Result<(u32, Value)> {
        // only n-1 fields are stored in the vec
        self.idx = self
            .idx
            .checked_add(1)
            .ok_or_else(|| serde_json::Error::custom("too many fields"))?;
        let v = match &mut self.fields {
            RecordDeserializerFields::Typed(fields) => fields.pop(),
            RecordDeserializerFields::Raw(fields) => fields.pop().map(Value::from),
        }
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
            deserializer::Deserializer::new(Deserializer::from(v)),
            deserializer::RecordDeserializer::new(this),
        )
    }
}

enum TupleDeserializer {
    Byte(Vec<u8>),
    Value(Vec<Value>),
    Raw(Vec<serde_json::Value>),
}

impl GuestTupleDeserializer for TupleDeserializer {
    fn next(
        this: deserializer::TupleDeserializer,
    ) -> (deserializer::Deserializer, deserializer::TupleDeserializer) {
        match this.into_inner::<Self>() {
            Self::Byte(mut vs) => {
                let v = vs.pop().expect("no fields left to iterate");
                (
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::TupleDeserializer::new(Self::Byte(vs)),
                )
            }
            Self::Value(mut vs) => {
                let v = vs.pop().expect("no fields left to iterate");
                (
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::TupleDeserializer::new(Self::Value(vs)),
                )
            }
            Self::Raw(mut vs) => {
                let v = vs.pop().expect("no fields left to iterate");
                (
                    deserializer::Deserializer::new(Deserializer::from(v)),
                    deserializer::TupleDeserializer::new(Self::Raw(vs)),
                )
            }
        }
    }
}

impl GuestListDeserializer for List {
    fn next(
        this: deserializer::ListDeserializer,
    ) -> Option<(deserializer::Deserializer, deserializer::ListDeserializer)> {
        match this.into_inner::<Self>() {
            Self::Bool(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::Bool(vs))),
            Self::U8(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::U8(vs))),
            Self::S8(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::S8(vs))),
            Self::U16(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::U16(vs))),
            Self::S16(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::S16(vs))),
            Self::U32(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::U32(vs))),
            Self::S32(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::S32(vs))),
            Self::U64(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::U64(vs))),
            Self::S64(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::S64(vs))),
            Self::F32(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::F32(vs))),
            Self::F64(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::F64(vs))),
            Self::Char(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::Char(vs))),
            Self::Bytes(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::Bytes(vs))),
            Self::String(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::String(vs))),
            Self::Raw(mut vs) => vs.pop().map(|v| (Deserializer::from(v), Self::Raw(vs))),
        }
        .map(|(de, next)| {
            (
                deserializer::Deserializer::new(de),
                deserializer::ListDeserializer::new(next),
            )
        })
    }
}

struct TupleVisitor<'a>(&'a TupleType);

impl<'de> de::Visitor<'de> for TupleVisitor<'_> {
    type Value = Vec<Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a tuple of type {:?}", self.0.0)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(self.0.len());
        for ty in self.0.iter() {
            let Some(v) = seq.next_element_seed(ty)? else {
                return Err(A::Error::invalid_length(values.len(), &self));
            };
            values.push(v);
        }
        Ok(values)
    }
}

struct RecordVisitor<'a> {
    ty: &'a RecordType,
    fields: HashMap<&'a str, (&'a Type, usize)>,
}

impl<'a> RecordVisitor<'a> {
    fn new(ty: &'a RecordType) -> Self {
        let fields: HashMap<_, _> = zip(ty.iter(), 0..)
            .map(|((name, ty), idx)| (name.as_ref(), (ty, idx)))
            .collect();
        Self { ty, fields }
    }
}

impl<'de> de::Visitor<'de> for RecordVisitor<'_> {
    type Value = Vec<Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "a record with fields: {:?}", self.ty.0)
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: de::SeqAccess<'de>,
    {
        let mut values = Vec::with_capacity(self.ty.len());
        for (_, ty) in self.ty.iter() {
            let Some(v) = seq.next_element_seed(ty)? else {
                return Err(A::Error::invalid_length(values.len(), &self));
            };
            values.push(v);
        }
        Ok(values)
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: de::MapAccess<'de>,
    {
        let mut values = vec![Value::Raw(serde_json::Value::Null); self.ty.len()];
        while let Some(k) = map.next_key::<Cow<str>>()? {
            let (ty, idx) = self
                .fields
                .get(k.as_ref())
                .ok_or_else(|| A::Error::custom(format!("unknown field `{k}`")))?;
            let v = map.next_value_seed(*ty)?;
            values[*idx] = v;
        }
        // TODO: Add strict mode
        Ok(values)
    }
}

struct VariantVisitor<'a>(&'a VariantType);

impl<'de> de::Visitor<'de> for VariantVisitor<'_> {
    type Value = (u32, Option<Value>);

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
        let Some(k) = map.next_key::<Cow<str>>()? else {
            return Err(A::Error::custom("the object must have exactly one field"));
        };
        for (idx, (name, ty)) in zip(0.., self.0.as_ref()) {
            if k == name.as_ref() {
                let v = if let Some(ty) = ty {
                    let v = map.next_value_seed(ty)?;
                    Some(v)
                } else {
                    let v = map.next_value::<serde_json::Value>()?;
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
        for (idx, (name, ty)) in zip(0.., self.0.as_ref()) {
            if k == name.as_ref() {
                let v = if let Some(ty) = ty {
                    let v = v.newtype_variant_seed(ty)?;
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

struct OptionVisitor<'a>(&'a OptionType);

impl<'de> de::Visitor<'de> for OptionVisitor<'_> {
    type Value = Option<Value>;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "option of type {:?}", self.0)
    }

    #[inline]
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(None)
    }

    #[inline]
    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(None)
    }

    #[inline]
    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        self.0.deserialize(deserializer).map(Some)
    }

    fn __private_visit_untagged_option<D>(self, deserializer: D) -> Result<Self::Value, ()>
    where
        D: de::Deserializer<'de>,
    {
        Ok(self.0.deserialize(deserializer).ok())
    }
}

struct Serializer(serde_json::Serializer<Vec<u8>>);

impl Deref for Serializer {
    type Target = serde_json::Serializer<Vec<u8>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Serializer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl serializer::Guest for Component {
    type TupleSerializer = TupleSerializer;
    type RecordSerializer = RecordSerializer;
    type ListSerializer = ListSerializer;
    type Serializer = Serializer;
}

impl serializer::GuestSerializer for Serializer {
    fn new() -> Self {
        Self(serde_json::Serializer::new(Vec::default()))
    }

    fn serialize_bool(this: serializer::Serializer, v: bool) {
        this.into_inner::<Self>().serialize_bool(v).unwrap()
    }

    fn serialize_u8(this: serializer::Serializer, v: u8) {
        this.into_inner::<Self>().serialize_u8(v).unwrap()
    }

    fn serialize_s8(this: serializer::Serializer, v: i8) {
        this.into_inner::<Self>().serialize_i8(v).unwrap()
    }

    fn serialize_u16(this: serializer::Serializer, v: u16) {
        this.into_inner::<Self>().serialize_u16(v).unwrap()
    }

    fn serialize_s16(this: serializer::Serializer, v: i16) {
        this.into_inner::<Self>().serialize_i16(v).unwrap()
    }

    fn serialize_u32(this: serializer::Serializer, v: u32) {
        this.into_inner::<Self>().serialize_u32(v).unwrap()
    }

    fn serialize_s32(this: serializer::Serializer, v: i32) {
        this.into_inner::<Self>().serialize_i32(v).unwrap()
    }

    fn serialize_u64(this: serializer::Serializer, v: u64) {
        this.into_inner::<Self>().serialize_u64(v).unwrap()
    }

    fn serialize_s64(this: serializer::Serializer, v: i64) {
        this.into_inner::<Self>().serialize_i64(v).unwrap()
    }

    fn serialize_f32(this: serializer::Serializer, v: f32) {
        this.into_inner::<Self>().serialize_f32(v).unwrap()
    }

    fn serialize_f64(this: serializer::Serializer, v: f64) {
        this.into_inner::<Self>().serialize_f64(v).unwrap()
    }

    fn serialize_char(this: serializer::Serializer, v: char) {
        this.into_inner::<Self>().serialize_char(v).unwrap()
    }

    fn serialize_bytes(this: serializer::Serializer, v: Vec<u8>) {
        this.into_inner::<Self>().serialize_bytes(&v).unwrap()
    }

    fn serialize_string(this: serializer::Serializer, v: String) {
        this.into_inner::<Self>().serialize_str(&v).unwrap()
    }

    fn serialize_record(
        this: serializer::Serializer,
        ty: RecordTypeBorrow<'_>,
    ) -> (serializer::Serializer, serializer::RecordSerializer) {
        todo!()
    }

    fn serialize_variant(
        this: serializer::Serializer,
        ty: VariantTypeBorrow<'_>,
        case: u32,
    ) -> serializer::Serializer {
        todo!()
    }

    fn serialize_list(
        this: serializer::Serializer,
        ty: reflect::Type<'_>,
        n: Option<u32>,
    ) -> serializer::ListSerializer {
        todo!()
    }

    fn serialize_tuple(
        this: serializer::Serializer,
        n: u32,
    ) -> (serializer::Serializer, serializer::TupleSerializer) {
        todo!()
    }

    fn serialize_flags(this: serializer::Serializer, ty: FlagsTypeBorrow<'_>, v: u32) {
        todo!()
    }

    fn serialize_enum(this: serializer::Serializer, ty: EnumTypeBorrow<'_>, v: u32) {
        todo!()
    }

    fn serialize_none(this: serializer::Serializer) {
        todo!()
    }

    fn serialize_some(
        this: serializer::Serializer,
        ty: reflect::Type<'_>,
    ) -> serializer::Serializer {
        todo!()
    }

    fn serialize_ok(
        this: serializer::Serializer,
        ty: Option<reflect::Type<'_>>,
    ) -> serializer::Serializer {
        todo!()
    }

    fn serialize_err(
        this: serializer::Serializer,
        ty: Option<reflect::Type<'_>>,
    ) -> serializer::Serializer {
        todo!()
    }
}

struct TupleSerializer;

impl serializer::GuestTupleSerializer for TupleSerializer {
    fn next(
        this: serializer::TupleSerializer,
    ) -> (serializer::Serializer, serializer::TupleSerializer) {
        todo!()
    }
}

struct RecordSerializer;

impl serializer::GuestRecordSerializer for RecordSerializer {
    fn next(
        this: serializer::RecordSerializer,
    ) -> (serializer::Serializer, serializer::RecordSerializer) {
        todo!()
    }
}

struct ListSerializer;

impl serializer::GuestListSerializer for ListSerializer {
    fn next(
        this: serializer::ListSerializer,
    ) -> (serializer::Serializer, serializer::ListSerializer) {
        todo!()
    }
}
