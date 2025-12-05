pub mod bindings;

use core::cell::OnceCell;
use core::fmt;
use core::iter::zip;
use core::ops::Deref;

use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

use bindings::exports::rvolosatovs::serde::reflect::{
    GuestEnumType, GuestFlagsType, GuestListOfListsType, GuestListOfListsValue, GuestOptionType,
    GuestOptionValue, GuestRecordType, GuestRecordValue, GuestResultType, GuestResultValue,
    GuestTupleType, GuestTupleValue, GuestVariantType, GuestVariantValue, List, Value,
};
use bindings::exports::rvolosatovs::serde::{deserializer, reflect, serializer};
use serde::de::{DeserializeSeed as _, Error as _, IntoDeserializer as _, VariantAccess as _};
use serde::ser::SerializeSeq as _;
use serde::{Deserialize, Serialize, de, ser};

impl<T> reflect::Guest for T {
    type RecordType = RecordType;
    type VariantType = VariantType;
    type ListOfListsType = Type;
    type TupleType = TupleType;
    type FlagsType = FlagsType;
    type EnumType = EnumType;
    type OptionType = OptionType;
    type ResultType = ResultType;

    type RecordValue = RecordValue;
    type VariantValue = VariantValue;
    type ListOfListsValue = ListOfListsValue;
    type TupleValue = TupleValue;
    type OptionValue = OptionValue;
    type ResultValue = ResultValue;
}

pub trait Serializer {
    fn from_value(v: &reflect::Value) -> Vec<u8>;
}

impl<T> serializer::Guest for T
where
    T: Serializer,
{
    #[inline]
    fn from_value(v: reflect::Value) -> Vec<u8> {
        T::from_value(&v)
    }
}

pub trait Deserializer {
    type Error;

    fn from_list(buf: Vec<u8>, ty: Type) -> Result<Value, Self::Error>;
}

impl<T> deserializer::Guest for T
where
    T: Deserializer,
    T::Error: de::Error + 'static,
{
    type Error = Error<T::Error>;

    #[inline]
    fn from_list(
        buf: Vec<u8>,
        ty: reflect::Type<'_>,
    ) -> Result<reflect::Value, deserializer::Error> {
        T::from_list(buf, ty.into()).map_err(|err| deserializer::Error::new(Error(err)))
    }
}

#[repr(transparent)]
pub struct Error<T>(pub T);

impl<T: de::Error + 'static> deserializer::GuestError for Error<T> {
    fn to_string(&self) -> String {
        T::to_string(&self.0)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Type {
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

#[inline]
pub fn deserialize_byte_buf<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
where
    D: de::Deserializer<'de>,
{
    serde_bytes::ByteBuf::deserialize(deserializer).map(serde_bytes::ByteBuf::into_vec)
}

impl<'de> de::DeserializeSeed<'de> for &'de Type {
    type Value = Value;

    #[inline]
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
            Type::String => String::deserialize(deserializer).map(Value::from),
            Type::Record(ty) => ty.deserialize(deserializer).map(Value::from),
            Type::Variant(ty) => ty.deserialize(deserializer).map(Value::from),
            Type::List(ty) => ty.deserialize(deserializer).map(Value::from),
            Type::Tuple(ty) => ty.deserialize(deserializer).map(Value::from),
            Type::Flags(ty) => ty.deserialize(deserializer).map(Value::Flags),
            Type::Enum(ty) => ty.deserialize(deserializer).map(Value::Enum),
            Type::Option(ty) => ty.deserialize(deserializer).map(Value::from),
            Type::Result(ty) => ty.deserialize(deserializer).map(Value::from),
        }
    }
}

impl From<reflect::Type<'_>> for Type {
    #[inline]
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
            reflect::Type::String => Self::String,
            reflect::Type::Record(ty) => Self::Record(ty.get::<RecordType>().clone()),
            reflect::Type::Variant(ty) => Self::Variant(ty.get::<VariantType>().clone()),
            reflect::Type::List(ty) => Self::List(Box::new(ty.into())),
            reflect::Type::Tuple(ty) => Self::Tuple(ty.get::<TupleType>().clone()),
            reflect::Type::Flags(ty) => Self::Flags(ty.get::<FlagsType>().clone()),
            reflect::Type::Enum(ty) => Self::Enum(ty.get::<EnumType>().clone()),
            reflect::Type::Option(ty) => Self::Option(Box::new(ty.get::<OptionType>().clone())),
            reflect::Type::Result(ty) => Self::Result(Box::new(ty.get::<ResultType>().clone())),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct RecordType(Rc<[(String, Type)]>);

impl Deref for RecordType {
    type Target = Rc<[(String, Type)]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestRecordType for RecordType {
    #[inline]
    fn new(fields: Vec<(String, reflect::Type<'_>)>) -> Self {
        let fields = fields
            .into_iter()
            .map(|(name, ty)| (name, ty.into()))
            .collect();
        Self(fields)
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de RecordType {
    type Value = reflect::RecordValue;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct RecordFieldVisitor<'a> {
            ty: &'a RecordType,
            fields: OnceCell<HashMap<&'a str, (usize, &'a Type)>>,
        }

        impl<'de> de::DeserializeSeed<'de> for RecordFieldVisitor<'de> {
            type Value = (usize, &'de Type);

            #[inline]
            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                deserializer.deserialize_identifier(self)
            }
        }

        impl<'de> de::Visitor<'de> for RecordFieldVisitor<'de> {
            type Value = (usize, &'de Type);

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                let expected: Vec<_> = self.ty.iter().map(|(name, ..)| name).collect();
                write!(formatter, "one of `{expected:?}`")
            }

            #[inline]
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some((_name, payload)) = self.ty.get(usize::from(v)) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok((v.into(), payload))
            }

            #[inline]
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some((_name, payload)) = self.ty.get(usize::from(v)) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok((v.into(), payload))
            }

            #[inline]
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some((_name, payload)) = self.ty.get(v as usize) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok((v as _, payload))
            }

            #[inline]
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Ok(i) = usize::try_from(v) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v), &self));
                };
                let Some((_name, payload)) = self.ty.get(i) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v), &self));
                };
                Ok((i, payload))
            }

            #[inline]
            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let fields = self.fields.get_or_init(|| {
                    zip(0.., self.ty.iter())
                        .map(|(i, (name, ty))| (name.as_ref(), (i, ty)))
                        .collect()
                });
                let Some((i, ty)) = fields.get(value) else {
                    return Err(E::invalid_value(de::Unexpected::Str(value), &self));
                };
                Ok((*i, ty))
            }

            #[inline]
            fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                for (i, (name, ty)) in zip(0.., self.ty.as_ref()) {
                    if value == name.as_bytes() {
                        return Ok((i, ty));
                    }
                }
                Err(E::invalid_value(de::Unexpected::Bytes(value), &self))
            }
        }

        struct RecordVisitor<'a>(&'a RecordType);

        impl<'de> de::Visitor<'de> for RecordVisitor<'de> {
            type Value = reflect::RecordValue;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "record with fields: {:?}", self.0.0)
            }

            #[inline]
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let mut fields = Vec::with_capacity(self.0.len());
                for (_, ty) in self.0.iter() {
                    let Some(v) = seq.next_element_seed(ty)? else {
                        return Err(A::Error::invalid_length(fields.len(), &self));
                    };
                    fields.push(v);
                }
                Ok(Self::Value::new(RecordValue(fields)))
            }

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut fields = Vec::with_capacity(self.0.len());
                while let Some((i, ty)) = map.next_key_seed(RecordFieldVisitor {
                    ty: self.0,
                    fields: OnceCell::new(),
                })? {
                    let v = map.next_value_seed(ty)?;
                    if fields.len() == i {
                        fields.push(v);
                    } else if fields.len() < i {
                        fields.resize_with(i, || Value::Option(None.into()));
                        fields.push(v);
                    } else {
                        fields[i] = v;
                    }
                }
                // TODO: Add strict mode
                Ok(Self::Value::new(RecordValue(fields)))
            }
        }

        deserializer.deserialize_struct("", &[], RecordVisitor(self))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct VariantType(Rc<[(String, Option<Type>)]>);

impl Deref for VariantType {
    type Target = Rc<[(String, Option<Type>)]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestVariantType for VariantType {
    #[inline]
    fn new(cases: Vec<(String, Option<reflect::Type<'_>>)>) -> Self {
        let cases = cases
            .into_iter()
            .map(|(name, ty)| (name, ty.map(Into::into)))
            .collect();
        Self(cases)
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de VariantType {
    type Value = reflect::VariantValue;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct VariantDiscriminantVisitor<'a>(&'a VariantType);

        impl<'de> de::DeserializeSeed<'de> for VariantDiscriminantVisitor<'de> {
            type Value = (u32, &'de Option<Type>);

            #[inline]
            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                deserializer.deserialize_identifier(self)
            }
        }

        impl<'de> de::Visitor<'de> for VariantDiscriminantVisitor<'de> {
            type Value = (u32, &'de Option<Type>);

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                let expected: Vec<_> = self.0.iter().map(|(k, _)| k).collect();
                write!(formatter, "one of `{expected:?}`")
            }

            #[inline]
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some((_name, payload)) = self.0.get(usize::from(v)) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok((v.into(), payload))
            }

            #[inline]
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some((_name, payload)) = self.0.get(usize::from(v)) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok((v.into(), payload))
            }

            #[inline]
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some((_name, payload)) = self.0.get(v as usize) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok((v, payload))
            }

            #[inline]
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Ok(c) = usize::try_from(v) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v), &self));
                };
                let Some((_name, payload)) = self.0.get(c) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v), &self));
                };
                Ok((c as _, payload))
            }

            #[inline]
            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                for (c, (name, payload)) in zip(0.., self.0.as_ref()) {
                    if value == name {
                        return Ok((c, payload));
                    }
                }
                Err(E::invalid_value(de::Unexpected::Str(value), &self))
            }

            #[inline]
            fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                for (c, (name, payload)) in zip(0.., self.0.as_ref()) {
                    if value == name.as_bytes() {
                        return Ok((c, payload));
                    }
                }
                Err(E::invalid_value(de::Unexpected::Bytes(value), &self))
            }
        }

        struct VariantVisitor<'a>(&'a VariantType);

        impl<'de> de::Visitor<'de> for VariantVisitor<'de> {
            type Value = reflect::VariantValue;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "variant with cases: {:?}", self.0.0)
            }

            #[inline]
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let (c, payload) = VariantDiscriminantVisitor(self.0).visit_u8(v)?;
                if payload.is_some() {
                    return Err(E::custom("missing payload"));
                }
                Ok(Self::Value::new(VariantValue(c, None)))
            }

            #[inline]
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let (c, payload) = VariantDiscriminantVisitor(self.0).visit_u16(v)?;
                if payload.is_some() {
                    return Err(E::custom("missing payload"));
                }
                Ok(Self::Value::new(VariantValue(c, None)))
            }

            #[inline]
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let (c, payload) = VariantDiscriminantVisitor(self.0).visit_u32(v)?;
                if payload.is_some() {
                    return Err(E::custom("missing payload"));
                }
                Ok(Self::Value::new(VariantValue(c, None)))
            }

            #[inline]
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let (c, payload) = VariantDiscriminantVisitor(self.0).visit_u64(v)?;
                if payload.is_some() {
                    return Err(E::custom("missing payload"));
                }
                Ok(Self::Value::new(VariantValue(c, None)))
            }

            #[inline]
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let (c, payload) = VariantDiscriminantVisitor(self.0).visit_str(v)?;
                if payload.is_some() {
                    return Err(E::custom("missing payload"));
                }
                Ok(Self::Value::new(VariantValue(c, None)))
            }

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let Some((c, payload)) = map.next_key_seed(VariantDiscriminantVisitor(self.0))?
                else {
                    return Err(A::Error::custom("the object must have exactly one field"));
                };
                let v = if let Some(ty) = payload {
                    let v = map.next_value_seed(ty)?;
                    Some(v)
                } else {
                    map.next_value::<()>()?;
                    None
                };
                if map
                    .next_key_seed(VariantDiscriminantVisitor(self.0))?
                    .is_some()
                {
                    return Err(A::Error::custom("the object must have exactly one field"));
                }
                Ok(Self::Value::new(VariantValue(c, v)))
            }

            #[inline]
            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                let ((c, payload), v) = data.variant_seed(VariantDiscriminantVisitor(self.0))?;
                let v = if let Some(ty) = payload {
                    let v = v.newtype_variant_seed(ty)?;
                    Some(v)
                } else {
                    v.unit_variant()?;
                    None
                };
                Ok(Self::Value::new(VariantValue(c, v)))
            }
        }

        deserializer.deserialize_enum("", &[], VariantVisitor(self))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct ListType(Type);

impl Deref for ListType {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestListOfListsType for Type {
    #[inline]
    fn new(ty: reflect::Type<'_>) -> Self {
        ty.into()
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de ListType {
    type Value = List;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        #[repr(transparent)]
        struct ListVisitor<'a, T>(&'a T);
        impl<'de, T> de::Visitor<'de> for ListVisitor<'de, T>
        where
            &'de T: de::DeserializeSeed<'de>,
        {
            type Value = Vec<<&'de T as de::DeserializeSeed<'de>>::Value>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "list")
            }

            #[inline]
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let mut values = Vec::default();
                while let Some(v) = seq.next_element_seed(self.0)? {
                    values.push(v)
                }
                Ok(values)
            }
        }

        match &self.0 {
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
            Type::String => Vec::deserialize(deserializer).map(List::String),
            Type::Record(ty) => deserializer
                .deserialize_seq(ListVisitor(ty))
                .map(List::from),
            Type::Variant(ty) => deserializer
                .deserialize_seq(ListVisitor(ty))
                .map(List::from),
            Type::List(ty) => deserializer
                .deserialize_seq(ListVisitor(ty.as_ref()))
                .map(List::from),
            Type::Tuple(ty) => deserializer
                .deserialize_seq(ListVisitor(ty))
                .map(List::from),
            Type::Flags(ty) => deserializer
                .deserialize_seq(ListVisitor(ty))
                .map(List::from),
            Type::Enum(ty) => deserializer
                .deserialize_seq(ListVisitor(ty))
                .map(List::from),
            Type::Option(ty) => deserializer
                .deserialize_seq(ListVisitor(ty.as_ref()))
                .map(List::from),
            Type::Result(ty) => deserializer
                .deserialize_seq(ListVisitor(ty.as_ref()))
                .map(List::from),
        }
    }
}

impl From<reflect::ListType<'_>> for ListType {
    #[inline]
    fn from(ty: reflect::ListType<'_>) -> Self {
        match ty {
            reflect::ListType::Bool => Self(Type::Bool),
            reflect::ListType::U8 => Self(Type::U8),
            reflect::ListType::S8 => Self(Type::S8),
            reflect::ListType::U16 => Self(Type::U16),
            reflect::ListType::S16 => Self(Type::S16),
            reflect::ListType::U32 => Self(Type::U32),
            reflect::ListType::S32 => Self(Type::S32),
            reflect::ListType::U64 => Self(Type::U64),
            reflect::ListType::S64 => Self(Type::S64),
            reflect::ListType::F32 => Self(Type::F32),
            reflect::ListType::F64 => Self(Type::F64),
            reflect::ListType::Char => Self(Type::Char),
            reflect::ListType::String => Self(Type::String),
            reflect::ListType::Record(ty) => Self(Type::Record(ty.get::<RecordType>().clone())),
            reflect::ListType::Variant(ty) => Self(Type::Variant(ty.get::<VariantType>().clone())),
            reflect::ListType::List(ty) => {
                Self(Type::List(Box::new(Self(ty.get::<Type>().clone()))))
            }
            reflect::ListType::Tuple(ty) => Self(Type::Tuple(ty.get::<TupleType>().clone())),
            reflect::ListType::Flags(ty) => Self(Type::Flags(ty.get::<FlagsType>().clone())),
            reflect::ListType::Enum(ty) => Self(Type::Enum(ty.get::<EnumType>().clone())),
            reflect::ListType::Option(ty) => {
                Self(Type::Option(Box::new(ty.get::<OptionType>().clone())))
            }
            reflect::ListType::Result(ty) => {
                Self(Type::Result(Box::new(ty.get::<ResultType>().clone())))
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct TupleType(Rc<[Type]>);

impl Deref for TupleType {
    type Target = Rc<[Type]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestTupleType for TupleType {
    #[inline]
    fn new(types: Vec<reflect::Type<'_>>) -> Self {
        let types = types.into_iter().map(Into::into).collect();
        Self(types)
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de TupleType {
    type Value = reflect::TupleValue;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_tuple(self.len(), self)
    }
}

impl<'de> de::Visitor<'de> for &'de TupleType {
    type Value = reflect::TupleValue;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "tuple of type {:?}", self.0)
    }

    #[inline]
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
        Ok(Self::Value::new(TupleValue(values)))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct FlagsType(Rc<[String]>);

impl Deref for FlagsType {
    type Target = Rc<[String]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestFlagsType for FlagsType {
    #[inline]
    fn new(cases: Vec<String>) -> Self {
        let cases = cases.into();
        Self(cases)
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de FlagsType {
    type Value = u32;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct FlagsVisitor<'a>(&'a FlagsType);

        impl<'de> de::Visitor<'de> for FlagsVisitor<'_> {
            type Value = u32;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "flags with cases: {:?}", self.0.0)
            }

            #[inline]
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

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                debug_assert!(self.0.len() <= 32, "flags may contain at most 32 cases");
                let mut flags = 0;
                let mut cases: HashMap<&str, u8> =
                    zip(self.0.iter().map(|s| s.as_ref()), 0..).collect();
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

        deserializer.deserialize_struct("", &[], FlagsVisitor(self))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct EnumType(Rc<[String]>);

impl Deref for EnumType {
    type Target = Rc<[String]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestEnumType for EnumType {
    #[inline]
    fn new(cases: Vec<String>) -> Self {
        let cases = cases.into();
        Self(cases)
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de EnumType {
    type Value = u32;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct EnumDiscriminantVisitor<'a>(&'a EnumType);

        impl<'de> de::DeserializeSeed<'de> for EnumDiscriminantVisitor<'de> {
            type Value = u32;

            #[inline]
            fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                deserializer.deserialize_identifier(self)
            }
        }

        impl<'de> de::Visitor<'de> for EnumDiscriminantVisitor<'de> {
            type Value = u32;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "one of `{:?}`", self.0.0)
            }

            #[inline]
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some(_name) = self.0.get(usize::from(v)) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok(v.into())
            }

            #[inline]
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some(_name) = self.0.get(usize::from(v)) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok(v.into())
            }

            #[inline]
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Some(_name) = self.0.get(v as usize) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v.into()), &self));
                };
                Ok(v)
            }

            #[inline]
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let Ok(c) = usize::try_from(v) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v), &self));
                };
                let Some(_name) = self.0.get(c) else {
                    return Err(E::invalid_value(de::Unexpected::Unsigned(v), &self));
                };
                Ok(c as _)
            }

            #[inline]
            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                for (c, name) in zip(0.., self.0.as_ref()) {
                    if value == name {
                        return Ok(c);
                    }
                }
                Err(E::invalid_value(de::Unexpected::Str(value), &self))
            }

            #[inline]
            fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                for (c, name) in zip(0.., self.0.as_ref()) {
                    if value == name.as_bytes() {
                        return Ok(c);
                    }
                }
                Err(E::invalid_value(de::Unexpected::Bytes(value), &self))
            }
        }

        struct EnumVisitor<'a>(&'a EnumType);

        impl<'de> de::Visitor<'de> for EnumVisitor<'de> {
            type Value = u32;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "enum with cases: {:?}", self.0.0)
            }

            #[inline]
            fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                EnumDiscriminantVisitor(self.0).visit_u8(v)
            }

            #[inline]
            fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                EnumDiscriminantVisitor(self.0).visit_u16(v)
            }

            #[inline]
            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                EnumDiscriminantVisitor(self.0).visit_u32(v)
            }

            #[inline]
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                EnumDiscriminantVisitor(self.0).visit_u64(v)
            }

            #[inline]
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                EnumDiscriminantVisitor(self.0).visit_str(v)
            }

            #[inline]
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let Some(c) = map.next_key_seed(EnumDiscriminantVisitor(self.0))? else {
                    return Err(A::Error::custom("the object must have exactly one field"));
                };
                if map
                    .next_key_seed(EnumDiscriminantVisitor(self.0))?
                    .is_some()
                {
                    return Err(A::Error::custom("the object must have exactly one field"));
                }
                Ok(c)
            }

            #[inline]
            fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
            where
                A: de::EnumAccess<'de>,
            {
                let (c, v) = data.variant_seed(EnumDiscriminantVisitor(self.0))?;
                v.unit_variant()?;
                Ok(c)
            }
        }

        deserializer.deserialize_enum("", &[], EnumVisitor(self))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct OptionType(Type);

impl Deref for OptionType {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestOptionType for OptionType {
    #[inline]
    fn new(ty: reflect::Type<'_>) -> Self {
        Self(ty.into())
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de OptionType {
    type Value = reflect::OptionValue;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        struct OptionVisitor<'a>(&'a Type);

        impl<'de> de::Visitor<'de> for OptionVisitor<'de> {
            type Value = reflect::OptionValue;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "option of type {:?}", self.0)
            }

            #[inline]
            fn visit_unit<E>(self) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Self::Value::new(OptionValue(None)))
            }

            #[inline]
            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Self::Value::new(OptionValue(None)))
            }

            #[inline]
            fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                self.0
                    .deserialize(deserializer)
                    .map(|v| Self::Value::new(OptionValue(Some(v))))
            }
        }

        deserializer.deserialize_option(OptionVisitor(&self.0))
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ResultType {
    ok: Option<Type>,
    err: Option<Type>,
}

impl GuestResultType for ResultType {
    #[inline]
    fn new(ok: Option<reflect::Type<'_>>, err: Option<reflect::Type<'_>>) -> Self {
        let ok = ok.map(Into::into);
        let err = err.map(Into::into);
        Self { ok, err }
    }
}

impl<'de> de::DeserializeSeed<'de> for &'de ResultType {
    type Value = reflect::ResultValue;

    #[inline]
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        deserializer.deserialize_enum("result", &["ok", "err"], self)
    }
}

#[derive(Deserialize)]
#[serde(variant_identifier, rename_all = "lowercase")]
enum ResultDiscriminant {
    Ok,
    Err,
}

impl<'de> de::Visitor<'de> for &'de ResultType {
    type Value = reflect::ResultValue;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "result of type {self:?}")
    }

    #[inline]
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match ResultDiscriminant::deserialize(v.into_deserializer())? {
            ResultDiscriminant::Ok if self.ok.is_none() => {
                Ok(Self::Value::new(ResultValue(Ok(None))))
            }
            ResultDiscriminant::Err if self.err.is_none() => {
                Ok(Self::Value::new(ResultValue(Err(None))))
            }
            _ => Err(E::custom("missing payload")),
        }
    }

    #[inline]
    fn visit_enum<A>(self, data: A) -> Result<Self::Value, A::Error>
    where
        A: de::EnumAccess<'de>,
    {
        let (c, v) = data.variant()?;
        match c {
            ResultDiscriminant::Ok => {
                if let Some(ty) = &self.ok {
                    v.newtype_variant_seed(ty)
                        .map(|v| Self::Value::new(ResultValue(Ok(Some(v)))))
                } else {
                    v.unit_variant()
                        .map(|()| Self::Value::new(ResultValue(Ok(None))))
                }
            }
            ResultDiscriminant::Err => {
                if let Some(ty) = &self.err {
                    v.newtype_variant_seed(ty)
                        .map(|v| Self::Value::new(ResultValue(Err(Some(v)))))
                } else {
                    v.unit_variant()
                        .map(|()| Self::Value::new(ResultValue(Err(None))))
                }
            }
        }
    }
}

macro_rules! impl_value_from {
    ($t:ty, $c:ident) => {
        impl From<$t> for Value {
            #[inline]
            fn from(v: $t) -> Self {
                Self::$c(v)
            }
        }
    };
}

impl_value_from!(bool, Bool);
impl_value_from!(u8, U8);
impl_value_from!(u16, U16);
impl_value_from!(u32, U32);
impl_value_from!(u64, U64);
impl_value_from!(i8, S8);
impl_value_from!(i16, S16);
impl_value_from!(i32, S32);
impl_value_from!(i64, S64);
impl_value_from!(f32, F32);
impl_value_from!(f64, F64);
impl_value_from!(char, Char);
impl_value_from!(String, String);
impl_value_from!(reflect::RecordValue, Record);
impl_value_from!(reflect::VariantValue, Variant);
impl_value_from!(reflect::List, List);
impl_value_from!(reflect::TupleValue, Tuple);
impl_value_from!(reflect::OptionValue, Option);
impl_value_from!(reflect::ResultValue, Result);

impl From<Vec<u8>> for Value {
    #[inline]
    fn from(v: Vec<u8>) -> Self {
        List::from(v).into()
    }
}

impl From<serde_bytes::ByteBuf> for Value {
    #[inline]
    fn from(v: serde_bytes::ByteBuf) -> Self {
        v.into_vec().into()
    }
}

impl From<Option<Value>> for Value {
    #[inline]
    fn from(v: Option<Value>) -> Self {
        reflect::OptionValue::from(v).into()
    }
}

impl From<Result<Option<Value>, Option<Value>>> for Value {
    #[inline]
    fn from(v: Result<Option<Value>, Option<Value>>) -> Self {
        reflect::ResultValue::from(v).into()
    }
}

impl ser::Serialize for Value {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        match self {
            Self::Bool(v) => v.serialize(serializer),
            Self::U8(v) => v.serialize(serializer),
            Self::S8(v) => v.serialize(serializer),
            Self::U16(v) => v.serialize(serializer),
            Self::S16(v) => v.serialize(serializer),
            Self::U32(v) => v.serialize(serializer),
            Self::S32(v) => v.serialize(serializer),
            Self::U64(v) => v.serialize(serializer),
            Self::S64(v) => v.serialize(serializer),
            Self::F32(v) => v.serialize(serializer),
            Self::F64(v) => v.serialize(serializer),
            Self::Char(v) => v.serialize(serializer),
            Self::String(v) => v.serialize(serializer),
            Self::Record(v) => v.serialize(serializer),
            Self::Variant(v) => v.serialize(serializer),
            Self::List(v) => v.serialize(serializer),
            Self::Tuple(v) => v.serialize(serializer),
            Self::Flags(v) => v.serialize(serializer),
            Self::Enum(v) => v.serialize(serializer),
            Self::Option(v) => v.serialize(serializer),
            Self::Result(v) => v.serialize(serializer),
        }
    }
}

impl Value {
    pub fn invalid_type<E: de::Error>(self, exp: &dyn de::Expected) -> E {
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
            Self::Record(..) => E::invalid_type(de::Unexpected::Other("record"), exp),
            Self::Variant(..) => E::invalid_type(de::Unexpected::Other("variant"), exp),
            Self::List(..) => E::invalid_type(de::Unexpected::Other("list"), exp),
            Self::Tuple(..) => E::invalid_type(de::Unexpected::Other("tuple"), exp),
            Self::Flags(..) => E::invalid_type(de::Unexpected::Other("flags"), exp),
            Self::Enum(..) => E::invalid_type(de::Unexpected::Other("enum"), exp),
            Self::Option(..) => E::invalid_type(de::Unexpected::Other("option"), exp),
            Self::Result(..) => E::invalid_type(de::Unexpected::Other("result"), exp),
        }
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct RecordValue(Vec<Value>);

impl Deref for RecordValue {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestRecordValue for RecordValue {
    #[inline]
    fn new(fields: Vec<Value>) -> Self {
        Self(fields)
    }

    #[inline]
    fn into_value(this: reflect::RecordValue) -> Vec<Value> {
        let Self(fields) = this.into_inner();
        fields
    }
}

impl ser::Serialize for &RecordValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl ser::Serialize for reflect::RecordValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.get::<RecordValue>().serialize(serializer)
    }
}

#[derive(Debug)]
pub struct VariantValue(u32, Option<Value>);

impl GuestVariantValue for VariantValue {
    #[inline]
    fn new(case: u32, payload: Option<Value>) -> Self {
        Self(case, payload)
    }

    #[inline]
    fn into_value(this: reflect::VariantValue) -> (u32, Option<Value>) {
        let Self(case, payload) = this.into_inner::<Self>();
        (case, payload)
    }
}

impl ser::Serialize for &VariantValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        match self {
            VariantValue(case, None) => case.serialize(serializer),
            VariantValue(case, Some(payload)) => (case, payload).serialize(serializer),
        }
    }
}

impl ser::Serialize for reflect::VariantValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.get::<VariantValue>().serialize(serializer)
    }
}

#[repr(transparent)]
pub struct ListOfListsValue(pub Vec<List>);

impl GuestListOfListsValue for ListOfListsValue {
    #[inline]
    fn new(lists: Vec<List>) -> Self {
        Self(lists)
    }

    #[inline]
    fn into_list(this: reflect::ListOfListsValue) -> Vec<List> {
        let Self(lists) = this.into_inner();
        lists
    }
}

impl ser::Serialize for &ListOfListsValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.0.len()))?;
        for v in &self.0 {
            seq.serialize_element(&v)?;
        }
        seq.end()
    }
}

impl ser::Serialize for reflect::ListOfListsValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.get::<ListOfListsValue>().serialize(serializer)
    }
}

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
impl_list_from!(String, String);
impl_list_from!(reflect::RecordValue, Record);
impl_list_from!(reflect::VariantValue, Variant);
impl_list_from!(reflect::TupleValue, Tuple);
impl_list_from!(reflect::OptionValue, Option);
impl_list_from!(reflect::ResultValue, Result);

impl From<reflect::ListOfListsValue> for List {
    #[inline]
    fn from(v: reflect::ListOfListsValue) -> Self {
        Self::List(v)
    }
}

impl From<Vec<List>> for List {
    #[inline]
    fn from(v: Vec<List>) -> Self {
        reflect::ListOfListsValue::new(ListOfListsValue(v)).into()
    }
}

impl List {
    #[inline]
    pub fn reverse(&mut self) {
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
            Self::String(vs) => vs.reverse(),
            Self::Record(vs) => vs.reverse(),
            Self::Variant(vs) => vs.reverse(),
            Self::List(vs) => vs.get_mut::<ListOfListsValue>().0.reverse(),
            Self::Tuple(vs) => vs.reverse(),
            Self::Flags(vs) => vs.reverse(),
            Self::Enum(vs) => vs.reverse(),
            Self::Option(vs) => vs.reverse(),
            Self::Result(vs) => vs.reverse(),
        }
    }
}

impl ser::Serialize for &List {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        match self {
            List::Bool(vs) => vs.serialize(serializer),
            List::U8(vs) => vs.serialize(serializer),
            List::S8(vs) => vs.serialize(serializer),
            List::U16(vs) => vs.serialize(serializer),
            List::S16(vs) => vs.serialize(serializer),
            List::U32(vs) => vs.serialize(serializer),
            List::S32(vs) => vs.serialize(serializer),
            List::U64(vs) => vs.serialize(serializer),
            List::S64(vs) => vs.serialize(serializer),
            List::F32(vs) => vs.serialize(serializer),
            List::F64(vs) => vs.serialize(serializer),
            List::Char(vs) => vs.serialize(serializer),
            List::String(vs) => vs.serialize(serializer),
            List::Record(vs) => vs.serialize(serializer),
            List::Variant(vs) => vs.serialize(serializer),
            List::List(vs) => vs.serialize(serializer),
            List::Tuple(vs) => vs.serialize(serializer),
            List::Flags(vs) => vs.serialize(serializer),
            List::Enum(vs) => vs.serialize(serializer),
            List::Option(vs) => vs.serialize(serializer),
            List::Result(vs) => vs.serialize(serializer),
        }
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct TupleValue(Vec<Value>);

impl Deref for TupleValue {
    type Target = Vec<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestTupleValue for TupleValue {
    #[inline]
    fn new(values: Vec<Value>) -> Self {
        Self(values)
    }

    #[inline]
    fn into_value(this: reflect::TupleValue) -> Vec<Value> {
        let Self(values) = this.into_inner();
        values
    }
}

impl ser::Serialize for &TupleValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl ser::Serialize for reflect::TupleValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.get::<TupleValue>().serialize(serializer)
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct OptionValue(Option<Value>);

impl Deref for OptionValue {
    type Target = Option<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestOptionValue for OptionValue {
    #[inline]
    fn new(v: Option<Value>) -> Self {
        Self(v)
    }

    #[inline]
    fn into_value(this: reflect::OptionValue) -> Option<Value> {
        let Self(v) = this.into_inner();
        v
    }
}

impl From<Option<Value>> for OptionValue {
    #[inline]
    fn from(v: Option<Value>) -> Self {
        Self(v)
    }
}

impl From<Option<Value>> for reflect::OptionValue {
    #[inline]
    fn from(v: Option<Value>) -> Self {
        Self::new(OptionValue::from(v))
    }
}

impl ser::Serialize for &OptionValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl ser::Serialize for reflect::OptionValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.get::<OptionValue>().serialize(serializer)
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct ResultValue(Result<Option<Value>, Option<Value>>);

impl Deref for ResultValue {
    type Target = Result<Option<Value>, Option<Value>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GuestResultValue for ResultValue {
    #[inline]
    fn new(v: Result<Option<Value>, Option<Value>>) -> Self {
        Self(v)
    }

    #[inline]
    fn into_value(this: reflect::ResultValue) -> Result<Option<Value>, Option<Value>> {
        let Self(v) = this.into_inner();
        v
    }
}

impl From<Result<Option<Value>, Option<Value>>> for ResultValue {
    #[inline]
    fn from(v: Result<Option<Value>, Option<Value>>) -> Self {
        Self(v)
    }
}

impl From<Result<Option<Value>, Option<Value>>> for reflect::ResultValue {
    #[inline]
    fn from(v: Result<Option<Value>, Option<Value>>) -> Self {
        Self::new(ResultValue::from(v))
    }
}

impl ser::Serialize for &ResultValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        match &self.0 {
            Ok(None) => {
                #[derive(Serialize)]
                #[serde(rename_all = "lowercase")]
                enum Result {
                    Ok,
                }
                Result::Ok.serialize(serializer)
            }
            Ok(Some(v)) => {
                #[derive(Serialize)]
                #[serde(rename_all = "lowercase")]
                enum Result<'a> {
                    Ok(&'a Value),
                }
                Result::Ok(v).serialize(serializer)
            }
            Err(None) => {
                #[derive(Serialize)]
                #[serde(rename_all = "lowercase")]
                enum Result {
                    Err,
                }
                Result::Err.serialize(serializer)
            }
            Err(Some(v)) => {
                #[derive(Serialize)]
                #[serde(rename_all = "lowercase")]
                enum Result<'a> {
                    Err(&'a Value),
                }
                Result::Err(v).serialize(serializer)
            }
        }
    }
}

impl ser::Serialize for reflect::ResultValue {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        self.get::<ResultValue>().serialize(serializer)
    }
}
