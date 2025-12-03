use serde_core::Serialize as _;
use serde_core::de::{DeserializeSeed as _, Error as _};
use wasm_serde::bindings::exports::rvolosatovs::serde::reflect;

struct Component;

wasm_serde::bindings::export!(Component with_types_in wasm_serde::bindings);

impl wasm_serde::Serializer for Component {
    #[inline]
    fn from_value(v: &reflect::Value) -> Vec<u8> {
        let mut buf = toml::ser::Buffer::new();
        let ser = toml::Serializer::new(&mut buf);
        v.serialize(ser).expect("failed to serialize");
        buf.to_string().into()
    }
}

impl wasm_serde::Deserializer for Component {
    type Error = toml::de::Error;

    #[inline]
    fn from_list(buf: Vec<u8>, ty: wasm_serde::Type) -> Result<reflect::Value, Self::Error> {
        let s = str::from_utf8(&buf).map_err(Self::Error::custom)?;
        let src = toml::Deserializer::parse(s)?;
        ty.deserialize(src)
    }
}
