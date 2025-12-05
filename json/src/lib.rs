use serde_core::Serialize as _;
use serde_core::de::DeserializeSeed as _;
use wasm_serde::bindings::exports::cosmonic::reflect::reflect;

struct Component;

wasm_serde::bindings::export!(Component with_types_in wasm_serde::bindings);

impl wasm_serde::Serializer for Component {
    #[inline]
    fn from_value(v: &reflect::Value) -> Vec<u8> {
        let mut ser = serde_json::Serializer::new(Vec::default());
        v.serialize(&mut ser).expect("failed to serialize");
        ser.into_inner()
    }
}

impl wasm_serde::Deserializer for Component {
    type Error = serde_json::Error;

    #[inline]
    fn from_list(buf: Vec<u8>, ty: wasm_serde::Type) -> Result<reflect::Value, Self::Error> {
        let mut src = serde_json::Deserializer::from_slice(&buf);
        ty.deserialize(&mut src)
    }
}
