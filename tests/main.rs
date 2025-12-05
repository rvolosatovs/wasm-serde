pub mod de;
pub mod ser;

use core::fmt;

use std::path::Path;
use std::process::{Command, ExitStatus};
use std::sync::LazyLock;

use wasmtime::component::{Component, Linker};
use wasmtime::{Engine, Store};
use wit_component::ComponentEncoder;

pub mod bindings {
    wasmtime::component::bindgen!({
        world: "format",
    });
}
use bindings::exports::cosmonic::serde::reflect;
use bindings::exports::cosmonic::serde::reflect::{List, ListType, Type};

static BUILD_COMPONENTS: LazyLock<ExitStatus> = LazyLock::new(|| {
    Command::new(env!("CARGO"))
        .args(["build", "--workspace", "--target", "wasm32-unknown-unknown"])
        .status()
        .expect("failed to build components")
});

pub static ENGINE: LazyLock<Engine> = LazyLock::new(Engine::default);

fn compile_codec(path: impl AsRef<Path>) -> bindings::FormatPre<()> {
    let buf = std::fs::read(path).expect("failed to read component");
    let codec = ComponentEncoder::default()
        .module(&buf)
        .expect("failed to set core module")
        .encode()
        .expect("failed to encode component");

    let codec = Component::new(&ENGINE, codec).expect("failed to compile component");
    let linker = Linker::new(&ENGINE);
    let codec = linker
        .instantiate_pre(&codec)
        .expect("failed to pre-instantiate component");
    bindings::FormatPre::new(codec).expect("failed to wrap component")
}

pub static CBOR: LazyLock<bindings::FormatPre<()>> = LazyLock::new(|| {
    assert!(BUILD_COMPONENTS.success());
    compile_codec("./target/wasm32-unknown-unknown/debug/wasm_serde_cbor.wasm")
});

pub static JSON: LazyLock<bindings::FormatPre<()>> = LazyLock::new(|| {
    assert!(BUILD_COMPONENTS.success());
    compile_codec("./target/wasm32-unknown-unknown/debug/wasm_serde_json.wasm")
});

pub static TOML: LazyLock<bindings::FormatPre<()>> = LazyLock::new(|| {
    assert!(BUILD_COMPONENTS.success());
    compile_codec("./target/wasm32-unknown-unknown/debug/wasm_serde_toml.wasm")
});

pub struct Value<'a> {
    pub store: &'a mut Store<()>,
    pub reflect: &'a reflect::Guest,
    pub value: reflect::Value,
}

impl fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Value")
            .field("store", &self.store)
            .field("value", &self.value)
            .finish()
    }
}

macro_rules! impl_unwrap_value_primitive {
    ($rt:ident, $f:ident, $f_list:ident, $t:ty) => {
        #[track_caller]
        pub fn $f(self) -> $t {
            let Self::$rt(v) = self else {
                panic!("invalid value type");
            };
            v
        }

        #[track_caller]
        pub fn $f_list(self) -> Vec<$t> {
            let Self::List(List::$rt(vs)) = self else {
                panic!("invalid value list type");
            };
            vs
        }
    };
}

impl reflect::Value {
    impl_unwrap_value_primitive!(Bool, unwrap_bool, unwrap_list_bool, bool);
    impl_unwrap_value_primitive!(U8, unwrap_u8, unwrap_list_u8, u8);
    impl_unwrap_value_primitive!(U16, unwrap_u16, unwrap_list_u16, u16);
    impl_unwrap_value_primitive!(U32, unwrap_u32, unwrap_list_u32, u32);
    impl_unwrap_value_primitive!(U64, unwrap_u64, unwrap_list_u64, u64);
    impl_unwrap_value_primitive!(S8, unwrap_s8, unwrap_list_s8, i8);
    impl_unwrap_value_primitive!(S16, unwrap_s16, unwrap_list_s16, i16);
    impl_unwrap_value_primitive!(S32, unwrap_s32, unwrap_list_s32, i32);
    impl_unwrap_value_primitive!(S64, unwrap_s64, unwrap_list_s64, i64);
    impl_unwrap_value_primitive!(F32, unwrap_f32, unwrap_list_f32, f32);
    impl_unwrap_value_primitive!(F64, unwrap_f64, unwrap_list_f64, f64);
    impl_unwrap_value_primitive!(Char, unwrap_char, unwrap_list_char, char);
    impl_unwrap_value_primitive!(String, unwrap_string, unwrap_list_string, String);
    impl_unwrap_value_primitive!(Flags, unwrap_flags, unwrap_list_flags, u32);
    impl_unwrap_value_primitive!(Enum, unwrap_enum, unwrap_list_enum, u32);

    #[track_caller]
    pub fn unwrap_record(self, store: &mut Store<()>, reflect: &reflect::Guest) -> Vec<Self> {
        let Self::Record(v) = self else {
            panic!("invalid value type `{self:?}`");
        };
        reflect.record_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    pub fn unwrap_tuple(self, store: &mut Store<()>, reflect: &reflect::Guest) -> Vec<Self> {
        let Self::Tuple(v) = self else {
            panic!("invalid value type `{self:?}`");
        };
        reflect.tuple_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    pub fn unwrap_list(self) -> List {
        let Self::List(v) = self else {
            panic!("invalid value type `{self:?}`");
        };
        v
    }

    #[track_caller]
    pub fn unwrap_variant(
        self,
        store: &mut Store<()>,
        reflect: &reflect::Guest,
    ) -> (u32, Option<Self>) {
        let Self::Variant(v) = self else {
            panic!("invalid value type `{self:?}`");
        };
        reflect.variant_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    pub fn unwrap_option(self, store: &mut Store<()>, reflect: &reflect::Guest) -> Option<Self> {
        let Self::Option(v) = self else {
            panic!("invalid value type `{self:?}`");
        };
        reflect.option_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    pub fn unwrap_result(
        self,
        store: &mut Store<()>,
        reflect: &reflect::Guest,
    ) -> Result<Option<Self>, Option<Self>> {
        let Self::Result(v) = self else {
            panic!("invalid value type `{self:?}`");
        };
        reflect.result_value().call_into_value(store, v).unwrap()
    }
}

macro_rules! impl_unwrap_deserializer_primitive {
    ($f:ident, $f_list:ident, $t:ty) => {
        #[track_caller]
        pub fn $f(self) -> $t {
            self.value.$f()
        }

        #[track_caller]
        pub fn $f_list(self) -> Vec<$t> {
            self.value.$f_list()
        }
    };
}

impl<'a> Value<'a> {
    impl_unwrap_deserializer_primitive!(unwrap_bool, unwrap_list_bool, bool);
    impl_unwrap_deserializer_primitive!(unwrap_u8, unwrap_list_u8, u8);
    impl_unwrap_deserializer_primitive!(unwrap_u16, unwrap_list_u16, u16);
    impl_unwrap_deserializer_primitive!(unwrap_u32, unwrap_list_u32, u32);
    impl_unwrap_deserializer_primitive!(unwrap_u64, unwrap_list_u64, u64);
    impl_unwrap_deserializer_primitive!(unwrap_s8, unwrap_list_s8, i8);
    impl_unwrap_deserializer_primitive!(unwrap_s16, unwrap_list_s16, i16);
    impl_unwrap_deserializer_primitive!(unwrap_s32, unwrap_list_s32, i32);
    impl_unwrap_deserializer_primitive!(unwrap_s64, unwrap_list_s64, i64);
    impl_unwrap_deserializer_primitive!(unwrap_f32, unwrap_list_f32, f32);
    impl_unwrap_deserializer_primitive!(unwrap_f64, unwrap_list_f64, f64);
    impl_unwrap_deserializer_primitive!(unwrap_char, unwrap_list_char, char);
    impl_unwrap_deserializer_primitive!(unwrap_string, unwrap_list_string, String);
    impl_unwrap_deserializer_primitive!(unwrap_enum, unwrap_list_enum, u32);
    impl_unwrap_deserializer_primitive!(unwrap_flags, unwrap_list_flags, u32);

    #[track_caller]
    pub fn unwrap_record(self) -> (Self, Vec<reflect::Value>) {
        let mut fields = self.value.unwrap_record(self.store, self.reflect);
        fields.reverse();
        let v = fields.pop().unwrap();
        (Self { value: v, ..self }, fields)
    }

    #[track_caller]
    pub fn unwrap_tuple(self) -> (Self, Vec<reflect::Value>) {
        let mut values = self.value.unwrap_tuple(self.store, self.reflect);
        values.reverse();
        let v = values.pop().unwrap();
        (Self { value: v, ..self }, values)
    }

    #[track_caller]
    pub fn unwrap_variant(self) -> (u32, Option<Self>) {
        let (case, payload) = self.value.unwrap_variant(self.store, self.reflect);
        (case, payload.map(|value| Self { value, ..self }))
    }

    #[track_caller]
    pub fn unwrap_option(self) -> Option<Self> {
        self.value
            .unwrap_option(self.store, self.reflect)
            .map(|value| Self { value, ..self })
    }

    #[track_caller]
    pub fn unwrap_result(self) -> Result<Option<Self>, Option<Self>> {
        match self.value.unwrap_result(self.store, self.reflect) {
            Ok(None) => Ok(None),
            Ok(Some(value)) => Ok(Some(Self { value, ..self })),
            Err(None) => Err(None),
            Err(Some(value)) => Err(Some(Self { value, ..self })),
        }
    }
}

#[track_caller]
pub fn serialize(
    codec: &bindings::FormatPre<()>,
    mk_val: impl FnOnce(&mut Store<()>, &reflect::Guest) -> reflect::Value,
) -> Vec<u8> {
    let mut store = Store::new(&ENGINE, ());
    let codec = codec
        .instantiate(&mut store)
        .expect("failed to instantiate codec");
    let val = mk_val(&mut store, codec.cosmonic_serde_reflect());
    codec
        .cosmonic_serde_serializer()
        .call_from_value(&mut store, &val)
        .unwrap()
}

#[macro_export]
macro_rules! mk_serialize {
    ($codec:ident) => {
        #[track_caller]
        fn serialize(
            mk_val: impl core::ops::FnOnce(&mut wasmtime::Store<()>, &$crate::bindings::exports::cosmonic::serde::reflect::Guest) -> crate::bindings::exports::cosmonic::serde::reflect::Value,
        ) -> std::vec::Vec<u8> {
            crate::serialize(&$codec, mk_val)
        }

        #[allow(dead_code)]
        #[track_caller]
        fn serialize_str(
            mk_val: impl core::ops::FnOnce(&mut wasmtime::Store<()>, &crate::bindings::exports::cosmonic::serde::reflect::Guest) -> crate::bindings::exports::cosmonic::serde::reflect::Value,
        ) -> String {
            String::from_utf8(serialize(mk_val)).unwrap()
        }
    };
}

#[track_caller]
pub fn deserialize<T>(
    codec: &bindings::FormatPre<()>,
    payload: impl AsRef<[u8]>,
    mk_ty: impl FnOnce(&mut Store<()>, &reflect::Guest) -> Type,
    f: impl FnOnce(Result<Value<'_>, String>) -> T,
) -> T {
    let mut store = Store::new(&ENGINE, ());
    let codec = codec
        .instantiate(&mut store)
        .expect("failed to instantiate codec");
    let ty = mk_ty(&mut store, codec.cosmonic_serde_reflect());
    match codec
        .cosmonic_serde_deserializer()
        .call_from_list(&mut store, payload.as_ref(), ty)
        .unwrap()
    {
        Ok(de) => f(Ok(Value {
            store: &mut store,
            reflect: codec.cosmonic_serde_reflect(),
            value: de,
        })),
        Err(err) => {
            let err = codec
                .cosmonic_serde_deserializer()
                .error()
                .call_to_string(&mut store, err)
                .unwrap();
            f(Err(err))
        }
    }
}

#[macro_export]
macro_rules! mk_deserialize {
    ($codec:ident) => {
        #[track_caller]
        fn deserialize<T>(
            payload: impl core::convert::AsRef<[u8]>,
            mk_ty: impl core::ops::FnOnce(
                &mut wasmtime::Store<()>,
                &$crate::bindings::exports::cosmonic::serde::reflect::Guest,
            ) -> crate::bindings::exports::cosmonic::serde::reflect::Type,
            f: impl core::ops::FnOnce(core::result::Result<crate::Value<'_>, std::string::String>) -> T,
        ) -> T {
            crate::deserialize(&$codec, payload, mk_ty, f)
        }
    };
}

#[track_caller]
pub fn mk_foo_bar_variant_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de
        .variant_type()
        .call_constructor(
            store,
            &[("foo".into(), None), ("bar".into(), Some(Type::U32))],
        )
        .unwrap();
    Type::Variant(ty)
}

#[track_caller]
pub fn mk_foo_bar_enum_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de
        .enum_type()
        .call_constructor(store, &["foo".into(), "bar".into()])
        .unwrap();
    Type::Enum(ty)
}

#[track_caller]
pub fn mk_a_b_c_d_flags_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de
        .flags_type()
        .call_constructor(store, &["a".into(), "b".into(), "c".into(), "d".into()])
        .unwrap();
    Type::Flags(ty)
}

#[track_caller]
pub fn mk_option_u32_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de.option_type().call_constructor(store, Type::U32).unwrap();
    Type::Option(ty)
}

#[track_caller]
pub fn mk_record_complex_type(mut store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let empty_result = de
        .result_type()
        .call_constructor(&mut store, None, None)
        .unwrap();

    let d_c_ty = de
        .flags_type()
        .call_constructor(&mut store, &["foo".into(), "bar".into(), "baz".into()])
        .unwrap();
    let b_ty = de
        .tuple_type()
        .call_constructor(
            &mut store,
            &[
                Type::U8,
                Type::U16,
                Type::U32,
                Type::U64,
                Type::S8,
                Type::S16,
                Type::S32,
                Type::S64,
                Type::F32,
                Type::F64,
            ],
        )
        .unwrap();
    let d_ty = de
        .record_type()
        .call_constructor(
            &mut store,
            &[
                ("d_a".into(), Type::List(ListType::U8)),
                ("d_b".into(), Type::List(ListType::String)),
                ("d_c".into(), Type::Flags(d_c_ty)),
            ],
        )
        .unwrap();
    let f_ty = de
        .variant_type()
        .call_constructor(
            &mut store,
            &[("none".into(), None), ("some".into(), Some(Type::String))],
        )
        .unwrap();
    let g_ty = f_ty;
    let h_ty = de
        .enum_type()
        .call_constructor(&mut store, &["no".into(), "yes".into()])
        .unwrap();
    let i_ty = de
        .flags_type()
        .call_constructor(&mut store, &["foo".into(), "bar".into(), "baz".into()])
        .unwrap();
    let j_ty = de
        .option_type()
        .call_constructor(&mut store, Type::Char)
        .unwrap();
    let k_ok_ty = de
        .tuple_type()
        .call_constructor(
            &mut store,
            &[Type::Result(empty_result), Type::Result(empty_result)],
        )
        .unwrap();
    let k_ty = de
        .result_type()
        .call_constructor(&mut store, Some(Type::Tuple(k_ok_ty)), None)
        .unwrap();
    let l_err_ty = k_ok_ty;
    let l_ty = de
        .result_type()
        .call_constructor(&mut store, None, Some(Type::Tuple(l_err_ty)))
        .unwrap();

    let ty = de
        .record_type()
        .call_constructor(
            store,
            &[
                ("a".into(), Type::Bool),
                ("b".into(), Type::Tuple(b_ty)),
                ("c".into(), Type::String),
                ("d".into(), Type::Record(d_ty)),
                ("e".into(), Type::Char),
                ("f".into(), Type::Variant(f_ty)),
                ("g".into(), Type::Variant(g_ty)),
                ("h".into(), Type::Enum(h_ty)),
                ("i".into(), Type::Flags(i_ty)),
                ("j".into(), Type::Option(j_ty)),
                ("k".into(), Type::Result(k_ty)),
                ("l".into(), Type::Result(l_ty)),
            ],
        )
        .unwrap();
    Type::Record(ty)
}
