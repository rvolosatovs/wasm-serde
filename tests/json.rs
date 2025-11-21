use std::process::Command;
use std::sync::LazyLock;

use wasmtime::component::{Component, Linker, ResourceAny};
use wasmtime::{Engine, Store};
use wit_component::ComponentEncoder;

mod bindings {
    wasmtime::component::bindgen!();
}
use bindings::exports::rvolosatovs::serde::reflect::Type;

static ENGINE: LazyLock<Engine> = LazyLock::new(Engine::default);

static CODEC: LazyLock<bindings::FormatPre<()>> = LazyLock::new(|| {
    let status = Command::new("cargo")
        .args([
            "build",
            "--manifest-path",
            "./json/Cargo.toml",
            "--target",
            "wasm32-unknown-unknown",
        ])
        .status()
        .expect("failed to build `json` component");
    assert!(status.success());
    let buf = std::fs::read("./json/target/wasm32-unknown-unknown/debug/wasm_serde_json.wasm")
        .expect("failed to read component");
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
});

struct Deserializer<'a> {
    store: &'a mut Store<()>,
    codec: &'a bindings::exports::rvolosatovs::serde::deserializer::Guest,
    reflect: &'a bindings::exports::rvolosatovs::serde::reflect::Guest,
    de: ResourceAny,
}

impl<'a> Deserializer<'a> {
    fn with(&mut self, de: ResourceAny) -> Deserializer<'_> {
        Deserializer {
            store: self.store,
            codec: self.codec,
            reflect: self.reflect,
            de,
        }
    }

    #[track_caller]
    fn string_from_error(&mut self, err: ResourceAny) -> String {
        self.codec
            .error()
            .call_to_string(&mut self.store, err)
            .unwrap()
    }

    #[track_caller]
    fn mk_string_from_error(&mut self) -> impl FnOnce(ResourceAny) -> String {
        |err| self.string_from_error(err)
    }

    #[track_caller]
    fn record_type(&mut self, fields: &[(String, Type)]) -> ResourceAny {
        self.reflect
            .record_type()
            .call_constructor(&mut self.store, fields)
            .unwrap()
    }

    #[track_caller]
    fn variant_type(&mut self, cases: &[(String, Option<Type>)]) -> ResourceAny {
        self.reflect
            .variant_type()
            .call_constructor(&mut self.store, cases)
            .unwrap()
    }

    #[track_caller]
    fn list_type(&mut self, ty: Type) -> ResourceAny {
        self.reflect
            .list_type()
            .call_constructor(&mut self.store, ty)
            .unwrap()
    }

    #[track_caller]
    fn tuple_type(&mut self, types: &[Type]) -> ResourceAny {
        self.reflect
            .tuple_type()
            .call_constructor(&mut self.store, types)
            .unwrap()
    }

    #[track_caller]
    fn flags_type(&mut self, cases: &[String]) -> ResourceAny {
        self.reflect
            .flags_type()
            .call_constructor(&mut self.store, cases)
            .unwrap()
    }

    #[track_caller]
    fn enum_type(&mut self, cases: &[String]) -> ResourceAny {
        self.reflect
            .enum_type()
            .call_constructor(&mut self.store, cases)
            .unwrap()
    }

    #[track_caller]
    fn option_type(&mut self, ty: Type) -> ResourceAny {
        self.reflect
            .option_type()
            .call_constructor(&mut self.store, ty)
            .unwrap()
    }

    #[track_caller]
    fn result_type(&mut self, ok: Option<Type>, err: Option<Type>) -> ResourceAny {
        self.reflect
            .result_type()
            .call_constructor(&mut self.store, ok, err)
            .unwrap()
    }

    #[track_caller]
    fn deserialize_record(
        &mut self,
        ty: ResourceAny,
    ) -> Result<(u32, ResourceAny, ResourceAny), String> {
        self.codec
            .deserializer()
            .call_deserialize_record(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_tuple(&mut self, ty: ResourceAny) -> Result<(ResourceAny, ResourceAny), String> {
        self.codec
            .deserializer()
            .call_deserialize_tuple(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_list(&mut self, ty: Type) -> Result<ResourceAny, String> {
        self.codec
            .deserializer()
            .call_deserialize_list(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_list_flat<T: Deserialize>(&mut self) -> Result<Vec<T>, String> {
        let els = self.deserialize_list(T::ty())?;
        let mut vs = Vec::new();
        let mut de = ListIter(self.with(els));
        while let Some(v_de) = de.next() {
            let v = T::deserialize(de.0.with(v_de))?;
            vs.push(v);
        }
        Ok(vs)
    }

    #[track_caller]
    fn deserialize_variant(&mut self, ty: ResourceAny) -> Result<(u32, ResourceAny), String> {
        self.codec
            .deserializer()
            .call_deserialize_variant(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_flags(&mut self, ty: ResourceAny) -> Result<u32, String> {
        self.codec
            .deserializer()
            .call_deserialize_flags(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_enum(&mut self, ty: ResourceAny) -> Result<u32, String> {
        self.codec
            .deserializer()
            .call_deserialize_enum(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_option(&mut self, ty: Type) -> Result<Option<ResourceAny>, String> {
        self.codec
            .deserializer()
            .call_deserialize_option(&mut self.store, self.de, ty)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_result(
        &mut self,
        ok: Option<Type>,
        err: Option<Type>,
    ) -> Result<Result<ResourceAny, ResourceAny>, String> {
        self.codec
            .deserializer()
            .call_deserialize_result(&mut self.store, self.de, ok, err)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }
}

trait Deserialize {
    fn ty() -> Type;

    fn deserialize(de: Deserializer<'_>) -> Result<Self, String>
    where
        Self: Sized;
}

macro_rules! impl_deserialize_flat {
    ($t:ty, $f:ident, $rt:ident) => {
        impl Deserialize for $t {
            fn ty() -> Type {
                Type::$rt
            }

            #[track_caller]
            fn deserialize(mut de: Deserializer<'_>) -> Result<Self, String> {
                de.codec
                    .deserializer()
                    .$f(&mut de.store, de.de)
                    .unwrap()
                    .map_err(de.mk_string_from_error())
            }
        }
    };
}

impl_deserialize_flat!(bool, call_deserialize_bool, Bool);
impl_deserialize_flat!(u8, call_deserialize_u8, U8);
impl_deserialize_flat!(u16, call_deserialize_u16, U16);
impl_deserialize_flat!(u32, call_deserialize_u32, U32);
impl_deserialize_flat!(u64, call_deserialize_u64, U64);
impl_deserialize_flat!(i8, call_deserialize_s8, S8);
impl_deserialize_flat!(i16, call_deserialize_s16, S16);
impl_deserialize_flat!(i32, call_deserialize_s32, S32);
impl_deserialize_flat!(i64, call_deserialize_s64, S64);
impl_deserialize_flat!(f32, call_deserialize_f32, F32);
impl_deserialize_flat!(f64, call_deserialize_f64, F64);
impl_deserialize_flat!(char, call_deserialize_char, Char);
impl_deserialize_flat!(String, call_deserialize_string, String);
impl_deserialize_flat!(Vec<u8>, call_deserialize_bytes, Bytes);

struct RecordIter<'a>(Deserializer<'a>);

impl Iterator for RecordIter<'_> {
    type Item = (u32, ResourceAny);

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let (idx, de, next) = self
            .0
            .codec
            .record_deserializer()
            .call_next(&mut self.0.store, self.0.de)
            .unwrap();
        self.0.de = next;
        Some((idx, de))
    }
}

struct ListIter<'a>(Deserializer<'a>);

impl Iterator for ListIter<'_> {
    type Item = ResourceAny;

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let (de, next) = self
            .0
            .codec
            .list_deserializer()
            .call_next(&mut self.0.store, self.0.de)
            .unwrap()?;
        self.0.de = next;
        Some(de)
    }
}

struct TupleIter<'a>(Deserializer<'a>);

impl Iterator for TupleIter<'_> {
    type Item = ResourceAny;

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let (de, next) = self
            .0
            .codec
            .tuple_deserializer()
            .call_next(&mut self.0.store, self.0.de)
            .unwrap();
        self.0.de = next;
        Some(de)
    }
}

#[track_caller]
fn with_deserializer<T>(payload: impl AsRef<[u8]>, f: impl FnOnce(Deserializer<'_>) -> T) -> T {
    let mut store = Store::new(&ENGINE, ());
    let codec = CODEC
        .instantiate(&mut store)
        .expect("failed to instantiate codec");
    let de = codec
        .rvolosatovs_serde_deserializer()
        .deserializer()
        .call_from_list(&mut store, payload.as_ref())
        .unwrap();
    f(Deserializer {
        store: &mut store,
        codec: codec.rvolosatovs_serde_deserializer(),
        reflect: codec.rvolosatovs_serde_reflect(),
        de,
    })
}

#[test]
fn bool_true() {
    with_deserializer("true", |de| {
        let v = bool::deserialize(de).unwrap();
        assert_eq!(v, true);
    })
}

#[test]
fn bool_false() {
    with_deserializer("false", |de| {
        let v = bool::deserialize(de).unwrap();
        assert_eq!(v, false);
    })
}

#[test]
fn bool_invalid() {
    with_deserializer("0", |de| {
        let err = bool::deserialize(de).unwrap_err();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected a boolean at line 1 column 1"
        );
    })
}

#[test]
fn char_tree_emoji() {
    with_deserializer(r#""🌳""#, |de| {
        let v = char::deserialize(de).unwrap();
        assert_eq!(v, '🌳');
    })
}

#[test]
fn char_invalid() {
    with_deserializer("0", |de| {
        let err = char::deserialize(de).unwrap_err();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected a character at line 1 column 1"
        );
    })
}

#[test]
fn bytes_array() {
    with_deserializer("[0, 1, 2]", |de| {
        let v = <Vec<u8>>::deserialize(de).unwrap();
        assert_eq!(v, [0, 1, 2]);
    })
}

#[test]
fn bytes_string() {
    with_deserializer(r#""test""#, |de| {
        let v = <Vec<u8>>::deserialize(de).unwrap();
        assert_eq!(v, b"test");
    })
}

#[test]
fn bytes_invalid() {
    with_deserializer("0", |de| {
        let err = <Vec<u8>>::deserialize(de).unwrap_err();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected byte array at line 1 column 1"
        );
    })
}

#[test]
fn string_ascii() {
    with_deserializer(r#""test""#, |de| {
        let v = String::deserialize(de).unwrap();
        assert_eq!(v, "test");
    })
}

#[test]
fn string_invalid() {
    with_deserializer("0", |de| {
        let err = String::deserialize(de).unwrap_err();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected a string at line 1 column 1"
        );
    })
}

#[test]
fn variant_empty_string() {
    with_deserializer(r#""foo""#, |mut de| {
        let ty = de.variant_type(&[("foo".into(), None), ("bar".into(), Some(Type::U32))]);
        let (v, _) = de.deserialize_variant(ty).unwrap();
        assert_eq!(v, 0);
    })
}

#[test]
fn variant_unknown_string() {
    with_deserializer(r#""baz""#, |mut de| {
        let ty = de.variant_type(&[("foo".into(), None), ("bar".into(), Some(Type::U32))]);
        let err = de.deserialize_variant(ty).unwrap_err();
        assert_eq!(
            err,
            r#"unknown variant case `baz`, expected one of `["foo", "bar"]`"#
        );
    })
}

#[test]
fn variant_empty_object() {
    with_deserializer(r#"{"foo": null}"#, |mut de| {
        let ty = de.variant_type(&[("foo".into(), None), ("bar".into(), Some(Type::U32))]);
        let (v, _de) = de.deserialize_variant(ty).unwrap();
        assert_eq!(v, 0);
    })
}

#[test]
fn variant_payload_object() {
    with_deserializer(r#"{"bar": 42}"#, |mut de| {
        let ty = de.variant_type(&[("foo".into(), None), ("bar".into(), Some(Type::U32))]);
        let (v, v_de) = de.deserialize_variant(ty).unwrap();
        assert_eq!(v, 1);
        let v = u32::deserialize(de.with(v_de)).unwrap();
        assert_eq!(v, 42);
    })
}

#[test]
fn variant_unknown_object() {
    with_deserializer(r#"{"baz": null}"#, |mut de| {
        let ty = de.variant_type(&[("foo".into(), None), ("bar".into(), Some(Type::U32))]);
        let err = de.deserialize_variant(ty).unwrap_err();
        assert_eq!(
            err,
            r#"unknown variant case `baz`, expected one of `["foo", "bar"]`"#
        );
    })
}

#[test]
fn enum_string() {
    with_deserializer(r#""bar""#, |mut de| {
        let ty = de.enum_type(&["foo".into(), "bar".into()]);
        let v = de.deserialize_enum(ty).unwrap();
        assert_eq!(v, 1);
    })
}

#[test]
fn enum_unknown_string() {
    with_deserializer(r#""baz""#, |mut de| {
        let ty = de.enum_type(&["foo".into(), "bar".into()]);
        let err = de.deserialize_enum(ty).unwrap_err();
        assert_eq!(
            err,
            r#"unknown enum case `baz`, expected one of `["foo", "bar"]`"#
        );
    })
}

#[test]
fn enum_object() {
    with_deserializer(r#"{"bar": null}"#, |mut de| {
        let ty = de.enum_type(&["foo".into(), "bar".into()]);
        let v = de.deserialize_enum(ty).unwrap();
        assert_eq!(v, 1);
    })
}

#[test]
fn enum_unknown_object() {
    with_deserializer(r#"{"baz": null}"#, |mut de| {
        let ty = de.enum_type(&["foo".into(), "bar".into()]);
        let err = de.deserialize_enum(ty).unwrap_err();
        assert_eq!(
            err,
            r#"unknown enum case `baz`, expected one of `["foo", "bar"]`"#
        );
    })
}

#[test]
fn flags_array() {
    with_deserializer(r#"[true, false, false, true]"#, |mut de| {
        let ty = de.flags_type(&["a".into(), "b".into(), "c".into(), "d".into()]);
        let v = de.deserialize_flags(ty).unwrap();
        assert_eq!(v, 0b1001);
    })
}

#[test]
fn flags_empty_array() {
    with_deserializer(r#"[]"#, |mut de| {
        let ty = de.flags_type(&["a".into(), "b".into(), "c".into(), "d".into()]);
        let v = de.deserialize_flags(ty).unwrap();
        assert_eq!(v, 0);
    })
}

#[test]
fn flags_unknown_array() {
    with_deserializer(r#"[true, false, false, true, true]"#, |mut de| {
        let ty = de.flags_type(&["a".into(), "b".into(), "c".into(), "d".into()]);
        let err = de.deserialize_flags(ty).unwrap_err();
        assert_eq!(
            err,
            "sequence must contain exactly 4 elements at line 1 column 32"
        );
    })
}

#[test]
fn flags_object() {
    with_deserializer(r#"{"d": true, "a": true}"#, |mut de| {
        let ty = de.flags_type(&["a".into(), "b".into(), "c".into(), "d".into()]);
        let v = de.deserialize_flags(ty).unwrap();
        assert_eq!(v, 0b1001);
    })
}

#[test]
fn flags_empty_object() {
    with_deserializer(r#"{}"#, |mut de| {
        let ty = de.flags_type(&["a".into(), "b".into(), "c".into(), "d".into()]);
        let v = de.deserialize_flags(ty).unwrap();
        assert_eq!(v, 0);
    })
}

#[test]
fn flags_unknown_object() {
    with_deserializer(r#"{"d": true, "a": true, "z": false}"#, |mut de| {
        let ty = de.flags_type(&["a".into(), "b".into(), "c".into(), "d".into()]);
        let err = de.deserialize_flags(ty).unwrap_err();
        assert_eq!(err, "unknown case `z` received at line 1 column 34");
    })
}

#[test]
fn option_none() {
    with_deserializer(r#"null"#, |mut de| {
        let v = de.deserialize_option(Type::U32).unwrap();
        assert_eq!(v, None);
    })
}

#[test]
fn option_some() {
    with_deserializer(r#"42"#, |mut de| {
        let v_de = de.deserialize_option(Type::U32).unwrap().unwrap();
        let v = u32::deserialize(de.with(v_de)).unwrap();
        assert_eq!(v, 42);
    })
}

#[test]
fn result_string_ok() {
    with_deserializer(r#""ok""#, |mut de| {
        let _v_de = de
            .deserialize_result(None, Some(Type::U32))
            .unwrap()
            .unwrap();
    })
}

#[test]
fn result_string_err() {
    with_deserializer(r#""err""#, |mut de| {
        let _v_de = de
            .deserialize_result(Some(Type::U32), None)
            .unwrap()
            .unwrap_err();
    })
}

#[test]
fn result_payload_ok() {
    with_deserializer(r#"{"ok": 42}"#, |mut de| {
        let v_de = de
            .deserialize_result(Some(Type::U32), Some(Type::U32))
            .unwrap()
            .unwrap();
        let v = u32::deserialize(de.with(v_de)).unwrap();
        assert_eq!(v, 42);
    })
}

#[test]
fn result_payload_err() {
    with_deserializer(r#"{"err": 42}"#, |mut de| {
        let v_de = de
            .deserialize_result(Some(Type::U32), Some(Type::U32))
            .unwrap()
            .unwrap_err();
        let v = u32::deserialize(de.with(v_de)).unwrap();
        assert_eq!(v, 42);
    })
}

#[test]
fn record_complex() {
    const PAYLOAD: &str = r#"{
      "a": true,
      "b": [0, 1, 2, 3, -4, -5, -6, -7, 8.1, 9.2],
      "c": "test",
      "d": ["bytes", ["foo", "bar", "baz"], []],
      "e": "🌍",
      "f": "none",
      "g": {"some": "test"},
      "h": "yes",
      "i": {"bar": true},
      "j": null,
      "k": {"ok": ["ok", {"ok": null}]},
      "l": {"err": ["err", {"err": null}]}
    }"#;
    with_deserializer(PAYLOAD, |mut de| {
        let empty_result = de.result_type(None, None);

        let d_b_ty = de.list_type(Type::String);
        let d_c_ty = de.flags_type(&["foo".into(), "bar".into(), "baz".into()]);
        let b_ty = de.tuple_type(&[
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
        ]);
        let d_ty = de.record_type(&[
            ("d_a".into(), Type::Bytes),
            ("d_b".into(), Type::List(d_b_ty)),
            ("d_c".into(), Type::Flags(d_c_ty)),
        ]);
        let f_ty = de.variant_type(&[("none".into(), None), ("some".into(), Some(Type::String))]);
        let g_ty = f_ty;
        let h_ty = de.enum_type(&["no".into(), "yes".into()]);
        let i_ty = de.flags_type(&["foo".into(), "bar".into(), "baz".into()]);
        let j_ty = de.option_type(Type::Char);
        let k_ok_ty = de.tuple_type(&[Type::Result(empty_result), Type::Result(empty_result)]);
        let k_ty = de.result_type(Some(Type::Tuple(k_ok_ty)), None);
        let l_err_ty = k_ok_ty;
        let l_ty = de.result_type(Some(Type::Tuple(l_err_ty)), None);

        let ty = de.record_type(&[
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
        ]);
        let (idx, v_de, fields) = de.deserialize_record(ty).unwrap();

        // "a"
        {
            assert_eq!(idx, 0);
            let v = bool::deserialize(de.with(v_de)).unwrap();
            assert_eq!(v, true);
        }

        let mut de = RecordIter(de.with(fields));

        // "b"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 1);

            let (v_de, els) = de.0.with(v_de).deserialize_tuple(b_ty).unwrap();
            let v = u8::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 0);

            let mut de = TupleIter(de.0.with(els));

            let v_de = de.next().unwrap();
            let v = u16::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 1);

            let v_de = de.next().unwrap();
            let v = u32::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 2);

            let v_de = de.next().unwrap();
            let v = u64::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 3);

            let v_de = de.next().unwrap();
            let v = i8::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, -4);

            let v_de = de.next().unwrap();
            let v = i16::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, -5);

            let v_de = de.next().unwrap();
            let v = i32::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, -6);

            let v_de = de.next().unwrap();
            let v = i64::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, -7);

            let v_de = de.next().unwrap();
            let v = f32::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 8.1);

            let v_de = de.next().unwrap();
            let v = f64::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 9.2);
        }

        // "c"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 2);
            let v = String::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, "test");
        }

        // "d"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 3);

            let (idx, v_de, fields) = de.0.with(v_de).deserialize_record(d_ty).unwrap();

            // "d_a"
            {
                assert_eq!(idx, 0);
                let v = <Vec<u8>>::deserialize(de.0.with(v_de)).unwrap();
                assert_eq!(v, b"bytes");
            }

            let mut de = RecordIter(de.0.with(fields));

            // "d_b"
            {
                let (idx, v_de) = de.next().unwrap();
                assert_eq!(idx, 1);

                let vs = de.0.with(v_de).deserialize_list_flat::<String>().unwrap();
                assert_eq!(vs, ["foo", "bar", "baz"])
            }

            // "d_c"
            {
                let (idx, v_de) = de.next().unwrap();
                assert_eq!(idx, 2);
                let v = de.0.with(v_de).deserialize_flags(d_c_ty).unwrap();
                assert_eq!(v, 0b000)
            }
        }

        // "e"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 4);
            let v = char::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, '🌍');
        }

        // "f"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 5);
            let (idx, _v_de) = de.0.with(v_de).deserialize_variant(f_ty).unwrap();
            assert_eq!(idx, 0);
        }

        // "g"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 6);
            let (idx, v_de) = de.0.with(v_de).deserialize_variant(g_ty).unwrap();
            assert_eq!(idx, 1);
            let v = String::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, "test");
        }

        // "h"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 7);
            let idx = de.0.with(v_de).deserialize_enum(h_ty).unwrap();
            assert_eq!(idx, 1);
        }

        // "i"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 8);
            let v = de.0.with(v_de).deserialize_flags(i_ty).unwrap();
            assert_eq!(v, 0b010);
        }

        // "j"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 9);
            let v = de.0.with(v_de).deserialize_option(Type::Char).unwrap();
            assert_eq!(v, None);
        }

        // "k"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 10);
            let v_de =
                de.0.with(v_de)
                    .deserialize_result(Some(Type::Tuple(k_ok_ty)), None)
                    .unwrap()
                    .unwrap();
            let (v_de, els) = de.0.with(v_de).deserialize_tuple(k_ok_ty).unwrap();
            de.0.with(v_de)
                .deserialize_result(None, None)
                .unwrap()
                .unwrap();

            let mut de = TupleIter(de.0.with(els));

            let v_de = de.next().unwrap();
            de.0.with(v_de)
                .deserialize_result(None, None)
                .unwrap()
                .unwrap();
        }

        // "l"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 11);
            let v_de =
                de.0.with(v_de)
                    .deserialize_result(None, Some(Type::Tuple(l_err_ty)))
                    .unwrap()
                    .unwrap_err();
            let (v_de, els) = de.0.with(v_de).deserialize_tuple(l_err_ty).unwrap();
            de.0.with(v_de)
                .deserialize_result(None, None)
                .unwrap()
                .unwrap_err();

            let mut de = TupleIter(de.0.with(els));

            let v_de = de.next().unwrap();
            de.0.with(v_de)
                .deserialize_result(None, None)
                .unwrap()
                .unwrap_err();
        }
    })
}

#[test]
fn record_out_of_order() {
    with_deserializer(r#"{"c": 0, "a": 1, "b": 2}"#, |mut de| {
        let ty = de.record_type(&[
            ("a".into(), Type::U8),
            ("b".into(), Type::U8),
            ("c".into(), Type::U8),
        ]);
        let (idx, v_de, fields) = de.deserialize_record(ty).unwrap();

        // "a"
        {
            assert_eq!(idx, 0);
            let v = u8::deserialize(de.with(v_de)).unwrap();
            assert_eq!(v, 1);
        }

        let mut de = RecordIter(de.with(fields));

        // "b"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 1);
            let v = u8::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 2);
        }

        // "c"
        {
            let (idx, v_de) = de.next().unwrap();
            assert_eq!(idx, 2);
            let v = u8::deserialize(de.0.with(v_de)).unwrap();
            assert_eq!(v, 0);
        }
    })
}

#[test]
fn record_invalid() {
    with_deserializer("0", |mut de| {
        let ty = de.record_type(&[("foo".into(), Type::U8), ("bar".into(), Type::String)]);
        let err = de.deserialize_record(ty).unwrap_err();
        assert_eq!(
            err,
            r#"invalid type: integer `0`, expected a record with fields: [("foo", U8), ("bar", String)] at line 1 column 1"#
        );
    })
}
