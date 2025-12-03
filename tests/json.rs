use core::fmt;
use std::process::Command;
use std::sync::LazyLock;

use wasmtime::component::{Component, Linker};
use wasmtime::{Engine, Store};
use wit_component::ComponentEncoder;

mod bindings {
    wasmtime::component::bindgen!({
        world: "format",
    });
}
use bindings::exports::rvolosatovs::serde::reflect;
use bindings::exports::rvolosatovs::serde::reflect::{List, ListType, Type, Value};

static ENGINE: LazyLock<Engine> = LazyLock::new(Engine::default);

static CODEC: LazyLock<bindings::FormatPre<()>> = LazyLock::new(|| {
    let status = Command::new(env!("CARGO"))
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
    let buf = std::fs::read("./target/wasm32-unknown-unknown/debug/wasm_serde_json.wasm")
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
    reflect: &'a bindings::exports::rvolosatovs::serde::reflect::Guest,
    value: Value,
}

impl fmt::Debug for Deserializer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Deserializer")
            .field("store", &self.store)
            .field("value", &self.value)
            .finish()
    }
}

macro_rules! impl_unwrap_value_primitive {
    ($rt:ident, $f:ident, $f_list:ident, $t:ty) => {
        #[allow(dead_code)]
        #[track_caller]
        fn $f(self) -> $t {
            let Value::$rt(v) = self else {
                panic!("invalid value type");
            };
            v
        }

        #[allow(dead_code)]
        #[track_caller]
        fn $f_list(self) -> Vec<$t> {
            let Value::List(List::$rt(vs)) = self else {
                panic!("invalid value list type");
            };
            vs
        }
    };
}

impl Value {
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
    fn unwrap_record(self, store: &mut Store<()>, reflect: &reflect::Guest) -> Vec<Value> {
        let Value::Record(v) = self else {
            panic!("invalid value type");
        };
        reflect.record_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    fn unwrap_tuple(self, store: &mut Store<()>, reflect: &reflect::Guest) -> Vec<Value> {
        let Value::Tuple(v) = self else {
            panic!("invalid value type");
        };
        reflect.tuple_value().call_into_value(store, v).unwrap()
    }

    #[expect(dead_code)]
    #[track_caller]
    fn unwrap_list(self) -> List {
        let Value::List(v) = self else {
            panic!("invalid value type");
        };
        v
    }

    #[track_caller]
    fn unwrap_variant(
        self,
        store: &mut Store<()>,
        reflect: &reflect::Guest,
    ) -> (u32, Option<Self>) {
        let Value::Variant(v) = self else {
            panic!("invalid value type");
        };
        reflect.variant_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    fn unwrap_option(self, store: &mut Store<()>, reflect: &reflect::Guest) -> Option<Self> {
        let Value::Option(v) = self else {
            panic!("invalid value type");
        };
        reflect.option_value().call_into_value(store, v).unwrap()
    }

    #[track_caller]
    fn unwrap_result(
        self,
        store: &mut Store<()>,
        reflect: &reflect::Guest,
    ) -> Result<Option<Self>, Option<Self>> {
        let Value::Result(v) = self else {
            panic!("invalid value type");
        };
        reflect.result_value().call_into_value(store, v).unwrap()
    }
}

macro_rules! impl_unwrap_deserializer_primitive {
    ($f:ident, $f_list:ident, $t:ty) => {
        #[allow(dead_code)]
        #[track_caller]
        fn $f(self) -> $t {
            self.value.$f()
        }

        #[allow(dead_code)]
        #[track_caller]
        fn $f_list(self) -> Vec<$t> {
            self.value.$f_list()
        }
    };
}

impl<'a> Deserializer<'a> {
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
    fn unwrap_record(self) -> (Self, Vec<Value>) {
        let mut fields = self.value.unwrap_record(self.store, self.reflect);
        fields.reverse();
        let v = fields.pop().unwrap();
        (Self { value: v, ..self }, fields)
    }

    #[expect(dead_code)]
    #[track_caller]
    fn unwrap_tuple(self) -> (Self, Vec<Value>) {
        let mut values = self.value.unwrap_tuple(self.store, self.reflect);
        values.reverse();
        let v = values.pop().unwrap();
        (Self { value: v, ..self }, values)
    }

    #[track_caller]
    fn unwrap_variant(self) -> (u32, Option<Self>) {
        let (case, payload) = self.value.unwrap_variant(self.store, self.reflect);
        (case, payload.map(|value| Self { value, ..self }))
    }

    #[track_caller]
    fn unwrap_option(self) -> Option<Self> {
        self.value
            .unwrap_option(self.store, self.reflect)
            .map(|value| Self { value, ..self })
    }

    #[track_caller]
    fn unwrap_result(self) -> Result<Option<Self>, Option<Self>> {
        match self.value.unwrap_result(self.store, self.reflect) {
            Ok(None) => Ok(None),
            Ok(Some(value)) => Ok(Some(Self { value, ..self })),
            Err(None) => Err(None),
            Err(Some(value)) => Err(Some(Self { value, ..self })),
        }
    }
}

#[track_caller]
fn with_deserializer<T>(
    payload: impl AsRef<[u8]>,
    mk_ty: impl FnOnce(&mut Store<()>, &reflect::Guest) -> Type,
    f: impl FnOnce(Result<Deserializer<'_>, String>) -> T,
) -> T {
    let mut store = Store::new(&ENGINE, ());
    let codec = CODEC
        .instantiate(&mut store)
        .expect("failed to instantiate codec");
    let ty = mk_ty(&mut store, codec.rvolosatovs_serde_reflect());
    match codec
        .rvolosatovs_serde_deserializer()
        .call_from_list(&mut store, payload.as_ref(), ty)
        .unwrap()
    {
        Ok(de) => f(Ok(Deserializer {
            store: &mut store,
            reflect: codec.rvolosatovs_serde_reflect(),
            value: de,
        })),
        Err(err) => {
            let err = codec
                .rvolosatovs_serde_deserializer()
                .error()
                .call_to_string(&mut store, err)
                .unwrap();
            f(Err(err))
        }
    }
}

#[track_caller]
fn mk_foo_bar_variant_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
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
fn mk_foo_bar_enum_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de
        .enum_type()
        .call_constructor(store, &["foo".into(), "bar".into()])
        .unwrap();
    Type::Enum(ty)
}

#[track_caller]
fn mk_a_b_c_d_flags_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de
        .flags_type()
        .call_constructor(store, &["a".into(), "b".into(), "c".into(), "d".into()])
        .unwrap();
    Type::Flags(ty)
}

#[track_caller]
fn mk_option_u32_type(store: &mut Store<()>, de: &reflect::Guest) -> Type {
    let ty = de.option_type().call_constructor(store, Type::U32).unwrap();
    Type::Option(ty)
}

#[test]
fn bool_true() {
    with_deserializer(
        "true",
        |_, _| Type::Bool,
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_bool();
            assert!(v);
        },
    )
}

#[test]
fn bool_false() {
    with_deserializer(
        "false",
        |_, _| Type::Bool,
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_bool();
            assert!(!v);
        },
    )
}

#[test]
fn bool_invalid() {
    with_deserializer(
        "0",
        |_, _| Type::Bool,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                "invalid type: integer `0`, expected a boolean at line 1 column 1"
            );
        },
    )
}

#[test]
fn char_tree_emoji() {
    with_deserializer(
        r#""🌳""#,
        |_, _| Type::Char,
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_char();
            assert_eq!(v, '🌳');
        },
    )
}

#[test]
fn char_invalid() {
    with_deserializer(
        "0",
        |_, _| Type::Char,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                "invalid type: integer `0`, expected a character at line 1 column 1"
            );
        },
    )
}

#[test]
fn bytes_array() {
    with_deserializer(
        "[0, 1, 2]",
        |_, _| Type::List(ListType::U8),
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_list_u8();
            assert_eq!(v, [0, 1, 2]);
        },
    )
}

#[test]
fn bytes_string() {
    with_deserializer(
        r#""test""#,
        |_, _| Type::List(ListType::U8),
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_list_u8();
            assert_eq!(v, b"test");
        },
    )
}

#[test]
fn bytes_invalid() {
    with_deserializer(
        "0",
        |_, _| Type::List(ListType::U8),
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                "invalid type: integer `0`, expected byte array at line 1 column 1"
            );
        },
    )
}

#[test]
fn string_ascii() {
    with_deserializer(
        r#""test""#,
        |_, _| Type::String,
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_string();
            assert_eq!(v, "test");
        },
    )
}

#[test]
fn string_invalid() {
    with_deserializer(
        "0",
        |_, _| Type::String,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                "invalid type: integer `0`, expected a string at line 1 column 1"
            );
        },
    )
}

#[test]
fn variant_empty_string() {
    with_deserializer(r#""foo""#, mk_foo_bar_variant_type, |de| {
        let de = de.unwrap();
        let (c, de) = de.unwrap_variant();
        assert_eq!(c, 0);
        assert!(de.is_none());
    })
}

#[test]
fn variant_unknown_string() {
    with_deserializer(r#""baz""#, mk_foo_bar_variant_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]` at line 1 column 5"#
        );
    })
}

#[test]
fn variant_empty_object() {
    with_deserializer(r#"{"foo": null}"#, mk_foo_bar_variant_type, |de| {
        let de = de.unwrap();
        let (c, de) = de.unwrap_variant();
        assert_eq!(c, 0);
        assert!(de.is_none());
    })
}

#[test]
fn variant_payload_object() {
    with_deserializer(r#"{"bar": 42}"#, mk_foo_bar_variant_type, |de| {
        let de = de.unwrap();
        let (c, de) = de.unwrap_variant();
        assert_eq!(c, 1);
        let de = de.unwrap();
        let c = de.unwrap_u32();
        assert_eq!(c, 42);
    })
}

#[test]
fn variant_unknown_object() {
    with_deserializer(r#"{"baz": null}"#, mk_foo_bar_variant_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]` at line 1 column 6"#
        );
    })
}

#[test]
fn enum_string() {
    with_deserializer(r#""bar""#, mk_foo_bar_enum_type, |de| {
        let de = de.unwrap();
        let c = de.unwrap_enum();
        assert_eq!(c, 1);
    })
}

#[test]
fn enum_unknown_string() {
    with_deserializer(r#""baz""#, mk_foo_bar_enum_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]` at line 1 column 5"#
        );
    })
}

#[test]
fn enum_object() {
    with_deserializer(r#"{"bar": null}"#, mk_foo_bar_enum_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_enum();
        assert_eq!(v, 1);
    })
}

#[test]
fn enum_unknown_object() {
    with_deserializer(r#"{"baz": null}"#, mk_foo_bar_enum_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]` at line 1 column 6"#
        );
    })
}

#[test]
fn flags_array() {
    with_deserializer(
        r#"[true, false, false, true]"#,
        mk_a_b_c_d_flags_type,
        |de| {
            let de = de.unwrap();
            let v = de.unwrap_flags();
            assert_eq!(v, 0b1001);
        },
    )
}

#[test]
fn flags_empty_array() {
    with_deserializer(r#"[]"#, mk_a_b_c_d_flags_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_flags();
        assert_eq!(v, 0);
    })
}

#[test]
fn flags_unknown_array() {
    with_deserializer(
        r#"[true, false, false, true, true]"#,
        mk_a_b_c_d_flags_type,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                "sequence must contain exactly 4 elements at line 1 column 32"
            );
        },
    )
}

#[test]
fn flags_object() {
    with_deserializer(r#"{"d": true, "a": true}"#, mk_a_b_c_d_flags_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_flags();
        assert_eq!(v, 0b1001);
    })
}

#[test]
fn flags_empty_object() {
    with_deserializer(r#"{}"#, mk_a_b_c_d_flags_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_flags();
        assert_eq!(v, 0);
    })
}

#[test]
fn flags_unknown_object() {
    with_deserializer(
        r#"{"d": true, "a": true, "z": false}"#,
        mk_a_b_c_d_flags_type,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(err, "unknown case `z` received at line 1 column 34");
        },
    )
}

#[test]
fn option_none() {
    with_deserializer(r#"null"#, mk_option_u32_type, |de| {
        let de = de.unwrap();
        let de = de.unwrap_option();
        assert!(de.is_none());
    })
}

#[test]
fn option_some() {
    with_deserializer(r#"42"#, mk_option_u32_type, |de| {
        let de = de.unwrap();
        let de = de.unwrap_option();
        let de = de.unwrap();
        let v = de.unwrap_u32();
        assert_eq!(v, 42);
    })
}

#[test]
fn result_string_ok() {
    with_deserializer(
        r#""ok""#,
        |store, de| {
            let ty = de
                .result_type()
                .call_constructor(store, None, Some(Type::U32))
                .unwrap();
            Type::Result(ty)
        },
        |de| {
            let de = de.unwrap();
            let de = de.unwrap_result();
            let de = de.unwrap();
            assert!(de.is_none());
        },
    )
}

#[test]
fn result_string_err() {
    with_deserializer(
        r#""err""#,
        |store, de| {
            let ty = de
                .result_type()
                .call_constructor(store, Some(Type::U32), None)
                .unwrap();
            Type::Result(ty)
        },
        |de| {
            let de = de.unwrap();
            let de = de.unwrap_result();
            let de = de.unwrap_err();
            assert!(de.is_none());
        },
    )
}

#[test]
fn result_payload_ok() {
    with_deserializer(
        r#"{"ok": 42}"#,
        |store, de| {
            let ty = de
                .result_type()
                .call_constructor(store, Some(Type::U32), Some(Type::U32))
                .unwrap();
            Type::Result(ty)
        },
        |de| {
            let de = de.unwrap();
            let de = de.unwrap_result();
            let de = de.unwrap();
            let de = de.unwrap();
            let v = de.unwrap_u32();
            assert_eq!(v, 42);
        },
    )
}

#[test]
fn result_payload_err() {
    with_deserializer(
        r#"{"err": 42}"#,
        |store, de| {
            let ty = de
                .result_type()
                .call_constructor(store, Some(Type::U32), Some(Type::U32))
                .unwrap();
            Type::Result(ty)
        },
        |de| {
            let de = de.unwrap();
            let de = de.unwrap_result();
            let de = de.unwrap_err();
            let de = de.unwrap();
            let v = de.unwrap_u32();
            assert_eq!(v, 42);
        },
    )
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
    with_deserializer(
        PAYLOAD,
        |mut store, de| {
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
        },
        |de| {
            let Deserializer {
                store,
                reflect,
                value,
                ..
            } = de.unwrap();
            let mut fields = value.unwrap_record(store, reflect);
            fields.reverse();

            // "a"
            {
                let v = fields.pop().unwrap().unwrap_bool();
                assert!(v);
            }

            // "b"
            {
                let mut values = fields.pop().unwrap().unwrap_tuple(store, reflect);
                values.reverse();

                let v = values.pop().unwrap().unwrap_u8();
                assert_eq!(v, 0);

                let v = values.pop().unwrap().unwrap_u16();
                assert_eq!(v, 1);

                let v = values.pop().unwrap().unwrap_u32();
                assert_eq!(v, 2);

                let v = values.pop().unwrap().unwrap_u64();
                assert_eq!(v, 3);

                let v = values.pop().unwrap().unwrap_s8();
                assert_eq!(v, -4);

                let v = values.pop().unwrap().unwrap_s16();
                assert_eq!(v, -5);

                let v = values.pop().unwrap().unwrap_s32();
                assert_eq!(v, -6);

                let v = values.pop().unwrap().unwrap_s64();
                assert_eq!(v, -7);

                let v = values.pop().unwrap().unwrap_f32();
                assert_eq!(v, 8.1);

                let v = values.pop().unwrap().unwrap_f64();
                assert_eq!(v, 9.2);
            }

            // "c"
            {
                let v = fields.pop().unwrap().unwrap_string();
                assert_eq!(v, "test");
            }

            // "d"
            {
                let mut fields = fields.pop().unwrap().unwrap_record(store, reflect);
                fields.reverse();

                // "d_a"
                {
                    let v = fields.pop().unwrap().unwrap_list_u8();
                    assert_eq!(v, b"bytes");
                }

                // "d_b"
                {
                    let v = fields.pop().unwrap().unwrap_list_string();
                    assert_eq!(v, ["foo", "bar", "baz"])
                }

                // "d_c"
                {
                    let v = fields.pop().unwrap().unwrap_flags();
                    assert_eq!(v, 0b000)
                }
            }

            // "e"
            {
                let v = fields.pop().unwrap().unwrap_char();
                assert_eq!(v, '🌍');
            }

            // "f"
            {
                let (c, v) = fields.pop().unwrap().unwrap_variant(store, reflect);
                assert_eq!(c, 0);
                assert!(v.is_none());
            }

            // "g"
            {
                let (c, v) = fields.pop().unwrap().unwrap_variant(store, reflect);
                assert_eq!(c, 1);
                let v = v.unwrap().unwrap_string();
                assert_eq!(v, "test");
            }

            // "h"
            {
                let c = fields.pop().unwrap().unwrap_enum();
                assert_eq!(c, 1);
            }

            // "i"
            {
                let v = fields.pop().unwrap().unwrap_flags();
                assert_eq!(v, 0b010);
            }

            // "j"
            {
                let v = fields.pop().unwrap().unwrap_option(store, reflect);
                assert!(v.is_none());
            }

            // "k"
            {
                let v = fields
                    .pop()
                    .unwrap()
                    .unwrap_result(store, reflect)
                    .unwrap()
                    .unwrap();
                let mut values = v.unwrap_tuple(store, reflect);
                let v = values.pop().unwrap().unwrap_result(store, reflect).unwrap();
                assert!(v.is_none());

                let v = values.pop().unwrap().unwrap_result(store, reflect).unwrap();
                assert!(v.is_none());
            }

            // "l"
            {
                let v = fields
                    .pop()
                    .unwrap()
                    .unwrap_result(store, reflect)
                    .unwrap_err()
                    .unwrap();
                let mut values = v.unwrap_tuple(store, reflect);
                let v = values
                    .pop()
                    .unwrap()
                    .unwrap_result(store, reflect)
                    .unwrap_err();
                assert!(v.is_none());

                let v = values
                    .pop()
                    .unwrap()
                    .unwrap_result(store, reflect)
                    .unwrap_err();
                assert!(v.is_none());
            }
        },
    )
}

#[test]
fn record_out_of_order() {
    with_deserializer(
        r#"{"c": 0, "a": 1, "b": 2}"#,
        |store, de| {
            let ty = de
                .record_type()
                .call_constructor(
                    store,
                    &[
                        ("a".into(), Type::U8),
                        ("b".into(), Type::U8),
                        ("c".into(), Type::U8),
                    ],
                )
                .unwrap();
            Type::Record(ty)
        },
        |de| {
            let de = de.unwrap();
            let (Deserializer { value, .. }, mut fields) = de.unwrap_record();

            // "a"
            {
                let v = value.unwrap_u8();
                assert_eq!(v, 1);
            }

            // "b"
            {
                let v = fields.pop().unwrap().unwrap_u8();
                assert_eq!(v, 2);
            }

            // "c"
            {
                let v = fields.pop().unwrap().unwrap_u8();
                assert_eq!(v, 0);
            }
        },
    )
}

#[test]
fn record_invalid() {
    with_deserializer(
        "0",
        |store, de| {
            let ty = de
                .record_type()
                .call_constructor(
                    store,
                    &[("foo".into(), Type::U8), ("bar".into(), Type::String)],
                )
                .unwrap();
            Type::Record(ty)
        },
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                r#"invalid type: integer `0`, expected record with fields: [("foo", U8), ("bar", String)] at line 1 column 1"#
            );
        },
    )
}
