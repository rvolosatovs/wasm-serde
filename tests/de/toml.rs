use crate::bindings::exports::cosmonic::reflect::reflect::{ListType, Type};
use crate::{
    TOML, Value, mk_a_b_c_d_flags_type, mk_deserialize, mk_foo_bar_enum_type,
    mk_foo_bar_variant_type, mk_option_u32_type, mk_record_complex_type,
};

mk_deserialize!(TOML);

#[test]
fn bool_true() {
    deserialize(
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
    deserialize(
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
    deserialize(
        "0",
        |_, _| Type::Bool,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(err, "invalid type: integer `0`, expected a boolean\n");
        },
    )
}

#[test]
fn char_tree_emoji() {
    deserialize(
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
    deserialize(
        "0",
        |_, _| Type::Char,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(err, "invalid type: integer `0`, expected a character\n");
        },
    )
}

#[test]
fn bytes_array() {
    deserialize(
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
    deserialize(
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
    deserialize(
        "0",
        |_, _| Type::List(ListType::U8),
        |de| {
            let err = de.unwrap_err();
            assert_eq!(err, "invalid type: integer `0`, expected byte array\n");
        },
    )
}

#[test]
fn string_ascii() {
    deserialize(
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
    deserialize(
        "0",
        |_, _| Type::String,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(err, "invalid type: integer `0`, expected a string\n");
        },
    )
}

#[test]
fn variant_empty_string() {
    deserialize(r#""foo""#, mk_foo_bar_variant_type, |de| {
        let de = de.unwrap();
        let (c, de) = de.unwrap_variant();
        assert_eq!(c, 0);
        assert!(de.is_none());
    })
}

#[test]
fn variant_unknown_string() {
    deserialize(r#""baz""#, mk_foo_bar_variant_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]`
"#
        );
    })
}

#[test]
fn variant_empty_object() {
    deserialize(r#"{ foo = {} }"#, mk_foo_bar_variant_type, |de| {
        let de = de.unwrap();
        let (c, de) = de.unwrap_variant();
        assert_eq!(c, 0);
        assert!(de.is_none());
    })
}

#[test]
fn variant_payload_object() {
    deserialize(r#"{ bar = 42 }"#, mk_foo_bar_variant_type, |de| {
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
    deserialize(r#"{ baz = {} }"#, mk_foo_bar_variant_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]`
"#
        );
    })
}

#[test]
fn enum_string() {
    deserialize(r#""bar""#, mk_foo_bar_enum_type, |de| {
        let de = de.unwrap();
        let c = de.unwrap_enum();
        assert_eq!(c, 1);
    })
}

#[test]
fn enum_unknown_string() {
    deserialize(r#""baz""#, mk_foo_bar_enum_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]`
"#
        );
    })
}

#[test]
fn enum_object() {
    deserialize(r#"{ bar = {} }"#, mk_foo_bar_enum_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_enum();
        assert_eq!(v, 1);
    })
}

#[test]
fn enum_unknown_object() {
    deserialize(r#"{ baz = {} }"#, mk_foo_bar_enum_type, |de| {
        let err = de.unwrap_err();
        assert_eq!(
            err,
            r#"invalid value: string "baz", expected one of `["foo", "bar"]`
"#
        );
    })
}

#[test]
fn flags_array() {
    deserialize(
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
    deserialize(r#"[]"#, mk_a_b_c_d_flags_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_flags();
        assert_eq!(v, 0);
    })
}

#[test]
fn flags_unknown_array() {
    deserialize(
        r#"[true, false, false, true, true]"#,
        mk_a_b_c_d_flags_type,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(err, "sequence must contain exactly 4 elements\n");
        },
    )
}

#[test]
fn flags_object() {
    deserialize(r#"{ d = true, a = true }"#, mk_a_b_c_d_flags_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_flags();
        assert_eq!(v, 0b1001);
    })
}

#[test]
fn flags_empty_object() {
    deserialize(r#"{}"#, mk_a_b_c_d_flags_type, |de| {
        let de = de.unwrap();
        let v = de.unwrap_flags();
        assert_eq!(v, 0);
    })
}

#[test]
fn flags_unknown_object() {
    deserialize(
        r#"{ d = true, a = true, z = false}"#,
        mk_a_b_c_d_flags_type,
        |de| {
            let err = de.unwrap_err();
            assert_eq!(
                err,
                "unknown case `z` received
"
            );
        },
    )
}

#[test]
fn option_some() {
    deserialize(r#"42"#, mk_option_u32_type, |de| {
        let de = de.unwrap();
        let de = de.unwrap_option();
        let de = de.unwrap();
        let v = de.unwrap_u32();
        assert_eq!(v, 42);
    })
}

#[test]
fn result_string_ok() {
    deserialize(
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
    deserialize(
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
    deserialize(
        r#"{ ok = 42 }"#,
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
    deserialize(
        r#"{ err = 42 }"#,
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
    const PAYLOAD: &str = r#"{ a = true, b = [0, 1, 2, 3, -4, -5, -6, -7, 8.1, 9.2], c = "test", d = ["bytes", ["foo", "bar", "baz"], []], e = "🌍", f = "none", g = { some = "test"}, h = "yes", i = { bar = true }, k = { ok = ["ok", { ok = {} }]}, l = { err = ["err", { err = {} }]} }"#;
    deserialize(PAYLOAD, mk_record_complex_type, |de| {
        let Value {
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
    })
}

#[test]
fn record_out_of_order() {
    deserialize(
        r#"{ c = 0, a = 1, b = 2}"#,
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
            let (Value { value, .. }, mut fields) = de.unwrap_record();

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
    deserialize(
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
                r#"invalid type: integer `0`, expected record with fields: ["foo", "bar"]
"#
            );
        },
    )
}
