use crate::bindings::exports::cosmonic::serde::reflect::Value;
use crate::{JSON, mk_serialize};

mk_serialize!(JSON);

#[test]
fn bool_false() {
    let s = serialize_str(|_, _| Value::Bool(false));
    assert_eq!(s, "false");
}

#[test]
fn bool_true() {
    let s = serialize_str(|_, _| Value::Bool(true));
    assert_eq!(s, "true");
}

#[test]
fn u8() {
    let s = serialize_str(|_, _| Value::U8(42));
    assert_eq!(s, "42");
}

#[test]
fn u16() {
    let s = serialize_str(|_, _| Value::U16(42));
    assert_eq!(s, "42");
}

#[test]
fn u32() {
    let s = serialize_str(|_, _| Value::U32(42));
    assert_eq!(s, "42");
}

#[test]
fn u64() {
    let s = serialize_str(|_, _| Value::U64(42));
    assert_eq!(s, "42");
}

#[test]
fn s8() {
    let s = serialize_str(|_, _| Value::S8(-42));
    assert_eq!(s, "-42");
}

#[test]
fn s16() {
    let s = serialize_str(|_, _| Value::S16(-42));
    assert_eq!(s, "-42");
}

#[test]
fn s32() {
    let s = serialize_str(|_, _| Value::S32(-42));
    assert_eq!(s, "-42");
}

#[test]
fn s64() {
    let s = serialize_str(|_, _| Value::S64(-42));
    assert_eq!(s, "-42");
}

#[test]
fn f32() {
    let s = serialize_str(|_, _| Value::F32(8.1));
    assert_eq!(s, "8.1");
}

#[test]
fn f64() {
    let s = serialize_str(|_, _| Value::F64(9.2));
    assert_eq!(s, "9.2");
}

#[test]
fn char() {
    let s = serialize_str(|_, _| Value::Char('🌳'));
    assert_eq!(s, r#""🌳""#);
}

#[test]
fn string() {
    let s = serialize_str(|_, _| Value::String("test".into()));
    assert_eq!(s, r#""test""#);
}

#[test]
fn tuple_strings() {
    let buf = serialize_str(|store, ser| {
        let val = ser
            .tuple_value()
            .call_constructor(
                store,
                &[Value::String("foo".into()), Value::String("bar".into())],
            )
            .unwrap();
        Value::Tuple(val)
    });
    assert_eq!(buf, r#"["foo","bar"]"#);
}

#[test]
fn tuple_numbers() {
    let buf = serialize_str(|store, ser| {
        let val = ser
            .tuple_value()
            .call_constructor(
                store,
                &[
                    Value::U8(0),
                    Value::U16(1),
                    Value::U32(2),
                    Value::U64(3),
                    Value::S8(-4),
                    Value::S16(-5),
                    Value::S32(-6),
                    Value::S64(-7),
                    Value::F32(8.1),
                    Value::F64(9.2),
                ],
            )
            .unwrap();
        Value::Tuple(val)
    });
    assert_eq!(buf, r#"[0,1,2,3,-4,-5,-6,-7,8.1,9.2]"#);
}

#[test]
fn result_ok_string() {
    let s = serialize_str(|store, ser| {
        let val = ser
            .result_value()
            .call_constructor(store, Ok(Some(&Value::String("test".into()))))
            .unwrap();
        Value::Result(val)
    });
    assert_eq!(s, r#"{"ok":"test"}"#);
}
