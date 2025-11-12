use std::process::Command;
use std::sync::LazyLock;

use wasmtime::component::{Component, Linker, ResourceAny};
use wasmtime::{Engine, Store};
use wit_component::ComponentEncoder;

mod bindings {
    wasmtime::component::bindgen!();
}

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

#[track_caller]
fn with_deserializer<T>(
    payload: impl AsRef<[u8]>,
    f: impl FnOnce(
        Store<()>,
        &bindings::exports::rvolosatovs::serde::deserializer::Guest,
        ResourceAny,
    ) -> T,
) -> T {
    let mut store = Store::new(&ENGINE, ());
    let codec = CODEC
        .instantiate(&mut store)
        .expect("failed to instantiate codec");
    let de = codec
        .rvolosatovs_serde_deserializer()
        .deserializer()
        .call_from_list(&mut store, payload.as_ref())
        .unwrap();
    f(store, codec.rvolosatovs_serde_deserializer(), de)
}

#[test]
fn bool_true() {
    with_deserializer("true", |mut store, codec, de| {
        let v = codec
            .deserializer()
            .call_deserialize_bool(&mut store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, true);
    })
}

#[test]
fn bool_false() {
    with_deserializer("false", |mut store, codec, de| {
        let v = codec
            .deserializer()
            .call_deserialize_bool(&mut store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, false);
    })
}

#[test]
fn bool_invalid() {
    with_deserializer("0", |mut store, codec, de| {
        let err = codec
            .deserializer()
            .call_deserialize_bool(&mut store, de)
            .unwrap()
            .unwrap_err();
        let err = codec.error().call_to_string(&mut store, err).unwrap();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected a bool at line 1 column 1"
        );
    })
}

#[test]
fn bytes_array() {
    with_deserializer("[0, 1, 2]", |mut store, codec, de| {
        let v = codec
            .deserializer()
            .call_deserialize_bytes(&mut store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, [0, 1, 2]);
    })
}

#[test]
fn bytes_string() {
    with_deserializer(r#""test""#, |mut store, codec, de| {
        let v = codec
            .deserializer()
            .call_deserialize_bytes(&mut store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, b"test");
    })
}

#[test]
fn bytes_invalid() {
    with_deserializer("0", |mut store, codec, de| {
        let err = codec
            .deserializer()
            .call_deserialize_bytes(&mut store, de)
            .unwrap()
            .unwrap_err();
        let err = codec.error().call_to_string(&mut store, err).unwrap();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected a byte buffer at line 1 column 1"
        );
    })
}

#[test]
fn string_string() {
    with_deserializer(r#""test""#, |mut store, codec, de| {
        let v = codec
            .deserializer()
            .call_deserialize_string(&mut store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, "test");
    })
}

#[test]
fn string_invalid() {
    with_deserializer("0", |mut store, codec, de| {
        let err = codec
            .deserializer()
            .call_deserialize_string(&mut store, de)
            .unwrap()
            .unwrap_err();
        let err = codec.error().call_to_string(&mut store, err).unwrap();
        assert_eq!(
            err,
            "invalid type: integer `0`, expected a string at line 1 column 1"
        );
    })
}

struct RecordIter<'a> {
    store: &'a mut Store<()>,
    codec: &'a bindings::exports::rvolosatovs::serde::deserializer::Guest,
    de: ResourceAny,
}

impl Iterator for RecordIter<'_> {
    type Item = (u32, ResourceAny);

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let (idx, de, next) = self
            .codec
            .record_deserializer()
            .call_next(&mut self.store, self.de)
            .unwrap();
        self.de = next;
        Some((idx, de))
    }
}

struct ListIter<'a> {
    store: &'a mut Store<()>,
    codec: &'a bindings::exports::rvolosatovs::serde::deserializer::Guest,
    de: ResourceAny,
}

impl Iterator for ListIter<'_> {
    type Item = ResourceAny;

    #[track_caller]
    fn next(&mut self) -> Option<Self::Item> {
        let (de, next) = self
            .codec
            .list_deserializer()
            .call_next(&mut self.store, self.de)
            .unwrap()?;
        self.de = next;
        Some(de)
    }
}

#[test]
fn record_0() {
    const PAYLOAD: &str = r#"{"a": 42, "b": [0, 1, 2, 3, -4, -5, -6, -7, 8.1, 9.2], "c": "test", "d": {"d_a": "bytes"}}"#;

    with_deserializer(PAYLOAD, |mut store, codec, de| {
        let (idx, de, fields) = codec
            .deserializer()
            .call_deserialize_record(
                &mut store,
                de,
                &["a".into(), "b".into(), "c".into(), "d".into()],
            )
            .unwrap()
            .unwrap();
        assert_eq!(idx, 0);
        let v = codec
            .deserializer()
            .call_deserialize_u32(&mut store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 42);

        let mut fields = RecordIter {
            store: &mut store,
            codec,
            de: fields,
        };

        let (idx, de) = fields.next().unwrap();
        assert_eq!(idx, 1);
        let de = codec
            .deserializer()
            .call_deserialize_list(&mut fields.store, de)
            .unwrap()
            .unwrap();

        let mut bar_els = ListIter {
            store: &mut fields.store,
            codec,
            de,
        };

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_u8(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 0);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_u16(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 1);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_u32(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 2);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_u64(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 3);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_s8(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, -4);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_s16(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, -5);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_s32(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, -6);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_s64(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, -7);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_f32(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 8.1);

        let de = bar_els.next().unwrap();
        let v = codec
            .deserializer()
            .call_deserialize_f64(&mut bar_els.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, 9.2);

        let (idx, de) = fields.next().unwrap();
        assert_eq!(idx, 2);
        let v = codec
            .deserializer()
            .call_deserialize_string(&mut fields.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, "test");

        let (idx, de) = fields.next().unwrap();
        assert_eq!(idx, 3);

        let (idx, de, _fields) = codec
            .deserializer()
            .call_deserialize_record(&mut fields.store, de, &["d_a".into()])
            .unwrap()
            .unwrap();
        assert_eq!(idx, 0);
        let v = codec
            .deserializer()
            .call_deserialize_bytes(&mut fields.store, de)
            .unwrap()
            .unwrap();
        assert_eq!(v, b"bytes");
    })
}

#[test]
fn record_invalid() {
    with_deserializer("0", |mut store, codec, de| {
        let err = codec
            .deserializer()
            .call_deserialize_record(&mut store, de, &["foo".into(), "bar".into()])
            .unwrap()
            .unwrap_err();
        let err = codec.error().call_to_string(&mut store, err).unwrap();
        assert_eq!(
            err,
            r#"invalid type: integer `0`, expected a record with fields: ["foo", "bar"] at line 1 column 1"#
        );
    })
}
