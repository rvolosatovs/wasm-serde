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

struct Deserializer<'a> {
    store: &'a mut Store<()>,
    codec: &'a bindings::exports::rvolosatovs::serde::deserializer::Guest,
    de: ResourceAny,
}

impl<'a> Deserializer<'a> {
    fn with(&mut self, de: ResourceAny) -> Deserializer<'_> {
        Deserializer {
            store: self.store,
            codec: self.codec,
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
    fn deserialize_record(
        &mut self,
        fields: impl AsRef<[String]>,
    ) -> Result<(u32, ResourceAny, ResourceAny), String> {
        self.codec
            .deserializer()
            .call_deserialize_record(&mut self.store, self.de, fields.as_ref())
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_tuple(&mut self, n: u32) -> Result<(ResourceAny, ResourceAny), String> {
        self.codec
            .deserializer()
            .call_deserialize_tuple(&mut self.store, self.de, n)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_list(&mut self) -> Result<ResourceAny, String> {
        self.codec
            .deserializer()
            .call_deserialize_list(&mut self.store, self.de)
            .unwrap()
            .map_err(self.mk_string_from_error())
    }

    #[track_caller]
    fn deserialize_list_flat<T: Deserialize>(&mut self) -> Result<Vec<T>, String> {
        let els = self.deserialize_list()?;
        let mut vs = Vec::new();
        let mut de = ListIter(self.with(els));
        while let Some(v_de) = de.next() {
            let v = T::deserialize(de.0.with(v_de))?;
            vs.push(v);
        }
        Ok(vs)
    }
}

trait Deserialize {
    fn deserialize(de: Deserializer<'_>) -> Result<Self, String>
    where
        Self: Sized;
}

macro_rules! impl_deserialize_flat {
    ($t:ty, $f:ident) => {
        impl Deserialize for $t {
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

impl_deserialize_flat!(bool, call_deserialize_bool);
impl_deserialize_flat!(u8, call_deserialize_u8);
impl_deserialize_flat!(u16, call_deserialize_u16);
impl_deserialize_flat!(u32, call_deserialize_u32);
impl_deserialize_flat!(u64, call_deserialize_u64);
impl_deserialize_flat!(i8, call_deserialize_s8);
impl_deserialize_flat!(i16, call_deserialize_s16);
impl_deserialize_flat!(i32, call_deserialize_s32);
impl_deserialize_flat!(i64, call_deserialize_s64);
impl_deserialize_flat!(f32, call_deserialize_f32);
impl_deserialize_flat!(f64, call_deserialize_f64);
impl_deserialize_flat!(String, call_deserialize_string);
impl_deserialize_flat!(Vec<u8>, call_deserialize_bytes);

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
            "invalid type: integer `0`, expected a bool at line 1 column 1"
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
            "invalid type: integer `0`, expected a byte buffer at line 1 column 1"
        );
    })
}

#[test]
fn string_string() {
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
fn record_0() {
    const PAYLOAD: &str = r#"{"a": true, "b": [0, 1, 2, 3, -4, -5, -6, -7, 8.1, 9.2], "c": "test", "d": {"d_a": "bytes", "d_b": ["foo", "bar", "baz"]}}"#;
    with_deserializer(PAYLOAD, |mut de| {
        let (idx, v_de, fields) = de
            .deserialize_record(["a".into(), "b".into(), "c".into(), "d".into()])
            .unwrap();

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

            let (v_de, els) = de.0.with(v_de).deserialize_tuple(10).unwrap();
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

            let (idx, v_de, fields) =
                de.0.with(v_de)
                    .deserialize_record(&["d_a".into(), "d_b".into()])
                    .unwrap();

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
        }
    })
}

#[test]
fn record_invalid() {
    with_deserializer("0", |mut de| {
        let err = de
            .deserialize_record(["foo".into(), "bar".into()])
            .unwrap_err();
        assert_eq!(
            err,
            r#"invalid type: integer `0`, expected a record with fields: ["foo", "bar"] at line 1 column 1"#
        );
    })
}
