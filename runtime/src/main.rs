#![allow(unused)]

mod bindings {
    wasmtime::component::bindgen!();
}

use anyhow::{Context as _, bail};
use wasmtime::component::{
    Component, HasData, Instance, Linker, Resource, ResourceAny, ResourceTable, Val,
};
use wasmtime::{Engine, Store};

use wit_component::ComponentEncoder;

use bindings::exports::rvolosatovs::serde::deserializer::{GuestDeserializer, Payload};

#[derive(Default)]
struct Ctx {
    table: ResourceTable,
}

impl HasData for Ctx {
    type Data<'a> = &'a mut Self;
}

pub struct Error;

fn deserialize_payload(
    store: &mut Store<Ctx>,
    instance: GuestDeserializer<'_>,
    de: ResourceAny,
    pld: Payload,
    val: &mut Val,
) -> wasmtime::Result<()> {
    match pld {
        Payload::ValU8(v) => {
            *val = Val::U8(v);
        }
        Payload::ValS8(_) => todo!(),
        Payload::ValU16(_) => todo!(),
        Payload::ValS16(_) => todo!(),
        Payload::ValU32(_) => todo!(),
        Payload::ValS32(_) => todo!(),
        Payload::ValU64(_) => todo!(),
        Payload::ValS64(_) => todo!(),
        Payload::ValF32(_) => todo!(),
        Payload::ValF64(_) => todo!(),
        Payload::ValChar(_) => todo!(),
        Payload::ValString(_) => todo!(),
        Payload::ValBytes(items) => todo!(),
        Payload::ValNone => todo!(),
        Payload::ValSome => todo!(),
        Payload::ValOk => todo!(),
        Payload::ValErr => todo!(),
        Payload::ValSeq => todo!(),
        Payload::ValMap => {
            // TODO: use types
            let mut fields = vec![];
            while instance.call_next_key(&mut *store, de)? {
                let Payload::ValString(k) =
                    instance.call_deserialize_string(&mut *store, de)?.unwrap()
                else {
                    bail!("invalid key type");
                };
                instance.call_next_value(&mut *store, de)?;
                let Payload::ValString(v) =
                    instance.call_deserialize_string(&mut *store, de)?.unwrap()
                else {
                    bail!("invalid value type");
                };
                fields.push((k, Val::String(v)))
            }
            *val = Val::Record(fields);
        }
    }
    Ok(())
}

fn deserialize_arguments(buf: &[u8], ty: wasmtime::component::Type) -> wasmtime::component::Val {
    todo!()
}

fn main() -> wasmtime::Result<()> {
    let engine = Engine::default();
    let mut enc = ComponentEncoder::default().module(include_bytes!(
        "../../target/wasm32-unknown-unknown/release/wasm_serde.wasm"
    ))?;
    let wasm = enc.encode()?;
    let wasm = Component::new(&engine, wasm)?;
    let mut linker = Linker::new(&engine);
    let mut store = Store::new(&engine, Ctx::default());
    let instance = bindings::Format::instantiate(&mut store, &wasm, &linker)?;
    let instance = instance.rvolosatovs_serde_deserializer().deserializer();
    let de = instance.call_from_list(&mut store, br#"{"foo": "test", "key": "foo"}"#)?;
    match instance.call_deserialize_record(&mut store, de, &["foo".to_string()])? {
        Ok(pld) => {
            let mut val = Val::Bool(false);
            deserialize_payload(&mut store, instance, de, pld, &mut val)?;
            eprintln!("{val:?}");
            Ok(())
        }
        Err(err) => {
            todo!("handle error")
        }
    }
}
