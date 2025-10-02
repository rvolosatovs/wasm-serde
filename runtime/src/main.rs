mod bindings {
    wasmtime::component::bindgen!();
}

use core::iter::zip;
use core::mem;

use anyhow::{Context as _, anyhow};
use wasmtime::component::types::Field;
use wasmtime::component::{Component, Linker, ResourceAny, Type, Val, types};
use wasmtime::{Engine, Store};
use wit_component::ComponentEncoder;

use bindings::exports::rvolosatovs::serde::deserializer::Guest;

pub struct Error;

fn deserialize(
    store: &mut Store<()>,
    de: ResourceAny,
    instance: &Guest,
    ty: Type,
    v: &mut Val,
) -> wasmtime::Result<()> {
    #[expect(unused, reason = "incomplete")]
    match ty {
        Type::Bool => todo!(),
        Type::S8 => todo!(),
        Type::U8 => todo!(),
        Type::S16 => todo!(),
        Type::U16 => todo!(),
        Type::S32 => todo!(),
        Type::U32 => todo!(),
        Type::S64 => todo!(),
        Type::U64 => todo!(),
        Type::Float32 => todo!(),
        Type::Float64 => todo!(),
        Type::Char => todo!(),
        Type::String => match instance.deserializer().call_deserialize_string(store, de)? {
            Ok(s) => {
                *v = Val::String(s);
                Ok(())
            }
            Err(_) => todo!(),
        },
        Type::List(ty) => todo!(),
        Type::Record(ty) => {
            let mut names = ty
                .fields()
                .map(|Field { name, .. }| name.into())
                .collect::<Vec<_>>();
            let tys = ty.fields().map(|Field { ty, .. }| ty).collect::<Vec<_>>();
            match instance
                .deserializer()
                .call_deserialize_record(&mut *store, de, &names)?
            {
                Ok((mut idx, mut de, mut iter)) => {
                    let num_fields = ty.fields().len();
                    let mut vs = Vec::with_capacity(num_fields);
                    let mut fv = Val::Bool(false);
                    deserialize(
                        &mut *store,
                        de,
                        instance,
                        tys[idx as usize].clone(),
                        &mut fv,
                    )
                    .with_context(|| {
                        format!("failed to deserialize record field with index `{idx}`")
                    })?;
                    vs.push((idx, fv));
                    for _ in 1..num_fields {
                        let next = instance
                            .record_deserializer()
                            .call_next(&mut *store, iter)
                            .context("failed to call `next`")?;
                        idx = next.0;
                        de = next.1;
                        iter = next.2;
                        let mut fv = Val::Bool(false);
                        deserialize(
                            &mut *store,
                            de,
                            instance,
                            tys[idx as usize].clone(),
                            &mut fv,
                        )
                        .with_context(|| {
                            format!("failed to deserialize record field with index `{idx}`")
                        })?;
                        vs.push((idx, fv));
                    }
                    vs.sort_unstable_by(|(l, ..), (r, ..)| l.cmp(r));
                    let vs = vs
                        .into_iter()
                        .map(|(idx, v)| (mem::take(&mut names[idx as usize]), v))
                        .collect();
                    *v = Val::Record(vs);
                    Ok(())
                }
                Err(err) => {
                    let err = instance.error().call_to_string(store, err)?;
                    Err(anyhow!(err))
                }
            }
        }
        Type::Tuple(ty) => todo!(),
        Type::Variant(ty) => todo!(),
        Type::Enum(ty) => todo!(),
        Type::Option(ty) => todo!(),
        Type::Result(ty) => todo!(),
        Type::Flags(ty) => todo!(),
        Type::Own(ty) => todo!(),
        Type::Borrow(ty) => todo!(),
        Type::Future(ty) => todo!(),
        Type::Stream(ty) => todo!(),
        Type::ErrorContext => todo!(),
    }
}

fn main() -> wasmtime::Result<()> {
    let pld = std::env::args().skip(1).next().context("payload missing")?;

    let engine = Engine::default();

    let mut app = ComponentEncoder::default().module(include_bytes!(
        "../../target/wasm32-unknown-unknown/release/app.wasm"
    ))?;
    let app = app.encode()?;
    let app = Component::new(&engine, app)?;
    let linker = Linker::new(&engine);
    let mut app_store = Store::new(&engine, ());
    let app_ty = linker.substituted_component_type(&app)?;
    let app = linker.instantiate(&mut app_store, &app)?;

    let mut codec = ComponentEncoder::default().module(include_bytes!(
        "../../target/wasm32-unknown-unknown/release/wasm_serde.wasm"
    ))?;
    let codec = codec.encode()?;
    let codec = Component::new(&engine, codec)?;
    let linker = Linker::new(&engine);
    let mut codec_store = Store::new(&engine, ());
    let codec = bindings::Format::instantiate(&mut codec_store, &codec, &linker)?;
    let codec = codec.rvolosatovs_serde_deserializer();

    let de = codec
        .deserializer()
        .call_from_list(&mut codec_store, pld.as_bytes())?;
    for (name, ty) in app_ty.exports(&engine) {
        match ty {
            types::ComponentItem::ComponentFunc(ty) => {
                let f = app
                    .get_func(&mut app_store, name)
                    .expect("export not found");
                let mut params = vec![Val::Bool(false); ty.params().len()];
                for ((_, ty), v) in zip(f.params(&app_store), &mut params) {
                    deserialize(&mut codec_store, de, codec, ty, v)
                        .context("failed to deserialize param")?;
                }
                let mut results = vec![Val::Bool(false); ty.results().len()];
                f.call(&mut app_store, &params, &mut results)
                    .context("failed to call function")?;
                eprintln!("{name}: {results:?}");
            }
            types::ComponentItem::ComponentInstance(ty) => {
                let (_, instance) = app
                    .get_export(&mut app_store, None, name)
                    .expect("instance export not found");
                for (name, ty) in ty.exports(&engine) {
                    let types::ComponentItem::ComponentFunc(ty) = ty else {
                        continue;
                    };
                    let (_, idx) = app
                        .get_export(&mut app_store, Some(&instance), name)
                        .expect("instance export not found");
                    let f = app.get_func(&mut app_store, idx).expect("export not found");
                    let mut params = vec![Val::Bool(false); ty.params().len()];
                    for ((_, ty), v) in zip(f.params(&app_store), &mut params) {
                        deserialize(&mut codec_store, de, codec, ty, v)
                            .context("failed to deserialize param")?;
                    }
                    let mut results = vec![Val::Bool(false); ty.results().len()];
                    f.call(&mut app_store, &params, &mut results)
                        .context("failed to call function")?;
                    eprintln!("{name}: {results:?}");
                }
            }
            _ => continue,
        }
    }
    Ok(())
}
