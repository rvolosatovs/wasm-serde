mod bindings {
    wasmtime::component::bindgen!();
}

use core::mem;

use anyhow::{Context as _, anyhow, bail, ensure};
use wasmtime::component::types::{self, Field};
use wasmtime::component::{Component, Func, Instance, Linker, ResourceAny, Type, Val};
use wasmtime::{AsContextMut, Engine, Store};
use wit_component::ComponentEncoder;

use bindings::exports::rvolosatovs::serde::deserializer;

fn try_flatten_result<T>(
    store: impl AsContextMut,
    instance: &deserializer::Guest,
    res: wasmtime::Result<T, deserializer::Error>,
) -> wasmtime::Result<T> {
    match res {
        Ok(v) => Ok(v),
        Err(err) => {
            let err = instance.error().call_to_string(store, err)?;
            Err(anyhow!(err))
        }
    }
}

fn mk_flatten_result<T>(
    store: impl AsContextMut,
    instance: &deserializer::Guest,
) -> impl FnOnce(wasmtime::Result<T, deserializer::Error>) -> wasmtime::Result<T> {
    |res| try_flatten_result(store, instance, res)
}

fn deserialize(
    store: &mut Store<()>,
    de: ResourceAny,
    instance: &deserializer::Guest,
    ty: Type,
    v: &mut Val,
) -> wasmtime::Result<()> {
    let de = match ty {
        Type::Bool => instance
            .deserializer()
            .call_deserialize_bool(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::Bool)?,
        Type::S8 => instance
            .deserializer()
            .call_deserialize_s8(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::S8)?,
        Type::U8 => instance
            .deserializer()
            .call_deserialize_u8(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::U8)?,
        Type::S16 => instance
            .deserializer()
            .call_deserialize_s16(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::S16)?,
        Type::U16 => instance
            .deserializer()
            .call_deserialize_u16(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::U16)?,
        Type::S32 => instance
            .deserializer()
            .call_deserialize_s32(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::S32)?,
        Type::U32 => instance
            .deserializer()
            .call_deserialize_u32(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::U32)?,
        Type::S64 => instance
            .deserializer()
            .call_deserialize_s64(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::S64)?,
        Type::U64 => instance
            .deserializer()
            .call_deserialize_u64(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::U64)?,
        Type::Float32 => instance
            .deserializer()
            .call_deserialize_f32(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::Float32)?,
        Type::Float64 => instance
            .deserializer()
            .call_deserialize_f64(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::Float64)?,
        Type::Char => instance
            .deserializer()
            .call_deserialize_char(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::Char)?,
        Type::String => instance
            .deserializer()
            .call_deserialize_string(&mut *store, de)
            .and_then(mk_flatten_result(store, instance))
            .map(Val::String)?,
        #[expect(unused, reason = "incomplete")]
        Type::List(ty) => todo!(),
        Type::Record(ty) => {
            let mut names = ty
                .fields()
                .map(|Field { name, .. }| name.into())
                .collect::<Vec<_>>();
            let tys = ty.fields().map(|Field { ty, .. }| ty).collect::<Vec<_>>();
            let (mut idx, mut de, mut iter) = instance
                .deserializer()
                .call_deserialize_record(&mut *store, de, &names)
                .and_then(mk_flatten_result(&mut *store, instance))?;
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
            .with_context(|| format!("failed to deserialize record field with index `{idx}`"))?;
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
            Val::Record(vs)
        }
        Type::Tuple(ty) => {
            let mut tys = ty.types();
            let num_elements = tys.len();
            let (mut de, mut iter) = instance
                .deserializer()
                .call_deserialize_tuple(&mut *store, de, num_elements as _)
                .and_then(mk_flatten_result(&mut *store, instance))?;
            let mut vs = Vec::with_capacity(num_elements);
            let mut ev = Val::Bool(false);
            let ty = tys
                .next()
                .context("failed to get first tuple element type")?;
            deserialize(&mut *store, de, instance, ty, &mut ev)
                .context("failed to deserialize first tuple element")?;
            vs.push(ev);
            for ty in tys {
                let next = instance
                    .tuple_deserializer()
                    .call_next(&mut *store, iter)
                    .context("failed to call `next`")?;
                de = next.0;
                iter = next.1;
                let mut ev = Val::Bool(false);
                deserialize(&mut *store, de, instance, ty, &mut ev)
                    .context("failed to deserialize tuple element")?;
                vs.push(ev);
            }
            Val::Tuple(vs)
        }
        #[expect(unused, reason = "incomplete")]
        Type::Variant(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Enum(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Option(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Result(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Flags(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Own(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Borrow(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Future(ty) => todo!(),
        #[expect(unused, reason = "incomplete")]
        Type::Stream(ty) => todo!(),
        Type::ErrorContext => todo!(),
    };
    *v = de;
    Ok(())
}

fn deserialize_params(
    store: &mut Store<()>,
    instance: &deserializer::Guest,
    ty: &types::ComponentFunc,
    payload: impl AsRef<[u8]>,
) -> wasmtime::Result<Vec<Val>> {
    let payload = payload.as_ref();
    let mut tys = ty.params();
    let num_params = tys.len();
    if num_params == 0 {
        ensure!(payload.is_empty());
        return Ok(Vec::default());
    }
    let de = instance
        .deserializer()
        .call_from_list(&mut *store, payload.as_ref())?;

    let (mut de, mut iter) = instance
        .deserializer()
        .call_deserialize_tuple(&mut *store, de, num_params as _)
        .and_then(mk_flatten_result(&mut *store, instance))?;
    let mut vs = Vec::with_capacity(num_params);

    let mut pv = Val::Bool(false);
    let (name, ty) = tys.next().context("failed to get first parameter type")?;
    deserialize(&mut *store, de, instance, ty, &mut pv)
        .with_context(|| format!("failed to deserialize param `{name}`"))?;
    vs.push(pv);
    for (name, ty) in tys {
        let next = instance
            .tuple_deserializer()
            .call_next(&mut *store, iter)
            .context("failed to call `next`")?;
        de = next.0;
        iter = next.1;
        let mut pv = Val::Bool(false);
        deserialize(&mut *store, de, instance, ty, &mut pv)
            .with_context(|| format!("failed to deserialize param `{name}`"))?;
        vs.push(pv);
    }
    Ok(vs)
}

fn get_func(
    engine: &Engine,
    mut store: impl AsContextMut,
    instance: Instance,
    ty: &types::Component,
    name: &str,
) -> wasmtime::Result<(Func, types::ComponentFunc)> {
    if let Some((instance_name, name)) = name.split_once('#') {
        let Some(types::ComponentItem::ComponentInstance(ty)) =
            ty.get_export(engine, instance_name)
        else {
            bail!("instance not found")
        };
        let (_, instance_idx) = instance
            .get_export(&mut store, None, instance_name)
            .context("instance not found")?;

        let Some(types::ComponentItem::ComponentFunc(ty)) = ty.get_export(engine, name) else {
            bail!("function not found")
        };
        let func_idx = instance
            .get_export_index(&mut store, Some(&instance_idx), name)
            .context("function index not found")?;
        let func = instance
            .get_func(&mut store, func_idx)
            .context("function not found")?;
        Ok((func, ty))
    } else {
        let Some(types::ComponentItem::ComponentFunc(ty)) = ty.get_export(engine, name) else {
            bail!("function not found")
        };
        let func = instance
            .get_func(&mut store, name)
            .context("function not found")?;
        Ok((func, ty))
    }
}

fn main() -> wasmtime::Result<()> {
    let mut args = std::env::args();
    let (func, payload) = match (args.next(), args.next(), args.next(), args.next()) {
        (_, Some(func), Some(payload), None) => (func, payload),
        (Some(exe), ..) => bail!("invalid arguments, usage: {exe} FUNC PAYLOAD"),
        _ => bail!("invalid arguments, usage: FUNC PAYLOAD"),
    };

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

    let (f, ty) = get_func(&engine, &mut app_store, app, &app_ty, &func).map_err(|err| {
        eprintln!("exported function `{func}` not found, perhaps you meant one of:");
        for (name, ty) in app_ty.exports(&engine) {
            match ty {
                types::ComponentItem::ComponentFunc(..) => {
                    eprintln!("\t{name}");
                }
                types::ComponentItem::ComponentInstance(ty) => {
                    for (fname, ty) in ty.exports(&engine) {
                        let types::ComponentItem::ComponentFunc(..) = ty else {
                            continue;
                        };
                        eprintln!("\t{name}#{fname}");
                    }
                }
                _ => continue,
            }
        }
        err
    })?;

    let params = deserialize_params(&mut codec_store, codec, &ty, payload)?;
    let mut results = vec![Val::Bool(false); ty.results().len()];
    f.call(&mut app_store, &params, &mut results)
        .context("failed to call function")?;
    println!("{results:?}");
    Ok(())
}
