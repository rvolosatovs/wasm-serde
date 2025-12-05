mod bindings {
    wasmtime::component::bindgen!({
        world: "format",
    });
}

use core::iter::zip;

use anyhow::{Context as _, anyhow, bail, ensure};
use wasmtime::component::types;
use wasmtime::component::{Component, Func, Instance, Linker, Type, Val};
use wasmtime::{AsContextMut, Engine, Store};
use wit_component::ComponentEncoder;

use bindings::exports::cosmonic::serde::{deserializer, reflect};

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

fn unwrap_val(
    mut store: &mut Store<()>,
    v: reflect::Value,
    instance: &reflect::Guest,
    ty: Type,
) -> wasmtime::Result<Val> {
    match (v, ty) {
        (reflect::Value::Bool(v), Type::Bool) => Ok(Val::Bool(v)),
        (reflect::Value::S8(v), Type::S8) => Ok(Val::S8(v)),
        (reflect::Value::U8(v), Type::U8) => Ok(Val::U8(v)),
        (reflect::Value::U16(v), Type::U16) => Ok(Val::U16(v)),
        (reflect::Value::S16(v), Type::S16) => Ok(Val::S16(v)),
        (reflect::Value::U32(v), Type::U32) => Ok(Val::U32(v)),
        (reflect::Value::S32(v), Type::S32) => Ok(Val::S32(v)),
        (reflect::Value::U64(v), Type::U64) => Ok(Val::U64(v)),
        (reflect::Value::S64(v), Type::S64) => Ok(Val::S64(v)),
        (reflect::Value::F32(v), Type::Float32) => Ok(Val::Float32(v)),
        (reflect::Value::F64(v), Type::Float64) => Ok(Val::Float64(v)),
        (reflect::Value::Char(v), Type::Char) => Ok(Val::Char(v)),
        (reflect::Value::String(v), Type::String) => Ok(Val::String(v)),
        (reflect::Value::Record(v), Type::Record(ty)) => {
            let values = instance.record_value().call_into_value(&mut store, v)?;
            ensure!(values.len() == ty.fields().len());

            let mut fields = Vec::with_capacity(values.len());
            for (types::Field { name, ty }, v) in zip(ty.fields(), values) {
                let v = unwrap_val(store, v, instance, ty)
                    .with_context(|| format!("failed to unwrap record field `{name}`"))?;
                fields.push((name.into(), v));
            }
            Ok(Val::Record(fields))
        }
        #[expect(unused, reason = "incomplete")]
        (reflect::Value::Variant(v), Type::Variant(ty)) => todo!(),
        #[expect(unused, reason = "incomplete")]
        (reflect::Value::List(v), Type::List(ty)) => todo!(),
        (reflect::Value::Tuple(v), Type::Tuple(ty)) => {
            let values = instance.tuple_value().call_into_value(&mut store, v)?;
            ensure!(values.len() == ty.types().len());

            let mut elems = Vec::with_capacity(values.len());
            for ((i, ty), v) in zip(ty.types().enumerate(), values) {
                let v = unwrap_val(store, v, instance, ty)
                    .with_context(|| format!("failed to unwrap tuple element `{i}`"))?;
                elems.push(v);
            }
            Ok(Val::Tuple(elems))
        }
        #[expect(unused, reason = "incomplete")]
        (reflect::Value::Flags(v), Type::Flags(ty)) => todo!(),
        #[expect(unused, reason = "incomplete")]
        (reflect::Value::Enum(v), Type::Enum(ty)) => todo!(),
        #[expect(unused, reason = "incomplete")]
        (reflect::Value::Option(v), Type::Option(ty)) => todo!(),
        #[expect(unused, reason = "incomplete")]
        (reflect::Value::Result(v), Type::Result(ty)) => todo!(),
        _ => bail!("type mismatch"),
    }
}

fn make_reflect_ty(
    store: &mut Store<()>,
    instance: &reflect::Guest,
    ty: Type,
) -> wasmtime::Result<reflect::Type> {
    match ty {
        Type::Bool => Ok(reflect::Type::Bool),
        Type::S8 => Ok(reflect::Type::S8),
        Type::U8 => Ok(reflect::Type::U8),
        Type::S16 => Ok(reflect::Type::S16),
        Type::U16 => Ok(reflect::Type::U16),
        Type::S32 => Ok(reflect::Type::S32),
        Type::U32 => Ok(reflect::Type::U32),
        Type::S64 => Ok(reflect::Type::S64),
        Type::U64 => Ok(reflect::Type::U64),
        Type::Float32 => Ok(reflect::Type::F32),
        Type::Float64 => Ok(reflect::Type::F64),
        Type::Char => Ok(reflect::Type::Char),
        Type::String => Ok(reflect::Type::String),
        Type::List(ty) => {
            match ty.ty() {
                Type::Bool => Ok(reflect::Type::List(reflect::ListType::Bool)),
                Type::S8 => Ok(reflect::Type::List(reflect::ListType::S8)),
                Type::U8 => Ok(reflect::Type::List(reflect::ListType::U8)),
                Type::S16 => Ok(reflect::Type::List(reflect::ListType::S16)),
                Type::U16 => Ok(reflect::Type::List(reflect::ListType::U16)),
                Type::S32 => Ok(reflect::Type::List(reflect::ListType::S32)),
                Type::U32 => Ok(reflect::Type::List(reflect::ListType::U32)),
                Type::S64 => Ok(reflect::Type::List(reflect::ListType::S64)),
                Type::U64 => Ok(reflect::Type::List(reflect::ListType::U64)),
                Type::Float32 => Ok(reflect::Type::List(reflect::ListType::F32)),
                Type::Float64 => Ok(reflect::Type::List(reflect::ListType::F64)),
                Type::Char => Ok(reflect::Type::List(reflect::ListType::Char)),
                Type::String => Ok(reflect::Type::List(reflect::ListType::String)),
                //Type::List(list) => Ok(reflect::Type::List(reflect::ListType::List)),
                //Type::Record(record) => Ok(reflect::Type::List(reflect::ListType::Record)),
                //Type::Tuple(tuple) => Ok(reflect::Type::List(reflect::ListType::Tuple)),
                //Type::Variant(variant) => Ok(reflect::Type::List(reflect::ListType::Variant)),
                //Type::Enum(_) => Ok(reflect::Type::List(reflect::ListType::Enum)),
                //Type::Option(option_type) => Ok(reflect::Type::List(reflect::ListType::Option)),
                //Type::Result(result_type) => Ok(reflect::Type::List(reflect::ListType::Result)),
                //Type::Flags(flags) => Ok(reflect::Type::List(reflect::ListType::Flags)),
                Type::Own(..) | Type::Borrow(..) => bail!("resources not supported"),
                Type::Future(..) => bail!("futures not supported"),
                Type::Stream(..) => bail!("streams not supported"),
                Type::ErrorContext => bail!("error context not supported"),
                _ => todo!(),
            }
        }
        Type::Record(ty) => {
            let fields = ty
                .fields()
                .map(|types::Field { name, ty }| {
                    make_reflect_ty(store, instance, ty).map(|ty| (name.into(), ty))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            let ty = instance.record_type().call_constructor(store, &fields)?;
            Ok(reflect::Type::Record(ty))
        }
        Type::Tuple(ty) => {
            let tys = ty
                .types()
                .map(|ty| make_reflect_ty(store, instance, ty))
                .collect::<anyhow::Result<Vec<_>>>()?;
            let ty = instance.tuple_type().call_constructor(store, &tys)?;
            Ok(reflect::Type::Tuple(ty))
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
    }
}

fn deserialize_params(
    mut store: &mut Store<()>,
    instance: &bindings::Format,
    ty: &types::ComponentFunc,
    payload: impl AsRef<[u8]>,
) -> wasmtime::Result<Vec<Val>> {
    let payload = payload.as_ref();
    let tys = ty.params();
    let num_params = tys.len();
    if num_params == 0 {
        ensure!(payload.is_empty());
        return Ok(Vec::default());
    }

    let reflect_tys = ty
        .params()
        .map(|(_, ty)| make_reflect_ty(store, instance.cosmonic_serde_reflect(), ty))
        .collect::<anyhow::Result<Vec<_>>>()?;
    let reflect_ty = instance
        .cosmonic_serde_reflect()
        .tuple_type()
        .call_constructor(&mut store, &reflect_tys)?;

    let values = instance
        .cosmonic_serde_deserializer()
        .call_from_list(
            &mut store,
            payload.as_ref(),
            reflect::Type::Tuple(reflect_ty),
        )
        .and_then(mk_flatten_result(
            &mut store,
            instance.cosmonic_serde_deserializer(),
        ))?;
    let reflect::Value::Tuple(values) = values else {
        bail!("deserialized value is not a tuple");
    };
    let values = instance
        .cosmonic_serde_reflect()
        .tuple_value()
        .call_into_value(&mut store, values)?;
    ensure!(values.len() == num_params);

    let mut params = Vec::with_capacity(num_params);
    for ((name, ty), v) in zip(tys, values) {
        let v = unwrap_val(store, v, instance.cosmonic_serde_reflect(), ty)
            .with_context(|| format!("failed to unwrap param `{name}`"))?;
        params.push(v);
    }
    Ok(params)
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
        "../target/wasm32-unknown-unknown/release/example_app.wasm"
    ))?;
    let app = app.encode()?;
    let app = Component::new(&engine, app)?;
    let linker = Linker::new(&engine);
    let mut app_store = Store::new(&engine, ());
    let app_ty = linker.substituted_component_type(&app)?;
    let app = linker.instantiate(&mut app_store, &app)?;

    let mut codec = ComponentEncoder::default().module(include_bytes!(
        "../target/wasm32-unknown-unknown/release/wasm_serde_json.wasm"
    ))?;
    let codec = codec.encode()?;
    let codec = Component::new(&engine, codec)?;
    let linker = Linker::new(&engine);
    let mut codec_store = Store::new(&engine, ());
    let codec = bindings::Format::instantiate(&mut codec_store, &codec, &linker)?;

    let (f, ty) = get_func(&engine, &mut app_store, app, &app_ty, &func).inspect_err(|err| {
        eprintln!("exported function `{func}` not found ({err}), perhaps you meant one of:");
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
    })?;

    let params = deserialize_params(&mut codec_store, &codec, &ty, payload)?;
    let mut results = vec![Val::Bool(false); ty.results().len()];
    f.call(&mut app_store, &params, &mut results)
        .context("failed to call function")?;
    println!("{results:?}");
    Ok(())
}
