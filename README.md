```
cargo build --target wasm32-unknown-unknown --release --workspace
cargo run --manifest-path ./runtime/Cargo.toml 'myapp:app/custom@0.1.0#foo' '[{"foo":"myfoo","bar":"mybar"}]'
```
