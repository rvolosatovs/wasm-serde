```
cargo build --target wasm32-unknown-unknown --release --workspace
cargo run --manifest-path ./runtime/Cargo.toml '{"foo":"myfoo","bar":"mybar"}'
```
