```
cargo build --manifest-path ./json/Cargo.toml --target wasm32-unknown-unknown --release
cargo build --manifest-path ./examples/app/Cargo.toml --target wasm32-unknown-unknown --release
cargo run --example runtime 'myapp:app/custom@0.1.0#foo' '[{"foo":"myfoo","bar":"mybar"}]'
```
