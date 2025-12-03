```
cargo build -p wasm-serde-json --target wasm32-unknown-unknown --release
cargo build -p example-app --target wasm32-unknown-unknown --release
cargo run --example runtime 'myapp:app/custom@0.1.0#foo' '[{"foo":"myfoo","bar":"mybar"}]'
```
