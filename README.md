```
cargo build --target wasm32-unknown-unknown --release --workspace
cargo build --target wasm32-unknown-unknown --release -p example-app 
cargo run --example runtime 'myapp:app/custom@0.1.0#foo' '[{"foo":"myfoo","bar":"mybar"}]' json
cargo run --example runtime 'myapp:app/custom@0.1.0#foo' '[{ foo = "myfoo", bar = "mybar" }]' toml
```
