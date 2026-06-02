# virdant-db

An incremental database/query system generator for Rust, inspired by salsa.

## Overview

virdant-db is a build-time code generator that creates custom incremental computation systems with automatic dependency tracking and caching. It scans your Rust code for functions annotated with `#[input]` and `#[query]`, then generates a database implementation with efficient change detection and lazy recomputation.

## Features

- **Automatic dependency tracking** - The system tracks which queries depend on which inputs/queries
- **Incremental recomputation** - Only dirty queries are recomputed when inputs change
- **Build-time code generation** - No runtime overhead, all code is generated at compile time
- **Type-safe** - Generated code preserves full type and borrow checker safety
- **Trace logging** - Optional tracing to understand query execution
- **GraphViz export** - Visualize the query dependency graph

## Quick Start

### 1. Add Dependencies

In your `Cargo.toml`:

```toml
[dependencies]
virdant-db = { path = "../virdant-db" }

[build-dependencies]
virdant-db = { path = "../virdant-db" }
```

### 2. Create Build Script

In `build.rs`:

```rust
fn main() {
    let output_dir = std::env::var_os("OUT_DIR").unwrap();
    let db_rs_path = std::path::Path::new(&output_dir).join("db.rs");
    
    virdant_db::Configuration::new()
        .input_dir("src")
        .output_file(db_rs_path)
        .db_name("Db")
        .builder_name("Builder")
        .query_enum_name("Query")
        .enable_tracing(true)
        .enable_graphviz(true)
        .process()
        .unwrap();
}
```

### 3. Annotate Functions

Mark input queries with `#[input]`:

```rust
use virdant_db::input;

#[input]
pub fn source_text(file_id: FileId) -> String {
    unimplemented!("Inputs are set manually")
}
```

Mark computed queries with `#[query]`:

```rust
use virdant_db::query;

#[query]
pub fn parse(builder: &mut Builder, file_id: FileId) -> Arc<SyntaxTree> {
    let text = builder.get_source_text(file_id);
    Arc::new(parse_file(&text))
}

#[query]
pub fn type_check(builder: &mut Builder, file_id: FileId) -> Vec<Error> {
    let syntax = builder.parse(file_id);
    // Type checking logic...
}
```

### 4. Include Generated Code

In your main database module:

```rust
// Include the generated database
include!(concat!(env!("OUT_DIR"), "/db.rs"));
```

### 5. Use the Database

```rust
let mut db = Db::new();

// Set input values
db.set_source_text(file1, "fn main() {}".to_string());

// Query derived values (computed on demand)
let syntax = db.parse(file1);
let errors = db.type_check(file1);

// Change an input
db.set_source_text(file1, "fn main() { println!(); }");

// Only affected queries are recomputed
let new_errors = db.type_check(file1);
```

## How It Works

1. **Build Time**: The build script scans your source code for annotated functions
2. **Code Generation**: Based on annotations, it generates:
   - `Query` and `QueryResult` enums
   - Accessor methods on `Db` and `Builder`
   - Dispatch logic for building queries
   - Caching and dependency tracking infrastructure

3. **Runtime**: When you query a value:
   - If cached and clean, returns immediately
   - If dirty or not cached, builds by calling the annotated function
   - Tracks dependencies during build
   - Caches result with dependency information

4. **Change Detection**: When inputs change:
   - Increments global revision counter
   - Clears dirty cache
   - Next query checks if dependencies are newer than cached value

## Design Principles

- **Inspired by salsa**: Similar incremental computation model
- **Simple**: No complex trait systems, just annotate functions
- **Build-time**: All code generation happens during build, no proc-macro overhead
- **Transparent**: Generated code is readable and debuggable

## Configuration Options

- `input_dir`: Directory to scan for annotated functions
- `output_file`: Path for generated db.rs
- `db_name`: Name of the generated database struct
- `builder_name`: Name of the builder struct
- `query_enum_name`: Name of the query enum
- `enable_tracing`: Enable trace logging
- `enable_graphviz`: Enable dependency graph export

##Limitations

- Functions must be `pub` so generated code can call them
- Query functions must take `&mut Builder` as first parameter
- Input functions are never called (only for type signature)
- All types in signatures must be in scope where db.rs is included

## Comparison to Alternatives

### vs salsa
- Simpler: No complex trait system
- Build-time generation instead of proc-macros
- Less featureful but easier to understand

### vs Manual Caching
- Automatic dependency tracking
- Systematic approach to incremental computation
- Less error-prone than manual bookkeeping

## License

Same as Virdant project.