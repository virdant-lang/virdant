# Query: PortsOf

## Summary

The builder `crate::analysis::ports::build_ports_of` (`ports`):

1. Returns an empty `Vec` immediately unless `symbol.kind() ==
   SymbolKind::ModDef` (only module definitions have ports).
2. Fetches `ComponentAnalysis` for the symbol and iterates
   `component_analysis.components()` (yields `(path, component)` pairs).
3. For each component, fetches its AST node and **skips submodule statements**
   (`AstNodePayload::Submodule(_)`).
4. Determines `is_port` (`ports`):
   - `Incoming`, `Outgoing`, `OutgoingWire`, `OutgoingReg` kinds are ports.
   - Components with `kind == None` (socket channels of the current module)
     whose `flow()` is `Source` or `Sink` are also ports.
   - Plain `Wire`/`Reg` and submodule channels are skipped.
5. Maps to a `PortDir` (`ports`):
   - `Incoming` -> `Input`.
   - `Outgoing`/`OutgoingWire`/`OutgoingReg` -> `Output` (note: the latter two
     have internal `Flow::Duplex` but are still outputs).
   - `kind == None` socket channel: `Flow::Source` -> `Input`, `Flow::Sink` ->
     `Output`, else skip.
6. Pushes `Port { path, dir, typ: component.typ() }`.
7. Returns `Arc::new(ports)`.

## Signature

```rust
PortsOf(symbol_id: SymbolId) -> Arc<Vec<Port>>
```

## Result

The list of ports (inputs and outputs) declared by a module definition, each
with its path, direction, and resolved type.

### `Port`

```rust
#[derive(Debug, Clone)]
pub struct Port {
    pub path: BString,
    pub dir: PortDir,
    pub typ: Option<Type>,
}
```

Defined in `virdant/src/analysis/ports`. All fields are `pub`.

- `path` - the component path as produced by `ComponentAnalysis` (a `BString`,
  dotted if it is a sub-component).
- `dir` - `PortDir::Input` or `PortDir::Output`.
- `typ` - the resolved `Type` if available, else `None`.

### Notable related types

`PortDir` (`common`):

```rust
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum PortDir { Input, Output }
```

`Type` (`types/typ`): the typechecker's resolved type enum (see `TypeAt`).

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`Parsing`](Parsing.md)

## Example

For `tests/pass/gcd/src/top.vir`'s `Gcd` module:

```virdant
mod Gcd {
    incoming clock : Clock
    incoming reset : Bit
    incoming x : Word[8]
    incoming y : Word[8]
    incoming fire : Bit
    outgoing result : Word[8]
    outgoing valid: Bit
    ...
}
```

`PortsOf(<Gcd id>)` returns a `Vec<Port>` containing:
- `Port { path: "clock", dir: Input, typ: Some(Type::Clock) }`
- `Port { path: "reset", dir: Input, typ: Some(Type::Bit) }`
- `Port { path: "x", dir: Input, typ: Some(Type::Word(8)) }`
- `Port { path: "y", dir: Input, typ: Some(Type::Word(8)) }`
- `Port { path: "fire", dir: Input, typ: Some(Type::Bit) }`
- `Port { path: "result", dir: Output, typ: Some(Type::Word(8)) }`
- `Port { path: "valid", dir: Output, typ: Some(Type::Bit) }`

The `state` register (`reg state : State on clock`) is not a port, so it is
skipped.
