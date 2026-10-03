# Query: ComponentAnalysis

## Summary

The builder `crate::analysis::component::build_component_analysis`
(`component`) resolves the moddef `Symbol` to its AST node, then
walks the moddef's statements:

- For each `Component` payload, it builds a `ComponentId { item_id: moddef,
  index: components.len() }`, resolves the type via `get_type_at`, derives
  `Flow` from `ComponentKind`, and pushes the component if its path is new.
- For each `Submodule`, it resolves the submodule item, fetches its AST, and
  iterates its port `Component`s, producing `instance.port` paths with flow
  inverted relative to the submodule's perspective. Socket channels inside the
  submodule produce `instance.socket.channel` paths.
- For each `Socket` declared directly in this moddef, it produces
  `instance.channel` paths.

After all components are registered, `collect_references` walks every
statement and expression, resolving `ExprReference`s, driver targets,
bidirectional-driver sides, and `unused` paths to their `ComponentId`s and
inserting them into `references`. The `it` keyword is handled by substituting
the enclosing component/submodule/socket name as a prefix, threaded through
recursion via an `it_context`. Pattern-bound variables (`PatIdent`/`PatCtor`
sub-vars) are skipped via `is_shadowed_by_pattern`.

`ComponentAnalysis` is the bridge between the syntax tree and the driver,
dependency, and elaboration analyses: `DriverAnalysis` uses it to resolve
driver targets, `DependencyGraph` uses it to resolve references, and
`Elaboration` inlines its components along the instance tree.

## Signature

```rust
ComponentAnalysis(symbol_id: SymbolId) -> Arc<ComponentAnalysis>
```

## Result

Per-module analysis of every signal-like component declared inside a module
definition: ports, wires, registers, submodule ports (flattened with dotted
paths), and socket channels, plus a map of where each one is referenced.

### `ComponentAnalysis`

```rust
#[derive(Debug)]
pub struct ComponentAnalysis {
    moddef: SymbolId,
    components: Vec<(BString, Component)>,
    diagnostics: Vec<Diagnostic>,

    references: IndexMap<Location, (ComponentId, ReferenceKind)>,
}
```

Defined in `virdant/src/analysis/component`.

- `moddef` - the `SymbolId` of the module definition (`ModDef`) this analysis is
  for. Exposed via `moddef_symbol_id()`.
- `components` - all components as `(path, Component)` pairs. The `path` is a
  dotted byte string: `"sig"` for a top-level component, `"sub.port"` for a
  submodule port, or `"sub.sock.chan"` for a socket channel. Submodule port
  lists and socket channel definitions are flattened in with qualified paths.
  Duplicate paths are skipped (the first declaration wins).
- `diagnostics` - diagnostics collected during construction (e.g.
  `UnresolvedItem` for unresolved submodule names, `ItNotInItBlock` for `it`
  references outside `it` blocks). Exposed via `diagnostics()`.
- `references` - map from each reference's `Location` (the AST node of an
  `ExprReference`, a driver target, a bidirectional-driver side, or an `unused`
  path) to the resolved `(ComponentId, ReferenceKind)` it refers to. Collected
  by `collect_references` after all components are registered.

### Notable related types

`ReferenceKind` (`component`):

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReferenceKind {
    Expr,
    DriverTarget,
    Unused,
}
```

`Component` and `ComponentId` - see the `Component` query report.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)
* [`PackageAnalysis`](PackageAnalysis.md)
* [`Packages`](Packages.md)
* [`TypeAt`](TypeAt.md)

## Example

For `tests/pass/sockets/src/top.vir`:

```virdant
mod Top {
    mod core of Core
    mod memory of Memory

    memory.mem :=: core.mem
}
```

`ComponentAnalysis(<Top's SymbolId>)` records no top-level `Component`s but
flattens the submodule instances: `core.mem` and `memory.mem` (each a socket
channel). The `references` map records that the `:=:` bidirectional driver's
two sides resolve to those two `ComponentId`s. For `tests/pass/edge/src/top.vir`, the
components are `clock`, `reset`, `inp`, `out` (Incoming/Outgoing ports) and
`last` (a `Reg`); `out := !last && inp` contributes references for `last` and
`inp`, while `unused reset` records `reset` with `ReferenceKind::Unused`.
