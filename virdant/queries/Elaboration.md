# Query: Elaboration

## Summary

The builder `crate::analysis::elaboration::build_elaboration`
(`elaboration`) calls the recursive worker `elaborate_module`
(`elaboration`), which walks the moddef's AST children:

- For each `Component`, it constructs a fully-qualified `path`
  (`{prefix}.{name}`), fetches the type via `get_type_at`, derives
  `driver_type` (`Latched` for `Reg`/`OutgoingReg`, else `Continuous`), and
  resolves the `driver`:
  - `Incoming` ports are driven from the **parent scope** via a `ParentCtx`
    (looks up `{instance_name}.{port}` in the parent's `ComponentAnalysis` +
    `DriverAnalysis`).
  - All other kinds look up `component_analysis.resolve(name)` and pull the
    first driver from `DriverAnalysis`.
- For each `Submodule`, it resolves the ofness (`of <pkg?> <name>`), looks up
  the submodule's `SymbolId`, builds a `ParentCtx` carrying this module's
  analyses plus the instance name, and recurses with prefix
  `{prefix}.{instance_name}`.
- For each `Socket`, it records an `ElaboratedSocket` (does not recurse).
- Clock resolution: if `stmt.clock()` is present, it looks up the named clock's
  full path in the already-accumulated `components`.

After recursion, it builds `path_to_id` from `components`, then `resolve_alias`
(`elaboration`) resolves aliases: for every component whose `driver`
is `Driver::Expr(_, loc)` whose AST node is an `ExprReference`, it computes the
scope prefix (dropping one segment for non-incoming components, two for incoming
ports) and looks up `{module_prefix}.{ref_path}` in `path_to_id`, writing the
result into each `component.alias`. Returns `Arc::new(Elaboration { ... })`.

## Signature

```rust
Elaboration(top: SymbolId) -> Arc<Elaboration>
```

## Result

The flat elaboration of a module hierarchy: every signal-level component after
inlining all submodules and sockets, with fully-qualified dotted paths,
resolved types, drivers, and clock references.

### `Elaboration`

```rust
#[derive(Debug)]
pub struct Elaboration {
    components: Vec<ElaboratedComponent>,
    path_to_id: IndexMap<BString, SignalId>,
    #[allow(dead_code)]
    modules: Vec<ElaboratedModule>,
    #[allow(dead_code)]
    sockets: Vec<ElaboratedSocket>,
}
```

Defined in `virdant/src/analysis/elaboration`. Fields are private; access
is via methods.

- `components` - the flat list of every elaborated signal-level component after
  inlining submodules/sockets. Indexed by dense `SignalId`. Exposed via
  `components() -> &[ElaboratedComponent]` and `component(SignalId)`.
- `path_to_id` - reverse index from a fully-qualified dotted `BString` path
  (e.g. `top.gcd.result`) to its `SignalId`. Powers `resolve<P: AsRef<BStr>>
  (path) -> Option<&ElaboratedComponent>`.
- `modules` - records of each module instance encountered (currently
  `#[allow(dead_code)]`).
- `sockets` - records of each socket instance encountered (currently
  `#[allow(dead_code)]`).

### `ElaboratedComponent`

```rust
#[derive(Debug)]
pub struct ElaboratedComponent {
    id: SignalId,
    path: BString,
    typ: Type,
    component_kind: ComponentKind,
    driver_type: DriverType,
    driver: Option<Driver>,
    alias: Option<SignalId>,
    clock: Option<SignalId>,
}
```

Defined in `virdant/src/analysis/elaboration`.

- `id` - dense index into `components`.
- `path` - fully-qualified dotted name, e.g. `top.core.x`.
- `typ` - resolved `Type` of the component.
- `component_kind` - original `ComponentKind` (Incoming/Outgoing/Wire/Reg/etc.).
- `driver_type` - `Continuous` for combinational, `Latched` for `Reg`/
  `OutgoingReg`. `is_reg()` returns true iff `Latched`.
- `driver` - optional `Driver` from `DriverAnalysis`; `None` means undriven.
- `alias` - if the driver is a bare `ExprReference` to another elaborated
  component, this is that target's `SignalId` (filled in by `resolve_alias`).
- `clock` - for `reg x : T on clk`, the `SignalId` of the referenced clock
  component.

### Notable related types

`SignalId` (`elaboration`): `pub struct SignalId(usize);` with
`index(self) -> usize`.

`ElaboratedModule` (`elaboration`):
```rust
#[derive(Debug)]
pub struct ElaboratedModule {
    #[allow(dead_code)] moddef: SymbolId,
    #[allow(dead_code)] prefix: BString,
}
```

`ElaboratedSocket` (`elaboration`):
```rust
#[derive(Debug)]
pub struct ElaboratedSocket {
    #[allow(dead_code)] socketdef: SymbolId,
    #[allow(dead_code)] role: SocketRole,
    #[allow(dead_code)] prefix: BString,
}
```

`Driver` (`analysis/drivers`): `enum Driver { Expr(DriverType,
Location), Bidirectional(Location), When(DriverWhen), Match(DriverMatch) }`.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`DriverAnalysis`](DriverAnalysis.md)
* [`TypeAt`](TypeAt.md)

## Example

For `tests/pass/gcd/src/top.vir`:

```virdant
export mod Top {
    incoming clock : Clock
    incoming reset : Bit
    incoming x : Word[8]
    incoming y : Word[8]
    incoming fire : Bit
    outgoing result : Word[8]
    outgoing valid  : Bit

    mod gcd of Gcd {
        it.reset := reset
        it.clock := clock
        it.x := x
        it.y := y
        it.fire := fire
        result := it.result
        valid := it.valid
    }
}
```

`Elaboration(<Top id>)` flattens the `gcd` submodule instance, producing
components like `top.clock`, `top.reset`, `top.x`, `top.gcd.clock`,
`top.gcd.result`, etc. The `Incoming` ports of `Gcd` (`clock`, `x`, etc.) are
driven from `Top`'s scope: `top.gcd.x`'s driver comes from the parent context
matching `gcd.x`, which resolves to `top.x`. `top.result`'s driver is
`it.result`, which `resolve_alias` resolves to `top.gcd.result`'s `SignalId`.
The `top.gcd.state` register records `clock = <SignalId of top.gcd.clock>`.
