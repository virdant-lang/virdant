# Query: CheckDrivers

## Summary

The builder `crate::queries::check_drivers` (`queries/check_drivers`)
performs:

1. **Early-out** for external modules (`mod_def.is_ext`): returns empty.
2. Fetches `ComponentAnalysis` and `DriverAnalysis` for the symbol.
3. `check_wrong_driver_types` - first forwards all diagnostics
   from `DriverAnalysis.diagnostics()` (the structural ones). Then, for each
   component with a known `kind`, computes the expected `DriverType`:
   - `Reg`/`OutgoingReg` -> `Latched`.
   - `Wire`/`Incoming`/`Outgoing`/`OutgoingWire` -> `Continuous`.
   - For each driver on that component, recurses through the driver tree and
     for every leaf `Driver::Expr(dt, loc)` where `dt != expected`, emits
     `WrongDriverType { region, target, expected_driver_type }`.
     `Bidirectional` leaves are ignored.
4. `get_all_driver_locations` walks the AST and collects, for
   each driver, `(path, DriverType, Location)`. For each path that resolves to a
   component that `!can_sink()` and has at least one driver entry, emits
   `DriverForSink { region, target }` for every entry.
5. For every component in `component_analysis.components()`:
   - If `can_sink()`:
     - `None` drivers: `NoRegDrivers` for `Reg`/`OutgoingReg`, else `NoDrivers`.
     - `Some(drivers)` with `len() > 1`: `MultipleDrivers` for each driver with
       a location.
6. Returns `Arc::new(diagnostics)`.

### Diagnostics emitted

- `WrongDriverType` - driver kind (`=` vs `<=`) doesn't match the component kind.
- `DriverForSink` - a driver targets a component that cannot sink.
- `NoDrivers` - a sink-capable non-reg component has zero drivers.
- `NoRegDrivers` - a reg component has zero drivers.
- `MultipleDrivers` - a sink-capable component has more than one driver.
- Plus forwarded `DriverAnalysis` diagnostics: `UnresolvedComponent`, `NotIt`,
  `ItNotInItBlock`, `EmptyDriverBlock`, `RedundantDriver`.

## Signature

```rust
CheckDrivers(symbol_id: SymbolId) -> Arc<Vec<Diagnostic>>
```

## Result

Per-module driver-correctness diagnostics: wrong driver type for a component
kind, drivers targeting non-sink components, missing drivers, and multiple
drivers.

The return type is `Arc<Vec<Diagnostic>>` (see `SyntaxErrors` for the
`Diagnostic` definition). There is no `CheckDrivers` struct; the query name is
the check, and the result is a vector of diagnostics.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`SymbolAst`](SymbolAst.md)
* [`Parsing`](Parsing.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`DriverAnalysis`](DriverAnalysis.md)
* [`LocationRegion`](LocationRegion.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
reg last : Bit on clock {
    it <= inp
}

out := !last && inp
```

`CheckDrivers(<Top id>)` finds that `last` (a `Reg`) is driven by one latched
driver (`<=`) - correct, no diagnostic. `out` (an `Outgoing`) is driven by one
continuous driver (`:=`) - correct. `reset` is marked `unused`, so no
`NoDrivers`. The result is an empty `Vec` for this valid file.

A file with `reg x : Bit on clock { it := 0 }` (using `:=` instead of `<=`)
would produce `WrongDriverType { target: "x", expected: Latched }`.
