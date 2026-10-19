# Query: CheckInitial

## Summary

The builder `crate::queries::check_initial` (`queries/check_initial`)
performs:

1. **Early-out** for anything that is not a non-external `ModDef`: returns empty.
2. Fetches `ComponentAnalysis` and `DriverAnalysis` for the symbol.
3. For each `(component_id, locations)` in `driver_analysis.initials()`:
   - If the component kind is not `Reg`/`OutgoingReg`, emits
     `InitialForNonReg` for every `initial` statement targeting it.
   - Otherwise, if there is more than one `initial` statement, emits
     `InitialMultiple` for each statement after the first.
4. Returns `Arc::new(diagnostics)`.

Diagnostic regions cover the whole `initial` statement (the parent of the
recorded RHS expression node, fetched from `Parsing`).

### Diagnostics emitted

- `InitialForNonReg` - an `initial` statement targets a non-register component.
- `InitialMultiple` - a register has more than one `initial` statement.

`InitialInsideWhenMatch` and `UnresolvedComponent` for `initial` statements
are emitted by `DriverAnalysis` and forwarded by `CheckDrivers`.

## Signature

```rust
CheckInitial(symbol_id: SymbolId) -> Arc<Vec<Diagnostic>>
```

## Result

Per-module `initial`-statement diagnostics, as `Arc<Vec<Diagnostic>>`.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`SymbolAst`](SymbolAst.md)
* [`Parsing`](Parsing.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`DriverAnalysis`](DriverAnalysis.md)

## Example

```virdant
reg r : Word[8] on clock
initial r <= 1
initial r <= 2
r <= r + 1
```

`CheckInitial(<id>)` emits a single `InitialMultiple { target: "r" }` at the
second `initial` statement.
