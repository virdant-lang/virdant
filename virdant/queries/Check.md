# Query: Check

## Summary

The builder `crate::queries::check` (`queries/check`) accumulates
diagnostics from many sources, then sorts and wraps in `Arc`:

1. **Syntax errors** (`check`): `get_syntax_errors()` - the
   `SyntaxErrors()` query.
2. **Package-name/keyword collision** (`check`, helper
   `check_package_name_not_keyword`): for every package except
   `builtin`, if the package name is a Virdant keyword, emits an `Unknown`
   diagnostic (`"Package name '{}' is a keyword"`).
3. **Symbol-table diagnostics** (`check`): `symboltable.diagnostics` -
   duplicate items, duplicate slots, unresolved imports, etc.
4. **Type-index diagnostics** (`check`): `get_type_index().diagnostics()`
   - `UnresolvedType`, malformed type applications, etc.
5. **Per-item driver checks** (`check`): for each item,
   `check_drivers(item.id())` - the `CheckDrivers(SymbolId)` query (wrong driver
   type, missing/unexpected `on` clause, `NoDrivers`, `MultipleDrivers`, etc.).
6. **Per-item match-coverage** (`check`): `get_match_coverage(item.id())`
   - the `MatchCoverage(SymbolId)` query.
7. **Per-item typecheck + component-analysis diagnostics** (`check`):
   for each item, if it is a `ModDef`, extends with
   `component_analysis.diagnostics()`; always extends with
   `typecheck(item.id())` - the `TypeCheck(SymbolId)` query (forwarded `Typing`
   diagnostics plus `UnusedSource`/`ReadFromSink`).
8. **Duplicate enum values** (`check`, helper
   `check_duplicate_enum_values`): for each `EnumDef`, scans
   `typedef.enumerant_values`; the first duplicate value per `WordValue` emits a
   `DuplicateEnumValue { region, enum_name, value }`.
9. **Enum unknown width** (`check`, helper `check_enum_unknown_width`): for each `EnumDef` whose `typedef.width.is_none()` (the first
   enumerant lacks an explicit width like `0w8`), emits an
   `EnumUnknownWidth { region }`.
10. **Module instantiation cycles** (`check`, helper `check_mod_cycles`): builds a directed `Graph<SymbolId>` of `ModDef` items with
    an edge `parent -> target` for each `mod X of Y` statement. For each edge
    where `target` can reach `parent` (a back-edge), emits a
    `ModuleCycle { region, module_cycle: Vec<BString> }` containing the cycle
    FQN list.
11. **Combinational loops** (`check`):
    `get_combinational_cycle_check()` - the `CombinationalCycleCheck()` query.

Finally (`check`), the diagnostics are **sorted by source region** -
key `(region.package(), start.line(), start.col(), end.line(), end.col())` -
and wrapped in `Arc::new(diagnostics)`.

## Signature

```rust
Check() -> Arc<Vec<Diagnostic>>
```

## Result

The top-level diagnostic aggregator: runs every front-end check over the
database and returns a single, source-sorted `Vec<Diagnostic>`.

The return type is `Arc<Vec<Diagnostic>>` (see `SyntaxErrors` for the
`Diagnostic` definition). Every diagnostic carries a `Region`, a `BString`
message, and a `DiagnosticLevel` (`Error`/`Warning`/`Info`, default `Error`).

## Dependencies

* [`Packages`](Packages.md)
* [`Parsing`](Parsing.md)
* [`SyntaxErrors`](SyntaxErrors.md)
* [`SymbolTable`](SymbolTable.md)
* [`TypeIndex`](TypeIndex.md)
* [`CheckDrivers`](CheckDrivers.md)
* [`MatchCoverage`](MatchCoverage.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`TypeCheck`](TypeCheck.md)
* [`CombinationalCycleCheck`](CombinationalCycleCheck.md)
* [`TypeDef`](TypeDef.md)

## Example

For the valid `tests/pass/edge/src/top.vir`, `Check()` returns an empty
`Arc<Vec<Diagnostic>>` (after sorting): no syntax errors, no symbol-table
diagnostics, no driver/type errors, no cycles, no coverage gaps.

For a file with a combinational loop:

```virdant
mod Loop {
    incoming a : Bit
    outgoing b : Bit
    b := a
    a := b
}
```

`Check()` accumulates the `CombinationalLoop` from
`CombinationalCycleCheck()`, sorts it by region, and returns a single-element
`Vec<Diagnostic>` pointing at the `Loop` module. A file with a non-exhaustive
match would also include the `MatchNonExhaustive` from `MatchCoverage`, sorted
by its own region.
