# Query: TypeCheck

## Summary

The builder `crate::types::typing::typecheck` (`types/typing`) runs:

1. Fetches the `SymbolTable`, the `exprroots`, the item `Symbol`, its `Parsing`
   and AST node, and the `ComponentAnalysis` (if the item is a `ModDef`).
2. If the item node does not contain errors:
   - `typecheck_item` - for each `ExprRoot` belonging to this
     item, fetches `get_typing(exprroot)` and extends `diagnostics` with
     `typing.diagnostics()`. It also aggregates `use_locations` from each
     `Typing`'s `reference_use_locations()` into a shared
     `IndexMap<BString, IndexSet<Location>>`.
   - `collect_unused` walks the AST recording `ModDefStmtUnused`
     paths into `already_unused` so they are exempt from unused warnings.
   - `collect_bidirectional_drivers` records bidirectional driver uses for
     `ModDef`s.
3. If the item is a non-external `ModDef`, emits per-component warnings:
   - For each component that `can_source()` and is never used (not in
     `use_locations`) and is not an `OutgoingReg`/`OutgoingWire`, emits
     `UnusedSource { region, path }`.
   - For each `Sink` component that *is* used, emits `ReadFromSink { region,
     path }` for each use location (reading from a sink is suspicious).
4. Returns `Arc::new(diagnostics)`.

### Diagnostics emitted

- All diagnostics forwarded from each `Typing` (`WrongType`, `NotWordType`,
  `CantInfer`, `Unknown`, `Todo`, etc.).
- `UnusedSource` - a source-capable component is never referenced.
- `ReadFromSink` - a sink component is read from.

## Signature

```rust
TypeCheck(symbol_id: SymbolId) -> Arc<Vec<Diagnostic>>
```

## Result

Per-item typechecking diagnostics plus unused-variable and read-from-sink
warnings. The return type is `Arc<Vec<Diagnostic>>` (see `SyntaxErrors` for the
`Diagnostic` definition). There is no `TypeCheck` struct.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`ExprRoots`](ExprRoots.md)
* [`Parsing`](Parsing.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)
* [`Typing`](Typing.md)
* [`LocationRegion`](LocationRegion.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
export mod Top {
    incoming clock : Clock
    incoming reset : Reset
    incoming inp   : Bit
    outgoing out   : Bit

    unused reset

    reg last : Bit on clock {
        it <= inp
    }

    out := !last && inp
}
```

`TypeCheck(<Top id>)` type-checks each `ExprRoot` in `Top` (the `!last && inp`
RHS and the `it <= inp` reg driver), aggregating their `Typing` diagnostics -
empty for this valid file. It then checks usage: `clock` is used (by the `reg
... on clock`), `reset` is marked `unused` (so no `UnusedSource`), `inp` is used
in `out := !last && inp` and in `it <= inp`, and `out` is an outgoing source
that is read externally (no `UnusedSource`). The result is an empty `Vec`.

If `out` were never read and not outgoing, an `UnusedSource` would be emitted;
if a `Sink` component (e.g. an `Incoming`) were read in an expression, a
`ReadFromSink` would flag each read.
