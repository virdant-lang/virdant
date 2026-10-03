# Query: DriverAnalysis

## Summary

The builder `crate::analysis::drivers::build_driver_analysis`
(`drivers`) looks up the moddef `Symbol`, fetches its `Parsing` and
`ComponentAnalysis`, then calls `collect_block_drivers` (`drivers`)
which walks the AST block by block:

- `Driver` statements resolve their target path (resolving `it`-references
  against the enclosing `it_context`, emitting `NotIt`/`ItNotInItBlock`/
  `UnresolvedComponent` as needed; a `Latched` driver whose RHS resolves to the
  same path as the target emits `RedundantDriver`), then push a
  `Driver::Expr(driver_type, expr_location)` keyed by `component.id()`.
- `BidirectionalDriver` statements push `Driver::Bidirectional` onto whichever
  side is the sink.
- `Submodule`/`Socket`/`Component` with an `It` block recurse, extending the
  `it_context` by the instance name.
- `ModDefStmtWhen` recurses into each guard+body and the else-body, collecting
  per-clause driver maps, then for every component touched in any clause builds
  a `Driver::When` with `driver_type` inferred from the first sub-driver.
- `ModDefStmtMatch` analogously builds `Driver::Match`.

## Signature

```rust
DriverAnalysis(symbol_id: SymbolId) -> Arc<DriverAnalysis>
```

## Result

Per-module analysis of what drives each component: a map from each driven
`ComponentId` to the tree of `Driver`s that write to it, plus structural
diagnostics collected during the walk.

### `DriverAnalysis`

```rust
#[derive(Debug)]
pub struct DriverAnalysis {
    drivers: IndexMap<ComponentId, Vec<Driver>>,
    diagnostics: Vec<Diagnostic>,
}
```

Defined in `virdant/src/analysis/drivers`.

- `drivers` - an `IndexMap` from each driven `ComponentId` to the list of
  `Driver`s writing to it, in insertion order. A component may have multiple
  entries (caught later by `CheckDrivers` as `MultipleDrivers`).
- `diagnostics` - diagnostics collected structurally during the walk
  (e.g. `UnresolvedComponent`, `NotIt`, `ItNotInItBlock`, `EmptyDriverBlock`,
  `RedundantDriver`).

### `Driver`

The driver tree. A driver is anything that assigns a value to a component.

```rust
#[derive(Debug, Clone)]
pub enum Driver {
    Expr(DriverType, Location),
    Bidirectional(Location),
    When(DriverWhen),
    Match(DriverMatch),
}

#[derive(Debug, Clone)]
pub struct DriverWhen {
    pub driver_type: DriverType,
    pub clauses: Vec<(Location, Box<Driver>)>,
    pub else_clause: Option<Box<Driver>>,
}

#[derive(Debug, Clone)]
pub struct DriverMatch {
    pub driver_type: DriverType,
    pub subject: Location,
    pub arms: Vec<(Location, Box<Driver>)>,
    pub else_clause: Option<Box<Driver>>,
}
```

Defined in `virdant/src/analysis/drivers` and following.

- `Expr(DriverType, Location)` - a simple `lhs = rhs` (continuous) or
  `lhs <= rhs` (latched) driver; carries the kind and the RHS location.
- `Bidirectional(Location)` - a `lhs := rhs` inout binding; only the statement
  location is kept.
- `When(DriverWhen)` - a `when { case ... else ... }` conditional driver.
  `clauses` are `(condition_location, sub_driver)` pairs; `driver_type` is
  inferred from the first sub-driver (defaulting to `Continuous`).
- `Match(DriverMatch)` - a `match subject { ... }` driver. `subject` is the
  match subject's location; `arms` are `(pattern_location, sub_driver)` pairs.

`DriverType` (`common`) is `enum { Continuous, Latched }`.

## Dependencies

* [`SymbolTable`](SymbolTable.md)
* [`Parsing`](Parsing.md)
* [`ComponentAnalysis`](ComponentAnalysis.md)

## Example

For `tests/pass/gcd/src/top.vir`'s `Gcd` module:

```virdant
reg state : State on clock {
    when {
        case reset {
            it <= @Idle()
        }
        else {
            it <= match it : State {
                case @Idle() => mux(fire, @Running(x, y), @Idle())
                ...
            }
        }
    }
}
```

`DriverAnalysis(<Gcd id>)` records that `state` is driven by a `Driver::When`
with one clause (`reset`) whose sub-driver is `Driver::Match(... Latched ...)`,
and an `else` clause also `Driver::Match`. `result := r` inside the `@Done(r)`
arm contributes a `Driver::Expr(Continuous, ...)` for `result`, and so on. The
`drivers` map thus has entries for `state`, `result`, `valid`, etc.
