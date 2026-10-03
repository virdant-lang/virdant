# Query: SyntaxErrors

## Summary

The builder `crate::queries::build_syntax_errors` (`queries/syntax`)
iterates every package returned by `Packages()`, fetches each cached
`Parsing(package)`, and extends the result with `parsing.diagnostics()`
(`syntax/parsing`). That method merges the `docstring_diagnostics`
with diagnostics synthesized from each LALRPOP `ParseError`, wrapped as
`diagnostics::SyntaxError { region, message }`.

This query is purely a fan-out over `Parsing`; it does no work of its own beyond
gathering. It is consumed by the top-level `Check` query.

## Signature

```rust
SyntaxErrors() -> Arc<Vec<Diagnostic>>
```

## Result

Every syntax-level diagnostic across all packages, concatenated into one vector.

### `Diagnostic`

A type-erased, `Clone`-cheap handle to any concrete diagnostic.

```rust
#[derive(Clone, Debug)]
pub struct Diagnostic(Arc<dyn IsDiagnostic + Send + Sync>);
```

Defined in `virdant/src/diagnostics`.

The private `IsDiagnostic` trait (`diagnostics`) requires:

```rust
trait IsDiagnostic: std::fmt::Debug + 'static + Send + Sync {
    fn region(&self) -> Region;
    fn message(&self) -> BString;
    fn level(&self) -> DiagnosticLevel { DiagnosticLevel::Error }
}
```

So every diagnostic carries a `Region` (source location), a `BString` message,
and a `DiagnosticLevel`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}
```

Concrete diagnostic structs (`SyntaxError`, `ImportNotAtTopError`,
`UnresolvedImportError`, `DuplicateItem`, etc.) implement `IsDiagnostic` and
convert into `Diagnostic` via `.into()`. There are dozens of concrete variants,
all defined in `virdant/src/diagnostics.rs`.

## Dependencies

* [`Packages`](Packages.md)
* [`Parsing`](Parsing.md)

## Example

Given a malformed file:

```virdant
mod Top {
    incoming x : Bit
    out := !!
}
```

The LALRPOP parser fails on `!!` and records a `ParseError` with a recovery.
`Parsing("top").diagnostics()` turns that into a `SyntaxError` whose `region`
points at the offending bytes and whose `message` describes the unexpected
token. `SyntaxErrors()` collects that (and any errors from other packages)
into one `Arc<Vec<Diagnostic>>`.
