# Query: Packages

## Summary

`Packages` is the root input of the query graph. The driver that owns the `Db`
pushes the set of packages to compile (typically discovered from the filesystem
or a build manifest) into the database. Every downstream query that needs to
enumerate "all packages" (such as `SyntaxErrors`, `SymbolTable`, `TypeIndex`,
`Check`) reads this list. Because it is an input query, it never rebuilds; it
only changes when the caller mutates it.

## Signature

```rust
Packages() -> Arc<Vec<PackageFqn>>
```

## Result

`Packages` is an **input query**: it has no builder and is supplied externally
(via `guts::set_packages`). It returns the complete, ordered list of every
package known to the compilation, each identified by a `PackageFqn`.

### `PackageFqn`

A package's fully-qualified name.

```rust
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct PackageFqn(&'static BStr);
```

Defined in `virdant/src/fqn`.

The inner `&'static BStr` is a byte string that has been intentionally leaked
(see `fqn`) so that `PackageFqn` is `'static`, cheap to `Clone` (it only
copies a pointer), and `Hash`/`Eq`-stable. This makes it an ideal key for the
salsa-style incremental query database: comparing two `PackageFqn`s is a single
pointer comparison after interning.

It implements `Display` (lossy, for diagnostics), `AsRef<BStr>`, `From<&str>`,
and `From<String>`. The related `ItemFqn` (`fqn`) pairs a `PackageFqn`
with an item name (`"pkg::item"`).

## Dependencies

This is an **input query**; it has no dependencies and is supplied externally by the host.

## Example

Given a project with two source files `uart.vir` and `fifo.vir`, the host sets:

```rust
db.set_packages(vec!["uart".into(), "fifo".into()]);
```

The result of `Packages()` is then `Arc<Vec<PackageFqn>>` containing the two
names `uart` and `fifo`, in that order. `Source("uart")` and `Source("fifo")`
are the corresponding input queries that supply each file's text.

A Virdant package is simply a `.vir` file. For example `tests/pass/uart/src/uart.vir`
begins:

```virdant
export mod Top {
    incoming clock : Clock
    incoming reset : Reset
    ...
}
```

Here the package name is `uart` (derived from the file stem), and `Top` is an
item inside it.
