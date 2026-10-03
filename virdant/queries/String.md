# Query: String

## Summary

The builder `crate::queries::parsing::build_string` (`queries/parsing`)
fetches the `Source` for `string.package()`, re-parses it, looks up the entry at
`string.id()`, and returns the byte content cloned into an owned `BString`
wrapped in `Arc`.

> Note: the `db` declaration carries `// TODO is this used anywhere?`,
> indicating this query may be vestigial. Most call sites resolve interned
> strings directly through a `Parsing` they already hold rather than via this
> query.

## Signature

```rust
String(string: InternedString) -> Arc<BString>
```

## Result

Resolves an `InternedString` handle back to the actual byte string it refers to.

### `InternedString`

A lightweight handle into a `Parsing`'s interned string table.

```rust
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString {
    package: PackageFqn,
    id: usize,
}
```

Defined in `virdant/src/syntax/parsing`.

- `package` - which package's string table owns this string (so the handle is
  self-describing and cross-package safe).
- `id` - index into `Parsing::strings`.

It is constructed inside `Parsing::intern` (`parsing`) which dedupes
by a linear scan. Its `Debug` renders `InternedString <package>#<id>`.

### `BString`

`bstr::BString` - the `bstr` crate's owned, UTF-8-agnostic byte string
(`Vec<u8>`-backed). Used pervasively throughout the compiler for source text,
identifiers, and diagnostic messages.

## Dependencies

* [`Source`](Source.md)

## Example

After parsing `tests/pass/gcd/src/top.vir`, the identifier `result` might be
interned as `InternedString { package: "gcd", id: 12 }`. Calling
`String(that_handle)` returns `Arc<BString>` containing the bytes `result`.
