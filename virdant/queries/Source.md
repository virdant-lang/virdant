# Query: Source

## Summary

`Source` pairs a package name with the bytes that were tokenized and parsed for
it. It is the textual foundation of the whole pipeline: `Parsing(package)` reads
`Source(package)` and runs the LALRPOP parser over `source.text()`. Because it is
an input query, editing a file and calling `set_source` invalidates exactly the
queries that transitively depend on that package's source.

`Source::load_file` (`source`) constructs a `Source` from a file path,
deriving the package name from the file stem.

## Signature

```rust
Source(package: PackageFqn) -> Source
```

## Result

`Source` is an **input query**: it has no builder and is supplied externally
(via `guts::set_source`). Given a package name, it returns the raw bytes of
that package's source file wrapped in a `Source`.

### `Source`

A source file loaded into memory, tagged with its package name.

```rust
/// A source file loaded into memory for use by the tokenizer with a given package name.
#[derive(Clone, Debug)]
pub struct Source {
    package: PackageFqn,
    text: BString, // TODO Make this an Arc
}
```

Defined in `virdant/src/common/source`.

- `package` - the `PackageFqn` this source belongs to. Accessed via `package()`.
- `text` - the raw bytes of the file as a `bstr::BString` (a UTF-8-agnostic owned
  byte string). A TODO notes the intent to wrap this in an `Arc` for cheaper
  sharing.

`Source` provides offset <-> `LineCol` conversion (`to_offset`, `to_linecol`),
`to_region(start, end) -> Region`, and indexing by `SourceOffset` (single byte)
and `Span` (byte slice).

### Notable related types

- `SourceOffset(pub u32)` (`source`) - a byte offset into the text.
- `LineCol(usize, usize)` (`source`) - a 1-indexed (line, column) pair.
- `Span(LineCol, LineCol)` (`source`) - a start/end position pair.
- `Region` (`source`) - a `Span` plus a `PackageFqn` (see `LocationRegion`).

## Dependencies

This is an **input query**; it has no dependencies and is supplied externally by the host.

## Example

For the file `tests/pass/edge/src/top.vir`:

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

`Source("top")` holds `package = "top"` and `text` = the full byte content of
that file. `to_linecol` on offset 60 might return `LineCol(3, 17)` (the `Clock`
token), and `to_region` can produce the `Region` for any sub-expression.
