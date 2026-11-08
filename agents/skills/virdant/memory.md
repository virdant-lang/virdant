# Memories

The `memory` statement declares a module-local, clocked, word-addressable
memory (behavioral RAM/ROM) with dynamic indexing, explicit write drivers,
and file-based initialization.

```virdant
pub mod Ram {
    incoming clock : Clock
    incoming write_data : Word[32]
    incoming addr : Word[10]
    incoming write_enable : Bit
    outgoing read_data : Word[32] dependson addr {
        it := mem[dyn addr]
    }

    memory mem[1024] : Word[32] {
        if {
            when write_enable {
                it[dyn addr] <= write_data
            }
        }
    }
}
```

Reading order: `memory mem[1024] : Word[32]` is "a memory `mem` with 1024
slots, each a `Word[32]`" — the brackets belong to the *component*, mirroring
how the memory is read (`mem[dyn addr]`).  `memory` is a reserved keyword.

## Shape rules

* Both the depth and the element width must be **literal powers of two**
  (`MemoryDepthNotPowerOf2`, `MemoryElementNotPowerOf2Word`); a non-literal
  depth is `MemoryDepthNotLiteral` (no generics in v1).
* A dynamic index must be **exactly** `Word[k]` where `depth == 2^k` — a wider
  address must be narrowed explicitly (`addr[4..0]`), never truncated
  silently (`MemoryIndexWidth`).
* Static indices read and write one word (`mem[3] <= x`; `mem[7]`), and must
  be in range (`MemoryIndexOutOfRange`).
* A memory is only readable through an index; a bare `mem` or a slice
  `mem[3..1]` is `MemoryUsedWithoutIndex`.

## Clocking and writes

* One clock slot per memory, resolved exactly like a `reg`'s: omit `on` when
  the module declares exactly one `Clock`, otherwise declare it
  (`memory mem[16] : Word[8] on clk_b`).
* A write is `mem[dyn idx] <= data` — the first indexed LHS in the language;
  `:=` parses but warns (`MemoryAssignInsteadOfLassign`) because a write
  commits on the clock edge.
* Inside the body, writes live either in an `on <CLOCK> { ... }` region
  (multi-clock modules) or at the top level of the body (single-clock
  modules, the same inference as `reg`); inside an `on` region only writes
  to `it[...]` are allowed.
* The write site may appear in the body or anywhere in the module (a
  blockless `memory mem[16] : Word[8]` plus a guarded module-level write).
* Exactly **one write site** per memory (`MultipleDrivers`), and sibling
  `when` clauses with non-exclusive guards may not both write it — use
  `when`/`else` or `match` arms (`OverlappingMemoryWrites`).
* A write outside a clocked context with no resolvable clock is
  `MemoryWriteOutsideOnRegion`.

## Reads

* Reads are combinational and usable anywhere an expression is: driver RHS,
  guards, submodule connections.  Multiple reads at different addresses in
  one cycle are legal (multiple async read ports).
* An `outgoing` port that combinationally reads a memory declares
  `dependson` naming the index's incoming ports — the only combinational
  dependency a read introduces is on the index.
* Simulation semantics: writes commit at the clock edge; a read of an
  address written in the same cycle sees the **old** value
  (read-before-write); never-written words read as pseudorandom.

## Initialization: `@image` / `@image_hex` / `@image_inline`

```virdant
@image_hex("rom_image.hex")
memory rom[256] : Word[32] {
    read_data := it[dyn addr]
}
```

* `@image_hex("path")` — text hex file, one word per line (à la
  `$readmemh`); `@image("path")` — raw binary, word-packed in declaration
  order (most-significant byte first within each word);
  `@image_inline("bytes")` — a string literal whose bytes *are* the image,
  packed like a raw binary file.  The literal supports C-style escapes:
  `\a \b \e \f \n \r \t \v`, `\\ \" \?` (and `\'`), octal `\ooo` of one
  to three octal digits (`\0` is NUL), and hex `\xNN` — so
  `"Hello, world!\r\n\0"` is the same 15 bytes as
  `"Hello, world!\x0d\x0a\x00"`.
* Paths resolve **relative to the project root** — the directory where
  `virdant.toml` is found.  `@image_inline` needs no file.
* An image shorter than the depth leaves the rest unspecified; longer is an
  error (`MemoryImageTooLong`).  Element widths above 64 bits are not
  supported for images in v1.
* An image memory is a ROM: it has no write site and needs no clock; writes
  to it are `MemoryImageWrite`.  `initial` statements on memories are not
  supported in v1 (`MemoryInitialUnsupported`).
* On platforms annotated `@noinitial`, memory images are disallowed, exactly
  like `initial` statements.

## Lowering

Memories lower to behavioral Verilog (inferred as block RAM by synthesis
tools):

```verilog
reg [31:0] mem [0:1023];

always @(posedge clock) begin
    if (write_enable) begin
        mem[addr] <= write_data;
    end
end

assign read_data = mem[addr];
```

`@image_hex` emits an `initial $readmemh(...)`; `@image` and
`@image_inline` emit explicit per-word initial assignments.

## Idioms

* A register file: one write port, several read ports at different
  addresses — declare each read's `dependson` list.
* A ROM: `@image_hex` (or `@image`/`@image_inline`) + read-only body; no
  write site, no clock needed.
* One write per cycle: guard the single write site with `if`/`match`
  structure whose arms are mutually exclusive.
