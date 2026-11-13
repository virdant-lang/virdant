# Modules and Instances

Virdant organizes hardware into modules that can be instantiated.

## Module instantiation

Instantiate a submodule with the `mod name of Module` syntax:

```virdant
mod foo of Foo
foo.clock := clock
foo.inp := inp
out := foo.out
```

The instance name (`foo`) is used as a prefix for port connections.
Ports are connected with dot notation: `instance.port := expr`.

## Instantiation with `it` blocks

When you connect several ports of the same submodule, use an `it` block
to reduce repetition:

```virdant
mod buffer of Buffer {
    it.clk := clock
    it.inp := r
    out := it.out
}
```

Inside the block, `it` refers to the module instance.
Connections inside an `it` block are equivalent to writing them outside:

```virdant
mod buffer of Buffer
buffer.clk := clock
buffer.inp := r
out := buffer.out
```

Prefer the `it` block form when connecting three or more ports,
especially for clock, reset, and data signals.

## `pub` visibility

Mark an item `pub` to make it visible to other packages:

```virdant
// buffer.vir
pub mod Buffer {
    incoming clock : Clock
    incoming inp : Word[8]
    outgoing out : Word[8]
}
```

- `pub` is available on every top-level item kind: `mod`, `ext mod`,
  `struct type`, `union type`, `enum type`, `builtin type`, `socket`,
  and `platform`.
- An item without `pub` is private to its package. Referencing it from
  another package is an **error at the reference site**:

  - `` `Priv` is not pub in package `lib`; mark it `pub` to reference it from other packages ``

- A warning is also emitted at the non-`pub` item's *definition*,
  naming the referencing package, reported once per referencing
  package no matter how many times it is referenced:

  - `` `Priv` is not pub but referenced from package `top` ``

- The `import` statement itself never fails for visibility reasons.
- Within the defining package, every item is visible to every other
  item, `pub` or not: `pub` only matters at package boundaries.
- An unused non-`pub` item warns at its declaration site
  (``Item `UnusedMod` is unused and not `pub```). `pub` items and mod
  defs are exempt: any uninstantiated mod is a potential `--top`
  target.

### Top modules

There is no `export` keyword. The entry point of a program is the
module named by `[prog] top` in `virdant.toml`, written as
`package::ModName`:

```toml
[prog]
top = "top::Top"
```

- A module implementing a platform with a `for` clause must be `pub`
  (top modules implement platforms):
  ``Top module top::Top must be `pub` ``
- `pub mod`s may be generic; the `[prog] top` module must be a
  concrete instantiation.
- See ./generics.md for generic modules and instantiation.

### The pub surface must be transitively pub

A `pub` item may not leak a non-`pub` type: every type appearing in a
`pub` item's public surface must itself be transitively `pub`. The
public surface covers:

- module port types (`incoming`, `outgoing`, `bus`)
- struct field types
- union variant payload types and enum variant payload types
- socket `cosi`/`soci` channel types
- platform port types

A leak is an error at the offending type's span:

`` `Priv` is not pub: the public surface of `pkg::PubShell` must be transitively pub ``

Generic parameters (`T` in `Word[T]`) are exempt: they are bound at
the instantiation site.

## `ext mod` (external modules)

Declare a module that is defined externally (e.g., in Verilog) with
`ext mod`:

```virdant
ext mod Memory {
    incoming clock     : Clock
    incoming read_addr : Word[16]
    outgoing read_data : Word[8]
}
```

- External modules can be instantiated and connected just like
  ordinary modules.
- The `ext mod` body lists only ports (no internal logic).
- `pub ext mod` makes the external module visible to other packages.
- An `ext mod` cannot have generic parameters: it parameterizes
  through `@parameter` annotations instead (below).

### `@parameter` on `ext mod`

Declare Verilog parameters on the ext mod definition; each
`@parameter` takes a string name and an optional default:

```virdant
@parameter("WIDTH", 8)
@parameter("MODE", "SYNC")
ext mod Cell {
    incoming clk : Clock
    outgoing data : Word[16]
}
```

Override per instance by attaching `@parameter` to the
`mod ... of` statement; instance values override declared defaults,
and parameter names not declared on the target are appended:

```virdant
pub mod Top {
    incoming clk : Clock
    outgoing a : Word[16]

    @parameter("WIDTH", 4)
    mod u0 of Cell {
        it.clk := clk
        a := it.data
    }
}
```

Rendering to Verilog:

- numbers render in decimal (`.WIDTH(4)`)
- strings render quoted (`.MODE("SYNC")`)
- booleans render as single-bit literals (`true` → `1'b1`,
  `false` → `1'b0`)

## `import` directive

Bring another Virdant package into scope with `import`:

```virdant
import uart
import resetter
import edge
```

- Imports are file-level: placed at the top of the `.vir` file,
  before any item definitions.
- The package name is the stem of the `.vir` filename (`buffer.vir`
  defines the `buffer` package), resolved against the importing
  file's directory and library search paths.
- A file's filename must be a valid package identifier: snake_case,
  no hyphens. `bad-name.vir` is rejected at load time
  (`bad-name.vir: file name is not a valid package identifier`).
- Circular imports are not permitted: if A imports B, B may not
  import A, directly or transitively
  (`Import cycle: ...`).

## Module path syntax (`::`)

Refer to an item inside an imported package with `::`:

```virdant
import uart

mod sender of uart::UartSender
sender.clock := clock
```

The part before `::` is the package name.
The part after `::` is the item name as defined inside that package.

- Bare names (no `::`) resolve relative to the current package.  If
  that fails, they resolve relative to the builtin package.  Bare
  names never resolve in imported packages: referencing an item from
  another package requires the qualified `pkg::Item` form, and
  `import` makes the package name usable as a qualifier.
- Only `pub` items of the imported package are visible (see `pub`
  visibility above).

## `incoming { ... }` forwarding block

When a single incoming port must be forwarded to multiple submodules,
use the block form of the `incoming` declaration:

```virdant
incoming clock : Clock {
    mux_0.clock := it
    mux_1.clock := it
    arb_0.clock := it
    arb_1.clock := it
}
```

This is especially useful for `clock` and `reset` ports.

Inside the block, `it` refers to the incoming port.
This pattern eliminates the need for an intermediate wire when fanning
out a signal, and makes the forwarding explicit at the declaration site.

## `dependson` on outgoing ports

Outgoing ports declare which `incoming` ports they may combinationally
depend on; `vir check` enforces the list. See ./dependson.md.
