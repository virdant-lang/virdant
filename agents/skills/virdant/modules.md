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

## `export` keyword

Mark a module as the top-level entry point with `export`:

```virdant
export mod Top {
    incoming clock : Clock
    outgoing fin    : Bit
}
```

Only exported modules are visible outside the compilation unit.
Typically exactly one module per file is exported.

## `ext mod` (external modules)

Declare a module that is defined externally (e.g., in Verilog) with
`ext mod`:

```virdant
ext mod Memory {
    incoming clock    : Clock
    incoming read_addr : Word[16]
    outgoing read_data : Word[8]
}

ext mod LedDriver {
    incoming clock : Clock
    incoming inp   : Bit
    outgoing out   : Bit
}
```

External modules can be instantiated and connected just like ordinary
modules.
The `ext mod` body lists only ports (no internal logic).

## `import` directive

Bring another Virdant package into scope with `import`:

```virdant
import uart
import resetter
import edge
```

Imports are file-level (placed at the top of the `.vir` file, before any
module definitions).
The import path is the stem of the `.vir` file, resolved against the
library/include search paths.

## Module path syntax (`::`)

Refer to a module inside an imported file with `::`:

```virdant
import uart

mod sender of uart::UartSender
sender.clock := clock
```

The part before `::` is the import name.
The part after `::` is the module name as defined inside that file.

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
