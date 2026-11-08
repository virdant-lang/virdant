---
name: virdant
description:
    Guidance when writing or editing Virdant (.vir) HDL source files.
    Use whenever virdant / Virdant / .vir files are involved, or when
    working on FPGA, HDL, Verilog, or digital-hardware design tasks in
    a Virdant project.
---

# Virdant

Virdant is a modern FPGA/HDL language: strongly typed, package-based, and
purpose-built for RTL design. Designs are written in `.vir` files organized
into packages; modules (`mod`) drive typed components (`wire`, `reg`, ports,
sockets) with driver blocks; `vir check` is the compiler-frontend you loop on,
and `vir build`/`synth`/`pnr`/`bitstream`/`upload` carry a design to an FPGA.
This skill is self-contained: everything load-bearing is here; the website is
further reading only.

## The five non-negotiables

1. **Slices are exclusive on top.** `x[hi..lo]` extracts bits `hi-1` down to
   `lo`; the width is `hi - lo`. Self-check every slice: a 4-bit slice of
   `x[8..4]` is bits 7..4. There is no `<<`/`>>` — shift with `word()` plus
   slices (see `./literals-and-builtins.md`).
2. **Prefer using driver blocks with `it`.** Prefer this:

```virdant
outgoing double : Word[4] {
    it := counter + counter
}
```

   over declaring then driving separately. Inside a driver block, `it`
   refers to the component being driven (see `./driver-blocks.md`).
3. **`{` performs, `=>` produces.** In statement position, conditional arms
   are driver blocks (`when reset { it <= 0 }`). In expression position,
   arms are values (`match state { with @Idle => 0, ... }`). An arm using
   the wrong form is a compile error (see `./control-flow.md`).
4. **Silence intentional-unused warnings with `unused`.** Unused instance
   members, socket members, and shadowed components warn; write e.g.
   `unused it.ready` or `unused mem.addr` where the design deliberately
   ignores them (see `./sockets.md`, `./modules.md`).
5. **Formatting is compiler-enforced.** 4-space indent (no tabs, no other
   widths), no trailing spaces
6. **Variable names must be descriptive**. This is not Verilog.
   Variable names should be meaningful and descriptive.
   They may be long, with snake case.


## Canonical module skeleton

```virdant
//> A 4-bit counter with an enable and a doubled output.
pub mod Counter {
    incoming clock : Clock
    incoming enable : Bit

    reg counter : Word[4] {
        initial it <= 0
        if {
            when enable {
                it <= it + 1w4
            }
        }
    }

    outgoing double : Word[4] {
        it := counter + counter
    }
}
```

Notes on the idiom above (v0.4.0):

* When a module has exactly one `Clock`, the clock is
  inferred; writing `on clock` anyway is a warning (see `./driver-blocks.md`).
* Driver blocks write drivers into `{ ... }`; `initial` seeds a `reg` for
  simulation. `initial` is disallowed on platforms which are annotated with `@noinitial`.
* An `outgoing` port that combinationally reads any incoming port must
  declare a `dependson` list (see `./dependson.md`).
* A top-level module for a program is `pub mod` plus `top = "pkg::Mod"` in
  `virdant.toml` (see `./modules.md`, `./tools.md`).

## Verification loop

1. After **every** edit, run `vir check` — exit 0 means valid; warnings are
   printed but do not fail. Treat warnings as bugs anyway.
2. Simulate with `vir run` (batch) or `vir poke` (interactive peek-poke,
   scriptable).
3. Deliver to hardware with the flow chain: `vir build` → `synth` → `pnr` →
   `bitstream` → `upload` (see `./tools.md`, `./platforms.md`).

If `vir check` fails, find the message in `./diagnostics.md` for cause and fix.

## Read this when…

* `./coding-conventions.md` — naming, whitespace, docstrings, `pub`, file
  and style rules.
* `./driver-blocks.md` — writing drivers with `it`, clock inference,
  `initial`, push-in drivers, direct chaining.
* `./memory.md` — the `memory` statement: RAMs and ROMs, indexed writes,
  `on` regions, `@image`/`@image_hex`/`@image_inline`, the one-write-site
  rules.
* `./control-flow.md` — `if`/`match` in statement and expression position,
  the five arm forms, guards, exhaustiveness.
* `./let-bindings.md` — `let` in statements and expressions, binding lists,
  terminator style, shadowing.
* `./types-and-patterns.md` — structs, unions, enums (one-hot default,
  payload enumerants), `Valid[T]`, patterns, `dyn`, ascription.
* `./literals-and-builtins.md` — number literals and bases, `word()`,
  `zext`/`sext`, `+.`/`-.`, `all`/`trunc`, `?` holes, casts, `tap`.
* `./modules.md` — mod definitions, instantiation, it-blocks, `pub`
  visibility, imports, ext mods with `@parameter`.
* `./sockets.md` — client/server sockets, cosi/soci, `:=:`, generic
  sockets, channel `dependson`.
* `./generics.md` — `[T : Type]` / `Nat` parameters on all definition
  forms, type arguments, mono modules.
* `./dependson.md` — combinational-dependency declarations on `outgoing`
  ports; cycle detection.
* `./tristate.md` — `bus`/`net`, `#=`/`#-`/`#=#` drivers, `tap`,
  open-drain idioms.
* `./platforms.md` — `platform` items, board ports and defaults,
  `@noinitial`, the shipped `ice40` package.
* `./tools.md` — the full `vir` CLI: `new`/`check`/`run`/`poke`,
  `doc`/`format`/`ls`/`completion`, the flow chain, `virdant.toml`.
* `./diagnostics.md` — cheat sheet mapping every error/warning family to
  cause and fix.
* `./idioms.md` — canonical patterns (FSMs, handshakes, counters,
  shifters) plus the Virdant-vs-Verilog differences table and the
  reserved-keyword list.

## Further reading

* Tutorial: https://virdant.org/tutorial/
* Reference: https://virdant.org/reference/
The commit hash of Virdant is marked in the footer of the HTML.
