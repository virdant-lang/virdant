# Diagnostics

Cheat sheet mapping `vir check` diagnostics to cause and fix.

* Run `vir check` early and often. Errors fail the build; warnings do not.
* Message fragments below are quoted from `tests/errors/**` and `tests/warnings/**`.
  When docs and tests disagree, the tests win.
* Families mirror the directories under `tests/errors/`.

## Errors

### add_carry

* `+.` / `-.` (add-with-carry / subtract-with-borrow) require operands of the
  *same* width, and yield `Word[n+1]` (the extra top bit is the carry/borrow).
  See ./literals-and-builtins.md for operator details.
* Message: `Expected Word[4] but found Word[5]` (pointed at the driver).
* Assign the `+.` result to a `Word[n+1]` target; use plain `+` / `-` when you
  want a same-width wrapped result.

```virdant
pub mod AddCarry {
    incoming a : Word[4]
    incoming b : Word[4]
    // BAD:    outgoing out : Word[4] { it := a +. b }
    //   => "Expected Word[4] but found Word[5]"
    outgoing out : Word[5] dependson a, b {
        it := a +. b
    }
}
```

### cycles

* `Combinational loop detected` — a cycle through continuous drivers
  (`x := y; y := z; z := x`). Break it with a `reg`.
* Loops are detected *through conditionals too*:
  * Drivers inside a `when` arm depend on the arm's guard: driving the guarded
    wire inside its own `when` arm is a loop.
  * Drivers inside a `match` arm depend on the match subject: driving the
    subject wire inside its own arm is a loop.
* ``Cycle in the `dependson` relation`` — a cycle in the declared `dependson`
  graph (ports, socket channels, submodule declarations). It is an error even
  if no real combinational path exists yet; trim a `dependson` entry or cut
  the path with a `reg`.
* `Module cycle: pkg::A, pkg::B` — module instantiation cycle (self-, mutual,
  or chained). Restructure so the instance graph is acyclic.

```virdant
// BAD: driver inside a `when` arm depends on the arm's guard.
mod WhenLoop {
    wire w : Bit
    if {
        when w {
            w := true   // "Combinational loop detected"
        }
    }
}

// BAD: driver inside a `match` arm depends on the match subject.
mod MatchLoop {
    wire w : Bit
    match w {
        with true {
            w := false  // "Combinational loop detected"
        }
        with false {
            w := true   // "Combinational loop detected"
        }
    }
}
```

### dependson

* `dependson` is a **declaration modifier only**; the old statement form
  (`clk_out dependson clk_in`) is gone:
  `Syntax Error: Unexpected token: "dependson"`.
* Entries must be the module's own `incoming` ports:
  * ``Bad `dependson` entry `X`: is not an `incoming` port of this module``
    (outgoing port, wire, or submodule path).
  * ``Bad `dependson` entry `X`: could not be resolved to a component of this module``.
* Carrier restriction — `dependson` is allowed **only** on `outgoing` and
  `outgoing wire`:
  * ``` `dependson` is not allowed on `X`: internal signals may not declare
    `dependson`; only `outgoing` and `outgoing wire` may ```
  * ``` `dependson` is not allowed on `X`: a registered output (`outgoing reg`)
    cannot change in the same cycle ```
  * ``` `dependson` is not allowed on `X`: `bus` is tristate; ... not yet supported ```
  * ``` `dependson` is not allowed on `X`: `incoming` ports may not declare `dependson` ```
* Socket channel lists: a `cosi` channel may only `dependson` `soci` channels
  of the same socket, and vice versa:
  ``a `cosi` channel may only `dependson` channels of the opposite direction word``
* Exhaustiveness (under-declaration is an error, not just a style issue):
  `` `out` could combinationally depend on `inp` but does not declare it ``
  * The declared set must cover every `incoming` the port can combinationally
    reach — directly, through wires, through submodule port declarations, and
    through submodule socket obligations.
  * An absent list counts as empty; `outgoing reg` is exempt (registered
    outputs cannot change in the same cycle).
  * Declaring *too much* is only a warning (see Warnings, `` Unused `dependson` ``
    below).
* See `docs/source/reference/modules.rst` ("The `dependson` List").

### drivers

* `Multiple drivers for X` — the same target is driven twice on one path:
  * Two plain drivers in the same scope.
  * Two drivers inside the *same* `when`/`with` arm.
  * Drivers in **separate** conditional blocks, even if the guards look
    complementary (`when reset { r <= true }` in one `if` plus
    `when !reset { r <= false }` in another `if` is still two drivers).
    Arms of the *same* `if`/`match` are one logical driver; a second
    standalone conditional is always a second driver.
* `No drivers for X` — every driveable target needs exactly one driver:
  outgoing ports and internal wires (`:=`), regs (`<=`), submodule/ext-mod
  `incoming` ports, socket sink channels when not `:=:`-connected.
* `Wrong driver type for X` — `<=` is only for `reg`/`outgoing reg`;
  `:=` for everything else.
* `Driver for sink X` — you drove a *source*: a submodule's `incoming` port
  from the parent, or the wrong socket side (a `client` drives `cosi`, a
  `server` drives `soci`). Set submodule inputs inside the instance's `it`
  block instead: `it.inp := ...`.
* `Missing on clause for reg X` — with zero or two-or-more `Clock` components
  a `reg` needs an explicit `on` clause. With exactly one Clock the clause is
  optional (see the ``Redundant `on` clause`` warning below).
* ``Redundant `unused`: X`` — the same path marked `unused` twice (also when an
  it-block `unused it` aliases a later `unused r`).
* ``Marked `unused` but used: X`` — the marked component is read (directly or
  implicitly: a unique Clock is "used" by a reg relying on the implicit `on`
  clause). Remove the mark or stop reading it.
* `` `unused` on outgoing X `` — the module's own plain `outgoing` port is
  sink-only, so `unused` is meaningless there (`outgoing reg` / `outgoing
  wire` / submodule outgoing ports are fine: they are readable).
* `initial` errors:
  * `initial targets non-register component X`.
  * `multiple initial statements for register X`.
  * `initial statement for X must not appear inside an if/match block`.
  * `Unresolved component X` for a bad target.

### match

* Non-exhaustive matches — cover every case or end with `else`:
  * `Non-exhaustive match on Bit: not covered: false`
  * `not covered: #Blue` (enum — one enumerant omitted)
  * `not covered: @Bar` (union — one constructor omitted)
  * `Non-exhaustive match` (Word subject — add `else` or cover all values)
* `Overlapping match arm: X` — a later arm covers an already-covered case:
  duplicate literal (`with true` twice, `with 0` twice, duplicate `@Foo`),
  or an open pattern (`with x`, `with dontcare`) shadowing or shadowed by
  anything.
* `else` placement:
  * `else arm must be the last arm of the match` (no `with` after `else`).
  * `Multiple else arms in match`.
  * `Redundant else arm` — every enumerant is already covered, so the `else`
    is unreachable; drop it.
* Arm pattern must match the subject's type:
  * Word pattern on an enum subject: `... which is not a Word type`.
  * Enumerant pattern on a Word subject: `PatEnumerant expects an enum type`.
  * Constructor pattern on a Word subject: `Unresolved constructor Foo`.
  * Bit pattern on an enum subject: `Wrong type`.
* Payload-bearing constructors match with `@Some(x)`, never `#Some`.

### memory

The `memory` statement: shape, indexing, write-site, and image rules.  See
./memory.md for the full picture.

* `memory depth must be a power of 2, found N` — the bracketed depth must be
  a literal power of two; `memory mem[8 + 8]` is
  `memory depth must be a literal constant`.
* `memory element type must be `Word[n]` with n a power of 2, found T` — the
  element type must be `Word[n]` (`Bit` or `Word[12]` are rejected).
* `index into memory mem must be exactly Word[K] (depth 2^K), found T` — a
  dynamic index must have *exactly* the address width; narrow a wider
  address explicitly (`addr[4..0]`), never rely on truncation.
* `index N out of range for memory depth D` — a constant index past the
  last word.
* `memory mem must be indexed: `mem[dyn i]` or `mem[c]`` — a memory cannot
  be read whole or sliced; index it to a word first.
* `overlapping writes to memory mem` — two sibling `when` clauses with
  non-exclusive guards both write the memory and may collide in one cycle;
  use `when`/`else` or `match` arms instead.
* `write to memory mem outside a clocked write context` — a write outside an
  `on` region needs a resolvable clock (explicit clause or a single-Clock
  module), and a body with an `on` region keeps its writes inside it.
* `` `on` region in memory mem must name the memory's clock CLK`` — a body
  region's clock must be the memory's own clock slot.
* `only writes to the memory are allowed inside an `on` region` — reads and
  drivers of other components belong at the top level of the body.
* `indexed driver target X is not a memory` — indexed LHSes write memory
  words only.
* `Multiple drivers for mem` — a memory has exactly one write site (the
  existing multiple-drivers machinery).
* ``use `<=` to write memory mem`` (warning) — `:=` parses and writes but
  warns; a memory write commits on the clock edge.
* `initial` errors:
  * `` `initial` is not supported on memory mem; initialize with
    @image/@image_hex instead``.
* Image errors:
  * `could not read memory image for mem: PATH: ...`.
  * `memory image PATH holds more words than the declared depth D`.
  * `memory image for mem not supported: ...` (element widths above 64
    bits in v1, malformed hex lines).
  * `memory mem is initialized by @image/@image_hex; writes to it are not
    supported` — an image memory is a ROM.
  * `memory image initialization for mem is disallowed on `@noinitial`
    platform P` — images are gated exactly like `initial`.

### naming

* File names must be valid package identifiers
  (`[_a-zA-Z][_a-zA-Z0-9]*`, snake_case). Hyphens are compile errors:
  `bad-name.vir: file name is not a valid package identifier`.
  Rename the file (e.g. `bad_name.vir`); the file stem is the package name.
* `Duplicate Item` — two same-named top-level items in one package (only the
  second is flagged). `Duplicate slot` — an enumerant repeated in an enum.
* `Unresolved import: X` — the imported package does not exist. Imports only
  accept identifiers, another reason file names cannot contain hyphens.
* `Unresolved type X` — a port/type annotation names a nonexistent type.
* `Unresolved component X` — a driver statement names a nonexistent target.
* `Unknown component X` — an expression references a nonexistent name,
  including bad submodule paths (`it.mem`, `it.carry`).
* Let bindings do not pull unrelated names into scope; referencing anything
  not bound is `Unknown component X`.

### platforms

* Ports must match the platform exactly:
  * `Module has port X which platform P does not declare`.
  * `Module is missing port X required by platform P` (also for platform
    `outgoing` ports that have no `default`).
* `default` clauses:
  * ``` `default` on outgoing port X is only allowed in a platform definition ```
  * A platform default whose type does not match the port is reported at the
    platform's expression (`Wrong type: Expected Word[4] but found Bit`).
  * A module may omit a defaulted platform port entirely, or declare and
    drive it — but driving a platform port it does *not* declare is an error:
    ``` `exit` is declared in platform `Virt`, but not declared in this
    module; declare it explicitly to drive it ```
* `@noinitial` platform (propagates through the whole instance hierarchy):
  `initial statement appears in platform Virt with @noinitial: top::Top top::Sub`
* Top modules must be concrete:
  `top module top::Fifo cannot have generic parameters`
  (`pub mod`s may be generic; tops may not).

### style

* `let` terminator — `{` means "performs", `=>` means "produces":
  * Statement-position `let` must end in `{ ... }`:
    ``For a `let` used as a statement, use `{ ... }` instead of `=>` ``
  * A statement `let` whose block contains only a bare expression is flagged
    the same way — use `=>` (or actually perform statements).
  * Chaining is legal in both positions: `let x = b if { ... }`.
* `if` / `match` arm style:
  * Expression position (right-hand side of a driver) must use `=>` arms:
    ``For expression-based conditionals, use `=>` instead of `{ ... }` ``
  * Statement position must use `{ ... }` driver blocks:
    ``For statement-based conditionals, use `{ ... }` instead of `=>` ``
* See ./control-flow.md for when to use which form.

### syntax

* `Syntax Error: Tabs are not allowed in Virdant. Use 4 spaces instead.`
  Tabs are dropped by the lexer, so a stray tab flags exactly once per line
  instead of cascading into parse errors. Never emit tabs; see
  ./coding-conventions.md.
* `Line must be indented by a multiple of 4 spaces` — every line holding a
  token starts at 0/4/8/... columns. Comment-only lines are exempt.
* `Doccomment must be followed by a space` — `//>Baz` and `//!>Bar` are
  invalid; write `//> Baz` / `//! Bar`.
* `Syntax Error` / `Unexpected token` — general parser errors (e.g. `@` in a
  type annotation). A valid module next to broken ones does not gain extra
  diagnostics from the breakage.

## Warnings

Warnings do not fail `vir check`, but an agent should treat them as work
items, not noise.

* ``Unused `dependson`: `out` declares it may depend on `inp` but does not``
  (dependson_unused) — trim the entry. `ext mod` declarations are exempt.
* `Empty driver block` (empty_driver_block) — an it-block with no drivers;
  add a driver or delete the block.
* `No drivers for X` on a `reg`/`outgoing reg` (no_reg_drivers) — a register
  with no `<=` driver; a wire with no driver is an *error* instead.
* `'X' should be written as 'it' inside of this driver block` (not_it /
  not_it_initial) — inside a component's it-block, refer to the component as
  `it`, never by its own name (covers drivers, `initial`, and `unused`).
* Platform-synthesized ports (platform_ports_no_unused) — an omitted
  `incoming` platform port or defaulted `outgoing` port is synthesized and
  does *not* raise `Unused signal`; no `unused` bookkeeping needed for them.
* `Read from sink X` (read_from_sink) — reading an outgoing port, which can
  only be driven; restructure so the value flows forward.
* `Redundant driver: X` (redundant_driver) — `r <= r` (including `it <= it`
  and mixed `it`/explicit forms) is a no-op latch; delete it.
* `` Redundant `on` clause: `X `` (redundant_on_clause) — with exactly one
  `Clock` component, an explicit `on clock` on any component kind is
  equivalent to omitting it; omit it. `on <another clock>` (e.g. a
  submodule's clock output) never warns. With two or more clocks an explicit
  clause is required (see the `Missing on clause for reg` error above).
* `trailing space` (trailing_spaces) — one warning per offending line; strip
  trailing whitespace.
* `` Item `X` is unused and not `pub` `` (unused_non_pub) — unused private item;
  delete it, use it, or mark `pub`. `mod` defs are exempt (any mod is a
  potential `--top` target).
* ``Marked `unused` but declared `outgoing wire`: X`` (unused_outgoing_wire)
  — declaring an `outgoing wire` already declares intent to use it; drop the
  mark (or switch the port to plain `outgoing`). The mark still suppresses
  `Unused signal` for the wire.
* `Unused signal X` (unused_source) — a driven-but-never-read source
  (incoming port, wire, submodule outgoing port, clock with no regs). Read
  it, or add `unused X` to silence it deliberately.
