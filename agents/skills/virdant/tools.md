# The `vir` CLI

`vir` is a single binary with subcommands for every stage of working on a design.
Run `vir -h` to see the same groups listed here (v0.4.0).
Ground truth: `docs/source/tools/*.rst`, plus `vir -h` / `vir <sub> -h` themselves.

## Top-level surface (`vir -h`)

```console
$ vir -h
Basic:      new, check, run, poke
Tools:      doc, format, ls, completion
Flows:      build, synth, pnr, bitstream, upload
About:      version
Learning:   tutor
```

- Global flags, placed **before** the subcommand: `vir -C <dir> <cmd>` (operate on
  the project in `<dir>`) and `vir -F <files> <cmd>` (load comma-separated `.vir`
  files as a self-contained project; `-F` is hidden from `vir -h` but accepted).
- Companion binaries: any `vir <foo>` not built in is resolved to a `vir-foo`
  binary on `$PATH` (e.g. `vir doc` runs `vir-doc`, `vir tutor` runs `vir-tutor`).
  Both spellings work; prefer `vir <foo>`.
- All commands run against a project: a directory containing `virdant.toml`
  (see `virdant.toml` below). Outside a project, most commands fail.

## The verification loop

- `vir check` is the primary loop: run it after **every** edit.
- Type-checks the design; no build artifacts.
- Exit 0 when the design is valid; nonzero on any error.
- Warnings do **not** fail the check: they are printed and exit stays 0.
  Treat warnings as bugs anyway.
- The language server does the same job live: `vir ls serve` (see below).

```console
$ vir check
WARNING   top.vir [3:30-35]   Redundant `on` clause: `counter`
Check OK
$ vir check          # with an error present
src/top.vir:4:9: Type error: Expected QuadU8, found Bit
Check failed         # exit 1
```

## Basic

### `vir new <PROJECT>` — scaffold a project

```console
$ vir new hello && cd hello
```

- Creates `<PROJECT>/` from a bundled template; fails if the directory exists.
- Layout: `virdant.toml`, `src/top.vir`, `.gitignore` (`build`), and a Git
  repository.
- `--no-git` skips `git init`. Git init is also skipped automatically when the
  project already sits inside a Git repo or Git is unavailable.
- The template `src/top.vir` uses `//>` docstrings and the implicit-`on`
  idiom (see ./coding-conventions.md, ./modules.md):

```virdant
import virt

//> `Top` is the top module and entry point to your design.
pub mod Top for virt::Virt {
    //> `clock` is the system clock
    incoming clock : Clock
    outgoing tx : Bit
    outgoing exit : Valid[Word[8]]
    mod hello of virt::Hello {
        it.clock := clock
        tx       := it.tx
        exit     := it.exit
    }
}
```

### `vir check` — verify the project

- See *The verification loop* above. Flags: none (use the global `-C`).

### `vir run` — simulate the design

- Runs the `[prog] top` module on the `virt::Virt` virtual board, straight from
  Virdant sources — no Verilog.
- Requires the top module to have a `for` clause naming `virt::Virt`; other
  platforms are refused (`vir run` simulates `virt::Virt` only).
- UART stdin/stdout: the design behaves like an ordinary command-line program.
  `Hello, world!` from the template.
- The `exit : Valid[Word[8]]` port ends the run with exit code `n` when it
  reads `@Valid(n)`; `@Invalid()` keeps simulating.
- Flags (typical invocations in tests):
  - `--cycles <N>` — upper bound on simulated cycles; the sim stops at the
    limit. **Always pass this when running non-interactively.**
  - `--trace [<FILE>]` — VCD waveform, written to the named file (default
    `trace.vcd`); view with Surfer/GTKWave.
  - `-v` / `--verbose` — extra simulation detail (flash activity, etc.).
  - `--fps-no-cap` — disable real-time pacing (default is paced to wall time);
    use for fast, unattended runs.
  - `--fps` — print `Average clock frequency: X MHz` on exit or Ctrl-C.
    Tests commonly combine both: `vir run --fps-no-cap --fps --verbose`.
  - `--flash <FILE>` — preload the simulated 16 MiB SPI flash device.

```console
$ vir run --cycles 1000
$ vir run --fps-no-cap --trace
$ test -f trace.vcd
```

### `vir poke` — interactive peek-poke

- Drives and inspects signals on the elaborated design, one clock edge at a
  time. Three modes:
  - `vir poke` — REPL on `[prog] top` (`--top <module>` selects another,
    which may be non-`pub`).
  - `vir poke <script>.poke` — REPL with the script preloaded.
  - `vir poke <script>.poke --script` — run standalone as an assertion test:
    exits 0 when every `assert` passes, exits 1 on the first failure
    (also true of design check errors). This is what CI-style `.poke`
    tests use.
- `-C <dir>` / `-F <files>` after the subcommand; `-F` lets a script target
  loose `.vir` files.
- REPL commands (type `help`; TAB completes commands and signal paths):

```text
cd <path>           drill into a submodule (cd .. up, cd top back)
ls                  list components (kind + type) and instances
show                print every signal's value, grouped by module
autoshow [on|off]   show the design after every poke/tick
vcd <file>          write a VCD trace from now on
peek <path>         print a signal's value (also sets `it`)
poke <path> <expr>  drive a top-level incoming port or a register
watch <path>        print the signal whenever it changes
break <path>        stop after the next change of the signal
tick [n] [<clock>]  apply n full clock cycles (default 1)
advance [n]         move time n timesteps, driving no clock (default 1)
assert <expr>       fail the script if the expression is false
print <expr>        evaluate and print an expression
restart             re-initialize the simulation
top <module>        reload the simulation with a new top module
exit                leave the session
```

- Rules:
  - Paths are relative to the current module; `top.foo.bar` is absolute.
  - `poke` is legal only on top-level incoming ports and registers (anything
    else is overridden by the next settle). Clocks are never poked: use `tick`.
  - Each `tick` is one full clock cycle ending on the rising edge; a `poke`
    issued between ticks is the stimulus the next `tick` latches.
  - `advance` steps simulated time (one full clock period per step) without
    driving any clock, so it works even on unclocked designs where `tick`
    cannot; use it to settle combinational logic after `poke`.
  - Expressions are Virdant syntax: literals (`0b1010`, `5w4`, `true`),
    `+ - & | ^`, comparisons, `&& || ^^`, `~ ! -`, slicing `x[i]`,
    `x[hi..lo]`, constructors `#Ctor` (see ./literals-and-builtins.md).
  - Recent change: REPL keywords (`cd`, `tick`, `top`, `on`, `off`, `it`, ...)
    keep their special meanings only in command position; anywhere a *name* is
    expected — path segments, module names, file names, constructor names —
    a keyword is accepted as plain spelling. Bare `top` with no `.` after it
    is an ordinary name; `top.foo` is an absolute path.
- Script syntax: one command per line, `#` comments, end with `exit`.

```virdant
// counter.vir (from tests/poke/counter)
pub mod Top {
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

```console
$ vir poke script.poke --script   # or: vir poke script.poke (REPL)
poke counter 0
poke enable true
tick 3
assert counter == 3
assert double == counter + counter
peek counter
print counter + 1
exit
```

Note the implicit `on`: `counter` has no `on clock` because the module has
exactly one Clock; writing `on clock` there is a WARNING.

## Tools

- `vir doc` — generate HTML documentation into `build/doc` from `//>`
  docstrings. `--open` opens it in a browser afterwards. All modules are
  considered documentation-worthy.
- `vir format` — format `.vir` files in place: 4-space indent, no tabs.
  With no arguments, formats every `src/*.vir`; or pass files:
  `vir format src/top.vir`. Preserves vertical alignment of consecutive
  driver operators (`:=`, `<=`, `:=:`) and of type-annotation colons within
  a block. Naming/whitespace rules: see ./coding-conventions.md.
- `vir ls` — the language server. Editors launch `vir ls serve` (LSP).
  Query subcommands for scripts and agents (all exit 0, even
  `vir ls diagnostics` on a project with errors; exit 2 means the tool
  itself failed):
  - `vir ls hover <loc>` — hover info; `<loc>` is `<file_or_pkg>:<line>:<col>`,
    e.g. `src/top.vir:10:5`.
  - `vir ls definition <target>` — definition of the item at a location, or
    of a name (`Foo::Bar`).
  - `vir ls references <target>` — locations referencing the target.
  - `vir ls diagnostics` — project diagnostics (lint loop for scripts).
  - `vir ls symbols [PACKAGE]` — items (moddef, structdef, ...).
  - `vir ls components <MODDEF>` — a module's components.
  - `vir ls type-of <loc>` — the type of the expression at a location.
  - All accept `--json` for stable machine-readable output.
- `vir completion <shell>` — print a completion script for `bash`, `zsh`,
  `fish`, `powershell`, or `elvish` (e.g. `vir completion bash >> ~/.bashrc`).

## Flows

- `vir build` — compile the design to Verilog into `build/` (self-contained:
  one module per Virdant module, same names and ports) plus
  `build/virmanifest.json`. No optimization; a faithful translation.
- `vir synth` — yosys synthesis of the built Verilog; netlist at
  `build/<project>.json`. Platform comes from the top module's `for` clause.
- `vir pnr` — place-and-route (nextpnr) reusing that netlist. Flags:
  - `--tries <N>` (default 1) — run across several seeds, keep the best.
  - `--seed <N>` (default 1) — base seed.
  - `--allow-timing-failure` — do not fail on missed timing constraints.
- `vir bitstream` — full toolchain (synth + pnr + bitstream); writes
  `build/<project>.bin` (ice40) or `.bit` (xilinx) plus the constraints file
  (PCF/XDC) derived from `@pin`/`@period_ns` annotations.
- `vir upload` — everything `bitstream` does, then flashes the board with the
  platform's programmer (`icesprog` IceSugar, `iceprog` IceStick, `dfu-util`
  Fomu, `openFPGALoader` Arty A7). Same `--tries`/`--seed`/
  `--allow-timing-failure` flags as `pnr`.

## About / Learning

- `vir version` — print version information.
- `vir tutor` — scaffold `tutorial/` with lesson directories
  (`000_passthrough/`, each with `README.md`, `src/`, `test.toml`,
  `virdant.toml`) and a Git repo; `--no-git` suppresses Git init.
- `vir tutor` refuses to run where the workspace already exists, inside the
  tutorial, or inside a lesson directory ("already set up").
- `vir tutor check` — run inside a lesson directory: checks the lesson;
  passing it unlocks the next lesson. Re-checking a passed lesson passes
  again without unlocking anything new.

## `virdant.toml`

- Lowercase manifest name at the project root. Minimal form:

```toml
[project]
name = "hello"

[prog]
top = "top::Top"
```

- `[project] name` — used in output file names (`build/<name>.json`,
  `build/<name>.bin`).
- `[prog] top` — the design entry point, a package-qualified moddef name
  (`"top::Top"`); a bare module name (`"Top"`) also works. A missing
  `[prog] top` fails `run`/`build`/the flows before any external tool runs.
