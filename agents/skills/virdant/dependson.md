# `dependson` lists

`dependson` is a list on an `outgoing` (or `outgoing wire`) port
declaration: the module's declaration of which `incoming` ports the
port *may* combinationally depend on.  A socket instance declaration
may carry `where` clauses making the same kind of declaration for the
socket's driven channels (see "`where` clauses on socket instances"
below).

- It declares **potential**, not actuality: it does not create a
  dependency, it promises what dependencies are allowed.
- It is part of the module's **public interface contract**: it must
  list EVERY `incoming` port the port can combinationally depend on —
  directly, through internal signals, or through submodule
  declarations.
- You MUST write correct lists. An incomplete list is an error and
  `vir check` fails.

```virdant
pub mod Top {
    incoming inp : Bit

    //> Combinational output; may depend on `inp` in the same cycle.
    outgoing out : Bit dependson inp {
        it := inp
    }
}
```

Multiple entries are comma separated:

```virdant
pub mod Pair {
    incoming inp_0 : Bit
    incoming inp_1 : Bit

    outgoing out : Bit dependson inp_0, inp_1 {
        it := inp_0 && inp_1
    }
}
```

## Exhaustiveness rules

- An entry must resolve to an `incoming` port **of this module**.
  An outgoing port, an internal signal, a path into an instance, or a
  name that resolves nowhere is an error:

  - ``Bad `dependson` entry `out_b`: is not an `incoming` port of this module``
  - ``Bad `dependson` entry `nowhere`: could not be resolved to a component of this module``

- The declared set must be exhaustive. Absence of a list is an empty
  declaration. If a port's drivers — or its submodules' declared
  `dependson` — reach an `incoming` port not in the list, that is an
  error:

  - `` `out` could combinationally depend on `inp` but does not declare it ``

- A declared entry the port does not actually depend on is an *unused*
  `dependson`: a warning, not an error:

  - ``Unused `dependson`: `out` declares it may depend on `unused_inp` but does not``

- A port whose dependency flows through another declared port is
  justified by that port's declaration: if `out` reads an `outgoing
  wire` `ow` that declares `dependson inp`, then `out dependson inp`
  is justified even if `ow`'s driver is constant today.

- An `ext mod`'s body is opaque, so its declarations cannot be checked
  against its drivers: ext mods are never flagged for unused entries.
  (Its declarations still count as potential for the instantiating
  parent.)

- Ports that never need a list:

  - A constant-driven port (`out := true`).
  - An `outgoing reg`: a registered output cannot change in the same
    cycle, so it is never exhaustiveness-checked and never carries the
    list.

## Where the list may appear

Only `outgoing` and `outgoing wire` may carry `dependson`. The grammar
accepts it on any component declaration so each invalid placement is
diagnosed precisely:

```virdant
mod Carriers {
    incoming clk : Clock
    incoming inp : Word[8]

    // CHECK: `dependson` is not allowed on `outreg`: a registered output (`outgoing reg`) cannot change in the same cycle
    outgoing reg outreg : Word[8] dependson inp {
        it <= inp
    }

    // CHECK: `dependson` is not allowed on `in2`: `incoming` ports may not declare `dependson`
    incoming in2 : Bit dependson inp

    // CHECK: `dependson` is not allowed on `b`: `bus` is tristate; `dependson` on tristate components is not yet supported
    bus b : Word[8] dependson inp

    // CHECK: `dependson` is not allowed on `w`: internal signals may not declare `dependson`; only `outgoing` and `outgoing wire` may
    wire w : Word[8] dependson inp
    w := inp
}
```

- `reg` and `net` get the same "internal signals may not declare
  `dependson`" error as `wire`.
- The old statement form (`lhs dependson rhs`) is removed: it is a
  syntax error. `dependson` exists only as a list on component and
  socket-channel declarations.

## Socket channel obligations

Socket channels may declare `dependson` too (see ./sockets.md for
socket definitions and roles).

```virdant
socket Wishbone {
    cosi cyc : Bit
    cosi stb : Bit
    soci ack : Bit dependson cyc, stb
}
```

- A list on a `cosi` channel applies when the socket is instantiated
  as a `client`; a list on a `soci` channel applies as a `server`.
- The obligation places the same requirement on the containing module
  as if the `dependson` were declared on the resulting port.
- In the receiving role the list is inert.
- Entries must be channels of the same socket with the opposite
  direction word (`cosi` depends only on `soci`, and vice versa).
- Socket-derived obligations are exempt from the unused warning (a
  protocol such as Wishbone legitimately varies its latency).

## `where` clauses on socket instances

A socketdef channel list can only name same-socket channels.  To
declare that a module's drive on a socket channel may depend on
*anything else* — another socket's channels, or a plain `incoming`
port — attach a `where` clause to the socket instance declaration:

```virdant
mod PassThrough {
    client socket out of Pipe[Word[8]]
        where it.data dependson inp.data
    {
        it.data := inp.data
    }

    server socket inp of Pipe[Word[8]]
        where it.ready dependson out.ready
    {
        it.ready := out.ready
    }
}
```

Rules:

- Each clause is `where <CARRIER> dependson <ENTRY>, ...`; a socket
  may carry several clauses, one per line.  `it` refers to the socket
  instance the clause is attached to (the same convention as socket
  driver blocks); spelling the carrier as `<SOCKET>.<CHANNEL>` instead
  of `it.<CHANNEL>` parses but warns (`WhereNotIt`).
- The clause may be written before or after the socket's `{ ... }`
  driver block.
- The **carrier** must be a *driven* channel of the attached socket
  instance (`client` + `cosi`, or `server` + `soci`).  Naming a
  receiving channel is an error (`DependsonBadCarrier`), as is naming
  another socket instance, a channel that does not exist, or the same
  channel in two clauses.
- Each **entry** must resolve to an incoming component of the module:
  a plain `incoming` port, or a receiving channel of any of the
  module's own socket instances (same-socket entries are allowed — a
  moddef may restate or strengthen its socketdef's guarantees;
  restating an obligation never warns unused).  A driven channel, an
  outgoing port, a submodule path, or an unresolvable name is an error
  (`DependsonBadEntry`).
- The declaration is **potential, not actuality**, exactly like a
  port's `dependson` list, and it is part of the module's public
  interface contract: parents see it and must account for it.
- A driven channel's declared set must be **exhaustive** over every
  incoming component it can combinationally reach — its socketdef
  obligations plus its `where` entries.  An undeclared dependency on a
  plain `incoming` port or on another socket's receiving channel is an
  error (`DependsonUndeclared`).  There is no exemption for
  cross-socket dependencies anymore: they are declarable, so they are
  required.
- A `where` entry nothing justifies warns unused (`DependsonUnused`),
  like a port list entry.  An entry justified by an actual dependency,
  by another component's declared potential, or by the socketdef's own
  obligation edge does not warn.  `ext mod` declarations are exempt.
- `where` edges join the potentiality graph (see the cycle rules
  below) and are exported to parents like every other declaration.

## Combinational cycle detection

Virdant checks every module for combinational cycles over the
*potentiality graph*, whose edges are:

- the module's driver-derived combinational edges (`:=` targets
  depending on the signals their right-hand sides read, including the
  per-channel edges of `:=:` socket links);
- the module's own `dependson` lists, socket obligations, and
  `where`-clause edges;
- the `dependson` lists, socket obligations, and `where` clauses
  declared by the module's immediate submodule instances (declarations
  are the boundary contract: Virdant never looks inside a child
  module).

- Sequential (`<=`) edges are never traversed: a clock edge breaks the
  combinational path.
- A cycle is an error **even when no actual combinational path exists
  today**, because `dependson` is potential: a designer must be free
  to make any declared dependency actual at any time without breaking
  clients.
- Each cycle is reported once, at the module whose own local edge
  closes it. A loop entirely inside a submodule is reported there; a
  loop spanning a parent and its children is reported at the parent.

```virdant
// Cycle in the `dependson` relation (reported at Loop2)
mod Loop2 {
    mod loop_a of LoopA
    mod loop_b of LoopB

    loop_a.inp := loop_b.out
    loop_b.inp := loop_a.out
}

mod LoopA {
    incoming inp : Bit
    outgoing out : Bit dependson inp {
        it := inp
    }
}
```

Here `loop_a.inp` depends on `loop_b.out`, which (by `LoopB`'s
declaration) may depend on `loop_b.inp`, which depends on
`loop_a.out`, which may depend on `loop_a.inp`: a cycle in the
`dependson` relation, reported at `Loop2`.
