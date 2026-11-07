# Sockets

Sockets are Virdant's mechanism for grouping related signals into reusable interfaces.
They are analogous to SystemVerilog interfaces or Chisel bundles.

## Socket definition

Define a socket with the `socket` keyword.
Inside, declare channels with two direction keywords:

- `cosi` — Client-Out, Server-In.
  The client (requesting side) drives this signal.
  Equivalent to a "request" direction.
- `soci` — Server-Out, Client-In.
  The server (responding side) drives this signal.
  Equivalent to a "response" direction.

```virdant
socket Mem {
    cosi addr : Word[16]
    soci data : Word[8]
}
```

Socket names begin with an uppercase letter and use camel case,
just like module and type names.

## Using sockets on module ports

Add a socket as a module port with the `socket` keyword and a direction:

- `server socket` — the module implements the server role.
  The cosi channels appear as *incoming* signals and soci channels as
  *outgoing* signals.
- `client socket` — the module implements the client role.
  The cosi channels appear as *outgoing* signals and soci channels as
  *incoming* signals.

```virdant
mod Core {
    client socket mem of Mem
}

mod Memory {
    server socket mem of Mem
}
```

## Wiring sockets: individual signals

You can assign individual signals on a socket using dot notation:

```virdant
core.mem.addr := 8
memory.mem.data := 10
```

## Socket bulk connect (`:=:`)

To connect two sockets of the same type signal by signal, use the `:=:`
(bulk connect) operator.
It connects every channel of one socket to the corresponding channel of
the other, with the correct direction mapping.

```virdant
memory.mem :=: core.mem
```

Bulk connect is preferred over individual signal wiring when the sockets
are fully connected.
It reduces boilerplate and prevents mismatches when a socket definition
changes.

## Socket driver blocks (it-blocks)

You can attach a driver block to a socket declaration to drive or read
its channels via `it`.

```virdant
server socket inp of Pipe {
    it.ready := match out_data {
        with @Valid(dontcare) => true
        with @Invalid() => !is_full
    }

    if {
        when inp_fires match it.data {
            with @Valid(in_data) { ... }
        }
    }
}
```

This pattern is especially useful for sockets with handshaking logic
(valid/ready, etc.).

## `unused` with socket members

When a module drives only some channels of a socket (e.g., a server that
ignores `addr`), suppress warnings with `unused` on the socket or on
individual members:

```virdant
mod Memory {
    server socket mem of Mem {
        it.data := 10
    }
    unused mem.addr
}
```

Or inside a socket driver block:

```virdant
mod Core {
    client socket mem of Mem {
        it.addr := 8
        unused it.data
    }
}
```

## Naming convention for sockets

Socket type names are nouns describing the protocol:
`TlUl`, `Wishbone`, `Mem`, `Pipe`, `SpiBus`.

Ports that instantiate a socket use the socket name in lowercase as the
port name:

```virdant
mod Top {
    server socket host of TlUl
    client socket dev of TlUl
}
```

When a module has multiple sockets of the same type, disambiguate with
a prefix or a numbered suffix:
`host_0`, `host_1`, `device`, `inp`, `out`.

## Generic sockets

A socket definition may declare generic parameters, exactly like type
and module definitions (see ./generics.md). The parameter names are in
scope in the channel types of the body:

```virdant
socket Bus[T : Type] {
    cosi req : T
    soci rsp : T
}
```

An instantiation supplies type arguments after the socket definition
name, in declaration order:

```virdant
mod Master {
    incoming d : Word[8]
    client socket busif of Bus[Word[8]] {
        it.req := d
    }
    unused busif.rsp
}

mod Slave {
    server socket busif of Bus[Word[8]] {
        it.rsp := it.req
    }
}
```

- Channel types are the definition's channel types with the arguments
  substituted: `busif.req` and `busif.rsp` are `Word[8]` above.
- The builtin `Pipe[T]` is a generic socket: `client socket p of Pipe[Word[8]]`.
  Omitting the arguments is an error:
  `Type builtin::Pipe takes 1 type argument, but 0 were given`
- A `Type` parameter given a number (`client socket busif of Bus[8]`) reports
  `Type argument mismatch: parameter T of Bus expects a type, but found a Nat literal`
- Multiple parameters are allowed and can differ per channel
  (`Bus[A : Type, B : Type]` with a wide request and narrow response).

## Mono-socket matching for `:=:`

- `:=:` matches on the **mono socket**: the socket definition together
  with its type arguments. Same definition AND same args connect;
  same definition at different args are different mono sockets and do
  not connect.
- A mismatch is an error naming both instantiations and pointing at the
  connection:

```virdant
socket Link[T : Type] {
    cosi data : T
    soci ready : Bit
}

mod Client {
    client socket pipe of Link[Word[8]] {
        it.data := 7w8
        unused it.ready
    }
}

mod Server {
    server socket pipe of Link[Word[9]] {
        it.ready := true
    }
    unused pipe.data
}

pub mod Top {
    mod master of Client
    mod slave of Server
    slave.pipe :=: master.pipe
}
```

reports:

```text
Cannot connect socket_connect_mismatch::Link[Word[9]] to socket_connect_mismatch::Link[Word[8]]: mismatched socket instantiations
```

(the package prefix matches where the socket is defined).

### Composition through generic module instances

Arguments written at a socket instantiation inside a generic module
compose with the arguments of the enclosing module's instantiation:

```virdant
mod Master[W : Type] {
    incoming d : W
    client socket busif of Bus[W] {
        it.req := d
    }
    unused busif.rsp
}

mod Slave[W : Type] {
    server socket busif of Bus[W] {
        it.rsp := it.req
    }
}

pub mod Top {
    incoming inp : Word[8]

    mod masterg of MasterG[Word[8]] {
        it.d := inp
    }
    mod slaveg of SlaveG[Word[8]] {
        masterg.busif :=: it.busif
    }

    outgoing out : Word[8]
    out := slaveg.busif.rsp
}
```

`masterg.busif` instantiates `Bus` at `W`, and `master` supplies `Word[8]`
for `W`, so both sides are the mono socket `Bus[Word[8]]` and the
connection checks. Connections written inside an it-block are checked
the same way: `it` resolves to the instance, and the instance's
arguments compose with the socket's.

## `dependson` on socket channels

A socket channel may carry a `dependson` list naming channels of the
opposite direction word of the same socket:

```virdant
socket Wishbone {
    cosi cyc : Bit
    cosi stb : Bit
    soci ack : Bit dependson cyc, stb
}
```

- The list applies when the socket is instantiated in the channel's
  **driving** role: `client` for `cosi` channels, `server` for `soci`
  channels. It places the same obligation on the containing module as
  if the `dependson` were declared on the resulting port: the module's
  drive on that channel may combinationally depend on its reads of the
  listed channels.
- In the **receiving** role the list is inert: a `soci` list says
  nothing about a `client` instantiation, a `cosi` list says nothing
  about a `server` instantiation.
- Socket-derived obligations are **exempt from the unused-`dependson`
  warning** when the drive does not actually exercise the list: bus
  protocols legitimately vary their latency, so the declared potential
  may stay dormant. It is still a contract: the module must not drive
  the channel with anything outside the declared set, and cycles in
  the composed `dependson` relation are errors.
- See ./dependson.md for the module-port rules this builds on.

## Cross-references

- Generic parameters and mono modules: ./generics.md
- Instances, it-blocks, ext mods, `@parameter`: ./modules.md
- `dependson` on module ports and the potentiality/cycle rules: ./dependson.md
