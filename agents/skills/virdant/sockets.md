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
        case @Valid(dontcare) => true
        case @Invalid() => !is_full
    }

    when {
        case inp_fires match it.data {
            case @Valid(in_data) { ... }
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
