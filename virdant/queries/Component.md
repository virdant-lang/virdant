# Query: Component

## Summary

The builder `crate::analysis::component::build_component`
(`component`) fetches the `ComponentAnalysis` for the moddef owning
the component (`component_id.item_id`), looks up the `Component` by id via
`component_analysis.component(component_id)` (a linear scan over `components`
matching on `component.id()`), clones it, and wraps it in `Arc`. This is a thin
lookup query; all the real work happens in `ComponentAnalysis`.

## Signature

```rust
Component(component_id: ComponentId) -> Arc<Component>
```

## Result

A single component (signal/port/register/wire/submodule-port/socket-channel)
looked up by its `ComponentId`.

### `Component`

```rust
#[derive(Debug, Clone)]
pub struct Component {
    id: ComponentId,
    path: BString,
    location: Location,
    flow: Flow,
    kind: Option<ComponentKind>,

    // Type may be absent when `builder.get_type_at()` fails during resolution
    // We keep the component anyway -- path, flow, kind, and location are independently useful.
    typ: Option<Type>,
}
```

Defined in `virdant/src/analysis/component`.

- `id` - this component's `ComponentId`.
- `path` - the dotted access path. Top-level: just the name (`"clk"`).
  Submodule ports: `"instance.port"`. Socket channels: `"instance.channel"` or
  `"instance.socket.channel"`.
- `location` - the `Location` of the *statement* that declared this component.
- `flow` - the data-flow direction from the perspective of the owning moddef:
  `Source` (drives out), `Sink` (driven in), or `Duplex` (both). Helpers
  `can_sink()` and `can_source()` derive booleans from this.
- `kind` - the original `ComponentKind` if the component came from a `Component`
  AST node; `None` for socket-channel components where the kind is synthesized
  from the channel/role combination.
- `typ` - the resolved `Type`, or `None` if type resolution failed (the
  component is still kept - see the inline comment).

### `ComponentId`

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ComponentId {
    item_id: SymbolId,
    index: usize,
}
```

Defined in `virdant/src/analysis/component`. `item_id` is the `SymbolId`
of the owning `ModDef`; `index` is the position within that moddef's
`ComponentAnalysis::components`. Together they uniquely identify a component
across the compilation.

### Notable related enums

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Flow { Source, Sink, Duplex }

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ComponentKind {
    Incoming,
    Outgoing,
    OutgoingWire,
    OutgoingReg,
    Reg,
    Wire,
}
```

Both from `virdant/src/common.rs`. `Flow` describes direction;
`ComponentKind` describes the declaration kind. Flow is derived from
`ComponentKind` (e.g. `Incoming` -> `Sink`, `Outgoing` -> `Source`,
`Reg`/`Wire` -> `Duplex`) or from `(SocketRole, ChannelDir)` for socket
channels.

## Dependencies

* [`ComponentAnalysis`](ComponentAnalysis.md)

## Example

For `tests/pass/edge/src/top.vir`:

```virdant
export mod Top {
    incoming clock : Clock
    incoming reset : Reset
    incoming inp   : Bit
    outgoing out   : Bit

    reg last : Bit on clock {
        it <= inp
    }

    out := !last && inp
}
```

`ComponentAnalysis(<Top id>)` registers `clock`, `reset`, `inp`, `out`, and
`last` at indices 0..4. `Component(ComponentId { item_id: <Top id>, index: 4 })`
returns the `last` component: `path = "last"`, `kind = Some(Reg)`,
`flow = Duplex`, `typ = Some(Type::Bit)`, and `location` pointing at the
`reg last : Bit on clock` statement.
