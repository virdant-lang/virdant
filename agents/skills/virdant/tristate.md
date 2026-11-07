# Tristate Components (`bus`, `net`, and Tristate Drivers)

Tristate components model wires that more than one driver can drive.
Undriven means *floating* (high impedance), not holding a previous value.

* `bus` declares a bidirectional port. It is visible to the parent
  module and appears as an `inout wire` in generated Verilog.
* `net` declares an internal tristate net. It is not a port; it is
  visible only within the module.

For ordinary (non-tristate) module structure, see ./modules.md.

## Declarations

```virdant
bus sda : Bit {
    if {
        when pull {
            it #= false
        }
    }
}

net line : Bit
```

* A `bus` or `net` may NOT have an `on` clause or a `default` clause.
* Both may have an `it` block; that is the usual place to put drivers.
* Drivers may also be written as module-body statements after the
  declaration (`sda #- true`).
* `dependson` does not yet cover tristate components; do not annotate
  a `bus` or `net` outgoing with `dependson` through one.

## Driver operators

Three operators work on tristate components. Unlike `:=` and `<=`,
they carry a *strength*, and one component may have any number of drivers.

| Operator | Meaning | Generated Verilog |
|---|---|---|
| `#=` | Strong drive (full strength) | plain `assign` |
| `#-` | Weak drive (pull resistor) | `assign (weak1, weak0)` |
| `#=#` | Attach two tristate components into one shared line | `tran` |

* `#=` drives at full strength. If the guarding `when`/`with` arm does
  not fire, the component is undriven in that case and floats.
* `#-` drives weakly, like a pull resistor. Any strong driver on the
  same net overrides it.
* `#=#` attaches a submodule's `bus` port to a `bus` or `net` in the
  current module, joining them into one electrically shared line.
  Both operands must be paths to tristate components.
* Resolution when several drivers are active: strong (`#=`) wins over
  weak (`#-`); if no driver is active, the net floats.

```virdant
net sda : Bit
sda #- true          // weak pull-up: idle-high

mod master of Participant
master.sda #=# sda   // attach the submodule's bus port
```

## Reading requires `tap`

* A plain reference to a `bus` or `net` is a compile error
  (`NetReadWithoutTap`), because the value may be floating.
* Read through `tap`, which asserts the net is driven at that point
  and evaluates to its value:

```virdant
seen := tap sda
```

* `tap` applied to anything that is not a `bus` or `net` is an error
  (`TapNotOnNet`).

## Guards need not be exhaustive

* An `if` or `match` guarding ONLY tristate drivers does not need to
  be exhaustive: an arm that does not fire leaves the net floating
  rather than holding its previous value.
* This does not extend to ordinary `:=`/`<=` drivers mixed into the
  same guard; those follow the usual rules (see ./driver-blocks.md and
  ./control-flow.md).

## I2C open-drain idiom

A shared bus works like real I2C wiring: no participant ever drives
the line high; each either pulls it low strongly or leaves it
undriven, and one weak pull-up holds the idle line high.

```virdant
mod Participant {
    incoming pull : Bit

    bus sda : Bit {
        if {
            when pull {
                it #= false
            }
        }
    }
}

pub mod Top {
    incoming master_pull : Bit
    incoming slave_ack : Bit

    outgoing seen : Bit

    net sda : Bit

    // Pull resistor on the shared line: idle-high.
    sda #- true

    mod master of Participant {
        it.pull := master_pull
        it.sda #=# sda
    }
    mod slave of Participant {
        it.pull := slave_ack
        it.sda #=# sda
    }

    // Reading the shared net requires `tap`: an assertion that SDA is
    // not floating at this point (the pull resistor guarantees it).
    seen := tap sda
}
```
