# Control Flow

Virdant has two styles of conditional and match logic:
*expression-level* (producing a value on the right-hand side of a driver)
and *statement-level* (used inside a module or driver block to drive multiple
targets selectively).

## Statement-level `when`

When you need to drive *multiple* targets under the same set of conditions,
use a statement-level `when` block instead of repeating the same `when`
expression for each target.

```virdant
when {
    case reset {
        phase <= #Red
        timer <= 8
    }
    case timer == 0 {
        phase <= next_phase
        timer <= next_timer
    }
    else {
        timer <= timer - 1
    }
}
```

Each arm's body is a block of zero or more driver statements (`:=`, `<=`, nested `when`, nested `match`).
Arms are evaluated in order; the first matching arm fires.
If no `case` matches and no `else` is present, nothing happens (registers hold, wires are unassigned).

## Statement-level `match`

Use statement-level `match` when different cases drive different sets of
targets.

```virdant
match priority {
    case @Client0First() => {
        idle_grant_0 := client_0.a_valid
        idle_grant_1 := client_1.a_valid && !client_0.a_valid
    }
    case @Client1First() => {
        idle_grant_0 := client_0.a_valid && !client_1.a_valid
        idle_grant_1 := client_1.a_valid
    }
}
```

## Choosing between expression and statement form

Prefer the **expression** form when you are driving a single target.
It is more concise and easier to read.

Prefer the **statement** form when:
- You need to drive two or more targets under the same condition.
- The logic would require a deeply nested expression.
- You are writing a top-level register-update section where multiple registers change together.

Do not mix both forms for the same logic.
If you use a statement-level `when`, drive all related targets inside it.
If you use per-target expressions, keep them all as expressions.

## Direct chaining also applies at statement level

The rule about removing unnecessary `=>` (see `./driver-blocks.md`) also
applies inside statement-level `when` and `match` arms.

```virdant
// GOOD: direct chaining
when {
    case reset {
        priority <= @Client0First()
    }
    case d_done match serving {
        case @ServingClient0() {
            priority <= @Client1First()
        }
        case @ServingClient1() {
            priority <= @Client0First()
        }
    }
}

// BAD: unnecessary => before nested match
when {
    case reset {
        priority <= @Client0First()
    }
    case d_done => match serving {
        case @ServingClient0() {
            priority <= @Client1First()
        }
    }
}
```
