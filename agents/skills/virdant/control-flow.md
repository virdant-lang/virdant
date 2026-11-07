# Control Flow

Virdant has two styles of conditional and match logic:
*expression-level* (producing a value on the right-hand side of a driver)
and *statement-level* (used inside a module or driver block to drive multiple
targets selectively).

For `let` bindings (standalone and as arm bodies), see `./let-bindings.md`.
For pattern syntax, see `./types-and-patterns.md`.
For tristate (`bus`/`net`) behavior under conditionals, see `./tristate.md`.

## Statement-level `if`

When you need to drive *multiple* targets under the same set of conditions,
use a statement-level `if` block instead of repeating the same `if`
expression for each target.

```virdant
if {
    when reset {
        phase <= #Red
        timer <= 8
    }
    when timer == 0 {
        phase <= next_phase
        timer <= next_timer
    }
    else {
        timer <= timer - 1
    }
}
```

* Each condition must be of type `Bit`.
* Arms are evaluated in order; the first matching arm fires.
  This is often less efficient than `match`, which tests its arms in parallel!
* The `else` arm is optional when using `<=`. If no `when` clause matches and no `else`
  is present, nothing happens (registers hold, wires are unassigned).

## Statement-level `match`

Use statement-level `match` when different cases drive different sets of
targets.

```virdant
match priority {
    with @Client0First() {
        idle_grant_0 := client_0.a_valid
        idle_grant_1 := client_1.a_valid && !client_0.a_valid
    }
    with @Client1First() {
        idle_grant_0 := client_0.a_valid && !client_1.a_valid
        idle_grant_1 := client_1.a_valid
    }
}
```

* Arm patterns must agree with the subject's type (`#Enum` / `@Ctor(...)`
  for enums and unions, literals for `Bit`/`Word[n]`).
* Pattern-bound names are read-only and in scope only within the arm.

## Arm forms

Every arm (`when`, `with`, `else`) may use one of five forms:

1. Bare expression: `when cond => expr` / `with pat => expr`
2. Block: `when cond { stmts }` / `with pat { stmts }`
3. Nested `if`: `when cond if { ... }` / `with pat if { ... }`
4. Nested `match`: `when cond match expr { ... }` / `with pat match expr { ... }`
5. `let` binding: `when cond let x = expr { ... }` / `with pat let x = expr { ... }`

The same forms apply to `else` arms.

The position determines which forms are legal (the style pass enforces
this; the grammar accepts all forms everywhere):

* **Statement position**: arms must *perform*. Use blocks, nested
  `if`/`match`, or `let` bodies. `=> expr` arms and `{ expr }` blocks
  are errors ("For statement-based conditionals, use `{ ... }` instead
  of `=>`").
* **Expression position**: arms must *produce*. Use `=> expr`. A
  `{ expr }` block arm is an error ("For expression-based conditionals,
  use `=>` instead of `{ ... }`").

A chained nested `if`/`match`/`let` is legal in both positions (its own
arms are checked against the enclosing position).

## Match guards

An arm chained into an `if` acts as a *guard*: the arm's pattern must
match **and** an inner `when` condition must hold for its body to fire.

```virdant
reg r : Word[8] {
    initial it <= 0
    match sel {
        with 0w2 {
            it <= 1w8
        }
        with 1w2 if {
            when flag {
                it <= 2w8
            }
            else {
                it <= 3w8
            }
        }
        else {
            it <= 4w8
        }
    }
}
```

* A guarded arm still covers its pattern for exhaustiveness, even when
  the guard condition is false — make sure the inner arms produce the
  behavior you want.
* In statement position the guard's inner `if` does not need an `else`;
  in expression position it does (`if expression requires else arm`).
* References in guard conditions count as uses (no false "Unused"
  warnings).

## The `else` arm

* At most one `else` arm per `if`/`match`, and it must be last:
  `else arm must be the last arm of the match`;
  `Multiple else arms in match`.
* Do **not** add `else` when the `with` arms already cover every case —
  it is an error ("Redundant else arm"), not a warning.
* An `if` **expression** always requires an `else` arm.

## Exhaustiveness

Matches must be exhaustive when used as exprssion or when including a `:=` driver.
Required coverage depends on the
scrutinee's type:

* **Enum**: every enumerant must appear in some arm. Payload enumerants
  (enumerants with parameters, like `Some(value : T)` or builtin
  `Valid[T]`) must be covered with the constructor pattern
  `@Some(v)`, not `#Some`.
* **Union**: every variant must appear in some arm (`@Foo()`, `@Bar(x)`).
* **`Bit`**: both `true` and `false` must be covered.
* **`Word[n]`**: every value of the width must be covered — usually
  impractical, so use an `else` arm as the catch-all.

Typical errors: `Non-exhaustive match on Bit: not covered: false`,
`not covered: #Blue`, `not covered: @Bar`, `Non-exhaustive match`.

* Exception: a match whose arms drive only tristate components (`bus`/
  `net`) is exempt from exhaustiveness — an undriven case just leaves
  the line floating. See `./tristate.md`.

## Undriven arms

* For registers, if no arm fires, it keeps its previous value
* For non-registers, it is an error to have a missing arm.
* An arm guarded only by tristate drivers may leave the line floating
  (high impedance) instead — see `./tristate.md`.

## Choosing between expression and statement form

Prefer the **expression** form when you are driving a single target.
It is more concise and easier to read.

Prefer the **statement** form when:
- You need to drive two or more targets under the same condition.
- The logic would require a deeply nested expression.
- You are writing a top-level register-update section where multiple registers change together.
- When giving the transition table for a FSM.

Do not mix both forms for the same logic.
If you use a statement-level `if`, drive all related targets inside it.
If you use per-target expressions, keep them all as expressions.

## Direct chaining also applies at statement level

The rule about removing unnecessary `=>` (see `./driver-blocks.md`) also
applies inside statement-level `if` and `match` arms.

```virdant
// GOOD: direct chaining
if {
    when reset {
        priority <= @Client0First()
    }
    when d_done match serving {
        with @ServingClient0() {
            priority <= @Client1First()
        }
        with @ServingClient1() {
            priority <= @Client0First()
        }
    }
}

// BAD: unnecessary => before nested match
if {
    when reset {
        priority <= @Client0First()
    }
    when d_done => match serving {
        with @ServingClient0() {
            priority <= @Client1First()
        }
    }
}
```
