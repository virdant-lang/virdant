# Let Bindings

`let` binds immutable names for use within a scope. It works in both
*statement position* (module body, driver blocks, arm bodies) and
*expression position* (anywhere an expression is allowed).

For the `if`/`match` constructs a `let` can chain into, see
`./control-flow.md`. For where driver statements live, see
`./driver-blocks.md`.

## Rules

* Bindings are immutable: each name is bound exactly once within its scope.
* Bindings may be untyped (type inferred) or explicitly typed:
  `let x = a { ... }` / `let x : Word[8] = a + b { ... }`.
* Several bindings may be given in one comma-separated list; each binding
  may reference the bindings *before* it:

```virdant
let lo = a,
    hi = b + lo {
    out := hi
}
```

* A later binding *shadows* an earlier one of the same name:

```virdant
let x = a,
    x = x + b {
    out := x     // sees the second binding: a + b
}
```

* Within the body, a bound name shadows any *component* of the same name
  (wire, reg, port, instance). If the shadowed component becomes unused,
  mark it `unused` to suppress the warning:

```virdant
wire sum : Word[8] {
    it := a + a
}

let sum = a {
    out := sum   // this is the binding, not the wire
}
unused sum
```

* Mixed typed/untyped bindings in one list are fine:

```virdant
out := let lo = a, hi : Word[8] = b => hi + lo
```

* Nothing outside a `let`'s body can see its bindings.
* A component referenced through a binding counts combinationally: a
  driven target's `dependson` list must include it (`dependson a, b`,
  comma-separated).

## Interaction with match patterns

The innermost binding wins:

* A match arm's *pattern variables* shadow `let` bindings from outside
  the match.
* A `let` written *inside* an arm shadows the arm's pattern variables.

```virdant
let value = 7w8 {
    out := match v {
        with @Invalid() => true
        with @Valid(value) => value == 3   // pattern var wins here
    }
}

out := match v {
    with @Invalid() => false
    with @Valid(value) let value = 3w8 => value == 3   // let wins here
}
```

## Type inference

* For an untyped binding, the type is inferred from its **first use** in
  the body.
* A widthless literal RHS must be anchored by a use site that fixes its
  width. Anchored by a ctor payload type or a driver target:

```virdant
reg pulse : Word[4] {
    let expire = it == 0w4 {
        it <= if {
            when go && expire => 0w4
            else => it - 1
        }
    }
}
```

* An unanchored widthless literal has no width to infer from and is an
  error:

```virdant
// ERROR: Can't infer the type of let binding `x`;
//        annotate it (`let x : T = ...`) or use it where a type is expected
let x = 0 {
    out := a    // `x` is never used with a known expected type
}
```

* If inference fails, either ascribe the type (`let x : Word[8] = 0`) or
  use the binding where a width is expected.

## Terminator style (compiler-enforced)

A `let` body is either a block (`{ ... }`) or a value (`=> expr`).
The spellings carry intent, and the compiler enforces the pairing:

* `{` means *this scope performs*: it runs driver statements.
* `=>` means *this scope produces*: it evaluates to a value.

| Position | Legal terminators | Flagged |
| --- | --- | --- |
| Statement | `{ stmts }` or chained `if`/`match` (arms perform) | `=> expr`; `{ expr }` (a bare expression in braces) |
| Expression | `=> expr` or chained `if`/`match` (arms produce) | any `{ ... }` block form |

```virdant
// ERROR: For a `let` used as a statement, use `{ ... }` instead of `=>`
let x = a => x

// OK: statement `let` performs
let x = a {
    out := x
}

// ERROR (same message): block whose only content is a bare expression
let x = a {
    x
}

// ERROR: For a `let` used as an expression, use `=>` instead of `{ ... }`
out := let x = a {
    x
}

// OK: expression `let` produces
out := let scaled = a + a => scaled + b
```

The chained forms are legal in **both** positions and are never flagged
by the style check:

```virdant
// Statement: chained if whose arms perform
let x = b if {
    when a {
        out := x
    }
    else {
        out := 0
    }
}

// Expression: chained match whose arms produce
it <= let base = 5w8 match s {
    with @Idle() => base
    with @Step(v) => base + v
}
```

## `let` as an arm body

A `let` may be the body of a `when`/`with`/`else` arm
(`when cond let x = expr { ... }`). It follows the same terminator rules
as any other `let` and inherits the arm's position:

```virdant
reg r : Word[8] {
    initial it <= 0
    if {
        when sel let next = it + 1 {
            it <= next
        }
        else {
            it <= it - 1
        }
    }
}
```

* Sibling arms may bind the same names to different values; each arm sees
  only its own bindings.
* In an *expression-position* arm (e.g. inside an `if` expression), the
  arm's `let` must produce: use `=>` or a chained form, not `{ ... }`.
