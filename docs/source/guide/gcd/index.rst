Union Types
===========
In the :doc:`SPI lesson <../spi/index>` we used *enum types* to name the
states of a state machine.
An enum is a set of named values: `#Idle`, `#Set`, `#Tick`.
Each value is just a label.

But often a state wants to *carry data with it*.
While our GCD circuit is busy, it holds two numbers that are still
being reduced; when it finishes, it holds the answer.
A plain enum can't hold those numbers.
For that, Virdant has *union types*.

This lesson builds `Gcd`, a circuit that computes the
:tooltip:`greatest common divisor (The largest integer that divides both inputs; e.g. gcd(48, 18) = 6)`
of two 8-bit words using Euclid's algorithm,
one subtraction per clock cycle.

.. literalinclude:: /guide/gcd/src/gcd.vir
    :caption: gcd.vir
    :language: virdant


Euclid's Algorithm
------------------
The greatest common divisor of two numbers is the largest number that
divides both of them evenly.
Euclid's algorithm finds it using nothing but subtraction:

* If the two numbers are equal, that number is the answer.
* Otherwise, replace the larger one with the difference
  and repeat.

For example, here is gcd(48, 18):

============  =========  ================================
Step          State      Action
============  =========  ================================
(start)       (48, 18)   48 > 18, subtract
1             (30, 18)   30 > 18, subtract
2             (12, 18)   12 < 18, subtract
3             (12, 6)    12 > 6, subtract
4             (6, 6)     equal -- the answer is 6
============  =========  ================================

Each step is one subtraction, which is one clock cycle's worth of work.
So again we trade time for gates: no divider, no multiplier,
just a comparator and a subtractor used over and over.


Union Types vs. Enum Types
--------------------------
The state of `Gcd` is a union type:

.. literalinclude:: /guide/gcd/src/gcd.vir
    :language: virdant
    :dedent:
    :lines: 1-9

A `union type` looks a lot like an `enum type`, and the two are close
relatives. The differences:

* **Variants carry payloads.**
  `Running(lhs : Word[8], rhs : Word[8])` is not just a label --
  it bundles the two numbers currently being reduced right into the
  state. `Done(result)` carries the finished answer.
* **Payloadless variants still get empty parentheses.**
  We write `Idle()` rather than `#Idle`. That keeps every variant
  constructed the same way.
* **Variants are constructed with `@`.**
  `@Idle()`, `@Running(48w8, 18w8)`, `@Done(6w8)`.

.. note::

    Virdant's built-in `Valid[T]` type is itself a payload-carrying
    enum: `#Invalid` for "nothing there", and `@Valid(value)` for
    "here is a value". We put it to work at the end of this lesson.


Constructing and Matching
-------------------------
The `state` register holds a `GcdState`, and it is driven by a `match`
expression:

.. literalinclude:: /guide/gcd/src/gcd.vir
    :language: virdant
    :dedent:
    :lines: 27-39

Each arm starts with a **pattern**, and patterns do more than select --
they *bind* the payload fields as new variables.

In the `@Idle()` arm, there is no payload to bind.
If `valid` is high, we construct `@Running(a, b)` from the incoming
ports and set out on our way.

In the `@Running(lhs, rhs)` arm, the pattern binds the two payload
fields to the names `lhs` and `rhs`.
Then a plain `if` expression does one Euclid step:
equal means `@Done(lhs)`;
otherwise subtract the smaller from the larger and stay `@Running`.

.. note::

    We named the bound variables `lhs` and `rhs` rather than `a` and
    `b`, because pattern variables shadow the module's incoming ports
    of the same name -- and Virdant warns about shadowing.

In the `@Done(_)` arm we use `_`, the discard pattern, to say
"whatever the result is, we don't need it here".
Discarded payloads are still matched; we just don't bind them to a name.

The `match` above is *exhaustive*: every variant of `GcdState` has an
arm, so no `else` is needed.


Destructuring in Outputs
------------------------
Patterns are not just for driving registers.
The `ready` output also matches on the state, exhaustively:

.. literalinclude:: /guide/gcd/src/gcd.vir
    :language: virdant
    :dedent:
    :lines: 41-48

The `@Running(_, _)` arm shows why `_` earns its keep:
we care only *which* variant we are in, not what it carries.

The `out` port is where the payload finally pays off.
Its type is `Valid[Word[8]]`: either there is no answer yet, or there
is an answer. We derive it by destructuring `state` one more time:

.. literalinclude:: /guide/gcd/src/gcd.vir
    :language: virdant
    :dedent:
    :lines: 50-55

This match is *not* exhaustive -- it lists only `@Done(result)`.
The `else` arm catches everything else (`@Idle()` and `@Running`, no
matter what they carry) and answers `@Invalid()`.
Use `else` when many variants share one answer;
list the arms individually when each variant matters.

Inside the `@Done(result)` arm, `result` is bound to the payload --
the finished GCD -- and we wrap it up as `@Valid(result)`.
Destructuring and constructing are two sides of the same idea:
the pattern takes a union apart, the constructor puts one together.


Trying It Out
-------------
The `gcd.poke` script runs a few pairs through the circuit and checks
the answers, including gcd(48, 18) = 6:

.. literalinclude:: /guide/gcd/gcd.poke
    :caption: gcd.poke
    :language: none

Run it with:

.. code-block:: console

    $ vir poke gcd.poke --script

The handshake works exactly as the ports promise:
`ready` is high while `Gcd` is idle,
`valid` and the operands are sampled together on that cycle,
and the answer appears on `out` for exactly one cycle,
wrapped in `@Valid`, before the circuit returns to `@Idle()`.
