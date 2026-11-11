Counter
=======
In this next example, we'll see a common use for `reg`\s: counting things.

`Counter` is a module which can count up or down, one at a time.

.. literalinclude:: src/counter.vir
    :caption: counter.vir
    :language: virdant

`Counter` has four `incoming` ports: `clock` `reset `increment` and `decrement`.
It also has one `outgoing` port.


Reset
-----
Before we go on to look at the driver block for `counter`,
let's take a look at the `reset` port.
`reset` has type `Bit`.
When you reset the circuit, it resets the counter back to zero.

It's not uncommon in hardware design to see reset pins that are **active low**.
This means your circuit resets when the reset signal is driven *low*.
However, in Virdant, the convention is to make reset **active high**.
So when `reset` is `true`, it means, "please reset!"

.. note::

    For those with some experience in hardware,
    using a `Bit` type means that this is a synchronous reset.
    Virdant provides another type `Reset` for asynchronous resets.

Resets in hardware often need to be held for several cycles to flush all of the registers.
But since there is only one register here, the circuit resets after just one cycle.


Outgoing Registers
------------------
The `counter` port is declared as `outgoing reg`.
This means that it acts *both* as an `outgoing` port and as a `reg`.

.. code-block::
    :language: virdant

    outgoing reg counter : Word[10]


Because it is an `outgoing`,
you can access `counter` when instantiating `Counter` as a submodule.
And because it is a `reg`, it holds its value,
and you have to assign its value with `<=`.

Why would you do this?
Well, sometimes it's as simple as: you want to output the value of a register.
But another important use is to help with **timing closure**.

Timing closure is ensuring your design can operate reliably at a target clock frequency.

The main obstacle to timing closure is when a circuit has a chain of logic which is too long.
Each gate takes a bit of time to complete its operation.
Even though this can be mere *nanoseconds*,
this can limit your design to run at less than 100 MHz.

By inserting a register to break long chains of logic, you can make a trade:
extra cycles per operation, but at a faster clock frequency.


Initial Drivers
---------------
We mentioned in the :doc:`PWM example <../pwm/index>` that, by default,
Virdant leaves registers uninitialized.
But we also said that most FPGAs have the ability to initialize registers when they first power on.
Virdant allows us to do this when the platform allows for it using an initial driver:

.. literalinclude:: src/counter.vir
    :caption: counter.vir
    :language: virdant
    :dedent:
    :lines: 9

To supply the initial value, we write `initial` followed by a driver.
The driver must be `<=`, since it sets the value of a register.
And since we're inside a driver block, we write `it` in place of `counter`.

.. note::

    When a platform does *not* support `initial` drivers,
    Virdant will flag them as an error instead.
    Thank you, Virdant, for always looking out for us designers!

`if` Statements
---------------
The heart of `Counter` is an `if` statement:

.. literalinclude:: src/counter.vir
    :caption: counter.vir
    :language: virdant
    :dedent:
    :lines: 11-21

These are just like the `if` statements that you would see in a programming language.
However, the syntax is a bit different.
Each condition being tested begin with the `when` keyword,
each of which must have type `Bit`.

The first condition is whether `reset` is set.
When it is, `counter` is set to `0`.
(Remember `it` refers to `counter` since we're inside a driver block).

The second condition is whether `increment` is set, but `decrement` *isn't* set.
(The `!` operator is for logical NOT).
In this case, `counter` is incremented.

The third condition is whether `decrement` is set, but `increment` isn't.
Naturally, in this case, `counter` is decremented.

The syntax is slightly heavier than it is in programming languages,
and this is for a good reason:
The conditions of an `if` statement (that is, each `when` condition)
is tested *in turn*.
This means that every branch adds to the latency of the circuit.
The extra typing is to remind you of this extra latency in the circuit.


`if` vs `match`
---------------
We used a `match` expression in :doc:`Pop Count example <../popcount>`:

.. literalinclude:: ../popcount/src/popcount.vir
    :language: virdant
    :lines: 5-10
    :dedent:

`if` and `match` are Virdant's two conditional structures.
As we just mentioned, `if` statements check their conditions one at a time.
And as we mentioned when discussing `PopCount2`,
`match` statements are checked in parallel.

However, `if` statements are more flexible, as they allow arbitrary conditions,
whereas `match` pattern-matching, which is more limiting.

All other things equal, you should prefer `match` over `if` whenever possible.


Statements vs Expressions
-------------------------
Another difference you may have noticed we called them `if` *statements* and `match` *expressions*.
Virdant also supports `if` expressions and `when` statements.

The experssion versions of `if` and `match` use the `=>` to select their values.
Because every expression in Virdant *has* a value,

The statement versions of `if` and `match` use curly braces `{` ... `}`.
Each of these is a block where we can place drivers.
And because these blocks can have more than one driver,
the statement versions are a bit more common.


Waveform
--------
Here is the waveform for `Counter`:

.. vcd:: trace.vcd

    signal top.clock:clk
    signal top.reset
    signal top.increment
    signal top.decrement
    signal top.counter:dec

Note that `top.counter` starts at `0` because of the `initial` statement.
It then gets reset near the end when `top.reset` is driven high.
However, it doesn't actually change for the next two clock cycles
since both `top.increment` and `top.decrement` are set at the same time.
