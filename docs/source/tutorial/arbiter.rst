Arbiter
=======
In this next example, we will demonstrate a more interesting use of `when` expressions.
We will also see the pattern for resetting a register.

`Arbiter` is a circuit which allows two clients to ask for access to a device,
but only grants access to one of them at a time.

.. literalinclude:: /examples/arbiter.vir
    :caption: arbiter.vir
    :language: virdant


What it Does
------------
In hardware designs, it's very common to share a hardware device between multiple clients.
If the device can only handle one transaction at a time, we need an arbiter to decide which client gets to use it.

When the first client wants access, it asserts `req0`.
Similarly, when the second client wants access, it asserts `req1`.

The `Arbiter` asserts `grant0` to grant access to the first client,
and it asserts `grant1` to grant access to the second.
But `Arbiter` never asserts both at the same time.


Clock and Reset
---------------
Most modules you will write will have both a clock and a reset like this:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 2-3

The type of reset signals is `Reset`.
You should pretty much always name your reset signal `reset`.


Priority
--------
An important piece of how `Arbiter` works is the `client0_priority` register.
Here's why:

Imagine if both clients make a request on the same cycle.
What happens?

When `client0_priority` is asserted, then client 0 has priority.
This means, in the event of a tie, if `client0_priority` is `true`, then client 0 wins.
Otherwise, if `client0_priority` is `false`, the tie goes to client 1.

Resets and Round-Robin
----------------------
`client0_priority` is given its value with a driver block:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 11-16

We see that the first case is `case reset`.
It is a very common pattern which gives a value to the register
when `reset` is asserted.
In effect, it gives `client0_priority` its initial value.

The `else` clause is where you end up *most* of the time.

The `!` operator is logical NOT.
This driver has the effect of *flipping* `client0_priority` from `true` to `false`
or from `false` to `true` each cycle.

So the pattern is `client0_priority` starts off `true`,
then changes to `false`,
then changes to `true`,
then changes to `false`,
and it just flips back and forth each cycle until it's reset again.

.. note::

    Because we are using `<=` here and not `:=`,
    we are allowed to use `client0_priority` (expressed here as `it`)
    on both the left hand side and the right hand side of the same driver.

    This works because `<=` means we are taking the value of the right hand side on *this* cycle
    and we will be making it the value of the left hand side for the *next* cycle.


The Heart of the Arbiter
------------------------
The heart of `Arbiter` is this `when` block:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 18-31

This `when` block is responsible for driving the values of `grant0` and `grant1`,
which indicate to each client if it has been given access to the device.
It does so based on `req0` and `req1`, which tell us which of our clients want access
on this cycle.

This block also depends on our friend `client0_priority` from above,
and technically on `reset` as well.

.. note::

    If you're coming from Verilog,
    you may be used to working with :verilog:`always` blocks and sensitivity lists.

    With Virdant, life is more peaceful.
    Each block knows when it should change.


Drivers in When Blocks
----------------------
This `when` block is different than the others we've seen before.

Rather than a single expression, each arm of this `when` block is a block of drivers.
This is very handy!
It's often the case in a module that two or more signals are determined
based on the same conditions.
This style of `when` block allows you to group them together.

The first `case` occurs on a reset.
This tells us that upon reset, neither client is granted access:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 19-22

The second `case` handles when client 0 has priority.
In this case, we grant access to client 0 if it asks.
We grant access to client 1 if it asks but client 0 hasn't asked,
using the `&&` operator (logical AND):

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 23-26

The `else` clause is the mirror of this, giving priority to client 1.
The logic is symmetric.

It's quite pleasing, isn't it?
