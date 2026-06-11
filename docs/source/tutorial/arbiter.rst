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
If the device can only handle on transaction at a time, we need an arbiter to decide which client gets to use it.

When the first client wants access, it asserts `client0`.
Similarly, when the second client wants access, it asserts `client1`.

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
An important piece of how `Arbiter` works is this register:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 11-12


Imagine if both clients make a request on the same cycle.
What happens?

When `priority_for_client_0` is asserted, then client 0 has priority.
This means, in the event of a tie, if `priority_for_client_0` is `true`, then client 0 wins.
Otherwise, if `priority_for_client_0` is `false` and the tie goes to client 1.


Comments
~~~~~~~~
One of the great challenges when working with computers is bridging between
what makes sense to the computer and what makes sense to people.
It's easy to write code where the intention might not be clear to others (or worse, to yourself a month from now).
In these cases, it is good to add comments to your code.

Virdant uses `//` for comments.


When Expressions
----------------
The heart of `Arbiter` is composed of `when` expressions.

First, let's look at how the priority register is updated:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 14-17

This `when` expression checks if we are resetting the circuit.
If so, `priority_for_client_0` will latch `true` on the next cycle.
Otherwise, it flips to the opposite value.

What happens after a reset?


Round Robin
-----------
The `else` case in the priority update looks interesting:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 16

The `!` operator is logical NOT.
This driver has the effect of *flipping* `priority_for_client_0` from `true` to `false`
or from `false` to `true` each cycle.

.. note::

    Because we are using `<=` here and not `:=`,
    we are allowed to use `priority_for_client_0` on both the left hand side
    and the right hand side of the same driver.

    This works because `<=` means we are taking the value of the right hand side on *this* cycle
    and we will be making it the value of the left hand side for the *next* cycle.


Granting Access
---------------
The grant signals use `when` expressions to select the appropriate logic:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 19-23

During reset, grants are always false.
When `priority_for_client0` is true, `grant0` gets the full request signal.
Otherwise, client 0 only gets access if client 1 isn't requesting.

The logic for `grant1` is symmetric:

.. literalinclude:: /examples/arbiter.vir
    :language: virdant
    :dedent:
    :lines: 25-29

The client with priority will get it simply by asking for it.
The other client will get it only if it both asks and if the other doesn't.

The logic is symmetric.
It's quite pleasing, isn't it?
