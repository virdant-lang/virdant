State Machines
==============
In this next example, we will demonstrate state machines built from
enum types and ``match`` blocks.

`TrafficLight` is a circuit which cycles through the three lamps of
a traffic light:
red -> green -> yellow -> red -> ...
A countdown timer decides how long each lamp stays lit.

.. literalinclude:: /guide_dev/fsm/src/fsm.vir
    :caption: fsm.vir
    :language: virdant


The States of the Traffic Light
-------------------------------
The controller needs to remember which lamp is currently lit.
We use a feature of Virdant called enum types:

.. literalinclude:: /guide_dev/fsm/src/fsm.vir
    :language: virdant
    :lines: 7-17

We use ``enum type`` to define a new enum type.
Inside the definition, we list all of the values in the type,
together with their numeric representations.

The first enumerant's value must also give the width.
Here, that is ``0b001w3``:
the ``0b`` prefix means binary,
the ``001`` gives the value,
and the ``w3`` at the end pins the whole type at `Word[3]`.
Every remaining enumerant may then be a bare literal;
giving some but not all would be an error.

(There is also an easier way:
when *no* enumerant gives its value explicitly, Virdant one-hot encodes
them for you -- the same numbering these literals spell out by hand.)

The values of our new type are prefixed with a ``#``.
They are: `#Red` `#Yellow` and `#Green`.

What does the enum buy us?
Three things stand out:

- **Named states.** The traffic light has exactly three meaningful
  configurations; giving them names makes every reference self-documenting.
- **One place for everything.** All the legal states live together --
  adding a fourth lamp later means editing this one block and nowhere else.
- **Exhaustiveness checking.** Virdant counts the arms of a ``match``
  against the arms of the enum itself: whenever we write code below,
  forgetting to handle a state becomes a compile error instead of a
  stuck-by-design simulation run.


The State Register
------------------
Like any state machine, we track the current state in a register:

.. literalinclude:: /guide_dev/fsm/src/fsm.vir
    :language: virdant
    :lines: 38-41

We give the register the `LightState` type (instead of a bare `Word[3]`)
and seed it with ``initial it <= #Red``, so the controller starts at power-on
with the red lamp lit.

(Without an explicit `initial`, a register starts with some unspecified bit
pattern, which here would be *no* declared variant -- so `initial` is what
makes "power on in red" reliable rather than lucky.)


A Countdown Timer
-----------------
We want each lamp to stay lit for several cycles, not just one or two.
A second register measures how long the current state has been running:

.. literalinclude:: /guide_dev/fsm/src/fsm.vir
    :language: virdant
    :lines: 43-63

The wire definitions also show off Virdant's block syntax:
each duration declares its own value inside `{}` via `it := ...`.

An important detail:
we initialize `timer` to five (`red_duration - 1w8`) instead of six,
because that is how long we can spend counting down before the timer expires --
that transition edge is itself the sixth cycle spent in Red.


Driving Both Registers Together
-------------------------------
Now for the meat of the lesson: driving these registers.
Both need updating together (the timer counts and the state advances), which is where
Virdant's distinction between **statement** blocks and **expression**
blocks shows up; recall this split from earlier lessons:

``{}`` performs
&&&&&&&&&&&&&&&
- When used as a statement, an arm *performs* driver actions (`<=`, etc.).

On the other hand:

``=>`` produces
&&&&&&&&&&&&&&&

- As an expression, the arm body must produce a value.

Here, ``state <= #Green`` (a driver) belongs under `{}`,
and would be rejected if written after ``=>``;
likewise arms like ``0b001w3 => 2`` are only allowed when producing values.

Our state machine uses one place to drive both registers -- a *statement-level*
match whose arms update whichever of them belong at their position in time:

.. literalinclude:: /guide_dev/fsm/src/fsm.vir
    :language: virdant
    :lines: 65-101

Each arm starts with ``with #Red { ... }``, matching on our enum pattern.
Inside each arm, an ``if`` block checks whether the countdown has expired:
when ``timer == 0``, we move on to the next state and reload the timer with
the next duration minus one;
otherwise we keep counting down by decrementing it once more.

Because every register is driven somewhere in this single match,
it is always clear which transitions happen together --
Verilog users will recognize why careful state-machine code guards against
two separate blocks racing into a latch; here there is exactly one block.


The Lamps
---------
Turning states into lamp levels turns out to use the other half of conditional logic,
producing each output as a value rather than driving inside blocks:

.. literalinclude:: /guide_dev/fsm/src/fsm.vir
    :language: virdant
    :lines: 23-36

Each lamp is high while the controller sits in the matching state,
computed from `state` without needing any additional registers.
In an expression like ``match state { with #Red => true else => false }``,
notice that arms produce their *result* directly after ``=>`` --
no drivers allowed.


Running the Demo
----------------
The lesson ships with a poke script, `fsm.poke`, which drives the design
through a full red -> green -> yellow -> red cycle:

.. literalinclude:: /guide_dev/fsm/fsm.poke
    :caption: fsm.poke

Power-on first: with no ticks at all, the controller is already in `#Red`
with the timer seeded at five and only the red lamp lit.
`initial` did its job before the first clock edge ever arrived.

Then four ticks with the countdown running:
`timer` reads four, three, two, one -- still under `#Red` throughout.
The fifth tick brings `timer` to zero, and the state *holds*:
red stays lit for this final countdown cycle, exactly the sixth cycle of red.
On the sixth tick the arm fires, `state` advances to `#Green`,
and the freshly reloaded timer (`green_duration - 1w8`) reads three.

The green and yellow stretches repeat the same rhythm.
Green holds through three countdowns, then hands over to `#Yellow`;
yellow, the shortest interval, needs only one countdown
before the controller returns to `#Red` with the timer back at five --
a complete lap around the block.

Notice the script never pokes a single input.
This tiny machine needs exactly one thing from the outside world: the clock.
Everything else -- state changes, countdowns, lamp levels -- falls out
of the transition table we wrote above.

You can replay this yourself from the lesson directory:

.. code-block:: console

    $ vir poke fsm.poke --script


Between Statements and Expressions
----------------------------------
Let's summarize what makes each piece above feel natural in its spot:

- The transition table reads like truth-table prose exactly because arms
  *perform* changes rather than compute answers.
- Outputs read like lookup tables precisely because their arms produce values.
- Match exhaustiveness then covers us either way: whether producing or performing,
  writing a state you forget about *cannot compile silently*.

Where next?
We used plain ``enum type`` here because our states are just names.
When a state needs to carry data with it -- like a pending bit count --
:doc:`../spi/index` shows a protocol whose states are a `union type`,
matched with payload patterns like ``@Start(pulse)``.
