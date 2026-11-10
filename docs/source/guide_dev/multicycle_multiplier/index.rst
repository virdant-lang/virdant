A Multi-Cycle Multiplier
========================
The :doc:`combinational multiplier <../multiplier/index>` bought speed with gates:
four masked copies of `b`, three adders, and the answer in a single dash of logic.

Hardware design is full of bargains like this,
and the most useful one says:
*you can trade time for space.*
Instead of owning three adders that each work once,
own *one* adder and ask it to work four times.

This lesson builds `MulticycleMultiplier`:
same multiplication, same answer,
but spread across four clock cycles.
And because the module now lives across many cycles,
it needs something our combinational lessons never had:
a proper **state machine**.

.. literalinclude:: /guide_dev/multicycle_multiplier/src/multicycle_multiplier.vir
    :language: virdant
    :lines: 16-74

The Algorithm
-------------
Here is grade-school multiplication again,
but with a twist: this time we work from the *left*.

To compute 13 x 11 (which is `0b1101` x `0b1011`),
walk down the bits of `a` one at a time.
Keep a running total.
At each step, double the total (that's a shift!)
and add a fresh copy of `b` whenever the current bit of `a` is 1:

============  =======  ==========================
Step          Bit      Running total
============  =======  ==========================
(start)       --       0
1             1        0 x 2 + 11     = 11
2             1        11 x 2 + 11    = 33
3             0        33 x 2         = 66
4             1        66 x 2 + 11    = 143
============  =======  ==========================

Notice there is only ever *one addition per step*.
That is the whole secret of the multi-cycle multiplier:
a circuit small enough to run once per clock cycle,
applied over and over.

The Protocol
------------
Because the module now takes several cycles to answer,
it needs a way to say *when it is busy* and *when it is done*.
This is called a **handshake**, and ours has two pins:

* `start` -- pulse it high for one cycle to drop `a` and `b` on the desk
* `done` -- high exactly when `product` holds a finished answer,
  and stays high until the next `start`

Pulsing `start` while a multiplication is in flight does nothing:
the module is busy, and the pulse is politely ignored.
While `done` is low, whatever is on `product` is not the answer yet.
Downstream modules (and waveforms, and testbenches)
wait for `done` before trusting `product`.

A State with a Memory
---------------------
How should the module keep track of where it is?

The obvious ingredients are a busy flag and a counter --
but flags and counters that only matter while "busy"
are a smell that the *state itself* wants to be a type.
Virdant lets us declare one:

.. literalinclude:: /guide_dev/multicycle_multiplier/src/multicycle_multiplier.vir
    :language: virdant
    :lines: 6-14

This is a `union type`:
a value that is one of several variants,
each with its own name and -- optionally -- its own payload.
Our three phases are:

* `@Idle()` -- waiting for a `start`
* `@Mul(bit)` -- multiplying, where `bit` records how far along we are
* `@Done()` -- holding a finished product

The counter from the step table did not vanish;
it moved *inside the state*.
When the module is in `@Mul(2w2)`, it literally carries the number 2 with it.
States without payloads (`Idle` and `Done`) carry nothing at all.

The state register is the brain of the module,
and its driver reads like the step table itself:

.. literalinclude:: /guide_dev/multicycle_multiplier/src/multicycle_multiplier.vir
    :language: virdant
    :lines: 31-42
    :dedent:

Read each arm as an answer to "what happens next?":

* from `@Idle`, a `start` launches `@Mul(0)`; otherwise stay put
* from `@Mul(bit)`, move on to the next bit --
  or, after bit 3, declare victory and become `@Done`
* from `@Done`, a `start` launches a fresh `@Mul(0)`; otherwise hold

.. note::

    The match must cover every variant of `MulState`.
    Try deleting an arm and `vir check` will remind you.
    This is the state-machine payoff:
    add a phase tomorrow, and the compiler walks you
    to every corner of the module that must learn about it.

The Datapath
------------
The state machine decides *when*; the datapath decides *what*.
The accumulator is the product itself:

.. literalinclude:: /guide_dev/multicycle_multiplier/src/multicycle_multiplier.vir
    :language: virdant
    :lines: 44-56
    :dedent:

`product` is an `outgoing reg`:
the answer-under-construction and the answer-we-present are the same register.
No shadow copy, no forwarding wire.
While multiplying, each cycle either shifts (doubling, via the
append-a-`0w1` trick from the barrel shifter)
or shifts *and* adds a copy of `b` widened into a `Word[8]` with `0w4`.

And the `else` arm?
While the module is *not* multiplying --
which covers both `@Idle` and `@Done` --
the product just holds its value,
unless `start` clears the slate for a fresh run.
The control refines `Idle` from `Done`;
to the datapath they look identical: nobody is multiplying.

Which bit of `a` is in play?
Rather than index into `a` with a runtime position
(the multiplexer-tower trouble we saw in the barrel shifter lesson),
we load `a` into a register and slide it left every cycle:

.. literalinclude:: /guide_dev/multicycle_multiplier/src/multicycle_multiplier.vir
    :language: virdant
    :lines: 58-65
    :dedent:

Now bit 3 of `areg` always holds *exactly the bit we want next*.
The register is a conveyor belt:
each cycle delivers the next bit of `a` to the same pickup window.

Finally, the handshake pin itself,
a plain `outgoing` derived from the state:

.. literalinclude:: /guide_dev/multicycle_multiplier/src/multicycle_multiplier.vir
    :language: virdant
    :lines: 67-73
    :dedent:

Reading Old Values
------------------
There is a subtlety worth savoring.

At the same clock tick,
`product` computes its next value *from the old* `product`,
while `areg` slides forward from its old position.
Every register driver reads the values as they were *before* the tick.
Nothing races, nothing glitches --
each register sees a consistent snapshot of the world,
updates itself, and everyone else sees the update on the *next* tick.

This is why the algorithm composes so cleanly:
the step "shift, then add" reads `product` and writes `product`
without any danger of tripping over itself.

It also explains a quiet elegance of this design:
`done` is simply a view of the state.
When the final addition lands in `product`,
the state lands in `@Done` at the very same tick --
so `done` rises at the exact moment `product` becomes trustworthy,
never a cycle early, never a cycle late.

.. note::

    We gave every register an `initial` value,
    which is what the simulator starts from.
    Real boards power up in no particular state,
    and some platforms forbid `initial` outright.
    The robust answer is a proper reset signal --
    which is exactly what the next lesson introduces.

Waveform
--------
Here are three multiplications in a row:
13 x 11, then 9 x 5, then 15 x 15.

.. vcd:: trace.vcd

    label Inputs
    signal top.start
    signal top.a:dec
    signal top.b:dec
    space

    label Conveyor
    signal top.areg:bin
    space

    label Result
    signal top.product:dec
    signal top.done

Watch the first multiplication closely.
`start` pulses, and on the next tick
`areg` loads `1101` while `product` snaps to 0.
From there, `product` climbs the table:
0, 11, 33, 66, and finally 143,
doubling and occasionally grabbing a copy of `b` along the way.
Meanwhile `areg` parades its bits leftward --
`1101`, `1010`, `0100`, `1000`, `0000` --
each cycle offering the next bit at the top.
When the last bit has passed, `done` rises to announce 143.

The next two multiplications begin *from* `done`:
the module never visits `@Idle` again in this trace.
9 x 5 lands on 45, and 15 x 15 fills every step
on its way to 225.

The Trade
---------
So which multiplier is better?
Wrong question -- better *for what*.

===============  =====================  =====================
                 Combinational          Multi-cycle
===============  =====================  =====================
Gates            ~3 adders worth        1 adder worth
Answer after     0 wait (instant)       4 cycles
New problem      every cycle            every 5 cycles
===============  =====================  =====================

The combinational multiplier is *faster* (no waiting)
and has more *throughput* (it can start a new problem every cycle).
The multi-cycle multiplier is *smaller*:
one adder instead of three-plus,
which matters enormously when the words get wide.

This bargain has a name you will meet again and again:
**area versus latency**.
Did you notice a third option hiding between them?
Pipeline the combinational multiplier --
registers between the adder stages --
and you get small-ish *and* one answer per cycle.
That lesson, though, is for another day.

Next Steps
----------
We now have a module with a genuine state machine:
a union-typed state, a datapath that obeys it,
and a handshake to tell the world where things stand.

Next, we exercise those muscles on a problem with no arithmetic at all:
the arbiter, which juggles competing requests --
and learns about reset.
