Barrel Shifter
==============
Back when we built the :doc:`multiplier <../multiplier/index>`,
every partial product needed a shift:
row `i` had to move `i` places to the left.
We handled each shift by hand, since each amount was a constant we knew
while writing the code.

But suppose the shift amount is not a constant.
Suppose it arrives at runtime, as an input:

* shift left by 0? easy, do nothing
* shift left by 1? glue a zero on the bottom, as before
* shift left by 5? ...uh

This lesson builds a **barrel shifter**:
a module that shifts a word by *any* amount,
picked from an input, all in a single dash of logic.
No looping, no clock ticks, no waiting.

.. literalinclude:: /guide_dev/barrelshifter/src/barrelshifter.vir
    :language: virdant
    :lines: 6-33

`BarrelShifter8` takes a `Word[8]` to shift
and a `Word[3]` saying how far,
and produces the shifted result.
(A 3-bit amount can name any shift from 0 through 7,
which is every shift an 8-bit word can usefully do.)

Two new tools make this work:
conditional selection, which we met in the multiplexer lesson,
and **slices**, which are new.
Let's build up to the whole module one piece at a time.

Divide and Conquer
------------------
The trick is to never shift by a large amount directly.
Every amount from 0 to 7 is a sum of some of the powers of two:
1, 2, and 4.
In fact, that's just the binary representation of the amount:

============  ====  ====  ====
Shift by      s[2]  s[1]  s[0]
============  ====  ====  ====
0 (0b000)     0     0     0
3 (0b011)     0     1     1
5 (0b101)     1     0     1
7 (0b111)     1     1     1
============  ====  ====  ====

And shifting by a sum of amounts
is the same as shifting repeatedly, once per term:

.. code-block:: none

    shift left by 5  =  shift left by 4, then by 1
    shift left by 7  =  shift left by 4, then by 2, then by 1

So instead of one big shifter,
we build three tiny ones:
a "shift by 4" stage, a "shift by 2" stage, and a "shift by 1" stage,
chained in a row.
Each stage looks at one bit of `s`
and decides whether to do its shift or sit this round out.

A Single Stage
--------------
Here is the biggest stage:

.. literalinclude:: /guide_dev/barrelshifter/src/barrelshifter.vir
    :language: virdant
    :lines: 14-17
    :dedent:

Read `mux` as our old friend from the multiplexer lesson:
if `s[2]` is `true`, the stage shifts by 4;
if not, it passes `x` through untouched.

The shifting half is the new part.
`x[4..0]` is a **slice**:
it extracts a stretch of bits out of the middle of a word.
Gluing `0w4` onto its bottom shifts those bits four places to the left.

Let's zoom in on slices, because they are everywhere in hardware design.

Slices
------
You already know how to grab a single bit: `x[3]` is bit number 3.
A slice grabs a whole run of bits at once:

.. code-block:: virdant

    x[4..0]     // the bottom four bits: bits 3, 2, 1, and 0
    x[8..4]     // the next four up: bits 7, 6, 5, and 4

The syntax is `x[hi..lo]`,
and it extracts bits `hi - 1` down through `lo`.
Watch out: the top index is *exclusive*!
It marks where the slice stops, not a bit you get.
So `x[4..0]` does **not** include bit 4.

The width of the result is `hi - lo`:
`x[4..0]` is a `Word[4]`, and `x[8..4]` is another `Word[4]`.
No matter how wide `x` itself is,
the slice is exactly as wide as the range you asked for.

.. note::

    Keeping the top index exclusive makes the arithmetic friendly:
    the width is a subtraction you can do in your head,
    and neighboring slices butt up against each other without gaps.
    `x[8..4]` and `x[4..0]` together cover all eight bits,
    with no bit claimed twice and no bit lost.

Three Stages in a Row
---------------------
With three stages chained together, the shifter is complete:

.. literalinclude:: /guide_dev/barrelshifter/src/barrelshifter.vir
    :language: virdant
    :lines: 19-27
    :dedent:

Each stage's output feeds the next stage's input,
like stations on an assembly line.
`stage4` shifts by 4 if `s[2]` demands it,
`stage2` shifts the result by 2 if `s[1]` demands it,
and `stage1` finishes with a final nudge of 1 if `s[0]` demands it.
Stages that sit out simply pass the value through unchanged.

Look closely at the slices in the later stages
and notice how they track what has already been lost.
After shifting by 4, four bits have already fallen off the top,
so `stage2` only bothers slicing `stage4[6..0]`
before gluing on its two zeros.
By the last stage, the slice is `stage2[7..0]` --
seven bits, one short of the whole word,
because one more bit is about to fall off.

.. literalinclude:: /guide_dev/barrelshifter/src/barrelshifter.vir
    :language: virdant
    :lines: 29-32
    :dedent:

Why "Barrel"?
-------------
The name comes from the way the bits move:
all eight of them slide to their new positions *at the same time*,
like the chambers of a revolver spinning around its barrel.
One burst of logic, one answer.

Contrast that with the laziest possible shifter:
a register that shifts one place per clock tick,
counting down from `s`.
That design is tiny --
but shifting by 7 takes seven clock cycles.

The barrel shifter pays gates to buy time.
It takes about two dozen 2-way multiplexers
(each stage is one mux per bit),
and the answer arrives after just three mux delays:
one per stage.
When someone says a CPU "shifts in one cycle,"
this -- or something like it -- is the circuit they mean.

.. note::

    Two popular cousins of this circuit fall out of the same skeleton.
    Swap the glued-on zeros for the bits falling off the end,
    and you have a *rotate*,
    which spins the word around instead of losing bits.
    Fill the new top bits with copies of the old top bit,
    and you have an *arithmetic* shift, the signed-numbers flavor.
    Try one as an exercise!

.. note::

    In the adder lesson, we mentioned `a[dyn i]`,
    Virdant's dynamic indexing,
    which looks up a single bit chosen at runtime.
    It is the right tool when you need one bit out of a word.
    But pulling a whole shifted *word* out bit-by-bit with `dyn`
    would build a tall tower of multiplexers for every single bit.
    The staged structure above gets the same job done
    with far less hardware --
    which is exactly why real barrel shifters look like this.

Waveform
--------
Here is the shifter at work, holding `x` steady
and walking `s` through every amount from 0 to 7:

.. vcd:: trace.vcd

    label Inputs
    signal top.x:bin
    signal top.s:dec
    space

    label Stages
    signal top.stage4:bin
    signal top.stage2:bin
    signal top.stage1:bin

`x` is `0b1011_0010`.
Watch what each stage contributes:

* At `s` = 5 (which is `0b101`), both `stage4` and `stage1` fire.
  After `stage4`, the top nibble is gone and four zeros have arrived;
  after `stage1`, one more zero slides in and another bit departs.
* At `s` = 0, every stage sits out, and `x` sails through unchanged.
* At `s` = 6 (`0b110`), `stage4` and `stage2` fire.
  A single bit of `x` survives the trip,
  landing at the very top of the result.

The three stages in the waveform are the assembly line itself:
each one is either busy shifting or waving the bits on through.

Next Steps
----------
We can now add, count, multiply, choose, and shift.

Next, we put a multiplier on a budget:
a multi-cycle design that trades time for gates,
multiplying with just one adder across four clock cycles.
