Population Count
================
Here is a classic circuit with a classic name: the *population count*.

A population count answers a very simple question:
**how many bits are set to 1?**

For example, the population count of `0b0111` is 3,
because three of its four bits are set.
The population count of `0b1000` is 1.
And the population count of `0b0000` is 0.

You will also hear this circuit called by other names:
*Hamming weight*, *sideways sum*, or just *popcount*.
It shows up all over digital design:
a parity bit is just the lowest bit of a population count,
Hamming error-correcting codes lean on it heavily,
and cryptographic circuits use it to measure how many bits
differ between two words.

.. note::

    Most processors today have a dedicated *population count instruction*
    (`POPCNT` on x86, `CNTPOP` on ARM) for exactly this purpose.
    Even your CPU wants fast hardware for this!

In this lesson, we will build `Popcount4`,
a module which counts the set bits of a 4-bit word.
And along the way, we will learn a brand-new operator.

Counting Two Bits
-----------------
Let's start with the smallest question worth asking:
how many bits are set in *two* bits?

.. literalinclude:: /guide/popcount/src/popcount.vir
    :language: virdant
    :lines: 6-18
    :dedent:

`Popcount2` takes a `Word[2]`, named `x`,
and produces `count`, also a `Word[2]`,
which can hold the answers 0, 1, and 2.

The key insight is this: adding numbers *is* counting.
The two-bit number you get from "adding the bits of `x`"
is exactly the number of set bits among them.
So instead of building a special counting circuit,
we can simply add.

Well... almost simply.
Look closely at the one line of real work:

.. literalinclude:: /guide/popcount/src/popcount.vir
    :language: virdant
    :lines: 15-17
    :dedent:

First, `word(x[1])` and `word(x[0])` fish out the two bits of `x`
and wrap each one up into a `Word[1]`.
(Concatenation works for single values too!)

Second, we add them with `+.`, pronounced "dot plus".
This is the **widening add**, and it is new to us.

The Widening Add
----------------
In the adder lessons, we hand-built adders out of gates,
carefully routing every carry by hand.
The built-in `+` operator computes the same thing behind the scenes --
but it has a strict rule:
the answer comes back the *same width* as the operands,
and anything that overflows is silently dropped.

That would be a disaster here!
If we add two `Word[1]`\s with plain `+`,
then `1 + 1` wraps around to `0`,
and our count would insist that two set bits are... zero set bits.

The widening add `+.` fixes this:
it adds the operands *and* keeps the carry-out,
so the result is one bit wider than the inputs.

.. code-block:: virdant

    word(x[1]) +. word(x[0])    // two Word[1]s in, one Word[2] out

The `count` of `Popcount2` is exactly this:
a `Word[2]` big enough to hold 0, 1, or 2.

Pairing Up
----------
Now for the full 4-bit version.

.. literalinclude:: /guide/popcount/src/popcount.vir
    :language: virdant
    :lines: 20-39

The strategy is divide and conquer.

First, split `x` in half:

* the upper pair: `x[3]` and `x[2]`
* the lower pair: `x[1]` and `x[0]`

Yes, `word()` again!
Here it bundles the two bits of each pair
into exactly the `Word[2]` shape that `Popcount2` asks for.

.. literalinclude:: /guide/popcount/src/popcount.vir
    :language: virdant
    :lines: 25-33
    :dedent:

Each `Popcount2` hands back its own little count,
a `Word[2]` between 0 and 2.
The population count of `x` is just the sum of the two counts.

And the sum of two `Word[2]`\s is a perfect job for `+.`
once again: two bits in, plus a carry bit, three bits out.
(And the timing couldn't be better:
the biggest answer is `4`, which is `0b100` -- three bits.
The widening add leaves room for exactly what we need.)

.. literalinclude:: /guide/popcount/src/popcount.vir
    :language: virdant
    :lines: 35-39
    :dedent:

Notice `count` is a `Word[3]`, not a `Word[4]`.
Can 4 bits be set at once? Certainly.
Does the count fit? `4` is `0b100` -- three bits.
When you count bits, the answer never needs as many bits
as you might fear: one bit wider than the operands is plenty.

.. note::

    In Virdant, you can rely on the compiler to catch mistakes like this.
    If you had declared `count` as a `Word[2]`,
    the widened sum would refuse to fit,
    and `vir check` would tell you right away.

Trees Beat Chains
------------------
Why split into pairs first,
instead of summing all four bits in one long chain?

Width tells the story.
Every `+.` adds a single bit of headroom,
so if you stack additions into a chain,
each one has to be a little wider than the last,
and the carries have to trudge through all of them.

By pairing first, the adds happen *in parallel*,
two at a time, and only one widening add remains at the end.
This divide-and-conquer arrangement is called an **adder tree**,
and it shows up whenever hardware needs to sum many values at once.

This is worth remembering:
whenever you can reformulate a problem as *counting* rather than *adding*,
you often save both gates and time.

Scaling Up
----------
The same recipe scales to any width.
To build `Popcount8`, you would:

* split the 8 bits into four pairs, counting each pair
* add the four counts pairwise, with widening adds
* add those two counts for the final 4-bit answer

The tree gets wider at the bottom,
but it only grows *one level deeper* each time you double the input width.
That is a far better trade than a chain,
which grows one level for every single added bit.

.. note::

    In future lessons, we will see how to write designs
    that adapt to any width automatically, using Virdant's generics.
    Until then, enjoy writing out the trees by hand --
    it builds character. (And intuition.)

Waveform
--------
Here is `Popcount4` counting its way through all sixteen inputs:

.. vcd:: trace.vcd

    label Input
    signal top.x:dec

    label Pair Counts
    signal top.lower_pair.count:dec
    signal top.upper_pair.count:dec
    space

    label Count
    signal top.count:dec

Follow along with a couple of inputs and watch the tree work.
When `x` is `5` (which is `0b0101`),
both pair counts are `1`,
and the final count is `2`.

And when `x` reaches `15` (all bits set!),
both pair counts are `2`,
and the widening add carries them up to the final answer of `4`.
