GCD
===
In this next example, we will build a state machine using a new kind of type called a union type.

`Gcd` is a module which computes the greatest common divisor of two numbers.
It uses the world's first algorithm: the Euclidean algorithm,
which works by repeatedly subtracting the smaller number from the larger one.

.. literalinclude:: src/gcd.vir
    :caption: gcd.vir
    :language: virdant

`Gcd` has an `incoming` port `clock`,
a `server` socket `inp` of `Pipe[GcdRequest]`,
and a `client` socket `out` of `Pipe[Word[8]]`.
Requests arrive on `inp`, and the answers leave on `out`.
We will see what all of this means below.

The Euclidean Algorithm
-----------------------
The Euclidean algorithm is one of the oldest algorithms we know of.
It computes the GCD of two numbers by subtracting the smaller from the larger,
over and over, until one of them reaches zero.
Whichever number is left over is the answer.

For example, here is the algorithm computing `gcd(48, 18)`:

.. code-block:: text

    (48, 18)
    (30, 18)
    (12, 18)
    ( 6, 12)
    ( 6,  6)
    ( 0,  6)   <-- the answer is 6

Each step, we subtract the smaller number from the larger one.
Since subtraction is all we need,
this is a natural fit for hardware, where subtraction is much cheaper than division.

Union Types
-----------
Back in the :doc:`Shifter example <../shifter/index>`, we defined an `enum type`
so that we could refer to a shift direction symbolically.
An `enum type` gives us a fixed set of values, with no data attached.

A `union type` is the more general version of the same idea.
Each alternative, called a *variant*, may carry data of its own:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 1-5
    :dedent:

Our `GcdState` has three variants:

- `Idle`, carrying no data. The circuit is waiting for something to do.
- `Running`, carrying the two operands `x` and `y`. The algorithm is in progress.
- `Done`, carrying the final `result`. The answer is ready.

To build a value of a union type, we write the variant name prefixed with `@`,
like `@Idle()` or `@Done(result)`.

.. note::

    Variant names are always written with the `@` prefix,
    both when constructing a value and when matching against one.


Requests and Pipes
------------------
A computation needs two operands,
so before we look at the module itself, we need a way to package them up.
For that, we use a `struct type`,
which bundles several fields together into a single value:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 7-10
    :dedent:

A `GcdRequest` carries an `x` and a `y`.
We can read the fields back out with the `->` operator,
as in `request->x`.

The operands travel into the module through a *socket*.
Virdant's builtin `Pipe[T]` socket is a ready/valid channel:
one side carries a `Valid[T]` payload called `data`,
and the other side carries a `ready` bit.

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 15-16
    :dedent:

`Gcd` declares two sockets.
The `inp` socket is a `server` socket of `Pipe[GcdRequest]`:
the module *receives* requests on `inp.data`
and *drives* `inp.ready` to say when it can accept one.
The `out` socket is a `client` socket of `Pipe[Word[8]]`:
the module *drives* `out.data` with the answer
and *receives* `out.ready` to learn when the answer has been taken.

.. note::

    We saw the `Valid` type in the :doc:`Shift Register example <../shiftreg/index>`.
    A value of type `Valid[T]` is either `@Valid(v)` for some value `v` of type `T`,
    or `@Invalid()`.


The State Register
------------------
The entire state of our state machine lives in a single register:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 18-20
    :dedent:

Notice that the type of `state` is `GcdState`, our union type.
A register can hold *any* type, not just `Bit`s and `Word`\s.

This is a big departure from how state machines are written in Verilog,
where you would declare a numeric state encoding
(like `localparam IDLE = 0;`) and manage the encoding yourself.
In Virdant, the variant *and* its payloads travel together in a single register.


The State Machine
-----------------
The heart of the module is one big `match` statement:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 23-61
    :dedent:

Remember from the :doc:`Counter example <../counter/index>`
that a `match` statement tests each of its patterns in turn.
Here, we `match` on `state`,
so each arm corresponds to one variant of `GcdState`.

Let's walk through each arm in turn.


`Idle`
^^^^^^
The `Idle` state is where we wait for work:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 24-33
    :dedent:

The `inp.data` socket carries a `Valid[GcdRequest]`,
which is itself a union type, so we `match` on it.
When a request arrives as `@Valid(request)`,
we bind the payload to the name `request`,
load the operands out of it with `request->x` and `request->y`,
and jump straight into the `Running` state.
When it is `@Invalid()`, there is nothing to do, so we stay put.

Either way, we drive the two sockets:
`inp.ready := true` invites the outside world to send a request,
and `out.data := @Invalid()` holds the result pipe empty.


`Running`
^^^^^^^^^
The `Running` state is where the actual work happens:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 34-51
    :dedent:

First, look at the pattern itself: `with @Running(x, y)`.
When `state` matches the `Running` variant,
the payloads of the variant are bound to the names `x` and `y`.

.. note::

    Inside this arm, `x` and `y` refer to the *payloads* of the `Running` variant.
    This is the same pattern binding we saw in the
    :doc:`Shift Register example <../shiftreg/index>`.

Inside the arm, we check the conditions in turn:

- If `y` is `0`, then `x` is the answer, and we move to `@Done(x)`.
- Otherwise, if `x` is `0`, then `y` is the answer, and we move to `@Done(y)`.
- Otherwise, if `x < y`, we subtract: the new state is `@Running(y - x, x)`.
- Otherwise, we subtract the other way: `@Running(x - y, y)`.

Notice that in both subtraction cases, the *new* pair is
(the difference, the smaller of the two operands).
That is exactly one step of the Euclidean algorithm from the table above.

While we are busy computing, the sockets stay quiet:
`inp.ready := false` refuses new requests,
and `out.data := @Invalid()` keeps the result pipe empty.


`Done`
^^^^^^
Finally, the `Done` state is where we announce the result:

.. literalinclude:: src/gcd.vir
    :language: virdant
    :lines: 52-60
    :dedent:

There are two drivers here.
The first is a latched driver (`<=`):
when `out.ready` is high, the consumer has taken the result,
and we send the state machine back to `Idle`.
The second is a continuous driver (`:=`) driving `out.data` with `@Valid(result)`,
where `result` is the payload carried by the `Done` variant.
As long as the state machine is in `Done`, the answer sits on the result pipe;
as soon as it leaves, `out.data` drops back to `@Invalid()`.

The Handshake
-------------
So when does a request actually get accepted,
and when does a result actually get delivered?

Both sockets follow the same rule:
a transfer happens on a cycle where the `data` side is `@Valid`
and the `ready` side is high.
Neither side acts alone:
a `@Valid` payload on `inp.data` is not enough if `inp.ready` is low,
and a high `out.ready` means nothing
without a `@Valid` payload on `out.data`.

We can see both sides of this bargain in the module.
On the request side, `Gcd` raises `inp.ready` only while it sits in `@Idle`.
On the result side, `Gcd` puts `@Valid(result)` on `out.data` only in `@Done`,
and leaves it there until the consumer raises `out.ready`.

Together, the two sockets form a simple **handshake**.
The outside world sends a request on `inp`,
and some cycles later, the module answers on `out`.

.. tip::

    Handshakes like this are everywhere in hardware design.
    Whenever one module needs to talk to another which runs at its own pace,
    a ready/valid pair is how they keep in step.


Waveform
--------
Here is the `Gcd` unit computing `gcd(21, 14) = 7`:

.. vcd:: trace.vcd

    label Clock
    signal top.clock:clk
    label State
    signal top.state:gcd::GcdState
    label Request pipe
    signal top.inp.data:Valid[gcd::GcdRequest]
    signal top.inp.ready
    label Result pipe
    signal top.out.data:Valid[Word[8]]
    signal top.out.ready

Watch `top.state` go `Idle` -> `Running` -> ... -> `Done` -> `Idle`.
While in `Running`, you can read the two operands right off the state:
they are subtracted from each other cycle by cycle
until one of them reaches zero.
That is the whole Euclidean algorithm, playing out in the waveform.

Notice two more things.
First, `top.inp.ready` is high only while the state machine sits in `Idle`,
and that is exactly when the `@Valid` request on `top.inp.data` gets accepted.
Second, `top.out.data` carries `@Valid(7)` only while the state machine passes
through `Done`, and drops back to `@Invalid()` as soon as it returns to `Idle`,
ready for the next computation.
