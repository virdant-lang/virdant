Hello
=====
This is the design you get "for free" when you start a new Virdant project.
When you run `vir new hello`, the freshly-created `hello/src/top.vir` looks
exactly like this:

.. literalinclude:: src/top.vir
    :language: virdant

This is on purpose.
A brand-new project should not be an empty file staring back at you.
It should be a small, complete program which already works,
so that your very first commands --- `vir check`, `vir run` --- exercise
the whole toolchain on something real.
Once you have seen it print, delete it and start writing your own `Top`.

The `virt::Virt` Platform
-------------------------
The first line imports the `virt` package,
and `Top` binds itself to its `Virt` platform with a `for` clause:

.. code-block:: virdant

    import virt

    pub mod Top for virt::Virt {

`Virt` is not a physical board.
It is a *virtual* FPGA: a module bound `for virt::Virt` runs under the
Virdant simulator, which emulates the board's peripherals for you.

Binding a module to a platform is a contract:
`Top` must declare exactly the platform's ports (or omit the ones the
platform gives a `default` to).
That is why `Top` only mentions three of `Virt`'s ports ---
`clock`, `exit`, and `debug` --- and nothing else.
The simulator fills in the rest.

The `incoming clock : Clock` port is the board's clock source.
`Top` uses it to drive a register.

Printing with `debug`
---------------------
The `Virt` platform's `debug` port is a simple printing service:
every cycle, if you drive it with `@Valid(ch)`,
the simulator prints the byte `ch` to the terminal.

So to print a message, `Top` just needs to walk through the bytes
of a string, one per cycle.
The string lives in a memory, initialized directly from the literal
in the source:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 13-13
    :dedent:

The `@image_inline` attribute says the bytes of the string *are* the
memory's initial image --- no separate file needed.

A register keeps track of where we are in the string.
It starts at `0` and counts up by one on every clock tick:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 16-19
    :dedent:

Each cycle, the `debug` port picks out the byte at the current index
and wraps it in `@Valid`, asking the simulator to print it:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 21-23
    :dedent:

Run this with `vir run` and the memory walks itself:
`H`, `e`, `l`, `l`, `o`, ... one character per cycle,
until all of `Hello, Virdant!` has streamed past.

Exiting with `exit`
-------------------
The `exit` port tells the simulator when to stop.
While it reads `@Invalid()`, the simulation keeps running.
When it reads `@Valid(n)`, the simulator exits with code `n`.

`Top` watches the memory for the `NUL` byte (`0`) that terminates the
string:

.. literalinclude:: src/top.vir
    :language: virdant
    :lines: 6-11
    :dedent:

As long as `mem[dyn index]` is a character of the message,
`exit` stays `@Invalid()` and the simulation keeps going.
Once the counter lands on the `NUL` --- after the whole message has
been printed --- `exit` becomes `@Valid(0)` and the simulator shuts
down cleanly with exit code 0.

This is the shape of nearly every program bound to `Virt`:
*do something each cycle, and decide each cycle whether you are done.*
