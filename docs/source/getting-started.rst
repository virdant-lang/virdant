Getting Started
===============

Tradition dictates that the first program that is written in any programming language should be Hello, World!
In that spirit, in hardware languages, we write a design which blinks an LED on and off.
So let's make an LED blink by writing the "hello world" of Virdant:

.. literalinclude:: examples/blink.vir
   :caption: blink.vir
   :language: virdant
   :linenos:

We see a module declaration, `mod Top`.
While programming languages typically name the entrypoint to a program "main",
in hardware, it is conventional to call the root of the design the "top".
And thus, we choose the name `Top` for our top module.

Inside, we see two ports, one `incoming` port named `clock` and one `outgoing` port named `led`.
The first port, `clock` has type `Clock`, and so it gives the circuit its pulse.
It's purpose is to tell the `reg`\s in the module when they should latch the next value.
The second port, `led`, will represent the state of our LED.
It has type `Bit` and so is represented with a single bit.
While it is `true`, the LED will be on, and when it's `false`, the LED will be off.

The next line declares a `reg` called `led_on` with type `Bit`.
A `reg` is a hardware register.
It is a memory cell in our design.

Every `reg` must be associated with a clock.
Our `led_on` register names `clock` as its clock.
This means that on the rising edge of `clock`, the `led_on` will assume a new value.

The next line after that is a driver statement which tells us which value `led_on` will take.
This will be the result of the expression `len_on->not()`, the logical NOT of the current value of `led_on`.
In other words, on the rising edge of the clock, `led_on` will flip from `true` to `false` or from `false` to `true`.

The last line of the module, `led := led_on` is another driver statement.
It drives `led` to have the same value as the current value of `led_on`.

The difference between `:=` and `<=` is that `:=` is a continuous driver, whereas `<=` is clocked.
We only ever use `<=` with registers.
