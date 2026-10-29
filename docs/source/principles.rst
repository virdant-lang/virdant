Principles
==========
There are a few principles Virdant adheres to in its general design.


Craftsmanship
-------------
Tools reflect the values of those who wield them.
Virdant was designed with the digital designer and the verification engineer in mind.
Attention has been paid to every detail.
And as a result, Virdant is a pleasure to work with.


Simplicity
----------
Virdant is simple and easy to learn.
It has a clean syntax and excellent developer tooling.


Strongly Typed
--------------
Mistakes in hardware can be expensive, hard to debug, and frustrating.
Virdant tells you when you're about to make mistakes
before you actually make them.

Strong typing allows you to think at a higher level than bits.
If those 7 bits represent an opcode, let's create a type called `Opcode`.

If a cache can receive one of several commands,
let's use make a type called `Command` which can be
`@Get(addr)` `Put(addr, data)` `@Hint(addr)` `Ack()`.


Synthesizable
-------------
Virdant has a clear delineation in terms of what is synthesizable and what is not.
The translation to actual hardware is always reasonable.
There are no constructs for verifying or generating hardware in the core language.

Both of these things are important, but there are many ways to do them.
Instead of choosing the one that is right for me,
you get to choose the one that is right for you.

Virdant is simple enough that testbenches and generators
can be built on top of the langauge instead of into it.


Easy
----
Virdant is easy to get started with,
even if you've never touched a piece of hardware
and even if you don't know what an :tip:`FPGA (Field-Programmable Gate Array)` is.

All you need is `vir` on your command line,
and you can get started with the builtin simulator.
From here, you can get all the way up to a working RISC-V core!

With the Virdant language server, you can learn from your mistakes as you type them.
Virdant's incredibly helpful error messages will never leave you scratching your head.

Once you're ready to graduate,
Virdant works seemlessly with any synthesis tool,
including the source tools, such as Yosys, Icarus Verilog, and Verilator.


Fun
---
Virdant is a fun language to write in.
Otherwise, what's the point?
