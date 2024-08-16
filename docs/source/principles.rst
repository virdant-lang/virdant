Principles
==========
There are few principles Virdant adheres to in its general design.


Craftsmanship
-------------
Tools reflect the values of those who weild them.
Virdant was designed with the digital designer and the verification engineer in mind.
Attention has been paid to every detail.
And as a result, Virdant is a pleasure to work with.


Simplicity
----------
Virdant is simple and easy to learn.
It has a clean syntax and excellent developer tooling.


Strongly Typed
--------------
Mistakes in hardware can be expensive.
While Virdant is primarily aimed at hobbyist FPGA designs,
it still detracts from the experience whenever a preventable error occurs.

Virdant's strong type systems is designed so that if your circuit synthesizes, it works.

Under normal operation, the expression sublanguage has many nice properties:

* Operations must always be totally defined.
* All conditional expressions are required to have full case coverage.
* You are prohibited from observing the unset bits in a value.
* Virdant's equivalent of Verilog's `X` values are handled in a principled way.

* No components may be left uninitialized.
* All nets must have a single driver.


Digital
-------
Virdant is oriented towards digital circuits.
It does not support latches or tristate buffers.
It has no support for analog hardware.


Synthesizable
-------------
Virdant has a clear delineation in terms of what is synthesizable and what is not.
The translation to actual hardware is always reasonable.


Fun
---
Virdant is a fun language to write in.
Otherwise, what's the point?
