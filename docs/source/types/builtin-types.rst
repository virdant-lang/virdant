Builtin Types
=============
Virdant includes the following built-in types:

`Clock`
-------
A `Clock` is a clock.
It represents a 1-bit signal that oscillates at a fixed frequency in standard operation.

Every `reg` statement must have a clock associated with it.
The register will latch its next value on the rising edge of that clock.

`Clock` is an opaque type.
There are no safe operations which can manipulate or inspect values of type `Clock`.
All you are allowed to do with clocks is pass them around through ports or to associate them to `reg`\s using the `on` keyword.


`Bit`
-----
A `Bit` is a 1-bit value.


`Word[n]`
---------
A `Word[n]` is an n-bit value.

When used as a number, it is interpreted to be unsigned integer in 2's complement.

Note that `Word[1]` and `Bit` are distinct types.
You can cast between them with `w[0]` (to go from `Word[1]` to `Bit`) or `cat(b)` (to go from `Bit` to `Word[1]`).

Finally, `Word[0]` is a legal type.
It has zero-width and requires special handling when compiling to Verilog.
The only value it can hold is `0`.
