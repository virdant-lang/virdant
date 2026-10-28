# sim/gcd

Behavioral test of a GCD computed by a union-typed state machine
(`src/top.vir` defines a union `State` with `Idle()` and a running state).
The testbench sweeps operand values through the generated Verilog and
asserts each computed result. The project's `Virdant.toml` was previously
misnamed (`temp`); it is now `gcd`.
