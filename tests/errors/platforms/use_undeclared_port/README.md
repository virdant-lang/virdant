# platforms/use_undeclared_port

Project-style error test for the declaration rule on platform ports
*used* by a module: a module bound to a platform with a `for` clause
may leave platform ports undeclared only by leaving them untouched.
Any use of an undeclared platform port -- reading it as a value, or
clocking a reg with it via `on <CLOCK>` -- is an error
(`UseUndeclaredPlatformPort`); declaring the port makes the use legal.

Runs `vir check` and filechecks `src/top.vir` against the asserted
diagnostics.
