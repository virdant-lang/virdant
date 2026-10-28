# platforms/platform_default

Project-style error test for platform-`default` rules: where a `default`
on a port is legal, when platform outgoing ports must still be declared by
the module, and the type of a default expression against its port's type.

Runs `vir check` and filechecks `src/top.vir` against the asserted
diagnostics. Note: this project was unwired before the restructure and is
now wired in via the suite's automatic recursion.
