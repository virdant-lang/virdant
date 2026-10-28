# platforms/platform_default

Project-style error test for platform-`default` rules. Three cases:

1. `default` on an outgoing port of a plain mod definition is a semantic
   error (the grammar accepts it; the analyser rejects it).
2. A platform `outgoing` port without a `default` must still be declared by
   the module (`PlatformMissingPort`).
3. A default expression whose type does not match the port's type is an
   error, reported at the platform's expression.

Runs `vir check` and filechecks `src/top.vir` against the asserted
diagnostics. This project was unwired before the restructure.
