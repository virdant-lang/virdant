# platforms/platform_noinitial

Error test for the `@noinitial` platform annotation: a module bound
(`for`) to a `@noinitial` platform, and every module reachable from it
through the module hierarchy, may not contain `initial` statements.
A module not reachable from such a top keeps its initials.

Runs `vir check` and filechecks `src/top.vir` against the asserted
diagnostics.
