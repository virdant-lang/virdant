# platforms/drive_undeclared_port

Project-style error test for the declaration rule on platform ports
synthesized from `default <EXPR>` clauses: a module may leave a defaulted
platform port untouched, and may declare and drive it itself, but an
explicit driver on an undeclared platform port is an error
(`DriveUndeclaredPlatformPort`).

Runs `vir check` and filechecks `src/top.vir` against the asserted
diagnostics.
