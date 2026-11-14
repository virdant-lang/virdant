# platform_outgoing_wire_reg

Positive test that a module bound with `for` may satisfy a platform's
plain `outgoing` ports with `outgoing wire` (combinational, with
`dependson`) or `outgoing reg` (clocked): the port match compares port
directions only, not the internal realization.

It also covers a platform `outgoing` port with a `default` clause being
realized by the module as an `outgoing reg` (an explicit declaration
overrides the platform default).

The negative counterpart — `outgoing wire`/`outgoing reg` inside a
platform *definition* being an error — is
`tests/errors/platforms/platform_wire_reg`.
