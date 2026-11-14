# platforms/platform_wire_reg

Negative test: `outgoing wire` and `outgoing reg` are errors in a
platform definition (`PlatformWireRegPort`).  A platform describes the
board's pins, not the implementing module's internal realization, so its
ports must be declared plain `outgoing`.

The module bound with `for` mirrors the platform's wire/reg
declarations, showing that mirroring them does not add port-match errors
(`PlatformExtraPort`/`PlatformMissingPort`) on top of the platform's own
errors.

A module bound by `for` may legitimately realize a platform's plain
`outgoing` port as `outgoing wire` or `outgoing reg`; that positive case
is covered by `tests/check/projects/platform_outgoing_wire_reg`.
