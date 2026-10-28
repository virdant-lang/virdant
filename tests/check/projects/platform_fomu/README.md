# projects/platform_fomu

Placeholder for a Fomu-target project. It has a `Virdant.toml` but no
`src/` directory, so `vir check` would fail on the missing source; the
Makefile therefore SKIPs with a notice instead of failing. Wire in a real
Fomu project here when one exists.
