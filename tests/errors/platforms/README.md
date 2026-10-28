# platforms

Platform-related errors, as project-style tests (each directory has a
`Virdant.toml`, `src/`, and its own Makefile):

- `platform_default/` — platform-default ports are synthesized; when the
  synthesis produces an unexpected error the test asserts it.
- `platform_mismatch/` — a module's `for <platform>` does not match the
  project's declared platform.
- `top_generics/` — the `[prog] top` module must be a concrete
  instantiation; a generic def has none, so `vir bitstream` must fail.

These projects only run `vir check` / `vir bitstream` — no hardware is
needed.
