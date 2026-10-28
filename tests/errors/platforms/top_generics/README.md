# platforms/top_generics

Project-style error test: the `[prog] top` module must be a concrete
instantiation — generic `pub mod`s are fine, but a generic top makes
`vir bitstream` fail.

Note: this project was explicitly listed in the old harness (`VIR_PROJS`)
and remains wired in under the suite's automatic recursion.
