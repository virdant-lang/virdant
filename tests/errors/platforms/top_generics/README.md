# platforms/top_generics

Project-style error test: the `[prog] top` module must be a concrete
instantiation — a generic def has none, so `vir bitstream` must fail. The
`pub mod`s themselves may be generic; only tops may not. This project was
explicitly listed in the old harness (`VIR_PROJS`) and remains wired in
under the automatic recursion.
