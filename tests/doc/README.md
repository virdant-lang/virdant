# tests/doc — virdoc tooling tests

Runs `vir doc` on the small project in `src/` and asserts the generated
HTML: that `build/doc/index.html`, the package page, and the module page
exist, that the package-level docstring (`//!`) and item-level docstrings
(`//>`) appear in the generated pages, and that a builtin item page carries
its docstring. It was `tests/virdoc/` before the restructure (the rename
groups tooling suites together; `virdoc` is a tooling test, not a language
test).

Platform doc-page generation is not implemented in the renderer (it drops
platform items), so the Makefile does not assert on platform pages yet —
there is a TODO in the Makefile.
