# tests/doc — virdoc tooling tests

Runs `vir doc` on the small project in `src/` and asserts the generated
HTML: that `build/doc/index.html`, the package page, and the module page
exist, that the package-level docstring (`//!`) and item-level docstrings
(`//>`) appear in the generated pages, that a builtin item page carries
its docstring, that the module page lists its submodules, and that the
full module-hierarchy page (`build/doc/hierarchy.html`, rooted at the
`[prog] top` module) is generated and linked from the project index.

Platform doc-page generation is not implemented in the renderer (it drops
platform items), so the test does not assert on platform pages yet. Once
fixed, grep build/doc for a platform page here.
