# visibility

`pub` surface errors — what may reference non-published items:

- `pub_leak.vir` — a non-pub item leaking through a published surface.
- `pub_leak_surfaces.vir` — leaks across the various surface kinds
  (ports, types, submodule definitions).
- `pub_for_notpub.vir` — a `for`-clause module referencing a non-pub item.
- `pub_notpub/` — project-style test: referencing a module that exists but
  is not `pub` produces a ReferencedWhileNotPub warning (exactly once,
  before the use-site errors). Its Makefile previously pointed filecheck
  outside the repository; that was fixed when the suite was restructured.
