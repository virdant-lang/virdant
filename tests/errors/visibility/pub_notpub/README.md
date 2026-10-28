# visibility/pub_notpub

Project-style error test spanning two packages: `src/lib.vir` is a library
package with private and public items, and `src/top.vir` references a
non-`pub` item from it. Referencing a non-pub item from another package
errors at the reference site (the import itself never visibility-errors),
and a warning is emitted at the *definition* site naming the referencing
package.

The FileCheck directives assert the ReferencedWhileNotPub warning appears
exactly once, ordered before the use-site errors. This project's Makefile
previously resolved `BASEDIR` one directory too far up, pointing filecheck
outside the repository, so it failed when run standalone and was never
wired into the suite. The BASEDIR was fixed (for the new, deeper location)
and the project is now wired in via the suite's automatic recursion.
