# visibility/pub_notpub

Project-style error test spanning two packages: a library package with
private and public items, and a top that references a non-`pub` item from
it. Referencing a non-pub item from another package errors at the reference
site (the import itself never visibility-errors), and a
`ReferencedWhileNotPub` warning is emitted at the definition site naming
the referencing package; the FileCheck directives assert the warning
appears exactly once, ordered before the use-site errors.

Note: the project's harness previously resolved the repo root one directory
too far up, pointing filecheck outside the repository. It was fixed for the
new, deeper location; the runner's automatic discovery now wires the
project in.
