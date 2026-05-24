Grammar
=======
This chapter describes the lexical elements of Virdant.
These are the low-level building blocks from which all Virdant programs are constructed.


Tokenization
------------
Virdant source files must be ASCII encoded, except for comments.


Whitespace
----------
Whitespace in Virdant consists of spaces and newlines.
Whitespace is used to separate tokens but is otherwise insignificant.

Virdant uses curly braces `{` and `}` to delimit blocks.
Indentation is not semantically meaningful.
Tabs are illegal characters.
Use of 4 spaces for indentation is highly encouraged.


Comments
--------
Virdant supports Line Comments and Docstrings

Line Comments
-------------
A line comment begins with `//` and extends to the end of the line.
Line comments are ignored by the compiler and do not appear in generated documentation.

.. code-block:: virdant

    // This is a line comment

There are no multi-line comments.


Docstrings
----------
Docstrings are comments that the compiler associates with declarations.
They are used to generate documentation for modules, types, sockets, and functions.

A documentation comment begins with `///` and extends to the end of the line.
Multiple consecutive `///` lines are combined into a single documentation string.

.. code-block:: virdant

    /// A simple passthrough module.
    /// Takes an 8-bit value and immediately sends it back out.
    mod Passthrough {
        incoming inp : Word[8]
        outgoing out : Word[8]

        out := inp
    }

Documentation comments may appear before any top-level declaration:
module definitions, struct types, union types, enum types, socket definitions,
and function definitions.

They may also appear on module body statements such as component declarations,
instance declarations, and port declarations.

The compiler collects these comments and makes them available to documentation
generation tools like Sphinx.


Package Docstrings
------------------
Package-level documentation begins with `//!` and extends to the end of the line.
This form of documentation comment is placed at the top of a package file
and describes the purpose of the entire package.

.. code-block:: virdant

    //! This package contains the UART transmitter and receiver modules.

    mod UartTx {
        // ...
    }

Multiple consecutive `//!` lines are combined into a single package-level
documentation string.


Identifiers
-----------
Identifiers in Virdant name things: modules, types, ports, wires, registers,
instances, sockets, type variants, and functions.

Identifiers must begin with an uppercase or lowercase letter (`A-Z` or `a-z`)
or an underscore (`_`).
The remainder may contain letters, digits (`0-9`), and underscores.

.. code-block:: grammar

    ident: (a-z | A-Z | "_") (a-z | A-Z | 0-9 | "_")*

The following are valid identifiers:

* `counter`
* `Clock`
* `_internal`
* ``RgbColor``
* `opcode3`

Identifiers are case-sensitive: `myReg` and `myreg` are different names.


Keywords
--------
The following words are reserved as keywords and may not be used as identifiers:

.. code-block::

    builtin  case     soci     client   cosi     dontcare   dyn
    else     enum     export   ext      false    fn
    if       import   incoming  it      match     mod
    of       on       outgoing  reg     server    socket
    struct   true     type     union    unused    width
    wire


Literals
--------
Virdant provides several kinds of literal values.


Bit Literals
~~~~~~~~~~~~
Bit literals represent the two possible values of the `Bit` type:

.. productionlist::
    bit-lit: "true" | "false"

.. code-block:: virdant

    wire a : Bit
    a := true

    wire b : Bit
    b := false


Integer Literals
~~~~~~~~~~~~~~~~
Integer literals represent numeric constants of type `Word[n]`.
The width `n` is specified by appending `w` followed by the bit width:

.. productionlist::
    word-lit: `Nat` ( "w" `Nat` )?
    nat: `digit`+

A literal without an explicit width requires type inference from context.
When the width cannot be inferred, it is a compile-time error.

.. code-block:: virdant

    wire explicit : Word[4]
    explicit := 0w4            // 4-bit value 0

    wire inferred : Word[4]
    inferred := 0              // Width inferred as 4

Integer literals may be written in several bases:

Decimal
    `42w8`, `100_000`

Binary
    `0b1010`, `0b1010w4`, `0b00_101_11`

Hexadecimal
    `0xfe`, `0xfew8`, `0xcafe_babe`

Underscores may be used as visual separators in integer literals.
They are ignored by the compiler.

.. warning::

    A literal written without the `w` annotation requires a surrounding context
    that determines the type.
    For example, if assigned to a `Word[8]` component, the literal ``42``
    is inferred as `42w8`.

    If the context cannot determine the width, the compiler will report an error.


Dontcare
--------
The `dontcare` keyword represents an unknown or undefined value.
It is used when a signal's value is irrelevant in a given context.

.. code-block:: virdant

    wire bit_x : Bit
    bit_x := dontcare

    wire word_x : Word[8]
    word_x := dontcare

Assigning `dontcare` to a component tells the compiler and synthesis tools
that the value may be anything.
This can lead to smaller, more efficient hardware because the tools are free
to choose the most convenient value for the don't-care condition.


Hole Expressions
----------------
Holes are expressions which act as a placeholder for incomplete Virdant designs.
They are functionally equivalent to `dontcare`,
but they produce a warning which acts as a signal to the developer
that they need to fill in the actual value later.

In Virdant, holes are written as `?`.

.. code-block:: virdant

    reg state : Word[8] on clock {
        it <= ?
    }

Holes typecheck as whatever type they are needed.
