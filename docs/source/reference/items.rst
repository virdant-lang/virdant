Items
=====
An item is a top-level declaration in a Virdant package.
Items include module definitions, type definitions, socket definitions,
and function definitions.

.. code-block:: grammar

    Item :=
        ModDef
        | StructDef
        | UnionDef
        | EnumDef
        | BuiltinDef
        | SocketDef
        | FnDef

Each item may be preceded by a documentation comment (`///`).
Items may be exported from a package with the `export` keyword.


Module Definitions
------------------
A module definition declares a reusable hardware block.
Modules are the primary building block of Virdant designs.

.. code-block:: grammar

    ModDef :=
        DocString "ext"? "export"? "mod" Ident "{" ModDefStmt* "}"

A module definition begins with the `mod` keyword, followed by the module name,
and a body enclosed in curly braces.

.. code-block:: virdant

    mod Passthrough {
        incoming inp : Word[8]
        outgoing out : Word[8]

        out := inp
    }

The module body contains ports, components, instances, and statements.
For a detailed description of module bodies, see the :doc:`modules` chapter.

The optional `ext` modifier declares an external module --- one whose
implementation is provided by the host language (Verilog).

.. code-block:: virdant

    ext mod LedDriver {
        incoming clock : Clock
        incoming inp : Bit
        outgoing out : Bit
    }

External modules may only declare ports.
The Virdant compiler generates a stub that connects to the corresponding
Verilog implementation.

The optional `export` modifier makes the module visible to other packages
that import this package.

.. code-block:: virdant

    export mod Top {
        // ...
    }


Struct Type Definitions
-----------------------
A struct type definition declares a new nominal product type with named fields.

.. code-block:: grammar

    StructDef :=
        DocString "struct" "type" Ident "{" StructDefStmt* "}"

    StructDefStmt :=
        DocString Ident ":" Type

.. code-block:: virdant

    struct type Color {
        red   : Word[8]
        green : Word[8]
        blue  : Word[8]
    }

Each field has a name and a type separated by `:`.
Fields may be preceded by documentation comments.

For more details on struct types, see :doc:`types`.


Union Type Definitions
----------------------
A union type definition declares a new sum type with a set of variants.

.. code-block:: grammar

    UnionDef :=
        DocString "union" "type" Ident "{" UnionDefStmt* "}"

    UnionDefStmt :=
        DocString Ident ParamList?

    ParamList :=
        "(" Params? ")"

    Params :=
        Param | Params "," Param

    Param :=
        DocString Ident ":" Type

.. code-block:: virdant

    union type MaybeWord {
        Nothing()
        Just(val : Word[8])
    }

Variants may carry associated data.
A variant with no data is written with empty parentheses.
Variants with data list typed parameters inside the parentheses.

For more details on union types, see :doc:`types`.


Enum Type Definitions
---------------------
An enum type definition declares a set of named constants with a fixed bit width.

.. code-block:: grammar

    EnumDef :=
        DocString "enum" "type" Ident "width" Width "{" EnumDefStmt* "}"

    EnumDefStmt :=
        DocString Ident "=" Expr

.. code-block:: virdant

    enum type State width 2 {
        Idle    = 0
        Active  = 1
        Done    = 2
        Error   = 3
    }

Each variant is assigned a constant expression matching the declared width.
The assigned values must be distinct within a single enum type.

For more details on enum types, see :doc:`types`.


Builtin Type Definitions
------------------------
A builtin type definition declares a type whose implementation is provided
by the compiler rather than by user code.

.. code-block:: grammar

    BuiltinDef :=
        DocString "builtin" "type" Ident Generics? "{" "}"

.. code-block:: virdant

    builtin type Clock {}
    builtin type Bit {}
    builtin type Word[n : Width] {}

Builtin types may have generic parameters.
The builtin types `Clock`, `Bit`, and `Word` are predefined in every
Virdant package.


Socket Definitions
------------------
A socket definition declares a reusable interface consisting of a group of
named ports with directions.

.. code-block:: grammar

    SocketDef :=
        DocString "socket" Ident "{" SocketDefStmt* "}"

    SocketDefStmt :=
        DocString "cosi" Ident ":" Type
        | DocString "ciso" Ident ":" Type

.. code-block:: virdant

    socket Mem {
        cosi addr : Word[16]
        ciso data : Word[8]
    }

`cosi` stands for "client-out, server-in".
`ciso` stands for "client-in, server-out".

For a client socket instance, `cosi` ports are outputs and `ciso` ports
are inputs.
For a server socket instance, the directions are reversed.

For more details on sockets, see :doc:`sockets`.


Function Definitions
--------------------
A function definition declares a combinational function that computes a value
from its arguments.

.. code-block:: grammar

    FnDef :=
        DocString "fn" Ident ParamList "->" Type "{" Expr "}"

.. code-block:: virdant

    fn add(x : Word[8], y : Word[8]) -> Word[8] {
        x + y
    }

Functions are pure: they compute a result based only on their inputs and have
no side effects.
Functions are instantiated as combinational logic at every call site.

For more details on functions, see :doc:`functions`.