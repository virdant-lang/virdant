Annotations
===========
Annotations are metadata tags that may be attached to declarations.
They are used to attach extra information to parts of a design,
such as pin assignments, synthesis attributes, and documentation hints.

Annotations are not part of Virdant's semantic model --- they are preserved
in the AST and may be inspected by tools, backends, and documentation
generators.


Syntax
------
An annotation starts with :vir:`@` followed by a name, optionally
followed by a parenthesized argument list.

.. code-block:: grammar

    Annotation :=
          "@" Ident "(" ArgList? ")"
        | "@" Ident

    ArgList := Expr ("," Expr)* ","?

The argument list may be empty, and each argument is a literal:

* :vir:`@name` or :vir:`@name()` --- a bare annotation with no arguments.
  Example: :vir:`@keep`, :vir:`@keep()`

* :vir:`@name(123)` --- an annotation with a natural number argument.
  Example: :vir:`@pin(35)`

* :vir:`@name("string")` --- an annotation with a string argument.
  Example: :vir:`@pin("E3")`

* :vir:`@name(true)` --- an annotation with a boolean argument
  (:vir:`true` or :vir:`false`).
  Example: :vir:`@keep(true)`

* :vir:`@name(a, b, ...)` --- an annotation with multiple
  comma-separated arguments.
  Example: :vir:`@parameter("WIDTH", 8)`
  (see :ref:`parameter-annotations` below)

Multiple annotations may be stacked on a single declaration:

.. code-block:: virdant

    @fpga("ice40")
    @part("up5k-sg48")
    mod Top {
        // ...
    }


Where Annotations Appear
-------------------------
Annotations may appear after the documentation comment (if any) and before
the keyword of any annotatable declaration.

Annotatable declarations include:

* Module definitions
* Platform definitions and their ports (see :doc:`platforms`)
* Struct type definitions and their fields
* Union type definitions and their variants (with params)
* Enum type definitions and their variants
* Builtin type definitions
* Socket definitions and their channels (:vir:`cosi` / :vir:`soci`)
* Component declarations (:vir:`incoming`, :vir:`outgoing`, :vir:`wire`, :vir:`reg`)
* Module instances (:vir:`mod` ... :vir:`of` ...)
* Socket instances (:vir:`client` :vir:`socket` / :vir:`server` :vir:`socket`)
* Struct type fields
* Union variant parameters (:vir:`Param`)

The grammar shows :vir:`Annotations` as a non-terminal in all of these productions:

.. code-block:: grammar

    Annotations := Annotation*

    ModDef := DocString Annotations "pub"? "ext"? "mod" Ident "{" ...
    PlatformDef := DocString Annotations "pub"? "platform" Ident "{" ...
    StructDef := DocString Annotations "pub"? "struct" "type" Ident "{" ...
    StructDefStmt := DocString Annotations Ident ":" Type
    UnionDef := DocString Annotations "pub"? "union" "type" Ident "{" ...
    UnionDefStmt := DocString Annotations Ident ParamList?
    EnumDef := DocString Annotations "pub"? "enum" "type" Ident "{" ...
    EnumDefStmt := DocString Annotations Ident "=" Expr
    BuiltinDef := DocString Annotations "pub"? "builtin" "type" Ident ...
    SocketDef := DocString Annotations "pub"? "socket" Ident "{" ...
    SocketDefStmt := DocString Annotations ("cosi"|"soci") Ident ":" Type
    Param := DocString Annotations Ident ":" Type

    ModDefStmtComponent :=
        DocString Annotations ("incoming"|"outgoing"|"wire"|"reg") ...
    ModDefStmtInstance :=
        DocString Annotations "mod" Ident "of" ...
    ModDefStmtSocket :=
        DocString Annotations ("client"|"server") "socket" ...


.. _parameter-annotations:

Parameter Annotations
---------------------

Most annotations are pure metadata, but the :vir:`@parameter` annotation
is interpreted by the Verilog backend: it passes Verilog module
parameters through to instances of :vir:`ext mod` (black-box) modules.
This is useful for width- or mode-configurable cells, such as vendor
library blocks whose data width or operating mode is selected by a
Verilog parameter.

Parameters are declared on the :vir:`ext mod` definition.  Each
:vir:`@parameter` annotation takes two arguments: the parameter name (a
string) and an optional default value:

.. code-block:: virdant

    @parameter("WIDTH", 8)
    @parameter("INIT", 0)
    @parameter("MODE", "SYNC")
    ext mod Cell {
        incoming clk : Clock
        outgoing data : Word[16]
    }

Any declared parameter may be overridden per instance by attaching
:vir:`@parameter` to the :vir:`mod` ... :vir:`of` ... statement.
Instance-supplied values take precedence over the declared defaults,
and parameter names supplied by an instance but not declared on the
target module are appended to the instance's parameter list as well.
Instances that supply no parameters use the declared defaults:

.. code-block:: virdant

    pub mod Top {
        incoming clk : Clock
        outgoing a : Word[16]
        outgoing b : Word[16]

        @parameter("WIDTH", 4)
        mod u0 of Cell {
            it.clk := clk
            a := it.data
        }

        mod u1 of Cell {
            it.clk := clk
            b := it.data
        }
    }

Argument values are rendered to Verilog as follows:

* Natural numbers render in decimal:
  :vir:`@parameter("WIDTH", 4)` produces ``.WIDTH(4)``.
* Strings render quoted:
  :vir:`@parameter("MODE", "SYNC")` produces ``.MODE("SYNC")``.
* Booleans render as single-bit Verilog literals:
  :vir:`@parameter("EXTRA", true)` produces ``.EXTRA(1'b1)``
  (:vir:`false` produces ``1'b0``).

The Verilog backend emits each instance with its merged parameter list
--- declared parameters first (instance override or declared default),
then instance-supplied extras --- as a Verilog parameter assignment.
For the example above, :vir:`u0` and :vir:`u1` are emitted as:

.. code-block:: verilog

    Cell #(
        .WIDTH(4),
        .INIT (0),
        .MODE ("SYNC"),
    ) u0(
        ...
    );

    Cell #(
        .WIDTH(8),
        .INIT (0),
        .MODE ("SYNC"),
    ) u1(
        ...
    );


Backends and tools may use these to extract metadata during code generation
or documentation processing.
