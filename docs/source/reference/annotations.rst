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
An annotation starts with :vir:`@` followed by a name and an optional value.

.. code-block:: grammar

    Annotation :=
          "@" Ident "(" Nat ")"
        | "@" Ident "(" Str ")"
        | "@" Ident

Three forms are supported:

* :vir:`@name` --- a bare annotation with no value.
  Example: :vir:`@keep`

* :vir:`@name(123)` --- an annotation with a natural number value.
  Example: :vir:`@pin(35)`

* :vir:`@name("string")` --- an annotation with a string value.
  Example: :vir:`@pin("E3")`

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

    ModDef := DocString Annotations "ext"? "export"? "mod" Ident "{" ...
    StructDef := DocString Annotations "struct" "type" Ident "{" ...
    StructDefStmt := DocString Annotations Ident ":" Type
    UnionDef := DocString Annotations "union" "type" Ident "{" ...
    UnionDefStmt := DocString Annotations Ident ParamList?
    EnumDef := DocString Annotations "enum" "type" Ident "{" ...
    EnumDefStmt := DocString Annotations Ident "=" Expr
    BuiltinDef := DocString Annotations "builtin" "type" Ident ...
    SocketDef := DocString Annotations "socket" Ident "{" ...
    SocketDefStmt := DocString Annotations ("cosi"|"soci") Ident ":" Type
    Param := DocString Annotations Ident ":" Type

    ModDefStmtComponent :=
        DocString Annotations ("incoming"|"outgoing"|"wire"|"reg") ...
    ModDefStmtInstance :=
        DocString Annotations "mod" Ident "of" ...
    ModDefStmtSocket :=
        DocString Annotations ("client"|"server") "socket" ...


Backends and tools may use these to extract metadata during code generation
or documentation processing.
