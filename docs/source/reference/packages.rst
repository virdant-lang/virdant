Packages
========
A Virdant design may be split across multiple files.
Each file is a Virdant package.
Packages allow you to organize your design into logical units and reuse
declarations across files.

.. code-block:: grammar

    Package :=
        DocBang PackageStmt*

    PackageStmt :=
        PackageImport
        | Item

A package consists of an optional package-level documentation comment
(`//!`, called `DocBang` in the grammar), followed by zero or more
import statements and item declarations.


Package Structure
-----------------
Each Virdant source file corresponds to one package.
The package name is derived from the filename:

* `buffer.vir` defines the `buffer` package
* `top.vir` defines the `top` package
* `uart/` would define the `uart` package

The file extension is `.vir`.
Filenames should begin with a lowercase letter and use snake_case for
multi-word names.


Import Statements
-----------------
An import statement makes declarations from another package visible in the
current package.

.. code-block:: grammar

    PackageImport :=
        "import" Ident

.. code-block:: virdant

    /// top.vir
    import buffer

    mod Top {
        incoming clock : Clock
        incoming inp : Word[8]
        outgoing out : Word[8]

        mod buf of buffer::Buffer {
            it.clock := clock
            it.inp := inp
        }
        out := buf.out
    }

The `import` keyword is followed by the package name (without the `.vir`
extension).
All items exported by the imported package become available through their
fully-qualified names.


Fully-Qualified Names
---------------------
Declarations from imported packages are accessed using fully-qualified names
with the `::` separator.

.. code-block:: grammar

    Ofness :=
        Ident | Ident "::" Ident

The first component is the package name, and the second is the declaration
name within that package.

.. code-block:: virdant

    import buffer

    mod buf of buffer::Buffer

Here, `buffer::Buffer` refers to the `Buffer` module defined in the
`buffer` package.

Fully-qualified names are used in type positions (after `of`, in `: Type`)
and in function call positions.

Within the same package, declarations may be referred to by their simple
name without the `::` prefix.


Exporting Declarations
----------------------
Only items marked with the `export` modifier are visible to other packages.

.. code-block:: virdant

    /// buffer.vir
    export mod Buffer {
        incoming clock : Clock
        incoming inp : Word[8]
        outgoing out : Word[8]

        reg r : Word[8] on clock
        r <= inp
        out := r
    }

    // Not exported: only visible within buffer.vir
    mod InternalHelper {
        // ...
    }

An item without the `export` keyword is private to the package.
Private items cannot be imported or referenced from other packages.

The `export` modifier is available on:

* Module definitions (`mod`)
* Struct type definitions (`struct type`)
* Union type definitions (`union type`)
* Enum type definitions (`enum type`)
* Socket definitions (`socket`)
* Function definitions (`fn`)


Package Dependency
------------------
When one package imports another, the compiler must be able to find the
imported package's source file.
The compiler searches for imported packages in the same directory as the
importing file and in any directories specified as library search paths.

Circular imports are not permitted.
If package A imports package B, then B may not import A (directly or
transitively).


Package-Level Documentation
---------------------------
A package may have a package-level documentation comment (`//!`) at the top of the file.
This comment serves as package-level documentation.

.. code-block:: virdant

    //! This package contains the UART transmitter and receiver modules.

    import clockgen

    mod UartTx {
        // ...
    }