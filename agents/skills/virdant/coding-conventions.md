Coding Conventions
==================
Virdant is a language we created so that hardware can be designed in a way that is lush and beautiful.

To help with this, we insist on adhering to certain coding conventions when writing Virdant.


Filenames
---------
Virdant files have the `.vir` extension and should begin with a lowercase letter.
For files with multiple words, we use snake case.


Spaces not Tabs
---------------
Please use only spaces in Virdant designs.
When indenting, always use 4 spaces.


Naming Things
-------------
Names are incredibly important.
They should be descriptive and accurate.

Do not abbreviate things needlessly.
Be considerate of others who might want to read and understand your code.
Make good use of your IDE, including autocomplete, to save effort on typing.

The names of module definitions, types, and sockets should begin with an uppercase letter.
Everything else, including the names of function definitions, should begin with a lowercase letter.

For names that begin with an uppercase letter, use camel case.
For abbreviations that appear in names, such as SPI, UART, or DDR, only the first letter is capitalized.
So we write `SpiController` instead of `SPIController`.

For names that begin with a lowercase letter, we use snake case.
A good name for a UART receiver is `uart_receiver`.

When naming variables with a number, always start from 0 and do not put an underscore before the number:

GOOD: `client0`
BAD:  `client_0`

Unless it is conventional for the technology, consider using "client" instead of "master" and "server" instead of "slave".
Alternatively, you may use the terms "controller" for "master" and "device" for "slave" as well.


Types
-----
Whenever you give the type of a component, you should put a space both before and after the colon.
That is, we write `wire w : Bit` instead of `wire w: Bit`.

When declaring many components on consecutive lines, if adding a few extra spaces would cause the colons to line up,
you may insert them to make it easier to read off the types.


Keep Code Organized
---------------------
The order of statements inside module definitions is irrelevant in Virdant.
Please use this to your advantage to make code easy to read!

For example, if you define a module, consider connecting the clock and reset immediately after declaring the submodule.
Think about whether the order you have defined things will make it easy for the next person to understand what your intent was.
Use blank spaces to help separate your statements into meaningful groups.


Curly Braces
------------
Virdant should be written with the opening curly brace at the end of the line that needs to open it.
So we write the following:

.. code-block:: virdant

  mod Top {
      // ...
  }

And similarly, the correct ways to write `if` and `match` expressions are:

.. code-block:: virdant

  if condition {
      0
  } else {
      1
  }

and

.. code-block:: virdant

  match maybe_data {
      case @Invalid() => 0
      case @Valid(payload) => payload
  }


Expressions
-----------
When writing driver statements, if the expression on the right-hand side is short, you can write it on the same line:

.. code-block:: virdant

   out := is_valid->and(counter[3])

If an expression is longer, especially if it contains an `if` or `match` expression, it may be written in one of two ways.

The first is to write it inline, as if the driver is part of the expression:

.. code-block:: virdant

  counter <= if reset {
      0
  } else {
      counter->inc()
  }


Or if it makes it clearer, with a newline and an extra layer of indentation:

.. code-block:: virdant

  counter <=
      if reset {
          0
      } else {
          counter->inc()
      }

The expression for each arm of a match expression follows a similar rule:

Inline:

.. code-block:: virdant

  match maybe_data {
      case @Invalid() => 0
      case @Valid(payload) => payload
  }


Newline and extra indentation:

.. code-block:: virdant

  match maybe_data {
      case @Invalid() =>
          if default_payload {
              default_payload
          } else {
              0
          }
      case @Valid(payload) => payload
  }


## Doc Comments
Use `//>` to write documentation comments on types, ports, and modules.
The `//>` prefix introduces a doc comment line that is extracted for
documentation tooling.

```virdant
//> `UartState` is used to model the state machine of `UartSender`
//> and `UartReceiver`.
//>
//> `Idle` - awaiting a request
//> `Start(pulse)` - received a request, sending start bit
union type UartState {
    Idle()
    Start(pulse : Word[11])
}
```

Write one `//>` line per sentence.
Keep line length under 100 characters.
Put a blank `//>` line between paragraphs.

Doc comments are appropriate for:
- Module-level descriptions (purpose, ports, behavior)
- Type definitions (what each variant or field means)
- Top-level socket definitions

For inline comments that explain a single line of logic, use `//` (two
slashes) instead.

```virdant
// Randomize LEDs on every new UART byte
edge_detector.clock := clock
edge_detector.inp := uart_receiver.data_valid
```


## Package Docstrings
Use `//!` at the top of a `.vir` file to document the package or
compilation unit.
These docstrings describe the purpose of the file as a whole.

```virdant
//! TL-UL 2x2 Crossbar
//!
//! Connects two TL-UL clients to two TL-UL servers with full
//! non-blocking crossbar connectivity.
//! Address bit 31 selects the server (0 -> server_0, 1 -> server_1).

socket TlUl {
    ...
}
```

Write one `//!` line per sentence.
Keep line length under 100 characters.
Put a blank `//!` line between paragraphs.

A package docstring typically appears at line 1 of the file, before any
`import` statements or module definitions.
It may be omitted for very small files whose purpose is obvious.
