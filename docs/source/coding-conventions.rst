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
Everythign else, including the names of function definitions, should begin with a lowercase letter.

For names that begin with an uppercase letter, use camel case.
For abbreviations that appear in names, such as SPI, UART, or DDR, only the first letter is capitalized.
So we write `SpiController` instead of `SPIController`.

For names that begin with a lowercase letter, we use snake case.
A good name for a UART receiver is `uart_receiver`.


Types
-----
Whenever you give the type of a component, you should put a place both before and after the colon.
That is, we write `wire w : Bit` instead of `wire w: Bit`.

When declaring many components on consecutive lines, if adding a few extra spaces would cause the colons to line up,
you may insert them to make it easier to read off the types.


Curly Braces
------------
Virdant should be written with the opening curly brace at the end of the line which needed to open it.
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
      @Invalid() => 0;
      @Valid(payload) => payload;
  }


Expressions
-----------
When writing driver statements, if the expression on the right hand side is short, you can write it on the same line:

.. code-block:: virdant

   out := is_valid->and(counter[3]);

If an expression is longer, especially if it contains an `if` or `match` expression, it may be written in one of two ways.

The first is to write it inline, as if the driver is part of the expression:

.. code-block:: virdant

  counter <= if reset {
      0
  } else {
      counter->inc()
  };


Or if it makes it clearer, with a newline and an extra layer of indentation:

.. code-block:: virdant

  counter <= 
      if reset {
          0
      } else {
          counter->inc()
      };

The expressions for the arm of each match expression follows a similar rule:

Inline:

.. code-block:: virdant

  match maybe_data {
      @Invalid() => 0;
      @Valid(payload) => payload;
  }


Newline and extra indentation:

.. code-block:: virdant

  match maybe_data {
      @Invalid() => 
          if default_payload {
              default_payload
          } else {
              0
          };
      @Valid(payload) => payload;
  }
