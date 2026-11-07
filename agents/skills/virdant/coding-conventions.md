# Coding Conventions
Virdant is a language we created so that hardware can be designed in a way that is lush and beautiful.

To help with this, we insist on adhering to certain coding conventions when writing Virdant.


## Filenames
Virdant files have the `.vir` extension.
The filename defines the package name, so it must be a valid package identifier:

* Begin with a lowercase letter; use snake_case for multi-word names.
* No hyphens, uppercase letters, or other characters that cannot appear
  in an identifier.
  A file named `my-top.vir` is a compile error (`InvalidPackageName`):
  write `my_top.vir` instead.


## Whitespace
* Tabs are a compile error. Use spaces only.
* Every indentation must be a multiple of 4 spaces (compile error otherwise).
* Trailing whitespace at the end of a line is a warning. Do not leave any.
* Keep lines under 100 characters.


## Naming Things
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


## Module Definition Slot Ordering
The general order for slots defined inside `mod` definition is:

* First, port components (`incoming` `outgoing` `bus`) and `socket`s
* Second, submodules (`mod`)
* Third, `reg`s
* Fourth, `wire`s and `net`s.
* Fifth, any drivers which were intentionally not placed in driver blocks (especially FSM state transitions)


## Types
Whenever you give the type of a component, you should put a space both before and after the colon.
That is, we write `wire w : Bit` instead of `wire w: Bit`.

When declaring many components on consecutive lines, if adding a few extra spaces would cause the colons to line up,
you may insert them to make it easier to read off the types.


## Keep Code Organized
The order of statements inside module definitions is irrelevant in Virdant.
Please use this to your advantage to make code easy to read!

For example, if you define a module, consider connecting the clock and reset immediately after declaring the submodule.
Think about whether the order you have defined things will make it easy for the next person to understand what your intent was.
Use blank spaces to help separate your statements into meaningful groups.


## Curly Braces
Virdant should be written with the opening curly brace at the end of the line that needs to open it.
So we write the following:

```virdant
mod Top {
    // ...
}
```

And similarly, the correct ways to write `if` and `match` expressions are:

```virdant
if {
    when condition => 0
    else => 1
}
```

and

```virdant
match maybe_data {
    with @Invalid() => 0
    with @Valid(payload) => payload
}
```

and

```virdant
match maybe_data {
    with @Invalid() {
        has_data <= false
    }
    with @Valid(payload) {
        has_data <= true
    }
}
```


## Expressions
When writing driver statements, if the expression on the right-hand side is short, you can write it on the same line:

```virdant
out := is_valid && counter[3]
```

If an expression is longer, especially if it contains an `if` or `match` expression, it may be written in one of two ways.

The first is to write it inline, as if the driver is part of the expression:

```virdant
counter <= if {
    when reset => 0
    else => counter + 1
}
```

The expression for each arm of a match expression follows a similar rule:

Inline:

```virdant
match maybe_data {
    with @Invalid() => 0
    with @Valid(payload) => payload
}
```


Newline and extra indentation:

```virdant
match maybe_data {
    with @Invalid() if {
        when default_payload => default_payload
        else => 0
    }
    with @Valid(payload) => payload
}
```


## Doc Comments
Use `//>` to write documentation comments on types, ports, and modules.
The `//>` prefix introduces a doc comment line that is extracted for
documentation tooling.

* The marker must be followed by a space or a newline: `//> text`, never `//>text`.
  A docstring without the space after the marker is a compile error
  (`InvalidDocstring`).
* Use `//` (two slashes, no marker) for inline comments explaining a
  single line of logic.

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

```virdant
// Randomize LEDs on every new UART byte
edge_detector.clock := clock
edge_detector.inp := uart_receiver.data_valid
```


## Package Docstrings

Use `//!` at line 1 of a `.vir` file to document the package or
compilation unit.
These docstrings describe the purpose of the file as a whole.

```virdant
//! TL-UL 2x2 Crossbar
//!
//! Connects two TL-UL clients to two TL-UL servers with full
//! non-blocking crossbar connectivity.
//! Address bit 31 selects the server (0 -> server_0, 1 -> server_1).

socket TlUl {
    // ...
}
```

The same space rule applies as for `//>`: `//! text`, never `//!text`.

Write one `//!` line per sentence.
Keep line length under 100 characters.
Put a blank `//!` line between paragraphs.

A package docstring typically appears at line 1 of the file, before any
`import` statements or module definitions.
It may be omitted for very small files whose purpose is obvious.


## The `pub` keyword

* Mark an item `pub` when it is referenced from another package.
  Only `pub` items may be used cross-package.
* Conversely, items that are not `pub` and never referenced warn at
  the declaration site (`UnusedNonPub`).
  If you meant the item to be used by other packages, make it `pub`.

```virdant
// buffer.vir
pub mod Buffer {
    // ...
}

// top.vir
import buffer

mod Top {
    mod buf of buffer::Buffer {
        // ...
    }
}
```

See ./modules.md for module definitions and ./sockets.md for socket
interfaces.


## The `Valid[T]` type and the `Pipe[T]` socket
Whenever you have a `*_valid` signal (or similar),
it is an incredibly strong hint that you should be using a `Valid[T]`
to pass the data that is being transferred.

Whenever you also see a `*_ready` (or sometimes, `*_busy`) signal,
it is an incredibly strong hint that you should be using a `Pipe[T]` socket,
which contains a `Valid[T]` for the data and a `*_ready` for backpressure.


## Unclocked signals
When using unclocked signals from the top level (esp. for buttons or switches),
you should carefully consider whether you need to synchronize it with the clock.
Use the builtin `Sync` module for this.
