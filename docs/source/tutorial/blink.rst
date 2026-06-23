Blink
=====
After the previous chapter, which was long and complicated,
we will take a little break and look at something long and simple.

Let's use two Virdant packages to get one :tip:`LED (Light-Emitting Diode)` to blink.

Packages
--------
As a Virdant design grows, it's useful to organize it into multiple files.
Each file is called a package.

We will have two packages in this example: `blink` (which lives in `blink.vir`)
and `strobe` (which lives in `strobe.vir`).

Let's look at `strobe` first:

.. literalinclude:: /examples/strobe.vir
    :caption: strobe.vir
    :language: virdant


Comments
--------
One of the great challenges with code is making sure it makes sense to people.
It is easy to write code where what you meant might not be clear to others
(or worse, to yourself a month from now).
In these cases, it is good to add comments to your code.

Virdant supports three kinds of comments.

Package Docstrings
~~~~~~~~~~~~~~~~~~
First, we have package docstrings.
These begin with `//!`.
They describe what the package does as a whole.
Package docstrings are optional,
but when present, they *must* appear at the very top of the file.

Here is the package docstring from `strobe.vir`:

.. literalinclude:: /examples/strobe.vir
    :language: virdant
    :lines: 1-2


Element Docstrings
~~~~~~~~~~~~~~~~~~
Second, we have docstrings for elements *inside* the package.
These begin with `//>`.
They must appear immediately before the thing they are describing.

Here is one of the four element docstrings in the `strobe` package:

.. literalinclude:: /examples/strobe.vir
    :language: virdant
    :lines: 10-11
    :dedent:

Both `//!` and `//>` comments are used to generate documentation for your project.
This is done using the `vir doc` command line tool.


Ordinary Comments
~~~~~~~~~~~~~~~~~
Finally, we have ordinary comments.
These begin with `//`.
They do *not* appear in the generated documentation.
But they are still useful for leaving notes for yourself
or others reading the source code.

Here are two ordinary comments from inside the `Strobe` module:

.. literalinclude:: /examples/strobe.vir
    :language: virdant
    :lines: 24-25
    :dedent:

Match and When Chaining
-----------------------
The actual code for this package is very simple, I think.
Hopefully you feel this way too.

There's only one tiny detail I want to point out to you.
It's inside of this `when` block:

.. literalinclude:: /examples/strobe.vir
    :language: virdant
    :lines: 21-27
    :dedent:

Normally, we have a `=>` after each `case` and after the `else` in a `when` block.
However, here, we do not.

If a `case` or `else` contains a `match` block,
we may write it joined together like this and elide the `=>`.
This is called `match` block chaining.

This actually works for `when` statements too.
But then we call it `when` block chaining instead.

When we do this, we write the `when` or `match` statement on the same line.
This lets the `case` and `else` statements nest neatly,
with only one level of indentation.


Wires
-----
There is one additional detail you may have noticed:
The expression `period - 1` appears twice.

Subtraction is done with an adder in hardware, if you didn't know that.
And so the `-` operator creates an adder.

Adders aren't considered cheap in hardware like they are in software.
In this little example, we probably don't care if we place one or two.
But in a larger design, we might!

The compiler will probably notice it can optimize this and just use one adder.
But if we want to make sure of it, we could have written it with the help of a `wire`:

.. code-block:: virdant

    // a wire which guarantees we only synthesize one adder
    wire period_minus_one : Word[24] {
        it := period - 1
    }

    //> `counter` repeatedly counts down from `period` to 0.
    reg counter : Word[24] on clock {
        it <= when {
            case reset => period_minus_one
            else match it {
                case 0 => period_minus_one  // loop when we get to 0
                else   => it - 1            // otherwise, decrement
            }
        }
    }

A `wire` is just an expression we've given a name.
Like an `incoming` or `reg`, we can use it as an expression to get its value.
Like `outgoing`, we assign it a value with a continuous driver `:=`.

I'll leave it to you to decide:
Would you like to use the `wire` version?
Or the version with two `period - 1` expressions?


Another Package
---------------
Let's move on to the next package, `blink`:

.. literalinclude:: /examples/blink.vir
    :caption: blink.vir
    :language: virdant


Imports
-------
We spent a lot of effort learning about the `strobe` package above.
Let's make use of it!

We use `import strobe` to import the `strobe` package.
This must be done at the top of the file, but below the package docstring.

Once you have imported a package, you can use the stuff inside of it.
We see at the bottom of the `Blink` module, we instantiate the `Strobe` module,
using its qualified name, `strobe::Strobe`:

.. literalinclude:: /examples/blink.vir
    :language: virdant
    :lines: 19-24
    :dedent:

While we're looking at this, let's also notice this driver:

.. literalinclude:: /examples/blink.vir
    :language: virdant
    :lines: 23
    :dedent:

It's nice to see Virdant allows underscores in number literals.
It makes long numbers like this easier to read.


Outgoing Registers
------------------
The `led` port is different than the other ports we've seen.
It is an `outgoing reg`:

.. literalinclude:: /examples/blink.vir
    :language: virdant
    :lines: 10-17
    :dedent:

From the outside of the module, it acts exactly like an `outgoing` port does.
From inside the module, it acts exactly like a `reg` does.
We drive it with `<=` not with `:=`.

And because `led` is a `reg`, it holds its value,
which is the last thing we'll look at in this chapter.


Registers Hold their Value
--------------------------
Let's zoom in closely on the `when` statement for `led`:

.. literalinclude:: /examples/blink.vir
    :language: virdant
    :lines: 12-16
    :dedent:

Do you notice something's missing?

When we learned about `when` blocks earlier,
they always had an `else` statement.
They *needed* to have one, or else Virdant would give you an error.
But this one doesn't.

Weird.

When a `when` block is used as an expression, they must handle all cases.
Otherwise, we wouldn't know what value they should take in the cases you didn't cover.
That means `when` blocks need an `else` block... when used as expressions.

But here, `when` is being used to guard against a latched driver: `it <= !it`,
and `it` refers to a `led`, which is a register.

On any given clock cycle, if a register is not driven with `<=`,
it simply keeps whatever value it had on the previous cycle.

It's equivalent to the following:

.. code-block:: virdant

    when {
        case strobe_timer.pulse {
            it <= !it
        }
        else {
            it <= it
        }
    }

Doesn't that look a little silly, though?
