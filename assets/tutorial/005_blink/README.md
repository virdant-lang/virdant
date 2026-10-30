# Blink

After the previous chapter, which was long and complicated,
we will take a little break and look at something long and simple.

Let's use two Virdant packages to get one LED (Light-Emitting Diode) to blink.

## Packages

As a Virdant design grows, it's useful to organize it into multiple files.
Each file is called a package.

We will have two packages in this example: `blink` (which lives in `blink.vir`)
and `strobe` (which lives in `strobe.vir`).

Let's look at `strobe` first:

```virdant
//! The `strobe` package: a timer which emits a periodic pulse.
//!

pub mod Strobe {
    incoming clock : Clock
    incoming reset : Reset

    //> The period of the strobe, in clock cycles.
    //> Tweak this to make the LED blink faster or slower.
    incoming period : Word[24]

    //> `pulse` is asserted for one cycle every `period` cycles.
    outgoing pulse : Bit

    //> `counter` repeatedly counts down from `period` to 0.
    //> When it reaches 0, it starts over and asserts `pulse`.
    reg counter : Word[24] on clock {
        it <= when {
            case reset => period - 1
            else match it {
                // loop when we get to 0
                // otherwise, decrement
                case 0 => period - 1
                else   => it - 1
            }
        }

        pulse := it == 0
    }
}
```

## Comments

One of the great challenges with code is making sure it makes sense to people.
It is easy to write code where what you meant might not be clear to others
(or worse, to yourself a month from now).
In these cases, it is good to add comments to your code.

Virdant supports three kinds of comments.

### Package Docstrings

First, we have package docstrings.
These begin with `//!`.
They describe what the package does as a whole.
Package docstrings are optional,
but when present, they *must* appear at the very top of the file.

Here is the package docstring from `strobe.vir`:

```virdant
//! The `strobe` package: a timer which emits a periodic pulse.
//!
```

### Element Docstrings

Second, we have docstrings for elements *inside* the package.
These begin with `//>`.
They must appear immediately before the thing they are describing.

Here is one of the four element docstrings in the `strobe` package:

```virdant
    //> The period of the strobe, in clock cycles.
    //> Tweak this to make the LED blink faster or slower.
    incoming period : Word[24]
```

Both `//!` and `//>` comments are used to generate documentation for your project.
This is done using the `vir doc` command line tool.

### Ordinary Comments

Finally, we have ordinary comments.
These begin with `//`.
They do *not* appear in the generated documentation.
But they are still useful for leaving notes for yourself
or others reading the source code.

Here are two ordinary comments from inside the `Strobe` module:

```virdant
                // loop when we get to 0
                // otherwise, decrement
```

## Match and When Chaining

The actual code for this package is very simple, I think.
Hopefully you feel this way too.

There's only one tiny detail I want to point out to you.
It's inside of this `when` block:

```virdant
        it <= when {
            case reset => period - 1
            else match it {
                // loop when we get to 0
                // otherwise, decrement
                case 0 => period - 1
                else   => it - 1
            }
        }
```

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

## Wires

There is one additional detail you may have noticed:
The expression `period - 1` appears twice.

Subtraction is done with an adder in hardware, if you didn't know that.
And so the `-` operator creates an adder.

Adders aren't considered cheap in hardware like they are in software.
In this little example, we probably don't care if we place one or two.
But in a larger design, we might!

The compiler will probably notice it can optimize this and just use one adder.
But if we want to make sure of it, we could have written it with the help of a `wire`:

```virdant
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
```

A `wire` is just an expression we've given a name.
Like an `incoming` or `reg`, we can use it as an expression to get its value.
Like `outgoing`, we assign it a value with a continuous driver `:=`.

I'll leave it to you to decide:
Would you like to use the `wire` version?
Or the version with two `period - 1` expressions?

## Another Package

Let's move on to the next package, `blink`:

```virdant
//! The `blink` package: blink an LED.
//!

import strobe

mod Blink {
    incoming clock : Clock
    incoming reset : Reset

    //> `led` is the LED output.
    //> It flips on every strobe pulse.
    outgoing reg led : Bit on clock {
        when {
            case strobe_timer.pulse {
                it <= !it
            }
        }
    }

    mod strobe_timer of strobe::Strobe {
        it.clock  := clock
        it.reset  := reset
        it.period := 16_000_000
    }
}
```

## Imports

We spent a lot of effort learning about the `strobe` package above.
Let's make use of it!

We use `import strobe` to import the `strobe` package.
This must be done at the top of the file, but below the package docstring.

Once you have imported a package, you can use the stuff inside of it.
We see at the bottom of the `Blink` module, we instantiate the `Strobe` module,
using its qualified name, `strobe::Strobe`:

```virdant
    mod strobe_timer of strobe::Strobe {
        it.clock  := clock
        it.reset  := reset
        it.period := 16_000_000
    }
```

While we're looking at this, let's also notice this driver:

```virdant
        it.period := 16_000_000
```

It's nice to see Virdant allows underscores in number literals.
It makes long numbers like this easier to read.

## Outgoing Registers

The `led` port is different than the other ports we've seen.
It is an `outgoing reg`:

```virdant
    //> `led` is the LED output.
    //> It flips on every strobe pulse.
    outgoing reg led : Bit on clock {
        when {
            case strobe_timer.pulse {
                it <= !it
            }
        }
    }
```

From the outside of the module, it acts exactly like an `outgoing` port does.
From inside the module, it acts exactly like a `reg` does.
We drive it with `<=` not with `:=`.

And because `led` is a `reg`, it holds its value,
which is the last thing we'll look at in this chapter.

## Registers Hold their Value

Let's zoom in closely on the `when` statement for `led`:

```virdant
        when {
            case strobe_timer.pulse {
                it <= !it
            }
        }
```

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

```virdant
    when {
        case strobe_timer.pulse {
            it <= !it
        }
        else {
            it <= it
        }
    }
```

Doesn't that look a little silly, though?
