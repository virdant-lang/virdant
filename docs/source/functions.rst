Functions
=========
Virdant allows you to define functions.

Functions are reusable snippets of combination logic.
They may be used anywhere in a design.

Functions are declared with the `fn` keyword:

.. literalinclude:: /examples/maximum.vir
    :language: virdant
    :lines: 1-7

The name and type of each parameter is given in a comma-separated list,
and the return type is given after the arrow.

The body of the function definition is a single expression which may make use of the parameters.

To call a function, we do so as you would in math class: `max(a, b)`,
where `a` and `b` can be any two expressions of the proper type.

Even when a function is only used in one place, they still provide two key benefits.
First, they allow you to name a piece of logic in a way that is meaningful to anyone who reads your code.
And second, they can be used to guarantee that a given result only depends on the arguments you pass to it.
(This is very useful when both the module definition gets large and the function body gets complicated).

Here is a a complete example that shows a module which will always output the larger byte that it has seen as an input:

.. literalinclude:: /examples/maximum.vir
    :language: virdant
    :caption: maximum.vir
    :linenos:
