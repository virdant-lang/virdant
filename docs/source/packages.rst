Packages
========
A Virdant design may be split across multiple files.
When doing so, each file is a Virdant package.

One Virdant package may import another using the `import` keyword:

.. literalinclude:: /examples/packages/top.vir
    :caption: top.vir
    :language: virdant
    :linenos:

The first line, `import buffer`, will tell Virdant that it needs to import the `buffer` package.
This will be located in another file, `buffer.vir`:

.. literalinclude:: /examples/packages/buffer.vir
    :caption: buffer.vir
    :language: virdant
    :linenos:

When we want to use a module definition, type definition, or other declaration from another package,
we refer to it by its fully-qualified name.

In this example, we see:

.. literalinclude:: /examples/packages/top.vir
    :language: virdant
    :lines: 10

The module definition is named with the fully-qualified name `buffer::Buffer`.
