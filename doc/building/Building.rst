.. _BUILD:

Building GHDL from Sources
##########################

.. rubric:: Download

GHDL can be downloaded as a `zip-file <https://github.com/ghdl/ghdl/archive/master.zip>`_/`tar-file <https://github.com/ghdl/ghdl/archive/master.tar.gz>`_
(latest 'master' branch) or cloned with ``git clone`` from GitHub. GitHub
offers HTTPS and SSH as transfer protocols. See the :ref:`RELEASE:Sources`
page for further details. The installation directory is referred to as ``GHDLRoot``.

.. rubric:: Available back-ends

GHDL currently supports three different back-ends (code generators):

* mcode - built-in x86(or x86_64) code generator,
* `GCC - Gnu Compiler Collection <http://gcc.gnu.org/>`_, and
* `LLVM - Low-Level Virtual Machine <http://llvm.org/>`_ .

Here is a short comparison, so that you can choose the one you want to use:

+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+
| Back-end                   | Pros                                                                       | Cons                                                    |
+============================+============================================================================+=========================================================+
| :ref:`mcode <BUILD:mcode>` | * Very easy to build                                                       | * Simulation is slower                                  |
|                            | * Very quick analysis                                                      | * x86_64/i386 only                                      |
|                            | * Can handle very large designs                                            |                                                         |
+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+
| :ref:`LLVM <BUILD:llvm>`   | * Generated code is faster                                                 | * Build is more complex                                 |
|                            | * Generated code can be debugged (with ``-g``)                             |                                                         |
|                            | * Easier to build than GCC                                                 |                                                         |
+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+
| :ref:`GCC <BUILD:gcc>`     | * Generated code is faster (particularly with ``-O`` or ``-O2``)           | * Build is even more complex                            |
|                            | * Generated code can be debugged (with ``-g``)                             | * Analysis can take time (particularly for large units) |
|                            | * Ported to many platforms (x86, x86_64, PowerPC, SPARC)                   | * Code coverage collection (``gcov``) is unique to GCC  |
+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+

.. toctree::
   :hidden:

   Directories
   With mcode Backend <mcode/index>
   With LLVM Backend <llvm/index>
   With GCC Backend <gcc/index>
   TestSuites

.. HINT::
   The output of both GCC and LLVM is an executable file, but `mcode` does not
   generate any. Therefore, if using GCC/LLVM, the call with argument ``-r`` can
   be replaced with direct execution of the binary. See section :ref:`USING:QuickStart`.

After making your choice, you can jump to the corresponding section.
However, we suggest you to read :ref:`BUILD:dir_structure` first, so that you
know where the content will be placed and which files are expected to be
created.

.. HINT::
   Since GHDL is written in `Ada`, independently of the code generator you use,
   the `GNU Ada compiler`, `GNAT GPL`, is required, 2014 (or later) for ``x86``
   (32 or 64 bit). `GNAT GPL` can be downloaded anonymously from `libre.adacore.com <http://libre.adacore.com/tools/gnat-gpl-edition/>`_.
   Then, untar and run the doinstall script. Alternatively, most GNU/Linux
   provide a package named ``gcc-ada`` or ``gcc-gnat``.

.. HINT::
  In these instructions, the configure script is executed in the source directory; but you can execute in a different
  directory too, like this:

	.. CODE:: Bash

		$ mkdir ghdl-objs
		$ cd ghdl-objs
		$ ../path/to/ghdl/configure ...
