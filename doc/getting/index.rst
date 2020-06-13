.. _BUILD:

Building GHDL from Sources
##########################

.. rubric :: Download

GHDL can be downloaded as a `zip-file <https://github.com/ghdl/ghdl/archive/master.zip>`_/`tar-file <https://github.com/ghdl/ghdl/archive/master.tar.gz>`_
(latest 'master' branch) or cloned with ``git clone`` from GitHub. GitHub
offers HTTPS and SSH as transfer protocols. See the :ref:`RELEASE:Sources`
page for further details.

.. IMPORTANT::
   Since GHDL is written in `Ada`, independently of the code generator you use,
   the a compiler is required. Most GNU/Linux package managers provide a package
   named ``gcc-ada`` or ``gcc-gnat``. Alternatively, `GNU Ada compiler`, `GNAT GPL`,
   can be downloaded anonymously from `libre.adacore.com <http://libre.adacore.com/tools/gnat-gpl-edition/>`_ (2014, or later; for x86, 32 or 64 bits).
   Then, untar and run the doinstall script.

.. ATTENTION::
   Since ``v0.37``, GHDL's synthesis features require GCC >=8.1, due to some new GNAT features which
   are not available in previous releases. Users with older versions (who don't need synthesis)
   can configure GHDL with option ``--disable-synth``.

.. rubric :: Available back-ends

GHDL currently supports three different back-ends (code generators):

* mcode - built-in x86 (or x86_64) code generator
* GCC - Gnu Compiler Collection (`gcc.gnu.org <http://gcc.gnu.org/>`_)
* LLVM - Low-Level Virtual Machine (`llvm.org <http://llvm.org/>`_)

Here is a short comparison, so that you can choose the one you want to use:

+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+
| Back-end                   | Pros                                                                       | Cons                                                    |
+============================+============================================================================+=========================================================+
| :ref:`mcode <BUILD:mcode>` | * Very easy to build                                                       | * Simulation is slower                                  |
|                            | * Very quick analysis                                                      | * x86_64/i386 only                                      |
|                            | * Can handle very large designs                                            |                                                         |
+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+
| :ref:`LLVM <BUILD:llvm>`   | * Generated code is faster                                                 | * Build is more complex than mcode                      |
|                            | * Generated code can be debugged (with ``-g``)                             |                                                         |
|                            | * Easier to build than GCC                                                 |                                                         |
|                            | * Ported to many platforms (x86, x86_64, armv7/aarch64)                    |                                                         |
+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+
| :ref:`GCC <BUILD:gcc>`     | * Generated code is faster (particularly with ``-O`` or ``-O2``)           | * Build is even more complex                            |
|                            | * Generated code can be debugged (with ``-g``)                             | * Analysis can take time (particularly for large units) |
|                            | * Ported to many platforms (x86, x86_64, PowerPC, SPARC)                   | * Code coverage collection (``gcov``) is unique to GCC  |
+----------------------------+----------------------------------------------------------------------------+---------------------------------------------------------+

.. toctree::
   :hidden:

   Directories
   mcode
   LLVM
   GCC

.. HINT ::
   The output of both GCC and LLVM is an executable file, but `mcode` does not
   generate any. Therefore, if using GCC/LLVM, the call with argument ``-r`` can
   be replaced with direct execution of the binary. See section :ref:`USING:QuickStart`.

After making your choice, you can jump to the corresponding section.
However, we suggest you to read :ref:`BUILD:dir_structure` first, so that you
know where the content will be placed and which files are expected to be
created.

.. HINT ::
  In these instructions, the configure script is executed in the source directory; but you can execute in a different
  directory too, like this:

	.. CODE:: Bash

		$ mkdir ghdl-objs
		$ cd ghdl-objs
		$ ../path/to/ghdl/configure ...

.. HINT :: On Windows, building GHDL with mcode backend and GNAT GPL 32 bit seems to be the only way to get a standalone native executable.

  * MINGW/MSYS2 builds depend on the environment/runtime.

.. HINT ::
  For MacOS 10.15 (Catalina), see `https://github.com/ghdl/ghdl/issues/1368` for
  workarounds to link failures.
