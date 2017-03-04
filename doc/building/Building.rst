.. _BUILD:

Building GHDL
#############

GHDL currently supports three supported different backends (code generators): `mcode` (built-in), `GCC <http://gcc.gnu.org/>`_ and `LLVM <http://llvm.org/>`_ . Here is a short comparison, so that you can choose the one you want to use:

+------------------------+------------------------------------------------------------------------------------------+---------------------------------------------------------+
|                        | pros                                                                                     | cons                                                    |
+========================+==========================================================================================+=========================================================+
| `mcode`                | - very easy to build                                                                     | - :samp:`x86_64`/:samp:`i386` only                      |
|                        | - very quick analysis                                                                    | - simulation is slower                                  |
|                        | - can handle very large designs                                                          |                                                         |
+------------------------+------------------------------------------------------------------------------------------+---------------------------------------------------------+
| GCC                    | - generated code is faster (particularly with :samp:`-O` or :samp:`-O2`)                 | - analysis can take time (particularly for large units) |
|                        | - generated code can be debugged (with :samp:`-g`)                                       | - build is more complex                                 |
|                        | - ported to many platforms (:samp:`x86`, :samp:`x86_64`, :samp:`powerpc`, :samp:`sparc`) |                                                         |
+------------------------+------------------------------------------------------------------------------------------+---------------------------------------------------------+
| LLVM                   | - Same as GCC                                                                            | Coverage, :samp:`gcov`, is unique to GCC                |
|                        | - Easier to build than GCC                                                               |                                                         |
+------------------------+------------------------------------------------------------------------------------------+---------------------------------------------------------+

.. HINT:: The output of both GCC and LLVM is an executable file, but `mcode` does not generate any. Therefore, if using GCC/LLVM, the call with argument :samp:`-r` can be replaced with direct execution of the binary. See section :ref:`USING:QuickStart`.

After making your choice, you can jump to the corresponding section below. However, we suggest you to read :ref:`BUILD:dir_structure` before, so that you know where the content is placed and which temporal files are expected to be created.

.. HINT:: Since GHDL is written in `Ada`, independently of the code generator you use, the `GNU Ada compiler`, `GNAT GPL`, is required, 2014 (or later) for :samp:`x86` (32 or 64 bits). `GNAT GPL` can be downloaded anonymously from `libre.adacore.com <http://libre.adacore.com/tools/gnat-gpl-edition/>`_. Then, untar and run the doinstall script. Alternatively, most GNU/Linux provide a package named :samp:`gcc-ada` or :samp:`gcc-gnat`.

.. TODO::

   * @1138 Backtraces optional -patchable-
   * Very briefly, why is mcode faster for analysis and GCC/LLVM for simulation?
   * The only other dependency is zlib (On ubuntu/debian, install zlib1g-dev).
   * From :ghdlsharp:`279`:
   * GCC: GHDL generates an intermediate representation for GCC, which creates an executable. So GHDL acts a a new language frontend like C for the GCC and uses the existing backend to generated e.g. x86(-64) binary code.
   * LLVM: GHDL generates instructions for the LLVM abstract register machine, which then generates x86(-64) instructions for the host system.
   * mcode: GHDL generates the x86(-64) instructions in memory and executes the model.
     * gcc is currently only supported on Linux, because you need to compile a complete GCC from zero and add GHDL as a frontend into the GCC. Compiling the complete GCC suite plus GHDL takes a lot of time. I think it's not possible to finish the compile task in the bounds of a Travis-CI run.
  

GHDL can be build with three different back-ends: mcode, LLVM or GCC.

* :ref:`mcode <BUILD:mcode>`
* :ref:`LLVM <BUILD:llvm>`
* :ref:`GCC <BUILD:gcc>`

.. toctree::
   :hidden:
   
   Directories
   With mcode Backend <mcode/index>
   With LLVM Backend <llvm/index>
   With GCC Backend <gcc/index>
   TestSuites
