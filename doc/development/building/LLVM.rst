.. _BUILD:llvm:

LLVM backend
############

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GNAT (Ada compiler for GCC)
* LLVM (Low-Level-Virtual Machine) and CLANG (Compiler front-end for LLVM): 3.5, 3.8, 3.9, 4.0, 5.0, 6.0, 7.0, 8.0,
  9.0, 10.0, 11.0, 11.1, 12.0, 13.0 or 14.0

.. _BUILD:llvm:GNAT:

GCC/GNAT: GNU/Linux or Windows (MinGW/MSYS2)
============================================

.. HINT:: You need to install LLVM (usually depends on ``libedit``, see :ghdlsharp:`29`). Debugging is supported with LLVM 3.5 or ``>=6``.

GHDL is configured by ``configure`` and built by ``make``.

* First, GHDL needs to be configured. It is common to specify a ``PREFIX``
  (installation directory like ``/usr/local`` or ``/opt/ghdl``). Set the proper
  arg, ``./configure --with-llvm-config``, to select LLVM backend. If
  ``llvm-config`` is not in your path, you can specify it:
  ``./configure --with-llvm-config=LLVM_INSTALL/bin/llvm-config``.

* Next, ``make`` starts the compilation process.

* Finally, ``make install`` installs GHDL into the installation directory
  specified by ``PREFIX``.

.. rubric:: Example:

.. code-block:: Bash

   $ cd <ghdl>
   $ mkdir build
   $ cd build
   $ ../configure --with-llvm-config --prefix=PREFIX
   $ make
   $ make install

.. HINT:: If you want to have stack backtraces on errors (like assert failure or index of out bounds), you need to configure and build ``libbacktrace`` from GCC (you don't need to configure GCC). Then add the following arg to configure: ``--with-backtrace-lib=/path-to-gcc-build/libbacktrace/.libs/libbacktrace.a``
