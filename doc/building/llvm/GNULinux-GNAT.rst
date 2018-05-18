.. _BUILD:llvm:GNULinux-GNAT:

LLVM Backend on GNU/Linux with GCC/GNAT
#######################################

.. HINT:: You need to install LLVM (usually depends on ``libedit``, see :ghdlsharp:`29`). The supported versions are 3.5 til 5.0, but debugging is only supported with LLVM 3.5.

* First configure GHDL with the proper arg ``./configure --with-llvm-config``. If ``llvm-config`` is not in your path, you can specify it: ``./configure --with-llvm-config=LLVM_INSTALL/bin/llvm-config``.

* Then, build with ``make`` and install with ``make install``.

.. rubric:: Example:

.. code-block:: Bash

   $ cd <ghdl>
   $ mkdir build
   $ cd build
   $ ../configure --with-llvm-config --prefix=PREFIX
   $ make
   $ make install

.. HINT:: If you want to have stack backtraces on errors (like assert failure or index of out bounds), you need to configure and build ``libbacktrace`` from GCC (you don't need to configure GCC). Then add the following arg to configure: ``--with-backtrace-lib=/path-to-gcc-build/libbacktrace/.libs/libbacktrace.a``
