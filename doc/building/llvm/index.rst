.. _BUILD:llvm:

LLVM Backend
############

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GNAT (Ada compiler for GCC)
* LLVM (Low-Level-Virtual Machine)

  * 3.5
  * 3.8

* CLANG (Compiler front-end for LLVM)

  * 3.5
  * 3.8

.. rubric:: Supported platforms

* :ref:`GNU/Linux with GCC <BUILD:mcode:GNULinux-GCC>`
* :ref:`Windows (x86_64) with MinGW32/MinGW64 <BUILD:mcode:Windows-WinGW>`

.. toctree::
   :hidden:

   GNULinux-GNAT
   Windows-WinGW

	 
	 
GNU/Linux
---------

You need to install LLVM (usually depends on :samp:`libedit`, see :ghdlsharp`29`). The supported versions are 3.5 till 3.9, but debugging is only supported with LLVM 3.5.

- First configure GHDL with the proper arg :samp:`./configure --with-llvm-config`. If :samp:`llvm-config` is not in your path, you can specify it: :samp:`./configure --with-llvm-config=LLVM_INSTALL/bin/llvm-config`.

.. HINT:: If you want to have stack backtraces on errors (like assert failure or index of out bounds), you need to configure and build :samp:`libbacktrace` from GCC (you don't need to configure GCC). Then add the following arg to configure: :samp:`--with-backtrace-lib=/path-to-gcc-build/libbacktrace/.libs/libbacktrace.a`

- Then build with :samp:`make` and install with :samp:`make install`.
  
Mac OS?
-------

Windows MinGW 32/64
-------------------
	 
	 