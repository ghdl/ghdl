.. _BUILD:gcc:

GCC Backend
###########

.. HINT::

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GNAT (Ada compiler for GCC)
* GCC source files. Download and untar the sources of version 4.9.x, 5.x, 6.x or 7.x

.. rubric:: Supported platforms

* :doc:`GNULinux-GNAT`
* :doc:`Windows-MinGW-GNAT`


.. HINT :: Once GCC (with GHDL) has been built once, it is possible to work on the GHDL source tree without copying it in the GCC tree. Commands are::

  $ make ghdl1-gcc           # Build the compiler
  $ make ghdl_gcc            # Build the driver
  $ make libs.vhdl.local_gcc # Compile the vhdl libraries
  $ make grt-all             # Build the GHDL runtime
  $ make install.vpi.local   # Locally install vpi files

  In ``src/ortho/gcc``, create a ``Makefile.conf`` file that sets the following
  variables:

  .. CODE:: Bash

    AGCC_GCCSRC_DIR=/path/to/gcc/sources
    AGCC_GCCOBJ_DIR=/path/to/gcc/build

.. HINT :: For ppc64 (and AIX ?) platform, the object file format contains an identifier for the source language. Because gcc doesn't know about VHDL, gcc crashes very early. This could be fixed with a very simple change in ``gcc/config/rs6000/rs6000.c``, ``function rs6000_output_function_epilogue`` (as of gcc 4.8):

	  .. CODE:: C

		  else if (! strcmp (language_string, "GNU Objective-C"))
			  i = 14;
		  else
		  -  gcc_unreachable ();
		  +  i = 0;
		  fprintf (file, "%d,", i);

		  /* 8 single bit fields: global linkage (not set for C extern linkage),

.. toctree::
   :hidden:

   GNU/Linux with GCC/GNAT <GNULinux-GNAT>
   Windows with GCC/GNAT (MinGW) <Windows-MinGW-GNAT>
