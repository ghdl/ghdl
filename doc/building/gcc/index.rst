.. _BUILD:gcc:

GCC Backend
###########

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GCC source files
* GNAT (Ada compiler for GCC)


.. rubric:: Supported platforms

* :ref:`GNU/Linux with GCC <BUILD:gcc:GNULinux-GNAT>`
* :ref:`Windows (x86_64) with MinGW32/MinGW64 <BUILD:gcc:Windows-WinGW-GNAT>`

.. toctree::
   :hidden:

   GNU/Linux with GCC/GNAT <GNULinux-GNAT>
   Windows with GCC/GNAT (MinGW) <Windows-MinGW-GNAT>

	 
	 
.. TODO::

	- You need to download and untar the sources of GCC version 4.9.x, 5.x or 6.x
	- gcc object dir
	- Notes for developpers developping with the GCC backend: once GCC (with GHDL) has been built once, it is possible to work on the GHDL source tree without copying it in the GCC tree. Commands are::
	
		$ make ghdl1-gcc           # Build the compiler
		$ make ghdl_gcc            # Build the driver
		$ make libs.vhdl.local_gcc # Compile the vhdl libraries
		$ make grt-all             # Build the GHDL runtime
		$ make grt.links           # Locally install the GHDL runtime	


.. TODO:

	Note for ppc64 (and AIX ?) platform: the object file format contains an identifier for the source language. Because gcc doesn't know about the VHDL, gcc crashes very early. This could be fixed with a very simple change in gcc/config/rs6000/rs6000.c, function rs6000_output_function_epilogue (as of gcc 4.8):
	
	.. CODE:: C
	
		else if (! strcmp (language_string, "GNU Objective-C"))
			i = 14;
		else
		-  gcc_unreachable ();
		+  i = 0;
		fprintf (file, "%d,", i);

       /* 8 single bit fields: global linkage (not set for C extern linkage,

	Install file for the binary distribution of GHDL. The binary are installed in /usr/local directory.  You cannot change this default location, unless you set links. You must be root to install this distribution. To install ghdl: `tar -C / -jxvf @TARFILE@.tar.bz2`. Note: you must also have a C compiler and zlib installed.
	 