.. _BUILD:gcc:

GCC Backend
###########

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GNAT (Ada compiler for GCC)


.. rubric:: Supported platforms

* :ref:`GNU/Linux with GCC <BUILD:mcode:GNULinux-GCC>`
* :ref:`Windows (x86_64) with GNAT GPL (x86) <BUILD:mcode:Windows-GNATGPL>`
* :ref:`Windows (x86_64) with MinGW32/MinGW64 <BUILD:mcode:Windows-WinGW>`

.. toctree::
   :hidden:

   GNULinux-GNAT
   Windows-WinGW

	 
	 
.. TODO::

	- You need to download and untar the sources of GCC version 4.9.x, 5.x or 6.x
	- gcc object dir
	- Notes for developpers developping with the GCC backend: once GCC (with GHDL) has been built once, it is possible to work on the GHDL source tree without copying it in the GCC tree. Commands are::
	
		$ make ghdl1-gcc           # Build the compiler
		$ make ghdl_gcc            # Build the driver
		$ make libs.vhdl.local_gcc # Compile the vhdl libraries
		$ make grt-all             # Build the GHDL runtime
		$ make grt.links           # Locally install the GHDL runtime	

GNU/Linux
---------

- First configure GHDL, specify GCC source dir and :samp:`prefix` (replace :samp:`/usr/local` with your desired installation directory)::

	./configure --with-gcc=/path/to/gcc/source/dir --prefix=/usr/local
	
- Then, invoke :samp:`make` to copy GHDL sources in the source dir::

	make copy-sources

.. HINT:: There are some dependencies for building GCC (:samp:`gmp`, :samp:`mpfr` and :samp:`mpc`). If you have not them installed on your system, you can either build them manually or use the :samp:`download_prerequisite` script provided in the GCC source tree (recommended): :samp:`cd /path/to/gcc/source/dir && ./contrib/download_prerequisites`

- Then, configure GCC. The list of :samp:`--disable` configure options can be adjusted to your needs. GHDL does not require all these optional libraries and disabling them will speed-up the build::

	../gcc-4.9.3/configure --prefix=/usr/local --enable-languages=c,vhdl \
	--disable-bootstrap --disable-lto --disable-multilib --disable-libssp \
	--disable-libgomp --disable-libquadmath ``.

.. HINT:: Note that the prefix directory must be the same as the one used to configure GHDL. If you have manually built :samp:`gmp`/:samp:`mpfr`/:samp:`mpc` (without using the script in :samp:`contrib`) and if you have installed them in a non-standard directory, you may need to add :samp:`--with-gmp=GMP_INSTALL_DIR`.

- Then, build and install GCC::

	make -j2 && make install

.. HINT:: If you don't want to install :samp:`makeinfo`, do :samp:`make install MAKEINFO=false` instead.

- Last, build and install GHDL libraries::

	cd /path/to/ghdl/source/dir
	make ghdllib
	make install

Windows?
--------

.. TODO:

    Since the content of BUILD.txt is moved to the docs, what shall we do with this line? +gcc version 6.3 [do not modify this line as this is read by scripts].

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
	 