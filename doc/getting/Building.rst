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
| `GCC`                  | - generated code is faster (particularly with :samp:`-O` or :samp:`-O2`)                 | - analysis can take time (particularly for large units) |
|                        | - generated code can be debugged (with :samp:`-g`)                                       | - build is more complex                                 |
|                        | - ported to many platforms (:samp:`x86`, :samp:`x86_64`, :samp:`powerpc`, :samp:`sparc`) |                                                         |
+------------------------+------------------------------------------------------------------------------------------+---------------------------------------------------------+
| `LLVM`                 | - Same as `GCC`                                                                          | Coverage, :samp:`gcov`, is unique to `GCC`              |
|                        | - Easier to build than `GCC`                                                             |                                                         |
+------------------------+------------------------------------------------------------------------------------------+---------------------------------------------------------+

.. HINT:: The output of both `GCC` and `LLVM` is an executable file, but `mcode` does not generate any. Therefore, if using `GCC`/`LLVM`, the call with argument :samp:`-r` can be replaced with direct execution of the binary.

After making your choice, you can jump to the corresponding section below. However, we suggest you to read :ref:`BUILD:dir_structure` before, so that you know where the content is placed and which temporal files are expected to be created.

.. HINT:: Since GHDL is written in `Ada`, independently of the code generator you use, the `GNU Ada compiler`, `GNAT GPL`, is required, 2014 (or later) for :samp:`x86` (32 or 64 bits). `GNAT GPL` can be downloaded anonymously from `libre.adacore.com <http://libre.adacore.com/tools/gnat-gpl-edition/>`_ . Then, untar and run the doinstall script. Alternatively, most GNU/Linux provide a package named :samp:`gcc-ada` or :samp:`gcc-gnat`.

.. TODO::

  - @1138 Backtraces optional -patchable-
  - Very briefly, why is mcode faster for analysis and GCC/LLVM for simulation?
  - The only other dependency is zlib (On ubuntu/debian, install zlib1g-dev).
  
.. _BUILD:dir_structure:
  
Directory structure
=================

.. TODO::

  Directory structure of the main branch [1138: #279]
  
Mcode backend
=================

This is the most simple procedure, because it requires the least dependencies and configuration options.

GNU/Linux
----------------

- First, execute :samp:`./configure --prefix=PREFIX`, where :samp:`PREFIX` is the directory for installation.
- Then :samp:`make`, that builds the :samp:`ghdl_mcode` executable, which can be used as is.
- Last, :samp:`make install` to install within :samp:`PREFIX`.
- That's all!

Windows MinGW 32/64
----------------

.. TODO:: For example, on Windows, you may want to set it to :samp:`--prefix=/c/Program Files (x86)/GHDL` .

Windows GNAT GPL (32 only)
----------------

.. TODO::
	Note: this was tested with Windows XP SP2 familly edition.
	Note: If you want to create the installer, GHDL should be built on a FAT partition.  NSIS rounds files date to be FAT compliant (seconds are always even) and because GHDL stores dates, the files date must not be modified.
	The Ada95 GNAT compiler (GNAT GPL 2005 is known to work), along with NSIS to create the installer. Then unzip, edit winbuild to use correct path for makensis, and run winbuild. The installer is in the windows directory.
	   
LLVM backend 
=================

GNU/Linux
----------------

You need to install `LLVM` (usually depends on :samp:`libedit`). The supported versions are 3.5 till 3.9, but debugging is only supported with `LLVM` 3.5.

- First configure GHDL with the proper arg :samp:`./configure --with-llvm-config`. If :samp:`llvm-config` is not in your path, you can specify it: :samp:`./configure --with-llvm-config=LLVM_INSTALL/bin/llvm-config`.

.. HINT:: If you want to have stack backtraces on errors (like assert failure or index of out bounds), you need to configure and build :samp:`libbacktrace` from `GCC` (you don't need to configure `GCC`). Then add the following arg to configure: :samp:`--with-backtrace-lib=/path-to-gcc-build/libbacktrace/.libs/libbacktrace.a`

- Then build with :samp:`make` and install with :samp:`make install`.
  
Mac OS?
----------------

Windows MinGW 32/64
----------------

GCC backend
=================

.. TODO::

	- You need to download and untar the sources of `GCC` version 4.9 [do not modify this line as this is read by scripts].
	- gcc object dir
	- Notes for developpers developping with the GCC backend: once `GCC` (with GHDL) has been built once, it is possible to work on the GHDL source tree without copying it in the `GCC` tree. Commands are::
	
		$ make ghdl1-gcc           # Build the compiler
		$ make ghdl_gcc            # Build the driver
		$ make libs.vhdl.local_gcc # Compile the vhdl libraries
		$ make grt-all             # Build the GHDL runtime
		$ make grt.links           # Locally install the GHDL runtime	

GNU/Linux
----------------

- First configure GHDL, specify `GCC` source dir and :samp:`prefix` (replace :samp:`/usr/local` with your desired installation directory)::

	./configure --with-gcc=/path/to/gcc/source/dir --prefix=/usr/local
	
- Then, invoke :samp:`make` to copy GHDL sources in the source dir::

	make copy-sources

.. HINT:: There are some dependencies for building `GCC` (:samp:`gmp`, :samp:`mpfr` and :samp:`mpc`). If you have not them installed on your system, you can either build them manually or use the :samp:`download_prerequisite` script provided in the `GCC` source tree (recommended): :samp:`cd /path/to/gcc/source/dir && ./contrib/download_prerequisites`

- Then, configure `GCC`. The list of :samp:`--disable` configure options can be adjusted to your needs. GHDL does not require all these optional libraries and disabling them will speed-up the build::

	../gcc-4.9.3/configure --prefix=/usr/local --enable-languages=c,vhdl \
	--disable-bootstrap --disable-lto --disable-multilib --disable-libssp \
	--disable-libgomp --disable-libquadmath ``.

.. HINT:: Note that the prefix directory must be the same as the one used to configure GHDL. If you have manually built :samp:`gmp`/:samp:`mpfr`/:samp:`mpc` (without using the script in :samp:`contrib`) and if you have installed them in a non-standard directory, you may need to add :samp:`--with-gmp=GMP_INSTALL_DIR`.

- Then, build and install `GCC`::

	make -j2 && make install

.. HINT:: If you don't want to install :samp:`makeinfo`, do :samp:`make install MAKEINFO=false` instead.

- Last, build and install GHDL libraries::

	cd /path/to/ghdl/source/dir
	make ghdllib
	make install

Windows?
----------------

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

	
Test suites
=================

.. TODO::

  @1138 explain that there are two (maybe three with vhdl08 tests)