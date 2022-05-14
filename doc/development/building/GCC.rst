.. _BUILD:gcc:

GCC backend
###########

.. TODO :: Instructions to build GHDL with GCC backend on Windows are not available yet.

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GNAT (Ada compiler for GCC)
* GCC source files. Download and untar the sources of version 4.9.x, 5.x, 6.x, 7.x, 8.x, 9.x, 10.x, 11.x or 12.x
  (`GCC mirror sites <https://gcc.gnu.org/mirrors.html>`__).

.. HINT :: There are some dependencies for building GCC (``gmp``, ``mpfr`` and ``mpc``). If you have not installed them on your system, you can either build them manually or use the ``download_prerequisites`` script provided in the GCC source tree (recommended): ``cd /path/to/gcc/source/dir && ./contrib/download_prerequisites``.

* First configure GHDL, specify GCC source directory and installation prefix (like  ``/usr/local`` or ``/opt/ghdl``).
* Next, invoke ``make copy-sources`` to copy GHDL sources in the source directory.
* Then, configure GCC. The list of ``--disable`` configure options can be adjusted to your needs. GHDL does not require all these optional libraries and disabling them will speed up the build.
* Now, build and install GCC with ``make``.
* Last, build and install GHDL libraries.

.. rubric:: Example:

.. code-block:: Bash

 $ cd <ghdl>
 $ mkdir build
 $ cd build
 $ ../configure --with-gcc=/path/to/gcc/source/dir --prefix=/usr/local
 $ make copy-sources
 $ mkdir gcc-objs; cd gcc-objs
 $ /path/to/gcc/source/dir/configure --prefix=/usr/local --enable-languages=c,vhdl \
 --disable-bootstrap --disable-lto --disable-multilib --disable-libssp \
 --disable-libgomp --disable-libquadmath
 $ make -j2 && make install
 $ cd /path/to/ghdl/source/dir/build
 $ make ghdllib
 $ make install

.. HINT :: Note that the prefix directory to configure ``gcc`` must be the same as the one used to configure GHDL. If you have manually built ``gmp``/``mpfr``/``mpc`` (without using the script in ``contrib``), and, if you have installed them in a non-standard directory, you may need to add ``--with-gmp=GMP_INSTALL_DIR``.

.. HINT :: If your system gcc was configured with ``--enable-default-pie`` (check if that option appears in the output of ``gcc -v``), you should also add it.

.. HINT :: If you don't want to install ``makeinfo``, do ``make install MAKEINFO=true`` instead.

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

  If your system gcc was built with ``--enable-default-pie``, add
  ``-no-pie`` option for linking.

.. HINT :: For ppc64/ppc64le platform, the object file format contains an identifier for the source language. Because gcc doesn't know about VHDL, gcc crashes very early. This could be fixed with a very simple change in ``gcc/config/rs6000/rs6000.c`` (``gcc/config/rs6000/rs6000-logue.c`` since gcc 10), function ``rs6000_output_function_epilogue``:

	  .. CODE:: diff

 	 	 	  || ! strcmp (language_string, "GNU GIMPLE")
 	 	 	  || ! strcmp (language_string, "GNU Go")
 	 	 	  || ! strcmp (language_string, "GNU D")
 	 	-	  || ! strcmp (language_string, "libgccjit"))
 	 	+	  || ! strcmp (language_string, "libgccjit")
 	 	+	  || ! strcmp (language_string, "vhdl"))
 	 	 	i = 0;
