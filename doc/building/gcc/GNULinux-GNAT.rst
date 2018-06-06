.. _BUILD:gcc:GNULinux-GNAT:

GCC Backend on GNU/Linux with GCC/GNAT
######################################

.. HINT:: There are some dependencies for building GCC (``gmp``, ``mpfr`` and ``mpc``). If you have not installed them on your system, you can either build them manually or use the ``download_prerequisites`` script provided in the GCC source tree (recommended): ``cd /path/to/gcc/source/dir && ./contrib/download_prerequisites``.

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
 $ cd /path/to/ghdl/source/dir
 $ make ghdllib
 $ make install

.. HINT:: Note that the prefix directory to configure ``gcc`` must be the same as the one used to configure GHDL. If you have manually built ``gmp``/``mpfr``/``mpc`` (without using the script in ``contrib``), and, if you have installed them in a non-standard directory, you may need to add ``--with-gmp=GMP_INSTALL_DIR``.

.. HINT:: If your system gcc was configured with ``--enable-default-pie`` (check if that option appears in the output of ``gcc -v``), you should also add it.

.. HINT:: If you don't want to install ``makeinfo``, do ``make install MAKEINFO=true`` instead.
