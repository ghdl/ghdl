.. _BUILD:mcode:GNULinux-GNAT:

GNU/Linux with GCC/GNAT
#######################

On Linux, GHDL is configured by ``configure`` and build by ``make``.

* First, GHDL needs to be configured. It is common to specify a PREFIX
  (installation directory like ``/usr/local`` or ``/opt/ghdl``). Without any
  other option, ``configure`` select `mcode` as backend.

* Next, ``make`` starts the compilation process.

* Finally, ``make install`` installs GHDL into the installation directory
  specified by PREFIX. You may need super user privileges (``sudo ...``).

.. rubric:: Example:

.. code-block:: Bash
   
   cd <ghdl>
   mkdir build
   cd build
   ../configure --prefix=PREFIX
   make
   sudo make install

.. TODO::
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
   