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
   You need to install LLVM (usually depends on :samp:`libedit`, see :ghdlsharp`29`). The supported versions are 3.5 till 3.9, but debugging is only supported with LLVM 3.5.

   - First configure GHDL with the proper arg :samp:`./configure --with-llvm-config`. If :samp:`llvm-config` is not in your path, you can specify it: :samp:`./configure --with-llvm-config=LLVM_INSTALL/bin/llvm-config`.
   
   .. HINT::
      If you want to have stack backtraces on errors (like assert failure or index of out bounds), you need to configure and build :samp:`libbacktrace` from GCC (you don't need to configure GCC). Then add the following arg to configure: :samp:`--with-backtrace-lib=/path-to-gcc-build/libbacktrace/.libs/libbacktrace.a`
   
   - Then build with :samp:`make` and install with :samp:`make install`.

