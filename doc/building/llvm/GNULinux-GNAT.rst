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
