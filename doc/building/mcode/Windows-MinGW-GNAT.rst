.. _BUILD:mcode:Windows-MinGW-GNAT:

GHDL with mcode backend build on Windows with GCC/GNAT (MinGW)
##############################################################

On Windows with MinGW, GHDL is configured by ``configure`` and build by ``make``.

* First, GHDL needs to be configured. It is common to specify a ``PREFIX``
  (installation directory like ``/usr/local`` or ``/opt/ghdl``). Without any
  other option, ``configure`` select `mcode` as backend.

* Next, ``make`` starts the compilation process.

* Finally, ``make install`` installs GHDL into the installation directory
  specified by ``PREFIX``.

.. rubric:: Example:

.. code-block:: Bash
   
   cd <ghdl>
   mkdir build
   cd build
   ../configure --prefix=PREFIX
   make
   make install
