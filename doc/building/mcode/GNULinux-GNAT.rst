.. _BUILD:mcode:GNULinux-GNAT:

mcode Backend on GNU/Linux with GCC/GNAT
########################################

On Linux, GHDL is configured by ``configure`` and built by ``make``.

* First, GHDL needs to be configured. It is common to specify a ``PREFIX``
  (installation directory like ``/usr/local`` or ``/opt/ghdl``). Without any
  other option, ``configure`` selects `mcode` as the backend.

* Next, ``make`` starts the compilation process.

* Finally, ``make install`` installs GHDL into the installation directory
  specified by ``PREFIX``. You may need super user privileges (``sudo ...``).


.. rubric:: Example:

.. code-block:: Bash

   $ cd <ghdl>
   $ mkdir build
   $ cd build
   $ ../configure --prefix=PREFIX
   $ make
   $ make install
