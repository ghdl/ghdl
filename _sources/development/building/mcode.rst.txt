.. _BUILD:mcode:

mcode backend
#############

The mcode backend is available for all supported platforms and is also the
simplest procedure, because it requires the fewest dependencies and configuration
options.

.. _BUILD:mcode:GNAT:

GCC/GNAT: GNU/Linux or Windows (MinGW/MSYS2)
============================================

.. rubric:: Requirements

* GCC (Gnu Compiler Collection)
* GNAT (Ada compiler for GCC)

GHDL is configured by ``configure`` and built by ``make``.

* First, GHDL needs to be configured. It is common to specify a ``PREFIX``
  (installation directory like ``/usr/local`` or ``/opt/ghdl``). Without any
  other option, ``configure`` selects `mcode` as the backend.

* Next, ``make`` starts the compilation process.

* Finally, ``make install`` installs GHDL into the installation directory
  specified by ``PREFIX``.

.. HINT :: ON GNU/Linux, you may need super user privileges (``sudo ...``).


.. rubric:: Example:

.. code-block:: Bash

   $ cd <ghdl>
   $ mkdir build
   $ cd build
   $ ../configure --prefix=PREFIX
   $ make
   $ make install
