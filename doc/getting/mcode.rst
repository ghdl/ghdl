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

.. _BUILD:mcode:GNATGPL-Windows:

GNAT GPL: Windows
=================

.. rubric:: Requirements

* GNAT GPL from http://libre.adacore.com
* PowerShell 4
* PowerShell Community Extensions (PSCX)

.. rubric:: `compile.ps1`

.. code-block::

   Commands          Description
   --------------------------------------------------------------------
   -Help             Display the integrated help pages
   -Clean            Clean up all files and directories
   -Compile          Compile GHDL
   -Install          Install all files into a directory (xcopy deployment)
   -Uninstall        Uninstall all files from a directory
   -Update           Update files in the installation directory
   -CreatePackage    create an installer package

   Install options:
   -InstallPath      Installation directory

   CreatePackage options:
   -Zip              Create a zip-file for xcopy deployment
