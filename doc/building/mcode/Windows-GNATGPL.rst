.. _BUILD:mcode:Windows-GNATGPL:

mcode Backend on Windows with GNAT GPL
######################################

Requirements
============

* GNAT GPL from http://libre.adacore.com
* PowerShell 4
* PowerShell Community Extensions (PSCX)


Scripts and Parameters
======================

`compile.ps1`
-------------

.. code-block:: plain

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
