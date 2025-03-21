.. program:: ghdl
.. _USING:QuickStart:Python:

Python Interfaces
#################

Currently, pyGHDL is not distributed through PyPI. Therefore, users need to install it from the git repository. However, the
version of the sources must be compatible with the installed version of GHDL (and the shared library ``libghdl``).
Installing from ``master`` is discouraged, because it might contain changes to the internal AST. Instead, ``ghdl version hash``
allows getting the commit hash of the version the installed binary was built from. Since ``pip`` allows installing packages
by providing the URL to the git repo, this is the recommended installation procedure:

.. code-block::

   pip install git+https://github.com/ghdl/ghdl.git@$(ghdl version hash)

.. _CMDREF:

Language Server
***************

When installed through ``pip``, pyGHDL provides executable entrypoints registered in the search PATH, such as ``ghdl-ls``.

.. #
   This files requires a Python module called 'AutoProgram' to be located in the
   'doc' root folder. It expects a variable 'parser' of type ArgumentParser.

.. _CMDREF-ghdlls:

.. autoprogram:: AutoProgram:lsp_parser
   :prog: ghdl-ls
   :groups:
   :label: CmdRef:ghdlls:
