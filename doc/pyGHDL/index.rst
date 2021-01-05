.. _python_interface:

Python Interfaces
#################

.. #
   we have 2 interfaces (libghdl, dom) and a service (LSP) as well as CLI entry points.

GHDL offers two Python interfaces and a language server protocol service. All
this is provided from a ``pyGHDL`` packages with four sub-packages:

* ``pyGHDL.cli`` - Command line interface (CLI) applications.
* ``pyGHDL.dom`` - High-level API as document object model (DOM).
* ``pyGHDL.libghdl`` - Low-level API to ``libghdl``
* ``pyGHDL.lsp`` - Language server protocol (LSP) implementation and service


.. topic:: pyGHDL.libghdl

   *pyGHDL.libghdl* is a low-level API directly interacting with the shared
   library ``libghdl....so``/``libghdl....dll``. This is a procedural and C-like
   interface.

   It comes with some Python generators for easier iterating linked lists.


.. topic:: pyGHDL.dom

   *pyGHDL.dom* is a high-level API offering a document object model (DOM).
   The underlying abstract VHDL language model is provided by `pyVHDLModel <https://github.com/VHDL/pyVHDLModel>`__.
   The DOM is using ``libghdl`` for file analysis and parsing.


.. topic:: pyGHDL.lsp

   *pyGHDL.lsp* is `language server protocol <https://en.wikipedia.org/wiki/Language_Server_Protocol>`__
   (LSP) written in Python. The implementation offers an HTTPS service that can
   be used e.g. by editors and IDEs supporting LSP.


.. toctree::
   :hidden:

   ../../pyGHDL/pyGHDL.cli
   ../../pyGHDL/pyGHDL.dom
   ../../pyGHDL/pyGHDL.libghdl
   ../../pyGHDL/pyGHDL.lsp


.. _CMDREF:

Scripts and Applications
########################

The pyVHDLParser package comes with an executables registered by pip in the
search path.

* ``VHDLParser`` is a wrapper for ``pyVHDLParser.CLI.VHDLParser:main``.


.. #
   This files requires a Python module called 'Frontend-AutoProgram' to be
   located in the 'doc' root folder. It expects a variable 'parser' of type
   ArgumentParser.

.. _CMDREF-ghdlls:

.. autoprogram:: AutoProgram:lsp_parser
   :prog: ghdl-ls
   :groups:
   :label: CmdRef:ghdlls:

.. _CMDREF-pnodes:

.. autoprogram:: AutoProgram:pnodes_parser
   :prog: pnodes
   :groups:
   :label: CmdRef:pnodes:

.. _CMDREF-pnodespy:

.. autoprogram:: AutoProgram:pnodespy_parser
   :prog: pnodespy
   :groups:
   :label: CmdRef:pnodespy:
