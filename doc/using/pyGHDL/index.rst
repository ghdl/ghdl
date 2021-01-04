.. _python_interface:

Python Interfaces
#################

.. #
   we have 2 interfaces (libghdl, dom) and a service (LSP) as well as CLI entry points.

GHDL offers two Python interfaces and a language server protocol service:

* ``pyGHDL.libghdl`` - low-level API to ``libghdl``
* ``pyGHDL.dom`` - high-level API as document object model (DOM)
* ``pyGHDL.lsp`` - language server protocol (LSP) implementation and service

.. rubric:: ``pyGHDL.libghdl``

   ``pyGHDL.libghdl`` is a low-level API directly interacting with the shared
   library ``libghdl....so``/``libghdl....dll``. This is a procedural and C-like
   interface.

   It comes with some Python generators for easier iterating linked lists.


.. rubric:: ``pyGHDL.dom``

   ``pyGHDL.dom`` is a high-level API offering a document object model (DOM).
   The underlying abstract VHDL language model is provided by `pyVHDLModel <https://github.com/VHDL/pyVHDLModel>`__.
   The DOM is using ``libghdl`` for file analysis and parsing.


.. rubric:: ``pyGHDL.dom``

   ``pyGHDL.dom`` is language server protocol (LSP) written in Python. The
   implementation offers an HTTPS service that can be used e.g. by editors and
   IDEs supporting LSP.

.. toctree::
   :hidden:

   ../../pyGHDL/index

.. toctree::
   :hidden:

   libghdl
   vhdl
   utils
