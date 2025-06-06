.. _INT:Overview:

Overview
########

.. figure:: ../_static/internals.png
   :width: 100%
   :align: center

   Architecture overview of the GHDL toolkit (CLI, libghdl, pyGHDL and ghdl-yosys-plugin).

`GHDL` is architectured like a traditional compiler. It has:

* a driver (sources in :ghdlsrc:`src/ghdldrv <ghdldrv>`) to call the programs (compiler, assembler, linker) if needed.

* a runtime library named GRT (sources in :ghdlsrc:`src/grt <grt>`) to help execution at run-time.
  This ensures that execution is cycle-accurate.

* a front-end (sources in :ghdlsrc:`src/vhdl <vhdl>`) to parse and analyse VHDL.
  See :ref:`INT:Frontend` and :ref:`INT:AST`.

* multiple back-ends to generate either code or netlists.

  * Three for :ref:`simulation <USING:Simulation>` (sources are in :ghdlsrc:`src/ortho <ortho>`): mcode, LLVM and GCC.
    See :ref:`BUILD`.
  * Two for :ref:`synthesis <USING:Synthesis>` (sources in :ghdlsrc:`src/synth <synth>`):
    :ref:`--synth <Synth:command>` and :ref:`Synth:plugin`.

The architecture is modular and some of the components are available through the shared library ``libghdl``.
``libghdl`` is used by the :ref:`Synth:plugin`, the Language Server Protocol (LSP) server, and to fill the data of
pyVHDLModel (see :doc:`vhdlmodel:index`).

The main work with regard to understanding the VHDL language is performed by the front-end, which is documented in the
next section.
