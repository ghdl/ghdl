.. _INT:Overview:

Overview
########

`GHDL` is architectured like a traditionnal compiler.  It has:

* a driver (sources in :file:`src/ghdldrv`) to call the programs (compiler, assembler, linker) if needed.

* a library (sources in :file:`src/grt`) to help execution at run-time.

* a front-end (sources in :file:`src/vhdl`) to parse and analyse VHDL.

* a back-end (in fact many, sources are in :file:`src/ortho`) to generate code.

The architecture is modular.  For example, it is possible to use the front-end in the `libghdl` library for the language server or to do synthesis (sources in :file:`src/synth`) instead of code generation.

The main work is performed by the front-end, which is documented in the next chapter.
