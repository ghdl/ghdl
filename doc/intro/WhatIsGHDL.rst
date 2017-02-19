.. include:: <isonum.txt>

.. _INTRO:GHDL:

What is `GHDL`?
###############

`GHDL` is a shorthand for G Hardware Design Language. Currently, `G` has no
meaning.

`GHDL` is a `VHDL` compiler that can execute (nearly) any `VHDL` program. `GHDL`
is *not* a synthesis tool: you cannot create a netlist with `GHDL`.

Unlike some other simulators, `GHDL` is a compiler: it directly translates a
`VHDL` file to machine code, using the `GCC` or `LLVM` back-end and without
using an intermediary language such as `C` or `C++`. Therefore, the compiled
code should be faster and the analysis time should be shorter than with a
compiler using an intermediary language.

The Windows\ |trade| version of `GHDL` is not based on `GCC` but on an internal
code generator.

The current version of `GHDL` does not contain any graphical viewer: you cannot
see signal waves. You can still check with a test bench. The current version can
produce a `VCD` file which can be viewed with a wave viewer, as well as `ghw`
files to be viewed by `gtkwave`.

`GHDL` aims at implementing `VHDL` as defined by IEEE 1076. It supports most of
the 1987 standard and most features added by the 1993 standard.
