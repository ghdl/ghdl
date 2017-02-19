************
Introduction
************


Content of this manual
======================

This manual is the user and reference manual for GHDL.  It does not
contain an introduction to VHDL.  Thus, the reader should have at least
a basic knowledge of VHDL.  A good knowledge of VHDL language reference
manual (usually called LRM) is a plus.

What is `VHDL`?
===============

`VHDL` is an acronym for Very High Speed Integrated Circuit Hardware
Description Language which is a programming language used to describe a
logic circuit by function, data flow behaviour, or structure.

`VHDL` *is* a programming language: although `VHDL` was
not designed for writing general purpose programs, you can write any
algorithm with the `VHDL` language.  If you are able to write
programs, you will find in `VHDL` features similar to those found
in procedural languages such as `C`, `Python`, or `Ada`.
`VHDL` derives most of its syntax and semantics from `Ada`.
Knowing `Ada` is an advantage for learning `VHDL` (it is an
advantage in general as well).

However, `VHDL` was not designed as a general purpose language but as an
`HDL` (hardware description language).  As the name implies, `VHDL`
aims at modeling or documenting electronics systems.  Due to the nature
of hardware components which are always running, `VHDL` is a highly
concurrent language, built upon an event-based timing model.

Like a program written in any other language, a `VHDL` program
can be executed. Since `VHDL` is used to model designs, the term
:dfn:`simulation` is often used instead of `execution`, with the
same meaning.

Like a program written in another hardware description language, a
`VHDL` program can be transformed with a :dfn:`synthesis tool`
into a netlist, that is, a detailed gate-level implementation.

What is `GHDL`?
===============

`GHDL` is a shorthand for G Hardware Design Language.  Currently,
`G` has no meaning.

`GHDL` is a `VHDL` compiler that can execute (nearly) any
`VHDL` program. `GHDL` is *not* a synthesis tool: you cannot
create a netlist with `GHDL`.

Unlike some other simulators, `GHDL` is a compiler: it directly
translates a `VHDL` file to machine code, using the `GCC` or `LLVM`
back-end and without using an intermediary language such as `C`
or `C++`.  Therefore, the compiled code should be faster and
the analysis time should be shorter than with a compiler using an
intermediary language.

The Windows(TM) version of `GHDL` is not based on `GCC` but on
an internal code generator.

The current version of `GHDL` does not contain any graphical
viewer: you cannot see signal waves.  You can still check with a test
bench.  The current version can produce a `VCD` file which can be
viewed with a wave viewer, as well as `ghw` files to be viewed by
`gtkwave`.

`GHDL` aims at implementing `VHDL` as defined by IEEE 1076.
It supports most of the 1987 standard and most features added by the
1993 standard.
