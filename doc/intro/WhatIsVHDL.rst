.. _INTRO:VHDL:

What is `VHDL`?
###############

`VHDL` is an acronym for Very High Speed Integrated Circuit Hardware Description
Language which is a programming language used to describe a logic circuit by
function, data flow behavior, or structure.

`VHDL` *is* a programming language: although `VHDL` was not designed for writing
general purpose programs, you can write any algorithm with the `VHDL` language.
If you are able to write programs, you will find in `VHDL` features similar to
those found in procedural languages such as `C`, `Python`, or `Ada`. `VHDL`
derives most of its syntax and semantics from `Ada`. Knowing `Ada` is an
advantage for learning `VHDL` (it is an advantage in general as well).

However, `VHDL` was not designed as a general purpose language but as an `HDL`
(hardware description language). As the name implies, `VHDL` aims at modeling or
documenting electronics systems. Due to the nature of hardware components which
are always running, `VHDL` is a highly concurrent language, built upon an
event-based timing model.

Like a program written in any other language, a `VHDL` program can be executed.
Since `VHDL` is used to model designs, the term :dfn:`simulation` is often used
instead of `execution`, with the same meaning.

Like a program written in another hardware description language, a `VHDL`
program can be transformed with a :dfn:`synthesis tool` into a netlist, that is,
a detailed gate-level implementation.

.. TODO::

  very very briefly explain that there are four major verions: 87, 93, 02 and 08