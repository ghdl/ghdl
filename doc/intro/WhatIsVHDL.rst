.. _INTRO:VHDL:

What is `VHDL`?
###############

`VHDL <https://en.wikipedia.org/wiki/VHDL>`_ is an acronym for Very High Speed Integrated Circuit (`VHSIC <https://en.wikipedia.org/wiki/VHSIC>`_) `Hardware Description Language <https://en.wikipedia.org/wiki/Hardware_description_language>`_ which is a programming language used to describe a logic circuit by function, data flow behavior, or structure.

Although `VHDL` was not designed for writing general purpose programs, `VHDL` *is* a programming language, and you can write any algorithm with it. If you are able to write programs, you will find in `VHDL` features similar to those found in procedural languages such as `C`, `Python`, or `Ada`. Indeed, `VHDL` derives most of its syntax and semantics from `Ada`. Knowing `Ada` is an advantage for learning `VHDL` (it is an advantage in general as well).

However, `VHDL` was not designed as a general purpose language but as an `HDL`. As the name implies, `VHDL` aims at modeling or documenting electronics systems. Due to the nature of hardware components which are always running, `VHDL` is a highly concurrent language, built upon an event-based timing model.

Like a program written in any other language, a `VHDL` program can be executed. Since `VHDL` is used to model designs, the term :dfn:`simulation` is often used instead of `execution`, with the same meaning. At the same time, like a design written in another `HDL`, a set of `VHDL` sources can be transformed with a :dfn:`synthesis tool` into a netlist, that is, a detailed gate-level implementation.

The development of `VHDL` started in 1983 and the standard is named `IEEE <https://www.ieee.org/>`_ `1076`. Four revisions exist: `1987 <http://ieeexplore.ieee.org/document/26487/>`_, `1993 <http://ieeexplore.ieee.org/document/392561/>`_, `2002 <http://ieeexplore.ieee.org/document/1003477/>`_ and `2008 <http://ieeexplore.ieee.org/document/4772740/>`_. The standarization is handled by the VHDL Analysis and Standardization Group (`VASG/P1076 <http://www.eda-twiki.org/vasg/>`_).
