.. include:: shieldswho.txt

About GHDL
############

.. _INTRO:VHDL:

What is `VHDL`?
==============

:wikipedia:`VHDL <VHDL>` is an acronym for Very High Speed Integrated Circuit (:wikipedia:`VHSIC <VHSIC>`) Hardware Description Language (:wikipedia:`HDL <HDL>`), which is a programming language used to describe a logic circuit by function, data flow behavior, or structure.

Although VHDL was not designed for writing general purpose programs, VHDL *is* a programming language, and you can write any algorithm with it. If you are able to write programs, you will find in VHDL features similar to those found in procedural languages such as `C`, `Python`, or `Ada`. Indeed, VHDL derives most of its syntax and semantics from Ada. Knowing `Ada` is an advantage for learning VHDL (it is an advantage in general as well).

However, VHDL was not designed as a general purpose language but as an `HDL`. As the name implies, VHDL aims at modeling or documenting electronics systems. Due to the nature of hardware components which are always running, VHDL is a highly concurrent language, built upon an event-based timing model.

Like a program written in any other language, a VHDL program can be executed. Since VHDL is used to model designs, the term :dfn:`simulation` is often used instead of `execution`, with the same meaning. At the same time, like a design written in another `HDL`, a set of VHDL sources can be transformed with a :dfn:`synthesis tool` into a netlist, that is, a detailed gate-level implementation.

The development of VHDL started in 1983 and the standard is named `IEEE <https://www.ieee.org/>`_ `1076`. Four revisions exist: `1987 <http://ieeexplore.ieee.org/document/26487/>`_, `1993 <http://ieeexplore.ieee.org/document/392561/>`_, `2002 <http://ieeexplore.ieee.org/document/1003477/>`_ and `2008 <http://ieeexplore.ieee.org/document/4772740/>`_. The standarization is handled by the VHDL Analysis and Standardization Group (`VASG/P1076 <http://www.eda-twiki.org/vasg/>`_).

.. _INTRO:GHDL:

What is GHDL?
==============

`GHDL` is a shorthand for `G Hardware Design Language` (currently, `G` has no meaning). It is a VHDL compiler that can execute (nearly) any VHDL program. GHDL is *not* a synthesis tool: you cannot create a netlist with GHDL (yet).

Unlike some other simulators, GHDL is a compiler: it directly translates a VHDL file to machine code, without using an intermediary language such as `C` or `C++`. Therefore, the compiled code should be faster and the analysis time should be shorter than with a compiler using an intermediary language.

GHDL can use multiple back-ends, i.e. code generators, (`GCC <http://gcc.gnu.org/>`_, `LLVM <http://llvm.org/>`_ or :wikipedia:`x86 <X86-64>`/:wikipedia:`i386 <Intel_80386>` only, a built-in one) and runs on :wikipedia:`GNU/Linux <Linux_distribution>`, :wikipedia:`Windows <Microsoft_Windows>` |trade|  and :wikipedia:`macOS <MacOS>` |trade| , both on x86 and on x86_64.

The current version of GHDL does not contain any graphical viewer: you cannot see signal waves. You can still check the behaviour of your design with a test bench. Moreover, the current version can produce a `GHW <http://ghdl.readthedocs.io/en/latest/using/Simulation.html?highlight=GHW#cmdoption-wave>`_, :wikipedia:`VCD <Value_change_dump>` or `FST` files which can be viewed with a :wikipedia:`waveform viewer <Waveform_viewer>`, such as `GtkWave <http://gtkwave.sourceforge.net/>`_.

GHDL aims at implementing VHDL as defined by `IEEE 1076 <http://ieeexplore.ieee.org/document/4772740/>`_. It supports the `1987 <http://ieeexplore.ieee.org/document/26487/>`_, `1993 <http://ieeexplore.ieee.org/document/392561/>`_ and `2002 <http://ieeexplore.ieee.org/document/1003477/>`_ revisions and, partially, the latest, `2008 <http://ieeexplore.ieee.org/document/4772740/>`_. :wikipedia:`PSL <Property_Specification_Language>` is also partially supported.

Several third party projects are supported: `VUnit <https://vunit.github.io/>`_, `OSVVM <http://osvvm.org/>`_, `cocotb <https://github.com/potentialventures/cocotb>`_ (through the `VPI interface <https://en.wikipedia.org/wiki/Verilog_Procedural_Interface>`_), ...

.. _INTRO:WHO:

Who uses GHDL?
==============

.. container:: whouses

	+-------------------+--------------------+---------------------------------------------------+----------------------------------------------------------------+
	| Project hub       | Documentation      | Name                                              | Brief description                                              |
	+===================+====================+===================================================+================================================================+
	| |SHIELD:gh-poc|   | |SHIELD:rtd-poc|   | `PoC-Library <https://github.com/VLSI-EDA/PoC>`_  | A Vendor-Independent, Open-Source IP Core and Utility Library. |
	+-------------------+--------------------+---------------------------------------------------+----------------------------------------------------------------+
	| |SHIELD:gh-vunit| | |SHIELD:doc-vunit| | `VUnit <http://vunit.github.io/>`_                | A unit testing framework for VHDL/SystemVerilog                |
	+-------------------+--------------------+---------------------------------------------------+----------------------------------------------------------------+ 
	| |SHIELD:gl-p1076| | |SHIELD:tw-p1076|  | `IEEE P1076 WG <http://www.eda-twiki.org/vasg/>`_ | IEEE P1076 Working Group [VASG]                                |
	+-------------------+--------------------+---------------------------------------------------+----------------------------------------------------------------+
