.. include:: <isonum.txt>

.. _INTRO:GHDL:

What is `GHDL`?
###############

`GHDL` is a shorthand for `G Hardware Design Language` (currently, `G` has no meaning). It is a `VHDL` compiler that can execute (nearly) any `VHDL` program. `GHDL` is *not* a synthesis tool: you cannot create a netlist with `GHDL` (yet).

Unlike some other simulators, `GHDL` is a compiler: it directly translates a `VHDL` file to machine code, without using an intermediary language such as `C` or `C++`. Therefore, the compiled code should be faster and the analysis time should be shorter than with a compiler using an intermediary language.

`GHDL` can use multiple back-ends, i.e. code generators, (`GCC <http://gcc.gnu.org/>`_, `LLVM <http://llvm.org/>`_ or `x86 <https://en.wikipedia.org/wiki/X86-64>`_/`i386 <https://en.wikipedia.org/wiki/Intel_80386>`_ only, a built-in one) and runs on `GNU/Linux <http://en.wikipedia.org/wiki/Linux_distribution>`_, `Windows <http://en.wikipedia.org/wiki/Microsoft_Windows>`_ |trade|  and `macOS <http://en.wikipedia.org/wiki/MacOS>`_ |trade| , both on `x86` and on `x86_64`.

The current version of `GHDL` does not contain any graphical viewer: you cannot see signal waves. You can still check the behaviour of your design with a test bench. Moreover, the current version can produce a `GHW <http://ghdl.readthedocs.io/en/latest/using/Simulation.html?highlight=GHW#cmdoption-wave>`_, `VCD <https://en.wikipedia.org/wiki/Value_change_dump>`_ or `FST` files which can be viewed with a `waveform viewer <https://en.wikipedia.org/wiki/Waveform_viewer>`_, such as `GtkWave <http://gtkwave.sourceforge.net/>`_.

`GHDL` aims at implementing `VHDL` as defined by `IEEE 1076 <http://ieeexplore.ieee.org/document/4772740/>`_. It supports the `1987 <http://ieeexplore.ieee.org/document/26487/>`_, `1993 <http://ieeexplore.ieee.org/document/392561/>`_ and `2002 <http://ieeexplore.ieee.org/document/1003477/>`_ revisions and, partially, the latest, `2008 <http://ieeexplore.ieee.org/document/4772740/>`_. `PSL <https://en.wikipedia.org/wiki/Property_Specification_Language>`_ is also partially supported.

Several third party projects are supported: `VUnit <https://vunit.github.io/>`_, `OSVVM <http://osvvm.org/>`_, `cocotb <https://github.com/potentialventures/cocotb>`_ (through the `VPI interface <https://en.wikipedia.org/wiki/Verilog_Procedural_Interface>`_), ...
