.. _DEV:Debugging:

Debugging
#########

Simulation and runtime debugging options
========================================

Besides the options described in :ref:`GHDL:options`, `GHDL` passes any debugging options (those that begin with
``-g``) and optimizations options (those that begin with ``-O`` or ``-f``) to `GCC`. Refer to the `GCC` manual for
details. Moreover, some debugging options are also available, but not described here. The :option:`--help` option lists
all options available, including the debugging ones.

.. option:: --trace-signals

  Display signals after each cycle.

.. option:: --trace-processes

  Display process name before each cycle.

.. option:: --stats

  Display run-time statistics.

.. option:: --disp-order

  Display signals order.

.. option:: --disp-sources

  Display sources while displaying signals.

.. option:: --disp-sig-types

  Display signal types.

.. option:: --disp-signals-map

  Display map bw declared signals and internal signals.

.. option:: --disp-signals-table

  Display internal signals.

.. option:: --checks

  Do internal checks after each process run.

.. option:: --activity=<LEVEL>

  Watch activity of LEVEL signals: LEVEL is ``all``, ``min`` (default) or ``none`` (unsafe).

.. option:: --dump-rti

  Dump Run Time Information (RTI).

.. option:: --bootstrap

  Allow ``--work=std``

GNU Debugger (GDB)
------------------

.. index:: `__ghdl_fatal`

.. WARNING:: Debugging VHDL programs using `GDB` is possible only with GCC/LLVM.

GDB is a general purpose debugger for programs compiled by GCC. Currently, there is no VHDL support for GDB. It may be difficult to inspect variables or signals in GDB. However, it is still able to display the stack frame in case of error or to set a breakpoint at a specified line.

GDB can be useful to catch a runtime error, such as indexing an array beyond its bounds. All error check subprograms call the ``__ghdl_fatal`` procedure. Therefore, to a catch runtime error, set a breakpoint like this::

  (gdb) break __ghdl_fatal

When the breakpoint is hit, use the ``where`` or ``bt`` command to display the stack frames.
