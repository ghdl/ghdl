.. _USING:Simulation:

**********************
Simulation and runtime
**********************

.. _simulation_options:

Simulation options
==================

In most system environments, it is possible to pass options while
invoking a program.  Contrary to most programming languages, there is no
standard method in VHDL to obtain the arguments or to set the exit
status.

In GHDL, it is impossible to pass parameters to your design.  A later version
could do it through the generics interfaces of the top entity.

However, the GHDL runtime behaviour can be modified with some options; for
example, it is possible to stop simulation after a certain time.

The exit status of the simulation is :samp:`EXIT_SUCCESS` (0) if the
simulation completes, or :samp:`EXIT_FAILURE` (1) in case of error
(assertion failure, overflow or any constraint error).

Here is the list of the most useful options.  Some debugging options are
also available, but not described here.  The :option:`--help` options lists
all options available, including the debugging one.

.. option:: --assert-level=<LEVEL>

  Select the assertion level at which an assertion violation stops the
  simulation.  `LEVEL` is the name from the `severity_level`
  enumerated type defined in the `standard` package or the
  :samp:`none` name.

  By default, only assertion violation of severity level :samp:`failure`
  stops the simulation.

  For example, if `LEVEL` was :samp:`warning`, any assertion violation
  with severity level :samp:`warning`, :samp:`error` or :samp:`failure` would
  stop simulation, but the assertion violation at the :samp:`note` severity
  level would only display a message.

  Option :option:`--assert-level=none` prevents any assertion violation to stop
  simulation.

.. option:: --ieee-asserts=<POLICY>

  Select how the assertions from :samp:`ieee` units are
  handled. `POLICY` can be :samp:`enable` (the default),
  :samp:`disable` which disables all assertion from :samp:`ieee` packages
  and :samp:`disable-at-0` which disables only at start of simulation.

  This option can be useful to avoid assertion message from
  :samp:`ieee.numeric_std` (and other :samp:`ieee` packages).

.. option:: --stop-time=<TIME>

  Stop the simulation after :samp:`TIME`.  :samp:`TIME` is expressed as a time
  value, *without* any space.  The time is the simulation time, not
  the real clock time.

  For example::

    $ ./my_design --stop-time=10ns
    $ ./my_design --stop-time=ps

.. option:: --stop-delta=<N>

  Stop the simulation after `N` delta cycles in the same current time.

  .. index:: display time

.. option:: --disp-time

  Display the time and delta cycle number as simulation advances.


.. option:: --disp-tree[=<KIND>]

  .. index:: display design hierarchy

  Display the design hierarchy as a tree of instantiated design entities.
  This may be useful to understand the structure of a complex
  design. `KIND` is optional, but if set must be one of:

  * none
    Do not display hierarchy.  Same as if the option was not present.

  * inst
    Display entities, architectures, instances, blocks and generates statements.

  * proc
    Like :samp:`inst` but also display processes.

  * port
    Like :samp:`proc` but display ports and signals too.
    If `KIND` is not specified, the hierarchy is displayed with the
    :samp:`port` mode.

.. option:: --no-run

  Do not simulate, only elaborate.  This may be used with
  :option:`--disp-tree` to display the tree without simulating the whole
  design.

.. option:: --unbuffered

  Disable buffering on stdout, stderr and files opened in write or append mode (TEXTIO).

.. option:: --read-opt-file=<FILENAME>

  Filter signals to be dumped to the wave file according to the wave option
  file provided.

  Here is a description of the wave option file format currently supported :

     $ version = 1.1  # Optional

     # Path format for signals in packages :
     my_pkg.global_signal_a

     # Path format for signals in entities :
     /top/sub/clk

     # Dumps every signals named reset in first level sub entities of top
     /top/*/reset

     # Dumps every signals named reset in recursive sub entities of top
     /top/**/reset

     # Dump every signals of sub2 which could be anywhere in design except on
     # top level
     /**/sub2/*

     # Dump every signals of sub3 which must be a first level sub entity of the
     # top level
     /*/sub3/*

     # Dump every signals of the first level sub entities of sub3 (but not
     # those of sub3)
     /**/sub3/*/*

.. option:: --write-opt-file=<FILENAME>

  If the wave option file doesn't exist, creates it with all the signals of
  the design. Otherwise throws an error, because it won't erase an existing
  file.

.. option:: --vcd=<FILENAME>

.. option:: --vcdgz=<FILENAME>

  .. index:: vcd

  .. index:: value change dump

  .. index:: dump of signals

  Option :option:`--vcd` dumps into the VCD file `FILENAME` the signal
  values before each non-delta cycle.  If `FILENAME` is :samp:`-`,
  then the standard output is used, otherwise a file is created or
  overwritten.

  The :option:`--vcdgz` option is the same as the *--vcd* option,
  but the output is compressed using the `zlib` (`gzip`
  compression).  However, you can't use the :samp:`-` filename.
  Furthermore, only one VCD file can be written.

  :dfn:`VCD` (value change dump) is a file format defined
  by the `verilog` standard and used by virtually any wave viewer.

  Since it comes from `verilog`, only a few VHDL types can be dumped.  GHDL
  dumps only signals whose base type is of the following:

  * types defined in the :samp:`std.standard` package:

  * :samp:`bit`

  * :samp:`bit_vector`

  * types defined in the :samp:`ieee.std_logic_1164` package:

  * :samp:`std_ulogic`

  * :samp:`std_logic` (because it is a subtype of :samp:`std_ulogic`)

  * :samp:`std_ulogic_vector`

  * :samp:`std_logic_vector`

  * any integer type

  I have successfully used `gtkwave` to view VCD files.

  Currently, there is no way to select signals to be dumped: all signals are
  dumped, which can generate big files.

  It is very unfortunate there is no standard or well-known wave file
  format supporting VHDL types.  If you are aware of such a free format,
  please mail me (:ref:`Reporting_bugs`).

.. option:: --fst=<FILENAME>

  Write the waveforms into a `fst`, that can be displayed by
  `gtkwave`. The `fst` files are much smaller than VCD or
  `GHW` files, but it handles only the same signals as the VCD format.

.. option:: --wave=<FILENAME>

  Write the waveforms into a `ghw` (GHdl Waveform) file.  Currently, all
  the signals are dumped into the waveform file, you cannot select a hierarchy
  of signals to be dumped.

  The format of this file was defined by myself and is not yet completely fixed.
  It may change slightly.  The :samp:`gtkwave` tool can read the GHW files.

  Contrary to VCD files, any VHDL type can be dumped into a GHW file.

.. option:: --psl-report=<FILENAME>

  Write a report for PSL assertions and coverage at the end of
  simulation.  The file is written using the JSON format, but still
  being human readable.

.. option:: --sdf=<PATH>=<FILENAME>

  Do VITAL annotation on `PATH` with SDF file :file:`FILENAME`.

  `PATH` is a path of instances, separated with :samp:`.` or :samp:`/`.
  Any separator can be used.  Instances are component instantiation labels,
  generate labels or block labels.  Currently, you cannot use an indexed name.

  Specifying a delay::

   --sdf=min=<PATH>=<FILENAME>
   --sdf=typ=<PATH>=<FILENAME>
   --sdf=max=<PATH>=<FILENAME>

  If the option contains a type of delay, that is :samp:`min=`,
  :samp:`typ=` or :samp:`max=`, the annotator use respectively minimum,
  typical or maximum values.  If the option does not contain a type of delay,
  the annotator use the typical delay.

  See :ref:`Backannotation`, for more details.

.. option:: --help

  Display a short description of the options accepted by the runtime library.

Debugging VHDL programs
=======================

.. index:: debugging

.. index:: `__ghdl_fatal`

Debugging VHDL programs using `GDB` is possible only on GNU/Linux systems.

`GDB` is a general purpose debugger for programs compiled by `GCC`.
Currently, there is no VHDL support for `GDB`.  It may be difficult
to inspect variables or signals in `GDB`, however, `GDB` is
still able to display the stack frame in case of error or to set a breakpoint
at a specified line.

`GDB` can be useful to precisely catch a runtime error, such as indexing
an array beyond its bounds.  All error check subprograms call the
`__ghdl_fatal` procedure.  Therefore, to catch runtime error, set
a breakpoint like this:

  (gdb) break __ghdl_fatal

When the breakpoint is hit, use the `where` or `bt` command to
display the stack frames.
