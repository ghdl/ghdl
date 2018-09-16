.. program:: ghdl
.. _USING:Simulation:

Simulation and runtime
######################

.. _simulation_options:

Simulation options
==================

In most system environments, it is possible to pass options while
invoking a program. Contrary to most programming languages, there is no
standard method in VHDL to obtain the arguments or to set the exit
status.

In GHDL, it is impossible to pass parameters to your design. A later version
could do it through the generic interfaces of the top entity.

However, the GHDL runtime behaviour can be modified with some options; for
example, it is possible to stop simulation after a certain time.

The exit status of the simulation is ``EXIT_SUCCESS`` (0) if the
simulation completes, or ``EXIT_FAILURE`` (1) in case of error
(assertion failure, overflow or any constraint error).

Here is the list of the most useful options. Some debugging options are
also available, but not described here. The :option:`--help` option lists
all options available, including the debugging one.

.. option:: --assert-level<=LEVEL>

  Select the assertion level at which an assertion violation stops the
  simulation. `LEVEL` is the name from the `severity_level`
  enumerated type defined in the `standard` package or the
  ``none`` name.

  By default, only assertion violation of severity level ``failure``
  stops the simulation.

  For example, if `LEVEL` was ``warning``, any assertion violation
  with severity level ``warning``, ``error`` or ``failure`` would
  stop simulation, but the assertion violation at the ``note`` severity
  level would only display a message.

  Option :option:`--assert-level=none` prevents any assertion violation from stopping
  simulation.

.. option:: --ieee-asserts<=POLICY>

  Select how the assertions from ``ieee`` units are
  handled. `POLICY` can be ``enable`` (the default),
  ``disable`` which disables all assertions from ``ieee`` packages
  and ``disable-at-0`` which disables only at the start of simulation.

  This option can be useful to avoid assertion messages from
  ``ieee.numeric_std`` (and other ``ieee`` packages).

.. option:: --stop-time<=TIME>

  Stop the simulation after ``TIME``. ``TIME`` is expressed as a time
  value, *without* any space. The time is the simulation time, not
  the real clock time.

  For example::

    $ ./my_design --stop-time=10ns
    $ ./my_design --stop-time=ps

.. option:: --stop-delta<=N>

  Stop the simulation after `N` delta cycles in the same current
  time.  The default is 5000.

  .. index:: display time

.. option:: --disp-time

  Display the time and delta cycle number as simulation advances.

.. option:: --unbuffered

  Disable buffering on stdout, stderr and files opened in write or append mode (TEXTIO).

.. option:: --sdf<=PATH=FILENAME>

  Do VITAL annotation on `PATH` with SDF file :file:`FILENAME`.

  `PATH` is a path of instances, separated with ``.`` or ``/``.
  Any separator can be used. Instances are component instantiation labels,
  generate labels or block labels. Currently, you cannot use an indexed name.

  Specifying a delay::

   --sdf=min=PATH=FILENAME
   --sdf=typ=PATH=FILENAME
   --sdf=max=PATH=FILENAME

  If the option contains a type of delay, that is ``min=``,
  ``typ=`` or ``max=``, the annotator use respectively minimum,
  typical or maximum values. If the option does not contain a type of delay,
  the annotator uses the typical delay.

  See section :ref:`Backannotation`, for more details.

.. option:: --vpi<=FILENAME>

  Load VPI module.

.. option:: --vpi-trace<=FILE>

  Trace vpi calls to FILE.

.. option:: --help

  Display a short description of the options accepted by the runtime library.

.. _export_waves:

Export waveforms
================

.. option:: --read-wave-opt=<FILENAME>

  Filter signals to be dumped to the wave file according to the wave option
  file provided.

  Here is a description of the wave option file format currently supported ::

     $ version = 1.1  # Optional

     # Path format for signals in packages :
     my_pkg.global_signal_a

     # Path format for signals in entities :
     /top/sub/clk

     # Dump every signal named reset in first level sub entities of top
     /top/*/reset

     # Dump every signal named reset in recursive sub entities of top
     /top/**/reset

     # Dump every signal of sub2 which could be anywhere in the design except
     # on the top level
     /**/sub2/*

     # Dump every signal of sub3 which must be a first level sub entity of the
     # top level
     /*/sub3/*

     # Dump every signal of the first level sub entities of sub3 (but not
     # those of sub3)
     /**/sub3/*/*

.. option:: --write-wave-opt=<FILENAME>

  If the wave option file doesn't exist, creates it with all the signals of
  the design. Otherwise throws an error, because it won't erase an existing
  file.

.. option:: --vcd<=FILENAME>

.. option:: --vcdgz<=FILENAME>

  .. index:: vcd

  .. index:: value change dump

  .. index:: dump of signals

  Option :option:`--vcd` dumps into the VCD file `FILENAME` the signal
  values before each non-delta cycle. If `FILENAME` is ``-``,
  then the standard output is used, otherwise a file is created or
  overwritten.

  The :option:`--vcdgz` option is the same as the *--vcd* option,
  but the output is compressed using the `zlib` (`gzip`
  compression). However, you can't use the ``-`` filename.
  Furthermore, only one VCD file can be written.

  :dfn:`VCD` (value change dump) is a file format defined
  by the `verilog` standard and used by virtually any wave viewer.

  Since it comes from `verilog`, only a few VHDL types can be dumped. GHDL
  dumps only signals whose base type is of the following:

  * types defined in the ``std.standard`` package:

    * ``bit``
    * ``bit_vector``

  * types defined in the ``ieee.std_logic_1164`` package:

    * ``std_ulogic``
    * ``std_logic`` (because it is a subtype of ``std_ulogic``)
    * ``std_ulogic_vector``
    * ``std_logic_vector``

  * any integer type

  I have successfully used `gtkwave` to view VCD files.

  Currently, there is no way to select signals to be dumped: all signals are
  dumped, which can generate big files.

  It is very unfortunate there is no standard or well-known wave file
  format supporting VHDL types. If you are aware of such a free format,
  please mail me (:ref:`Reporting_bugs`).

.. option:: --vcd-nodate

  Do not write date in the VCD file.

.. option:: --fst<=FILENAME>

  Write the waveforms into an `fst` file that can be displayed by
  `gtkwave`. The `fst` files are much smaller than VCD or
  `GHW` files, but it handles only the same signals as the VCD format.

.. option:: --wave<=FILENAME>

  Write the waveforms into a `ghw` (GHdl Waveform) file. Currently, all
  the signals are dumped into the waveform file, you cannot select a hierarchy
  of signals to be dumped.

  The format of this file was defined by myself and is not yet completely fixed.
  It may change slightly. The ``gtkwave`` tool can read the GHW files.

  Contrary to VCD files, any VHDL type can be dumped into a GHW file.

Export hierarchy and references
===============================

.. option:: --disp-tree<[=KIND]>

  .. index:: display design hierarchy

  Display the design hierarchy as a tree of instantiated design entities.
  This may be useful to understand the structure of a complex
  design. `KIND` is optional, but if set must be one of:

  * ``none`` Do not display hierarchy. Same as if the option was not present.

  * ``inst`` Display entities, architectures, instances, blocks and generates statements.

  * ``proc`` Like ``inst`` but also display processes.

  * ``port`` Like ``proc`` but display ports and signals too.
    If `KIND` is not specified, the hierarchy is displayed with the
    ``port`` mode.

.. option:: --no-run

  Stop the simulation before the first cycle. This may be used with :option:`--disp-tree` to display the tree without simulating the whole design. This option actually elaborates the design, so it will catch any bound error in port maps.

.. option:: --xref-html <[options] file...>

  To easily navigate through your sources, you may generate cross-references. This command generates an html file for each ``file`` given in the command line, with syntax highlighting and full cross-reference: every identifier is a link to its declaration. An index of the files is created too.

  The set of ``file`` are analyzed, and then, if the analysis is successful, html files are generated in the directory specified by the :option:`-o <dir>` option, or :file:`html/` directory by default.

  * If the option :option:`--format=html2` is specified, then the generated html files follow the HTML 2.0 standard, and colours are specified with `<FONT>` tags. However, colours are hard-coded.

  * If the option :option:`--format=css` is specified, then the generated html files follow the HTML 4.0 standard, and use the CSS-1 file :file:`ghdl.css` to specify colours. This file is generated only if it does not already exist (it is never overwritten) and can be customized by the user to change colours or appearance. Refer to a generated file and its comments for more information.

.. option:: --psl-report<=FILENAME>

  Write a report for PSL at the end of simulation. For each PSL cover and assert statements, the name, source location and whether it passed or failed is reported. The file is written using the JSON format, but is still human readable.

.. option:: --file-to-xml

  Outputs an XML representation of the decorated syntax tree for the input file and its dependencies. It can be used for VHDL tooling using semantic information, like style checkers, documentation extraction, complexity estimation, etc.

.. WARNING::
   * The AST slightly changes from time to time (particularly when new nodes are added for new language features), so be liberal in what is allowed by your tool. Also, the XML can be quite large so consider it only during prototyping.
   * Note that at this time there is no XML dump of the elaborated design.


.. index:: debugging

Debugging
=========

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

.. option:: --activity<=LEVEL>

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
