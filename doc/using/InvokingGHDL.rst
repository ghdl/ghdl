.. program:: ghdl
.. _USING:Invoking:

Invoking GHDL
#############

The form of the :program:`ghdl` command is ``ghdl command [options...]``. There are multiple available commands, but these
general rules apply:

* The first argument selects the command. The options are used to slightly modify the action.
* No option is allowed before the command. Except for the run command, no option is allowed after a filename or a unit name.

.. HINT::
  If the number of options is large and the command line length is beyond the system limit, you can use a response file. An
  argument that starts with a ``@`` is considered as a response file; it is replaced by arguments read from the file
  (separated by blanks and end of line).

.. HINT::
  Only the most common commands and options are shown here. For the most advanced and experimental features see section
  :ref:`REF:Command`.

.. WARNING::
  During analysis and elaboration GHDL may read the ``std`` and ``ieee`` files. The location of these files is based on the
  prefix, which is (in order of priority):

  * the :option:`--PREFIX` command line option
  * the :envvar:`GHDL_PREFIX` environment variable
  * a built-in default path. It is a hard-coded path on GNU/Linux, and it corresponds to the value of the
    ``HKLM\Software\Ghdl\Install_Dir`` registry entry on Windows.

  You should use the :option:`--disp-config` command to display and debug installation problems.

Design building commands
========================

The most commonly used commands of GHDL are those to analyze and elaborate a design.


.. index:: cmd analysis

.. _Analysis:command:

Analysis [``-a``]
-----------------

.. option:: -a <[options...] file...>

Analyzes/compiles one or more files, and creates an object file for each source file. Any argument starting with a
dash is an option, the others are filenames. No options are allowed after a filename argument. GHDL analyzes each
filename in the given order, and stops the analysis in case of error (remaining files are not analyzed).

See :ref:`GHDL:options`, for details on the GHDL options. For example, to produce debugging information such as line
numbers, use: ``ghdl -a -g my_design.vhdl``.


.. index:: cmd elaboration

.. _Elaboration:command:

Elaboration [``-e``]
--------------------

.. option:: -e <[options...] primary_unit [secondary_unit]>

Re-analyzes all the configurations, entities, architectures and package declarations, and creates the default
configurations and the default binding indications according to the LRM rules. It also generates the list of object
files required for the executable. Then, it links all these files with the runtime library.

* The elaboration command, :option:`-e`, must be followed by a name of either:

  * a configuration unit
  * an entity unit
  * an entity unit followed by a name of an architecture unit

  Name of the units must be a simple name, without any dot. You can select the name of the `WORK` library with the
  :option:`--work=NAME <--work>` option, as described in :ref:`GHDL:options`. See section :ref:`Top_entity`, for the
  restrictions on the root design of a hierarchy.

* If the GCC/LLVM backend was enabled during the compilation of GHDL, the elaboration command creates an executable
  containing the code of the VHDL sources, the elaboration code and simulation code to execute a design hierarchy. The
  executable is created in the current directory and the the filename is the name of the primary unit, or for the latter
  case, the concatenation of the name of the primary unit, a dash, and the name of the secondary unit (or architecture).
  Option ``-o`` followed by a filename can override the default executable filename.

* If mcode is used, this command elaborates the design but does not generate anything. Since the run command also
  elaborates the design, this can be skipped.

  .. WARNING::
    This elaboration command is not a complete elaboration in terms of the VHDL standard. The actual elaboration is
    performed at runtime. Therefore, in order to get a complete VHDL elaboration without running the simulation,
    ``ghdl --elab-run --no-run`` is required. See :option:`--no-run`.


.. index:: cmd run

.. _Run:command:

Run [``-r``]
------------

.. option:: -r <[options...] primary_unit [secondary_unit] [simulation_options...]>

Runs/simulates a design. Two sets of options are accepted, both of them being separated by ``primary_unit [secondary_unit]``.
For the first set, ``options...``, arguments are the same as for the :ref:`elaboration command <Elaboration:command>`. For
the second set, ``simulation_options...``, arguments are defined in :ref:`USING:Simulation`.

* GGC/LLVM: the filename of the executable is determined and it is executed. Elaboration options are ignored. You may also
  directly execute the program. The executable must be in the current directory.
* mcode: the design is elaborated and the simulation is launched. As a consequence, you must use the same options used during
  analysis.

This command exists for three reasons:

* You are using GCC/LLVM, but you don't need to create the executable program name.
* It is coherent with the :option:`-a` and :option:`-e` commands.
* It works with mcode implementation, where the executable code is generated in memory.


.. index:: cmd elaborate and run

.. _elab_and_run:command:

Elaborate and run [``--elab-run``]
----------------------------------

.. option:: --elab-run <[options...] primary_unit [secondary_unit] [simulation_options...]>

Acts like the elaboration command followed by the run command. Note that this command accepts two sets of options. See
:option:`-e`, :option:`-r` and :ref:`USING:Simulation`.


.. index:: cmd checking syntax

Check syntax [``-s``]
---------------------

.. option:: -s <[options] files>

Analyze files but do not generate code. This command may be used to check the syntax of files. It does not update the
library.


.. index:: cmd analyze and elaborate

Analyze and elaborate [``-c``]
------------------------------

.. option:: -c <[options] file... -<e|r> primary_unit [secondary_unit]>

.. HINT::
  With GCC/LLVM, :option:`-e` should be used, and :option:`-r` with mcode.

The files are first parsed, and then a elaboration is performed, which drives an analysis. Effectively, analysis and
elaboration are combined, but there is no explicit call to :option:`-a`. With GCC/LLVM, code is generated during the
elaboration. With mcode, the simulation is launched after the elaboration.

All the units of the files are put into the `work` library. But, the work library is neither read from disk nor saved.
Therefore, you must give all the files of the `work` library your design needs.

The advantages over the traditional approach (analyze and then elaborate) are:

* The compilation cycle is achieved in one command.
* Since the files are only parsed once, the compilation cycle may be faster.
* You don't need to know an analysis order.
* This command produces a smaller executable, since unused units and subprograms do not generate code.

.. HINT::
  However, you should know that most of the time is spent in code generation and the analyze and elaborate command
  generates code for all units needed, even units of ``std`` and ``ieee`` libraries. Therefore, according to the design,
  the time for this command may be higher than the time for the analyze command followed by the elaborate command.

.. WARNING::
  This command is still under development. In case of problems, you should go back to the traditional way.


Design rebuilding commands
==========================

Analyzing and elaborating a design consisting of several files can be tricky, due to dependencies. GHDL has a few commands
to rebuild a design.


.. index:: cmd importing files

.. _Import:command:

Import [``-i``]
---------------

.. option:: -i <[options] file...>

All the files specified in the command line are scanned, parsed and added into the libraries but as not yet analyzed. No
object files are created. Its purpose is to localize design units in the design files. The make command will then be able to
recursively build a hierarchy from an entity name or a configuration name.

.. HINT::
  * Note that all the files are added to the work library. If you have many libraries, you must use the command for each
    library.
  * Since the files are parsed, there must be correct files. However, since they are not analyzed, many errors are tolerated
    by this command.

See :option:`-m`, to actually build the design.


.. index:: cmd make

.. _Make:command:

Make [``-m``]
-------------

.. option:: -m <[options] primary [secondary]>

Analyze automatically outdated files and elaborate a design. The primary unit denoted by the ``primary`` argument must already
be known by the system, either because you have already analyzed it (even if you have modified it) or because you have imported
it. A file may be outdated because it has been modified (e.g. you have just edited it), or because a design unit contained in
the file depends on a unit which is outdated. This rule is of course recursive.

* With option ``--bind``, GHDL will stop before the final linking step. This is useful when the main entry point is not GHDL
  and you're linking GHDL object files into a foreign program.
* With option ``-f`` (force), GHDL analyzes all the units of the work library needed to create the design hierarchy. Outdated
  units are recompiled. This is useful if you want to compile a design hierarchy with new compilation flags (for example, to
  add the ``-g`` debugging option).

The make command will only re-analyze design units in the work library. GHDL fails if it has to analyze an outdated unit from
another library.

The purpose of this command is to be able to compile a design without prior knowledge of file order. In the VHDL model, some
units must be analyzed before others (e.g. an entity before its architecture). It might be a nightmare to analyze a full
design of several files if you don't have the ordered list of files. This command computes an analysis order.

The make command fails when a unit was not previously parsed. For example, if you split a file containing several design
units into several files, you must either import these new files or analyze them so that GHDL knows in which file these
units are.

The make command imports files which have been modified. Then, a design hierarchy is internally built as if no units are
outdated. Then, all outdated design units, using the dependencies of the design hierarchy, are analyzed. If necessary, the
design hierarchy is elaborated.

This is not perfect, since the default architecture (the most recently analyzed one) may change while outdated design files
are analyzed. In such a case, re-run the make command of GHDL.


.. index:: cmd generate makefile

Generate Makefile [``--gen-makefile``]
--------------------------------------

.. option:: --gen-makefile <[options] primary [secondary]>

This command works like the make command (see :option:`-m`), but only a makefile is generated on the standard output.

.. index:: --gen-depends command

Generate dependency file command [``--gen-depends``]
----------------------------------------------------

.. option:: --gen-depends <[options] primary [secondary]>

Generate a Makefile containing only dependencies to build a design unit.

This command works like the make and gen-makefile commands (see :option:`-m`), but instead of a full makefile only
dependencies without rules are generated on the standard output.
Theses rules can then be integrated in another Makefile.

.. _GHDL:options:

Options
=======

.. index:: IEEE 1164
.. index:: 1164
.. index:: IEEE 1076.3
.. index:: 1076.3

.. index:: WORK library

.. option:: --work=<LIB_NAME>

  Specify the name of the ``WORK`` library. Analyzed units are always placed in the library logically named ``WORK``. With
  this option, you can set its name. By default, the name is ``work``.

  `GHDL` checks whether ``WORK`` is a valid identifier. Although being more or less supported, the ``WORK`` identifier
  should not be an extended identifier, since the filesystem may prevent it from working correctly (due to case sensitivity
  or forbidden characters in filenames).

  `VHDL` rules forbid you from adding units to the ``std`` library. Furthermore, you should not put units in the ``ieee`` library.

.. option:: --workdir=<DIR>

  Specify the directory where the ``WORK`` library is located. When this option is not present, the ``WORK`` library is in
  the current directory. The object files created by the compiler are always placed in the same directory as the ``WORK`` library.

  Use option :option:`-P <-P>` to specify where libraries other than ``WORK`` are placed.

.. option:: --std=<STANDARD>

  Specify the standard to use. By default, the standard is ``93c``,
  which means VHDL-93 with relaxed rules. For details on ``STANDARD``
  values see section :ref:`VHDL_standards`.

  .. IMPORTANT:: This option resets the effect of :option:`-frelaxed`, so it should
    be the first option.

.. option:: -fsynopsys

  Allow the use of synopsys non-standard packages
  (``std_logic_arith``, ``std_logic_signed``, ``std_logic_unsigned``,
  ``std_logic_textio``).  These packages are
  present in the ieee library but without this option it's an error to
  use them.

  The synopsys packages were created by some companies, and are popular. However
  they are not standard packages, and have been placed in the `IEEE`
  library without the permission from the ``ieee``.

.. option:: --ieee=<IEEE_VAR>

  .. index:: ieee library
  .. index:: synopsys library

  Select the ``IEEE`` library to use. ``IEEE_VAR`` must be one of:

  none
    Do not supply an `IEEE` library. Any library clause with the ``IEEE``
    identifier will fail, unless you have created your own library with
    the `IEEE` name.

  standard
    Supply an `IEEE` library containing only packages defined by
    ``ieee`` standards. Currently, there are the multivalue logic system
    package ``std_logic_1164`` defined by IEEE 1164, the synthesis
    packages ``numeric_bit`` and ``numeric_std`` defined by IEEE
    1076.3, and the ``vital`` packages ``vital_timing`` and
    ``vital_primitives``, defined by IEEE 1076.4. The version of these
    packages is defined by the VHDL standard used. See section :ref:`VITAL_packages`,
    for more details.

  synopsys
    This option is now deprecated.  It is equivalent to
    ``--ieee=standard`` and ``-fsynopsys``.

  To avoid errors, you must use the same `IEEE` library for all units of
  your design, and during elaboration.

.. option:: -P<DIRECTORY>

  Add `DIRECTORY` to the end of the list of directories to be searched for
  library files. A library is searched in `DIRECTORY` and also in
  `DIRECTORY/LIB/vVV` (where `LIB` is the name of the library and `VV`
  the vhdl standard).

  The `WORK` library is always searched in the path specified by the
  :option:`--workdir` option, or in the current directory if the latter
  option is not specified.

.. option:: -fexplicit

  When two operators are overloaded, give preference to the explicit declaration.
  This may be used to avoid the most common pitfall of the ``std_logic_arith``
  package. See section :ref:`IEEE_library_pitfalls`, for an example.

.. WARNING:: This option is not set by default. I don't think this option is a good feature, because it breaks the
  encapsulation rule. When set, an operator can be silently overridden in another package. You'd do better to fix your
  design and use the ``numeric_std`` package.

.. option:: -frelaxed
.. option:: -frelaxed-rules

  .. IMPORTANT:: The effects of this option are reset by :option:`--std`, so it should be placed *after* that option.

  Slightly relax some rules to be compatible with various other
  simulators or synthesizers:

  * VHDL-87 file declarations are accepted;

  * Default binding indication rules of VHDL-02 are used. Default binding rules
    are often used, but they are particularly obscure before VHDL-02.

  * Within an object declaration, allow references to the name (which references the hidden declaration). This ignores the
    error in the following code:

    .. code-block:: VHDL

      package pkg1 is
        type state is (state1, state2, state3);
      end pkg1;

      use work.pkg1.all;
      package pkg2 is
        constant state1 : state := state1;
      end pkg2;

    Some code (such as Xilinx packages) have such constructs, which are invalid.

    (The scope of the ``state1`` constant starts at the `constant`
    keyword. Because the constant ``state1`` and the enumeration
    literal ``state1`` are homographs, the enumeration literal is
    hidden in the immediate scope of the constant).

  This option also relaxes the rules about pure functions. Violations result in warnings instead of errors.

.. option:: -fpsl

  Enable parsing of PSL assertions within comments. See section :ref:`PSL_implementation` for more details.

.. option:: --mb-comments, -C

  Allow UTF8 or multi-bytes chars in a comment.

  According to the VHDL standards before 2002, the only characters
  allowed in a source file (and that includes the comments) are the
  graphical characters of the ISO 8859-1 character set.  This is
  incompatible with comments using UTF-8 or some other encoding.  This
  option lift this restriction.

.. option:: --syn-binding

  Use synthesizer rules for component binding. During elaboration, if
  a component is not bound to an entity using VHDL LRM rules, try to
  find in any known library an entity whose name is the same as the
  component name.

  This rule is known as the synthesizer rule.

  There are two key points: normal VHDL LRM rules are tried first and
  entities are searched only in known libraries. A known library is a
  library which has been named in your design.

  This option is only useful during elaboration.

.. option:: --format=<FORMAT>

  Define the output format of some options, such as :option:`--pp-html` or :option:`--xref-html`.

  * By default or when :option:`--format=html2 <--format>` is specified, generated files follow the HTML 2.0 standard, and
    colours are specified with `<FONT>` tags. However, colours are hard-coded.

  * If :option:`--format=css <--format>` is specified, generated files follow the HTML 4.0 standard, and use the CSS-1 file
    :file:`ghdl.css` to specify colours. This file is generated only if it does not already exist (it is never overwritten)
    and can be customized by the user to change colours or appearance. Refer to a generated file and its comments for more
    information.

.. option:: --no-vital-checks
.. option:: --vital-checks

  Disable or enable checks of restriction on VITAL units. Checks are enabled by default.

  Checks are performed only when a design unit is decorated by a VITAL attribute. The VITAL attributes are ``VITAL_Level0``
  and ``VITAL_Level1``, both declared in the ``ieee.VITAL_Timing`` package.

  Currently, VITAL checks are only partially implemented. See section :ref:`VHDL_restrictions_for_VITAL` for more details.

.. option:: --PREFIX=<PATH>

  Use :file:`PATH` as the prefix path to find commands and pre-installed (``std`` and ``ieee``) libraries.

.. option:: -v

  Be verbose. For example, for analysis, elaboration and make commands, GHDL displays the commands executed.

.. option:: -o=<FNAME>

  All the commands that perform a link (:option:`-e`, :option:`--elab-run`, :option:`--link`, :option:`-c`, :option:`-m`,
  etc.) support overriding the location and name of the generated artifact.

.. option:: --time-resolution=<UNIT>

  .. ATTENTION::
    This feature is supported with *mcode* backend only. It is not possible to support it with either LLVM or GCC backends,
    because it needs to apply globally.

  Set the base time resolution of the simulation. This option is supported in commands :option:`-a` and :option:`-r` only.
  Allowed values are ``auto`` (default), ``fs``, ``ps``, ``ns``, ``us``, ``ms`` or ``sec``. With LLVM/GCC, the value is fixed
  to ``fs``.

  .. HINT::
    When overriding the time resolution, all the time units that are used in the design must be larger. Using units below
    the resolution will produce a failure.


Warnings
========

Some constructions are not erroneous but dubious. Warnings are diagnostic messages that report such constructions. Some
warnings are reported only during analysis, others during elaboration.

.. HINT::
  You could disable a warning by using the ``--warn-no-XXX`` or ``-Wno-XXX`` instead of ``--warn-XXX`` or ``-WXXX``.

.. HINT::
  The warnings ``-Wbinding``, ``-Wlibrary``, ``-Wshared``,
  ``-Wpure``, ``-Wspecs``, ``-Whide``, ``-Wport`` are enabled by
  default.

.. option:: --warn-library

  Warns if a design unit replaces another design unit with the same name.

.. option:: --warn-default-binding

  During analyze, warns if a component instantiation has neither configuration specification nor default binding. This may
  be useful if you want to detect during analyze possibly unbound components if you don't use configuration. See section
  :ref:`VHDL_standards` for more details about default binding rules.

.. option:: --warn-binding

  During elaboration, warns if a component instantiation is not bound (and not explicitly left unbound). Also warns if a
  port of an entity is not bound in a configuration specification or in a component configuration. This warning is enabled
  by default, since default binding rules are somewhat complex and an unbound component is most often unexpected.

  However, warnings are still emitted if a component instantiation is inside a generate statement. As a consequence, if you
  use the conditional generate statement to select a component according to the implementation, you will certainly get
  warnings.

.. option:: --warn-reserved

  Emit a warning if an identifier is a reserved word in a later VHDL standard.

.. option:: --warn-nested-comment

  Emit a warning if a ``/*`` appears within a block comment (vhdl 2008).

.. option:: --warn-parenthesis

  Emit a warning in case of weird use of parentheses.

.. option:: --warn-vital-generic

  Warns if a generic name of a vital entity is not a vital generic name. This
  is set by default.

.. option:: --warn-delayed-checks

  Warns for checks that cannot be done during analysis time and are postponed to elaboration time. This is because not all
  procedure bodies are available during analysis (either because a package body has not yet been analysed or because `GHDL`
  doesn't read not required package bodies).

  These are checks for no wait statements in a procedure called in a sensitized process and checks for pure rules of a function.

.. option:: --warn-body

  Emit a warning if a package body which is not required is analyzed. If a package does not declare a subprogram or a
  deferred constant, the package does not require a body.

.. option:: --warn-specs

  Emit a warning if an all or others specification does not apply.

.. option:: --warn-runtime-error

  Emit a warning in case of runtime error that is detected during
  analysis.

.. option:: --warn-shared

  Emit a warning when a shared variable is declared and its type it
  not a protected type.

.. option:: --warn-hide

  Emit a warning when a declaration hides a previous hide.

.. option:: --warn-unused

  Emit a warning when a subprogram is never used.

.. option:: --warn-others

  Emit a warning is an `others` choice is not required because all the
  choices have been explicitly covered.

.. option:: --warn-pure

  Emit a warning when a pure rules is violated (like declaring a pure
  function with access parameters).

.. option:: --warn-static

  Emit a warning when a non-static expression is used at a place where
  the standard requires a static expression.

.. option:: --warn-error

  When this option is set, warnings are considered as errors.


Diagnostics Control
===================

.. option:: -fcolor-diagnostics
.. option:: -fno-color-diagnostics

  Control whether diagnostic messages are displayed in color. The default is on when the standard output is a terminal.

.. option:: -fdiagnostics-show-option
.. option:: -fno-diagnostics-show-option

  Control whether the warning option is displayed at the end of warning messages, so that the user can easily know how to
  disable it.

.. option:: -fcaret-diagnostics
.. option:: -fno-caret-diagnostics

  Control whether the source line of the error is displayed with a
  caret indicating the column of the error.


Library commands
================

.. _Create_a_Library:
.. index:: create your own library

A new library is created implicitly, by compiling entities (packages etc.) into it: ``ghdl -a --work=my_custom_lib my_file.vhdl``.

A library's source code is usually stored and compiled into its own directory, that you specify with the :option:`--workdir`
option: ``ghdl -a --work=my_custom_lib --workdir=my_custom_libdir my_custom_lib_srcdir/my_file.vhdl``. See also the
:option:`-P <-P>` command line option.

Furthermore, GHDL provides a few commands which act on a library:


.. index:: cmd library directory

.. option:: --dir <[options] [libs]>

Displays the content of the design libraries (by default the ``work`` library). All options are allowed, but only a few are
meaningful: :option:`--work`, :option:`--workdir` and :option:`--std`.


.. index:: cmd library clean

.. _Clean:command:

.. option:: --clean <[options]>

Try to remove any object, executable or temporary file it could have created. Source files are not removed. The library is kept.


.. index:: cmd library remove

.. _Remove:command:

.. option:: --remove <[options]>

Acts like the clean command but removes the library too. Note that after removing a design library, the files are not
known anymore by GHDL.


.. index:: cmd library copy

.. option:: --copy <--work=name [options]>

Make a local copy of an existing library. This is very useful if you want to add units to the ``ieee`` library:

.. code-block:: shell

  ghdl --copy --work=ieee --ieee=synopsys
  ghdl -a --work=ieee numeric_unsigned.vhd

.. _VPI_build_commands:

VPI build commands
==================

These commands simplify the compile and the link of a user vpi module. They are all wrappers: the arguments are in fact a
whole command line that is executed with additional switches. Currently a unix-like compiler (like `cc`, `gcc` or `clang`)
is expected: the additional switches use their syntax. The only option is `-v` which displays the command before its
execution.


.. index:: cmd VPI compile

.. option:: --vpi-compile <command>

Add an include path to the command and execute it::

  ghdl --vpi-compile command

This will execute::

  command -Ixxx/include

For example, ``ghdl --vpi-compile gcc -c vpi1.c`` executes ``gcc -c vpi1.c -fPIC -Ixxx/include``.


.. _VPI_link_command:

.. index:: cmd VPI link

.. option:: --vpi-link <command>

Add a library path and name to the command and execute it::

  ghdl --vpi-link command

This will execute::

  command -Lxxx/lib -lghdlvpi

For example, ``ghdl --vpi-link gcc -o vpi1.vpi vpi1.o`` executes ``gcc -o vpi1.vpi vpi1.o --shared -Lxxx/lib -lghdlvpi``.


.. _VPI_cflags_command:

.. index:: cmd VPI cflags

.. option:: --vpi-cflags

Display flags added by :option:`--vpi-compile`.


.. index:: cmd VPI ldflags

.. option:: --vpi-ldflags

Display flags added by :option:`--vpi-link`.


.. index:: cmd VPI include dir

.. option:: --vpi-include-dir

Display the include directory added by the compile flags.


.. index:: cmd VPI library dir

.. option:: --vpi-library-dir

Display the library directory added by the link flags.

.. option:: --vpi-library-dir-unix

Display the library directory added by the link flags, forcing UNIX syntax.

.. _ieee_library_pitfalls:

IEEE library pitfalls
=====================

When you use options :option:`--ieee=synopsys <--ieee>`, the ``ieee``
library contains non standard packages such as
``std_logic_arith``. These packages are not standard because there are
not described by an IEEE standard, even if they have been put in the
`IEEE` library. Furthermore, they are not really de-facto standard,
because there are slight differences between the packages of Mentor
and those of Synopsys. Furthermore, since they are not well thought
out, their use has pitfalls. For example, this description has an
error during compilation:

.. code-block:: VHDL

  library ieee;
  use ieee.std_logic_1164.all;

  --  A counter from 0 to 10.
  entity counter is
    port (val : out std_logic_vector (3 downto 0);
          ck : std_logic;
          rst : std_logic);
  end counter;

  library ieee;
  use ieee.std_logic_unsigned.all;

  architecture bad of counter
  is
    signal v : std_logic_vector (3 downto 0);
  begin
    process (ck, rst)
    begin
      if rst = '1' then
        v <= x"0";
      elsif rising_edge (ck) then
        if v = "1010" then -- Error
          v <= x"0";
        else
          v <= v + 1;
        end if;
      end if;
    end process;

    val <= v;
  end bad;


When you analyze this design, GHDL does not accept it (two long lines have been split for readability):

.. code-block:: shell

  ghdl -a --ieee=synopsys bad_counter.vhdl
  bad_counter.vhdl:13:14: operator "=" is overloaded
  bad_counter.vhdl:13:14: possible interpretations are:
  ../../libraries/ieee/std_logic_1164.v93:69:5: implicit function "="
      [std_logic_vector, std_logic_vector return boolean]
  ../../libraries/synopsys/std_logic_unsigned.vhdl:64:5: function "="
      [std_logic_vector, std_logic_vector return boolean]
  ../translate/ghdldrv/ghdl: compilation error

Indeed, the `"="` operator is defined in both packages, and both are visible at the place it is used. The first declaration
is an implicit one, which occurs when the `std_logic_vector` type is declared and is an element to element comparison. The
second one is an explicit declared function, with the semantics of an unsigned comparison.

With some analysers, the explicit declaration has priority over the implicit declaration, and this design can be analyzed
without error. However, this is not the rule given by the VHDL LRM, and since GHDL follows these rules, it emits an error.

You can force GHDL to use this rule with the *-fexplicit* option (see :ref:`GHDL:options` for further details). However it
is easy to fix this error, by using a selected name:

.. code-block:: VHDL

  library ieee;
  use ieee.std_logic_unsigned.all;

  architecture fixed_bad of counter
  is
    signal v : std_logic_vector (3 downto 0);
  begin
    process (ck, rst)
    begin
      if rst = '1' then
        v <= x"0";
      elsif rising_edge (ck) then
        if ieee.std_logic_unsigned."=" (v, "1010") then
          v <= x"0";
        else
          v <= v + 1;
        end if;
      end if;
    end process;

    val <= v;
  end fixed_bad;

It is better to only use the standard packages defined by IEEE, which provide the same functionalities:

.. code-block:: VHDL

  library ieee;
  use ieee.numeric_std.all;

  architecture good of counter
  is
    signal v : unsigned (3 downto 0);
  begin
    process (ck, rst)
    begin
      if rst = '1' then
        v <= x"0";
      elsif rising_edge (ck) then
        if v = "1010" then
          v <= x"0";
        else
          v <= v + 1;
        end if;
      end if;
    end process;

    val <= std_logic_vector (v);
  end good;

.. index:: Math_Real

.. index:: Math_Complex

.. HINT::
  The ``ieee`` math packages (``math_real`` and ``math_complex``) provided with `GHDL` are fully compliant with the `IEEE`
  standard.
