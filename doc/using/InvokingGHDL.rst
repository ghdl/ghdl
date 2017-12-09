.. program:: ghdl
.. _USING:Invoking:

Invoking GHDL
#############

The form of the :program:`ghdl` command is ``ghdl command [options...]``. There are multiple available commands, but these general rules apply:

* The first argument selects the command. The options are used to slightly modify the action.
* No option is allowed before the command. Except for the run command, no option is allowed after a filename or a unit name.

.. HINT::
   If the number of options is large and the command line length is beyond the system limit, you can use a response file. An argument that starts with a ``@`` is considered as a response file; it is replaced by arguments read from the file (separated by blanks and end of line).

.. HINT::
   Only the most common commands and options are shown here. For most advanced and experimental features see section :ref:`REF:Command`.

.. WARNING::
   During analysis and elaboration GHDL may read the ``std`` and ``ieee`` files. The location of these files is based on the prefix, which is (in priority order):

	* the :option:`--PREFIX` command line option
	* the :envvar:`GHDL_PREFIX` environment variable
	* a built-in default path. It is a hard-coded path on GNU/Linux, and it corresponds to the value of the ``HKLM\Software\Ghdl\Install_Dir`` registry entry on Windows.

	You should use the :option:`--disp-config` command to display and debug installation problems.

Design building commands
========================

The mostly used commands of GHDL are those to analyze and elaborate a design.


.. index:: cmd analysis

Analysis [``-a``]
---------------------

.. option:: -a <[options...] file...>

Analyzes/compiles one or more files, and creates an object file for each source file. Any argument starting with a dash is an option, the others are filenames. No options are allowed after a filename argument. GHDL analyzes each filename in the given order, and stops the analysis in case of error (remaining files are not analyzed).

See :ref:`GHDL_options`, for details on the GHDL options. For example, to produce debugging information such as line numbers, use: ``ghdl -a -g my_design.vhdl``.


.. index:: cmd elaboration

Elaboration [``-e``]
------------------------

.. option:: -e <[options...] primary_unit [secondary_unit]>

Re-analyzes all the configurations, entities, architectures and package declarations, and creates the default configurations and the default binding indications according to the LRM rules. It also generates the list of objects files required for the executable. Then, it links all these files with the runtime library. The actual elaboration is performed at runtime.

* The elaboration command, :option:`-e`, must be followed by a name of either:

	* a configuration unit
	* an entity unit
	* an entity unit followed by a name of an architecture unit

Name of the units must be a simple name, without any dot.  You can select the name of the `WORK` library with the :option:`--work=NAME` option, as described in :ref:`GHDL_options`. See section ':ref:`Top_entity`', for the restrictions on the root design of a hierarchy.

* If the GCC/LLVM backend was enabled during the compilation of GHDL, the elaboration command creates an executable containing the code of the VHDL sources, the elaboration code and simulation code to execute a design hierarchy. The executable is created in the current directory and the the filename is the name of the primary unit, or for the later case, the concatenation of the name of the primary unit, a dash, and the name of the secondary unit (or architecture). Option :option:`-o` followed by a filename can override the default executable filename.

* If mcode is used, this command elaborates the design but does not generate anything. Since the run command also elaborates the design, this con be skipped.


.. index:: cmd run

Run [``-r``]
----------------

.. option:: -r <[options...] primary_unit [secondary_unit] [simulation_options...]>

Runs/simulates a design. The options and arguments are the same as for the :ref:`elaboration command <Elaboration_command>`.

* GGC/LLVM: simply, the filename of the executable is determined and it is executed. Options are ignored. You may also directly execute the program. The executable must be in the current directory.
* mcode: the design is elaborated and the simulation is launched. As a consequence, you must use the same options used during analysis.

This command exists for three reasons:

* You are using GCC/LLVM, but you don't need to create the executable program name.
* It is coherent with the :option:`-a` and :option:`-e` commands.
* It works with mcode implementation, where the executable code is generated in memory.

See section ':ref:`USING:Simulation`', for details on options.


.. index:: cmd elaborate and run

Elaborate and run [``--elab-run``]
--------------------------------------

.. option:: --elab-run <[elab_options...] primary_unit [secondary_unit] [run_options...]>

Acts like the elaboration command (see :option:`-e`) followed by the run command (see :option:`-r`).


.. index:: cmd checking syntax

Check syntax [``-s``]
-------------------------

.. option:: -s <[options] files>

Analyze files but do not generate code. This command may be used to check the syntax of files. It does not update the library.


.. index:: cmd analyze and elaborate

Analyze and elaborate [``-c``]
----------------------------------

.. option:: -c <[options] file... -<e|r> primary_unit [secondary_unit]>

.. HINT::
   With GCC/LLVM, :option:`-e` should be used, and :option:`-r` with mcode.

The files are first parsed, and then a elaboration is performed, which drives an analysis. Effectively, analysis and elaboration are combined, but there is no explicit call to :option:`-a`. With GCC/LLVM, code is generated during the elaboration. With mcode, the simulation is launched after the elaboration.

All the units of the files are put into the `work` library. But, the work library is neither read from disk nor saved. Therefore, you must give all the files of the `work` library your design needs.

The advantages over the traditional approach (analyze and then elaborate) are:

* The compilation cycle is achieved in one command.
* Since the files are only parsed once, the compilation cycle may be faster.
* You don't need to know an analysis order.
* This command produces smaller executable, since unused units and subprograms do not generate code.

.. HINT::
   However, you should know that currently most of the time is spent in code generation and the analyze and elaborate command generate code for all units needed, even units of ``std`` and ``ieee`` libraries.  Therefore, according to the design, the time for this command may be higher than the time for the analyze command followed by the elaborate command.

.. WARNING::
   This command is still under development. In case of problems, you should go back to the traditional way.


Design rebuilding commands
==========================

Analyzing and elaborating a design consisting in several files can be tricky, due to dependencies. GHDL has a few commands to rebuild a design.


.. index:: cmd importing files

Import [``-i``]
-------------------

.. option:: -i <[options] file...>

All the files specified in the command line are scanned, parsed and added in the libraries but as not yet analyzed. No object files are created. It's purpose is to localize design units in the design files. The make command will then be able to recursively build a hierarchy from an entity name or a configuration name.

.. HINT::

	* Note that all the files are added to the work library. If you have many libraries, you must use the command for each library.
	* Since the files are parsed, there must be correct files. However, since they are not analyzed, many errors are tolerated by this command.

See :option:`-m`, to actually build the design.


.. index:: cmd make

Make [``-m``]
-----------------

.. option:: -m <[options] primary [secondary]>

Analyze automatically outdated files and elaborate a design. The primary unit denoted by the ``primary`` argument must already be known by the system, either because you have already analyzed it (even if you have modified it) or because you have imported it. A file may be outdated because it has been modified (e.g. you just have edited it), or because a design unit contained in the file depends on a unit which is outdated. This rule is of course recursive.

* With option :option:`--bind`, GHDL will stop before the final linking step. This is useful when the main entry point is not GHDL and you're linking GHDL object files into a foreign program.
* With option :option:`-f` (force), GHDL analyzes all the units of the work library needed to create the design hierarchy. Not outdated units are recompiled.  This is useful if you want to compile a design hierarchy with new compilation flags (for example, to add the *-g* debugging option).

The make command will only re-analyze design units in the work library. GHDL fails if it has to analyze an outdated unit from another library.

The purpose of this command is to be able to compile a design without prior knowledge of file order. In the VHDL model, some units must be analyzed before others (e.g. an entity before its architecture). It might be a nightmare to analyze a full design of several files, if you don't have the ordered list of file. This command computes an analysis order.

The make command fails when a unit was not previously parsed. For example, if you split a file containing several design units into several files, you must either import these new files or analyze them so that GHDL knows in which file these units are.

The make command imports files which have been modified. Then, a design hierarchy is internally built as if no units are outdated. Then, all outdated design units, using the dependencies of the design hierarchy, are analyzed. If necessary, the design hierarchy is elaborated.

This is not perfect, since the default architecture (the most recently analyzed one) may change while outdated design files are analyzed. In such a case, re-run the make command of GHDL.


.. index:: cmd generate makefile

Generate Makefile [``--gen-makefile``]
------------------------------------------

.. option:: --gen-makefile <[options] primary [secondary]>

This command works like the make command (see :option:`-m`), but only a makefile is generated on the standard output.

.. index:: --gen-depends command

Generate dependency file command [``--gen-depends``]
-------------------------

.. option:: --gen-depends <[options] primary [secondary]>

Generate a Makefile containing only dependencies to build a design unit.

This command works like the make and gen-makefile commands (see :option:`-m`), but instead of a full makefile only dependencies without rules are generated on the standard output.
Theses rules can then be integrated in another Makefile.

Options
=======

.. index:: IEEE 1164
.. index:: 1164
.. index:: IEEE 1076.3
.. index:: 1076.3

.. HINT:: Besides the options described below, `GHDL` passes any debugging options (those that begin with :option:`-g`) and optimizations options (those that begin with :option:`-O` or :option:`-f`) to `GCC`.  Refer to the `GCC` manual for details.

.. index:: WORK library

.. option:: --work<=NAME>

  Specify the name of the ``WORK`` library.  Analyzed units are always placed in the library logically named ``WORK``.  With this option, you can set its name.  By default, the name is ``work``.

  `GHDL` checks whether ``WORK`` is a valid identifier. Although being more or less supported, the ``WORK`` identifier should not be an extended identifier, since the filesystem may prevent it from correctly working (due to case sensitivity or forbidden characters in filenames).

  `VHDL` rules forbid you to add units to the ``std`` library. Furthermore, you should not put units in the ``ieee`` library.

.. option:: --workdir<=DIR>

  Specify the directory where the ``WORK`` library is located. When this option is not present, the ``WORK`` library is in the current directory.  The object files created by the compiler are always placed in the same directory as the ``WORK`` library.

  Use option :option:`-P` to specify where libraries other than ``WORK`` are placed.

.. option:: --std<=STD>

  Specify the standard to use.  By default, the standard is ``93c``, which means VHDL-93 accepting VHDL-87 syntax.  For details on ``STD`` values see section ':ref:`VHDL_standards`'.

.. option:: --ieee<=VER>

  .. index:: ieee library
  .. index:: synopsys library
  .. index:: mentor library

  Select the ``IEEE`` library to use. ``VER`` must be one of:

  none
    Do not supply an `IEEE` library.  Any library clause with the ``IEEE``
    identifier will fail, unless you have created by your own a library with
    the `IEEE` name.

  standard
    Supply an `IEEE` library containing only packages defined by
    ``ieee`` standards.  Currently, there are the multivalue logic system
    packages ``std_logic_1164`` defined by IEEE 1164, the synthesis
    packages , ``numeric_bit`` and ``numeric_std`` defined by IEEE
    1076.3, and the ``vital`` packages ``vital_timing`` and
    ``vital_primitives``, defined by IEEE 1076.4.  The version of these
    packages is defined by the VHDL standard used.  See section ':ref:`VITAL_packages`',
    for more details.

  synopsys
    Supply the former packages and the following additional packages:
    ``std_logic_arith``, ``std_logic_signed``,
    ``std_logic_unsigned``, ``std_logic_textio``.

    These packages were created by some companies, and are popular.  However
    they are not standard packages, and have been placed in the `IEEE`
    library without the permission from the ``ieee``.

  mentor
    Supply the standard packages and the following additional package:
    ``std_logic_arith``.  The package is a slight variation of a definitely
    not standard but widely mis-used package.

  To avoid errors, you must use the same `IEEE` library for all units of
  your design, and during elaboration.

.. option:: -P<DIRECTORY>

  Add `DIRECTORY` to the end of the list of directories to be searched for
  library files.  A library is searched in `DIRECTORY` and also in
  `DIRECTORY/LIB/vVV` (where `LIB` is the name of the library and `VV`
  the vhdl standard).

  The `WORK` library is always searched in the path specified by the
  :option:`--workdir` option, or in the current directory if the latter
  option is not specified.

.. option:: -fexplicit

  When two operators are overloaded, give preference to the explicit declaration.
  This may be used to avoid the most common pitfall of the ``std_logic_arith``
  package.  See section ':ref:`IEEE_library_pitfalls`', for an example.

.. WARNING:: This option is not set by default. I don't think this option is a good feature, because it breaks the encapsulation rule.  When set, an operator can be silently overridden in another package.  You'd better fix your design and use the ``numeric_std`` package.

.. option:: -frelaxed-rules

  Within an object declaration, allow to reference the name (which references the hidden declaration).  This ignores the error in the following code:

  .. code-block:: VHDL

    package pkg1 is
     type state is (state1, state2, state3);
    end pkg1;

    use work.pkg1.all;
    package pkg2 is
     constant state1 : state := state1;
    end pkg2;

  Some code (such as Xilinx packages) have such constructs, which are valid.

  (The scope of the ``state1`` constant start at the `constant` word. Because the constant ``state1`` and the enumeration literal ``state1`` are homograph, the enumeration literal is hidden in the immediate scope of the constant).

  This option also relaxes the rules about pure functions. Violations result in warnings instead of errors.

.. option:: -fpsl

  Enable parsing of PSL assertions within comments.  See section ':ref:`PSL_implementation`' for more details.

.. option:: --no-vital-checks
.. option:: --vital-checks

  Disable or enable checks of restriction on VITAL units. Checks are enabled by default.

  Checks are performed only when a design unit is decorated by a VITAL attribute. The VITAL attributes are ``VITAL_Level0`` and ``VITAL_Level1``, both declared in the ``ieee.VITAL_Timing`` package.

  Currently, VITAL checks are only partially implemented. See section ':ref:`VHDL_restrictions_for_VITAL`' for more details.

.. option:: --PREFIX<=PATH>

  Use :file:`PATH` as the prefix path to find commands and pre-installed (``std`` and ``ieee``) libraries.

.. option:: -v

  Be verbose. For example, for analysis, elaboration and make commands, GHDL displays the commands executed.


Warnings
========

Some constructions are not erroneous but dubious. Warnings are diagnostic messages that report such constructions. Some warnings are reported only during analysis, others during elaboration.

.. HINT::
   You could disable a warning by using the ``--warn-no-XXX`` or ``-Wno-XX`` instead of ``--warn-XXX`` or ``-WXXX``.

.. option:: --warn-reserved

  Emit a warning if an identifier is a reserved word in a later VHDL standard.

.. option:: --warn-default-binding

  During analyze, warns if a component instantiation has neither configuration specification nor default binding.  This may be useful if you want to detect during analyze possibly unbound component if you don't use configuration. See section ':ref:`VHDL_standards`' for more details about default binding rules.

.. option:: --warn-binding

  During elaboration, warns if a component instantiation is not bound (and not explicitly left unbound).  Also warns if a port of an entity is not bound in a configuration specification or in a component configuration. This warning is enabled by default, since default binding rules are somewhat complex and an unbound component is most often unexpected.

  However, warnings are even emitted if a component instantiation is inside a generate statement. As a consequence, if you use the conditional generate statement to select a component according to the implementation, you will certainly get warnings.

.. option:: --warn-library

  Warns if a design unit replaces another design unit with the same name.

.. option:: --warn-vital-generic

  Warns if a generic name of a vital entity is not a vital generic name.  This
  is set by default.

.. option:: --warn-delayed-checks

  Warns for checks that cannot be done during analysis time and are postponed to elaboration time.  This is because not all procedure bodies are available during analysis (either because a package body has not yet been analysed or because `GHDL` doesn't read not required package bodies).

  These are checks for no wait statement in a procedure called in a sensitized process and checks for pure rules of a function.

.. option:: --warn-body

  Emit a warning if a package body which is not required is analyzed.  If a package does not declare a subprogram or a deferred constant, the package does not require a body.

.. option:: --warn-specs

  Emit a warning if an all or others specification does not apply.

.. option:: --warn-unused

  Emit a warning when a subprogram is never used.

.. option:: --warn-error

  When this option is set, warnings are considered as errors.

.. option:: --warn-nested-comment

  Emit a warning if a ``/*`` appears within a block comment (vhdl 2008).

.. option:: --warn-parenthesis

  Emit a warning in case of weird use of parenthesis

.. option:: --warn-runtime-error

  Emit a warning in case of runtime error that is detected during
  analysis.


Diagnostics Control
===================

.. option:: -fcolor-diagnostics
.. option:: -fno-color-diagnostics

  Control whether diagnostic messages are displayed in color. The default is on when the standard output is a terminal.

.. option:: -fdiagnostics-show-option
.. option:: -fno-diagnostics-show-option

  Control whether the warning option is displayed at the end of warning messages, so that user can easily know how to disable it.


Library commands
================

.. _Create_a_Library:
.. index:: create your own library

A new library is created implicitly, by compiling entities (packages etc.) into it: ``ghdl -a --work=my_custom_lib my_file.vhd``.

A library's source code is usually stored and compiled into its own directory, that you specify with the :option:`--workdir` option: ``ghdl -a --work=my_custom_lib --workdir=my_custom_libdir my_custom_lib_srcdir/my_file.vhd``. See also the :option:`-P` command line option.

Furthermore, GHDL provides a few commands which act on a library:


.. index:: cmd library directory

Directory [``--dir``]
-------------------------

.. option:: --dir <[options] [libs]>

Displays the content of the design libraries (by default the ``work`` library). All options are allowed, but only a few are meaningful: :option:`--work`, :option:`--workdir` and :option:`--std`.


.. index:: cmd library clean

Clean [``--clean``]
-----------------------

.. option:: --clean <[options]>

Try to remove any object, executable or temporary file it could have created. Source files are not removed. The library is kept.


.. index:: cmd library remove

Remove [``--remove``]
-------------------------

.. option:: --remove <[options]>

Do like the clean command but remove the library too. Note that after removing a design library, the files are not
known anymore by GHDL.


.. index:: cmd library copy

Copy [``--copy``]
---------------------

.. option:: --copy <--work=name [options]>

Make a local copy of an existing library.  This is very useful if you want to add unit to the ``ieee`` library:

.. code-block:: shell

  ghdl --copy --work=ieee --ieee=synopsys
  ghdl -a --work=ieee numeric_unsigned.vhd


VPI build commands
==================

These commands simplify the compile and the link of a user vpi module. They are all wrapper: the arguments are in fact a whole command line that is executed with additional switches. Currently a unix-like compiler (like `cc`, `gcc` or `clang`) is expected: the additional switches use their syntax. The only option is `-v` which displays the
command before its execution.


.. index:: cmd VPI compile

compile [``--vpi-compile``]
-------------------------------

.. option:: --vpi-compile <command>

Add include path to the command and execute it::

  ghdl --vpi-compile command

This will execute::

  command -Ixxx/include

For example::

  ghdl --vpi-compile gcc -c vpi1.c

executes::

  gcc -c vpi1.c -fPIC -Ixxx/include

.. _VPI_link_command:

.. index:: cmd VPI link

link [``--vpi-link``]
-------------------------

.. option:: --vpi-link <command>

Add library path and name to the command and execute it::

  ghdl --vpi-link command

This will execute::

  command -Lxxx/lib -lghdlvpi

For example::

  ghdl --vpi-link gcc -o vpi1.vpi vpi1.o

executes::

  gcc -o vpi1.vpi vpi1.o --shared -Lxxx/lib -lghdlvpi


.. _VPI_cflags_command:

.. index:: cmd VPI cflags

cflags [``--vpi-cflags``]
-----------------------------

.. option:: --vpi-cflags

Display flags added by :option:`--vpi-compile`.

.. index:: cmd VPI ldflags

ldflags [``--vpi-ldflags``]
-------------------------------

.. option:: --vpi-ldflags

Display flags added by :option:`--vpi-link`.

.. index:: cmd VPI include dir

include dir [``--vpi-include-dir``]
---------------------------------------

.. option:: --vpi-include-dir

Display the include directory added by the compile flags.

.. index:: cmd VPI library dir

library dir [``--vpi-library-dir``]
---------------------------------------

.. option:: --vpi-library-dir

Display the library directory added by the link flags.


.. _ieee_library_pitfalls:

IEEE library pitfalls
=====================

When you use options :option:`--ieee=synopsys` or :option:`--ieee=mentor`, the ``ieee`` library contains non standard packages such as ``std_logic_arith``. These packages are not standard because there are not described by an IEEE standard, even if they have been put in the `IEEE` library. Furthermore, they are not really de-facto standard, because there are slight differences between the packages of Mentor and those of Synopsys. Furthermore, since they are not well-thought, their use has pitfalls. For example, this description has error during compilation:

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


When you analyze this design, GHDL does not accept it (too long lines
have been split for readability):

.. code-block:: shell

  ghdl -a --ieee=synopsys bad_counter.vhdl
  bad_counter.vhdl:13:14: operator "=" is overloaded
  bad_counter.vhdl:13:14: possible interpretations are:
  ../../libraries/ieee/std_logic_1164.v93:69:5: implicit function "="
      [std_logic_vector, std_logic_vector return boolean]
  ../../libraries/synopsys/std_logic_unsigned.vhdl:64:5: function "="
      [std_logic_vector, std_logic_vector return boolean]
  ../translate/ghdldrv/ghdl: compilation error

Indeed, the `"="` operator is defined in both packages, and both are visible at the place it is used.  The first declaration is an implicit one, which occurs when the `std_logic_vector` type is declared and is an element to element comparison, the second one is an explicit declared function, with the semantic of an unsigned comparison.

With some analyser, the explicit declaration has priority over the implicit declaration, and this design can be analyzed without error.  However, this is not the rule given by the VHDL LRM, and since GHDL follows these rules,
it emits an error.

You can force GHDL to use this rule with the *-fexplicit* option (see :ref:`GHDL_options` for further details). However it is easy to fix this error, by using a selected name:

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

It is better to only use the standard packages defined by IEEE, which provides the same functionalities:

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
   The ``ieee`` math packages (``math_real`` and ``math_complex``) provided with `GHDL` are fully compliant with the `IEEE` standard.
