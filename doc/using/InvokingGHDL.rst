.. _USING:Invoking:
.. program:: ghdl

*************
Invoking GHDL
*************

The form of the :program:`ghdl` command is::

  ghdl command [options...]

The GHDL program has several commands.  The first argument selects
the command.  The options are used to slightly modify the action.

No option is allowed before the command.  Except for the run command,
no option is allowed after a filename or a unit name.

If the number of options is large and the command line length is
beyond the system limit, you can use a response file. An argument that
starts with a :samp:`@` is considered as a response file; it is replaced
by arguments read from the file (separated by blanks and end of line).

Design building commands
=================

The mostly used commands of GHDL are those to analyze and elaborate a design.

Analysis command
----------------

.. index:: analysis

.. index:: -a command

Analyze one or several files::

  ghdl -a [options...] file...

The analysis command compiles one or more files, and creates an
object file for each source file.  The analysis command is selected with
:option:`-a` switch.  Any argument starting with a dash is an option, the
others are filenames.  No options are allowed after a filename
argument. GHDL analyzes each filename in the given order, and stops the
analysis in case of error (the following files are not analyzed).

See :ref:`GHDL_options`, for details on the GHDL options.  For example,
to produce debugging information such as line numbers, use::

  ghdl -a -g my_design.vhdl


.. _Elaboration_command:

Elaboration command
-------------------

.. index:: elaboration

.. index:: -e command

Elaborate a design::

  ghdl -e [options..] primary_unit [secondary_unit]


On GNU/Linux, if the GCC backend was enabled during the compilation of `GHDL`,
the elaboration command creates an executable containing the code of the `VHDL`
sources, the elaboration code and simulation code to execute a design
hierarchy. The executable is created in the current directory.
On Windows or if the GCC backend was not enabled, this command elaborates the design
but does not generate anything.

The elaboration command is selected with :option:`-e` switch, and must be
followed by either:

* a name of a configuration unit
* a name of an entity unit
* a name of an entity unit followed by a name of an architecture unit

Name of the units must be a simple name, without any dot.  You can
select the name of the `WORK` library with the :option:`--work=NAME`
option, as described in :ref:`GHDL_options`.

See :ref:`Top_entity`, for the restrictions on the root design of a
hierarchy.

On GNU/Linux the filename of the executable is the name of the
primary unit, or for the later case, the concatenation of the name of
the primary unit, a dash, and the name of the secondary unit (or
architecture).  On Windows there is no executable generated.

The :option:`-o` followed by a filename can override the default
executable filename.

For the elaboration command, `GHDL` re-analyzes all the
configurations, entities, architectures and package declarations, and
creates the default configurations and the default binding indications
according to the LRM rules.  It also generates the list of objects files
required for the executable.  Then, it links all these files with the
runtime library.

The actual elaboration is performed at runtime.

On Windows this command can be skipped because it is also done by the
run command.

.. _Run_command:

Run command
-----------

.. index:: run

.. index:: -r command

Run (or simulate) a design::

  ghdl -r [options...] primary_unit [secondary_unit] [simulation_options...]


The options and arguments are the same as for the elaboration command, :ref:`Elaboration_command`.

On GNU/Linux this command simply determines the filename of the executable
and executes it.  Options are ignored. You may also directly execute
the program. The executable must be in the current directory.

This command exists for three reasons:

* You don't have to create the executable program name.
* It is coherent with the :option:`-a` and :option:`-e` commands.
* It works with the Windows implementation, where the code is generated in
  memory.

On Windows this command elaborates and launches the simulation.  As a consequence
you must use the same options used during analysis.

See :ref:`Simulation_and_runtime`, for details on options.

Elaborate and run command
-------------------------

.. index:: elaborate and run

.. index:: --elab-run command

Elaborate and then simulate a design unit::

  ghdl --elab-run [elab_options...] primary_unit [secondary_unit] [run_options...]


This command acts like the elaboration command (see :ref:`Elaboration_command`)
followed by the run command (see :ref:`Run_command`).

.. _Bind_command:

Bind command
------------

.. index:: binding

.. index:: --bind command

Bind a design unit and prepare the link step::

  ghdl --bind [options] primary_unit [secondary_unit]


This command is only available on GNU/Linux.

This performs only the first stage of the elaboration command; the list
of objects files is created but the executable is not built.  This
command should be used only when the main entry point is not ghdl.

.. _Link_command:

Link command
------------

.. index:: linking

.. index:: --link command

Link an already bound design unit::

  ghdl --link [options] primary_unit [secondary_unit]

This performs only the second stage of the elaboration command: the
executable is created by linking the files of the object files list.
This command is available only for completeness.  The elaboration command is
equivalent to the bind command followed by the link command.

.. _List_link_command:

List link command
-----------------

.. index:: --list-link command

Display files which will be linked::

  ghdl --list-link primary_unit [secondary_unit]

This command is only available on GNU/Linux.

This command may be used only after a bind command.  GHDL displays all
the files which will be linked to create an executable.  This command is
intended to add object files in a link of a foreign program.

.. _Check_syntax_command:

Check syntax command
--------------------

.. index:: checking syntax

.. index:: -s command

Analyze files but do not generate code::

  ghdl -s [options] files

This command may be used to check the syntax of files.  It does not update
the library.

.. _Analyze_and_elaborate_command:

Analyze and elaborate command
-----------------------------

.. index:: Analyze and elaborate command

.. index:: -c command

Analyze files and elaborate them at the same time.

On GNU/Linux::

  ghdl -c [options] file... -e primary_unit [secondary_unit]


On Windows::

  ghdl -c [options] file... -r primary_unit [secondary_unit]


This command combines analysis and elaboration: files are analyzed and
the unit is then elaborated.  However, code is only generated during the
elaboration.  On Windows the simulation is launched.

To be more precise, the files are first parsed, and then the elaboration
drives the analysis.  Therefore, there is no analysis order, and you don't
need to care about it.

All the units of the files are put into the `work` library.  But, the
work library is neither read from disk nor saved.  Therefore, you must give
all the files of the `work` library your design needs.

The advantages over the traditional approach (analyze and then elaborate) are:

* The compilation cycle is achieved in one command.
* Since the files are only parsed once, the compilation cycle may be faster.
* You don't need to know an analysis order
* This command produces smaller executable, since unused units and subprograms
  do not generate code.

However, you should know that currently most of the time is spent in code
generation and the analyze and elaborate command generate code for all units
needed, even units of :samp:`std` and :samp:`ieee` libraries.  Therefore,
according to the design, the time for this command may be higher than the time
for the analyze command followed by the elaborate command.

This command is still experimental.  In case of problems, you should go back
to the traditional way.

.. _GHDL_Options:

GHDL options
============

.. index:: IEEE 1164

.. index:: 1164

.. index:: IEEE 1076.3

.. index:: 1076.3

Besides the options described below, `GHDL` passes any debugging options
(those that begin with :option:`-g`) and optimizations options (those that
begin with :option:`-O` or :option:`-f`) to `GCC`.  Refer to the `GCC`
manual for details.



.. option::--work=<NAME>

  .. index:: WORK library

  Specify the name of the :samp:`WORK` library.  Analyzed units are always
  placed in the library logically named :samp:`WORK`.  With this option,
  you can set its name.  By default, the name is :samp:`work`.

  `GHDL` checks whether :samp:`WORK` is a valid identifier.  Although being
  more or less supported, the :samp:`WORK` identifier should not be an
  extended identifier, since the filesystem may prevent it from correctly
  working (due to case sensitivity or forbidden characters in filenames).

  `VHDL` rules forbid you to add units to the :samp:`std` library.
  Furthermore, you should not put units in the :samp:`ieee` library.


.. option:: --workdir=<DIR>

  Specify the directory where the :samp:`WORK` library is located.  When this
  option is not present, the :samp:`WORK` library is in the current
  directory.  The object files created by the compiler are always placed
  in the same directory as the :samp:`WORK` library.

  Use option :option:`-P` to specify where libraries other than :samp:`WORK`
  are placed.


.. option:: --std=<STD>

  Specify the standard to use.  By default, the standard is :samp:`93c`, which
  means VHDL-93 accepting VHDL-87 syntax.  For details on :samp:`STD` values see
  :ref:`VHDL_standards`.


.. option:: --ieee=<VER>

  .. index:: ieee library
  .. index:: synopsys library
  .. index:: mentor library

  Select the :samp:`IEEE` library to use. :samp:`VER` must be one of:

  none
    Do not supply an `IEEE` library.  Any library clause with the :samp:`IEEE`
    identifier will fail, unless you have created by your own a library with
    the `IEEE` name.

  standard
    Supply an `IEEE` library containing only packages defined by
    :samp:`ieee` standards.  Currently, there are the multivalue logic system
    packages :samp:`std_logic_1164` defined by IEEE 1164, the synthesis
    packages , :samp:`numeric_bit` and :samp:`numeric_std` defined by IEEE
    1076.3, and the :samp:`vital` packages :samp:`vital_timing` and
    :samp:`vital_primitives`, defined by IEEE 1076.4.  The version of these
    packages is defined by the VHDL standard used.  See :ref:`VITAL_packages`,
    for more details.

  synopsys
    Supply the former packages and the following additional packages:
    :samp:`std_logic_arith`, :samp:`std_logic_signed`,
    :samp:`std_logic_unsigned`, :samp:`std_logic_textio`.

    These packages were created by some companies, and are popular.  However
    they are not standard packages, and have been placed in the `IEEE`
    library without the permission from the :samp:`ieee`.

  mentor
    Supply the standard packages and the following additional package:
    :samp:`std_logic_arith`.  The package is a slight variation of a definitely
    not standard but widely mis-used package.

  To avoid errors, you must use the same `IEEE` library for all units of
  your design, and during elaboration.


.. option:: -P<DIRECTORY>

  Add `DIRECTORY` to the end of the list of directories to be searched for
  library files.  A library is searched in `DIRECTORY` and also in
  `DIRECTORY/LIB/vVV` (where `LIB` is the name of the library and `VV`
  the vhdl standard).

  The `WORK` library is always searched in the path specified by the
  :option:`--workdir=` option, or in the current directory if the latter
  option is not specified.


.. option:: -fexplicit

  When two operators are overloaded, give preference to the explicit declaration.
  This may be used to avoid the most common pitfall of the :samp:`std_logic_arith`
  package.  See :ref:`IEEE_library_pitfalls`, for an example.

  This option is not set by default.  I don't think this option is a
  good feature, because it breaks the encapsulation rule.  When set, an
  operator can be silently overridden in another package.  You'd better to fix
  your design and use the :samp:`numeric_std` package.


.. option:: -frelaxed-rules

  Within an object declaration, allow to reference the name (which
  references the hidden declaration).  This ignores the error in the
  following code:

  .. code-block:: VHDL

    package pkg1 is
     type state is (state1, state2, state3);
    end pkg1;

    use work.pkg1.all;
    package pkg2 is
     constant state1 : state := state1;
    end pkg2;

  Some code (such as Xilinx packages) have such constructs, which
  are valid.

  (The scope of the :samp:`state1` constant start at the `constant`
  word. Because the constant :samp:`state1` and the enumeration literal
  :samp:`state1` are homograph, the enumeration literal is hidden in the
  immediate scope of the constant).

  This option also relaxes the rules about pure functions.  Violations
  result in warnings instead of errors.


.. option:: -fpsl

  Enable parsing of PSL assertions within comments.  See :ref:`PSL_implementation`,
  for more details.


.. option:: --no-vital-checks
.. option:: --vital-checks

  Disable or enable checks of restriction on VITAL units.  Checks are enabled
  by default.

  Checks are performed only when a design unit is decorated by a VITAL attribute.
  The VITAL attributes are :samp:`VITAL_Level0` and :samp:`VITAL_Level1`, both
  declared in the :samp:`ieee.VITAL_Timing` package.

  Currently, VITAL checks are only partially implemented.  See
  :ref:`VHDL_restrictions_for_VITAL`, for more details.


.. option:: --syn-binding

  Use synthesizer rules for component binding.  During elaboration, if a
  component is not bound to an entity using VHDL LRM rules, try to find
  in any known library an entity whose name is the same as the component
  name.

  This rule is known as synthesizer rule.

  There are two key points: normal VHDL LRM rules are tried first and
  entities are searched only in known library.  A known library is a
  library which has been named in your design.

  This option is only useful during elaboration.


.. option:: --PREFIX=<PATH>

  Use :file:`PATH` as the prefix path to find commands and pre-installed (std and
  ieee) libraries.


.. option:: --GHDL1=<COMMAND>

  Use :samp:`COMMAND` as the command name for the compiler.  If :samp:`COMMAND` is
  not a path, then it is searched in the path.


.. option:: --AS=<COMMAND>

  Use :samp:`COMMAND` as the command name for the assembler.  If :samp:`COMMAND` is
  not a path, then it is searched in the path.  The default is :samp:`as`.


.. option:: --LINK=<COMMAND>

  Use :samp:`COMMAND` as the linker driver.  If :samp:`COMMAND` is
  not a path, then it is searched in the path.  The default is :samp:`gcc`.


.. option:: -v

  Be verbose.  For example, for analysis, elaboration and make commands, GHDL
  displays the commands executed.


Passing options to other programs
=================================

These options are only available on GNU/Linux.

For many commands, `GHDL` acts as a driver: it invokes programs to perform
the command.  You can pass arbitrary options to these programs.

Both the compiler and the linker are in fact GCC programs.  See the
GCC manual for details on GCC options.



.. option:: -Wc,<OPTION>

  Pass `OPTION` as an option to the compiler.


.. option:: -Wa,<OPTION>

  Pass `OPTION` as an option to the assembler.


.. option:: -Wl,<OPTION>

  Pass `OPTION` as an option to the linker.

GHDL Diagnostics Control
========================

.. option:: -fcolor-diagnostics
.. option:: -fno-color-diagnostics

  Control whether diagnostic messages are displayed in color.  The
  default is on when the standard output is a terminal.

.. option:: -fdiagnostics-show-option
.. option:: -fno-diagnostics-show-option

  Control whether the warning option is displayed at the end of
  warning messages, so that user can easily know how to disable it.


GHDL warnings
=============

Some constructions are not erroneous but dubious.  Warnings are diagnostic
messages that report such constructions.  Some warnings are reported only
during analysis, others during elaboration.

You could disable a warning by using the :samp:`--warn-no-XXX` or
:samp:`-Wno-XX` instead of :samp:`--warn-XXX` or :samp:`-WXXX`.


.. option:: --warn-reserved

  Emit a warning if an identifier is a reserved word in a later VHDL standard.


.. option:: --warn-default-binding

  During analyze, warns if a component instantiation has neither
  configuration specification nor default binding.  This may be useful if you
  want to detect during analyze possibly unbound component if you don't use
  configuration.  :ref:`VHDL_standards`, for more details about default binding
  rules.


.. option:: --warn-binding

  During elaboration, warns if a component instantiation is not bound
  (and not explicitly left unbound).  Also warns if a port of an entity
  is not bound in a configuration specification or in a component
  configuration.  This warning is enabled by default, since default
  binding rules are somewhat complex and an unbound component is most
  often unexpected.

  However, warnings are even emitted if a component instantiation is
  inside a generate statement.  As a consequence, if you use the conditional
  generate statement to select a component according to the implementation,
  you will certainly get warnings.


.. option:: --warn-library

  Warns if a design unit replaces another design unit with the same name.


.. option:: --warn-vital-generic

  Warns if a generic name of a vital entity is not a vital generic name.  This
  is set by default.


.. option:: --warn-delayed-checks

  Warns for checks that cannot be done during analysis time and are
  postponed to elaboration time.  This is because not all procedure
  bodies are available during analysis (either because a package body
  has not yet been analysed or because `GHDL` doesn't read not required
  package bodies).

  These are checks for no wait statement in a procedure called in a
  sensitized process and checks for pure rules of a function.


.. option:: --warn-body

  Emit a warning if a package body which is not required is analyzed.  If a
  package does not declare a subprogram or a deferred constant, the package
  does not require a body.


.. option:: --warn-specs

  Emit a warning if an all or others specification does not apply.


.. option:: --warn-unused

  Emit a warning when a subprogram is never used.


.. option:: --warn-error

  When this option is set, warnings are considered as errors.


.. option:: --warn-nested-comment

  Emit a warning if a :samp:`/*` appears within a block comment (vhdl 2008).


.. option:: --warn-parenthesis

  Emit a warning in case of weird use of parenthesis


.. option:: --warn-runtime-error

  Emit a warning in case of runtime error that is detected during
  analysis.


Rebuilding commands
===================

Analyzing and elaborating a design consisting in several files can be tricky,
due to dependencies.  GHDL has a few commands to rebuild a design.

Import command
--------------

.. index:: importing files

.. index:: -i command

Add files in the work design library::

  ghdl -i [options] file...


All the files specified in the command line are scanned, parsed and added in
the libraries but as not yet analyzed.  No object files are created.

The purpose of this command is to localize design units in the design files.
The make command will then be able to recursively build a hierarchy from
an entity name or a configuration name.

Since the files are parsed, there must be correct files.  However, since they
are not analyzed, many errors are tolerated by this command.

Note that all the files are added to the work library.  If you have many
libraries, you must use the command for each library.

See :ref:`Make_command`, to actually build the design.

.. _Make_command:

Make command
------------

.. index:: make

.. index:: -m command


Analyze automatically outdated files and elaborate a design::

  ghdl -m [options] primary [secondary]


The primary unit denoted by the :samp:`primary` argument must already be
known by the system, either because you have already analyzed it (even
if you have modified it) or because you have imported it.  GHDL analyzes
all outdated files.  A file may be outdated because it has been modified
(e.g. you just have edited it), or because a design unit contained in
the file depends on a unit which is outdated.  This rule is of course
recursive.

With the @code{-b} (bind only) option, GHDL will stop before the final linking
step. This is useful when the main entry point is not GHDL and you're linking
GHDL object files into a foreign program.

With the :option:`-f` (force) option, GHDL analyzes all the units of the
work library needed to create the design hierarchy.  Not outdated units
are recompiled.  This is useful if you want to compile a design hierarchy
with new compilation flags (for example, to add the *-g*
debugging option).

The make command will only re-analyze design units in the work library.
GHDL fails if it has to analyze an outdated unit from another library.

The purpose of this command is to be able to compile a design without prior
knowledge of file order.  In the VHDL model, some units must be analyzed
before others (e.g. an entity before its architecture).  It might be a
nightmare to analyze a full design of several files, if you don't have
the ordered list of file.  This command computes an analysis order.

The make command fails when a unit was not previously parsed.  For
example, if you split a file containing several design units into
several files, you must either import these new files or analyze them so
that GHDL knows in which file these units are.

The make command imports files which have been modified.  Then, a design
hierarchy is internally built as if no units are outdated.  Then, all outdated
design units, using the dependencies of the design hierarchy, are analyzed.
If necessary, the design hierarchy is elaborated.

This is not perfect, since the default architecture (the most recently
analyzed one) may change while outdated design files are analyzed. In
such a case, re-run the make command of GHDL.

Generate Makefile command
-------------------------

.. index:: --gen-makefile command

Generate a Makefile to build a design unit::

  ghdl --gen-makefile [options] primary [secondary]


This command works like the make command (see :ref:`Make_command`), but only a
makefile is generated on the standard output.

Library commands
================

GHDL has a few commands which act on a library.

Directory command
-----------------

.. index:: displaying library

.. index:: --dir command
.. option::--dir

Display the name of the units contained in a design library::

  ghdl --dir [options] [libs]

The directory command, selected with the `--dir` command line argument
displays the content of the design libraries (by default the
:samp:`work` library).  All options are
allowed, but only a few are meaningful: :option:`--work=NAME`,
:option:`--workdir=PATH` and :option:`--std=VER`.

Clean command
-------------

.. index:: cleaning

.. index:: --clean command

Remove object and executable files but keep the library::

  ghdl --clean [options]


GHDL tries to remove any object, executable or temporary file it could
have created.  Source files are not removed.

There is no short command line form for this option to prevent accidental
clean up.

.. _Remove_command:

Remove command
--------------

.. index:: cleaning all

.. index:: --remove command

Do like the clean command but remove the library too::

  ghdl --remove [options]


There is no short command line form for this option to prevent accidental
clean up.  Note that after removing a design library, the files are not
known anymore by GHDL.

.. _Copy_command:

Copy command
------------

.. index:: copying library

.. index:: --copy command

Make a local copy of an existing library::

  ghdl --copy --work=name [options]


Make a local copy of an existing library.  This is very useful if you want to
add unit to the :samp:`ieee` library:

.. code-block:: shell

  ghdl --copy --work=ieee --ieee=synopsys
  ghdl -a --work=ieee numeric_unsigned.vhd


.. _Create_a_Library:

Create a Library
----------------

.. index:: create your own library

A new library is created by compiling entities (packages etc.) into it::

  ghdl -a --work=my_custom_lib my_file.vhd


A library's source code is usually stored and compiled into its own directory,
that you specify with the :option:`--workdir` option::

  ghdl -a --work=my_custom_lib --workdir=my_custom_libdir my_custom_lib_srcdir/my_file.vhd


See also the :option:`-PPATH` command line option.

.. _Cross-reference_command:

Cross-reference command
=======================

To easily navigate through your sources, you may generate cross-references::

  ghdl --xref-html [options] file...


This command generates an html file for each :samp:`file` given in the command
line, with syntax highlighting and full cross-reference: every identifier is
a link to its declaration.  Besides, an index of the files is created too.

The set of :samp:`file`  are analyzed, and then, if the analysis is
successful, html files are generated in the directory specified by the
:option:`-o dir` option, or :file:`html/` directory by default.

If the option :option:`--format=html2` is specified, then the generated html
files follow the HTML 2.0 standard, and colours are specified with
`<FONT>` tags.  However, colours are hard-coded.

If the option :option:`--format=css` is specified, then the generated html files
follow the HTML 4.0 standard, and use the CSS-1 file :file:`ghdl.css` to
specify colours.  This file is generated only if it does not already exist (it
is never overwritten) and can be customized by the user to change colours or
appearance.  Refer to a generated file and its comments for more information.

File commands
=============

The following commands act on one or several files.  They do not analyze
files, therefore, they work even if a file has semantic errors.

Pretty print command
--------------------

.. index:: --pp-html command

.. index:: pretty printing

.. index:: vhdl to html

Generate HTML on standard output from VHDL::

  ghdl --pp-html [options] file...


The files are just scanned and an html file, with syntax highlighting is
generated on standard output.

Since the files are not even parsed, erroneous files or incomplete designs
can be pretty printed.

The style of the html file can be modified with the :option:`--format=` option.
By default or when the :option:`--format=html2` option is specified, the output
is an HTML 2.0 file, with colours set through `<FONT>` tags.  When the
:option:`--format=css` option is specified, the output is an HTML 4.0 file,
with colours set through a CSS file, whose name is :file:`ghdl.css`.
See :ref:`Cross-reference_command`, for more details about this CSS file.

Find command
------------

.. index:: -f command

Display the name of the design units in files::

  ghdl -f file...


The files are scanned, parsed and the names of design units are displayed.
Design units marked with two stars are candidate to be at the apex of a
design hierarchy.

Chop command
------------

.. index:: --chop command

Chop (or split) files at design unit::

  ghdl --chop files


`GHDL` reads files, and writes a file in the current directory for
every design unit.

The filename of a design unit is build according to the unit.  For an
entity declaration, a package declaration or a configuration the file
name is :file:`NAME.vhdl`, where `NAME` is the name of the design
unit.  For a package body, the filename is :file:`NAME-body.vhdl`.
Finally, for an architecture `ARCH` of an entity `ENTITY`, the
filename is :file:`ENTITY-ARCH.vhdl`.

Since the input files are parsed, this command aborts in case of syntax
error.  The command aborts too if a file to be written already exists.

Comments between design units are stored into the most adequate files.

This command may be useful to split big files, if your computer has not
enough memory to compile such files.  The size of the executable is
reduced too.

Lines command
-------------

.. index:: --lines command

Display on the standard output lines of files preceded by line number::

  ghdl --lines files


Misc commands
=============

There are a few GHDL commands which are seldom useful.

.. _Help_command:

Help command
------------

.. index:: -h command

.. index:: --help command

Display (on the standard output) a short description of the all the commands
available.  If the help switch is followed by a command switch, then options
for this later command are displayed::

  ghdl --help
  ghdl -h
  ghdl -h command


.. _Disp_config_command:

Disp config command
-------------------

.. index:: --disp-config command

.. index:: display configuration

Display the program paths and options used by GHDL::

  ghdl --disp-config [options]


This may be useful to track installation errors.

Disp standard command
---------------------

.. index:: --disp-standard command

.. index:: display :samp:`std.standard`

Display the :samp:`std.standard` package::

  ghdl --disp-standard [options]


Version command
---------------

.. index:: --version command

.. index:: version

Display the `GHDL` version and exit::

  ghdl --version


VPI build commands
==================

These commands simplify the compile and the link of a user vpi
module. They are all wrapper: the arguments are in fact a whole
command line that is executed with additional switches.  Currently a
unix-like compiler (like `cc`, `gcc` or `clang`) is expected: the additional
switches use their syntax.  The only option is `-v` which displays the
command before its execution.

.. _VPI_compile_command:

VPI compile command
-------------------

.. index:: --vpi-compile command

Add include path to the command and execute it::

  ghdl --vpi-compile command

This will execute::

  command -Ixxx/include

For example::

  ghdl --vpi-compile gcc -c vpi1.c

executes::

  gcc -c vpi1.c -fPIC -Ixxx/include

.. _VPI_link_command:

VPI link command
----------------

.. index:: --vpi-link command

Add library path and name to the command and execute it::

  ghdl --vpi-link command

This will execute::

  command -Lxxx/lib -lghdlvpi

For example::

  ghdl --vpi-link gcc -o vpi1.vpi vpi1.o

executes::

  gcc -o vpi1.vpi vpi1.o --shared -Lxxx/lib -lghdlvpi


.. _VPI_cflags_command:

VPI cflags command
------------------

.. index:: --vpi-cflags command

Display flags added by :option:`--vpi-compile`::

  ghdl --vpi-cflags


.. _VPI_ldflags_command:

VPI ldflags command
-------------------

.. index:: --vpi-ldflags command

Display flags added by :option:`--vpi-link`::

  ghdl --vpi-ldflags

.. _VPI_include_dir_command:

VPI include dir command
-----------------------

.. index:: --vpi-include-dir command

Display the include directory added by the compile flags::

  ghdl --vpi-include-dir

.. _VPI_library_dir_command:

VPI library dir command
-----------------------

.. index:: --vpi-library-dir command

Display the library directory added by the link flags::

  ghdl --vpi-library-dir


Installation Directory
======================

During analysis and elaboration `GHDL` may read the `std`
and `ieee` files.  The location of these files is based on the prefix,
which is (in priority order):

* the :option:`--PREFIX=` command line option

* the :envvar:`GHDL_PREFIX` environment variable

*
  a built-in default path.  It is a hard-coded path on GNU/Linux and the
  value of the :samp:`HKLM\Software\Ghdl\Install_Dir` registry entry on Windows.

You should use the :option:`--disp-config` command (:ref:`Disp_config_command` for details) to disp and debug installation problems.

.. _ieee_library_pitfalls:

IEEE library pitfalls
=====================

When you use options :option:`--ieee=synopsys` or :option:`--ieee=mentor`,
the `IEEE` library contains non standard packages such as
:samp:`std_logic_arith`.

These packages are not standard because there are not described by an IEEE
standard, even if they have been put in the `IEEE` library.  Furthermore,
they are not really de-facto standard, because there are slight differences
between the packages of Mentor and those of Synopsys.

Furthermore, since they are not well-thought, their use has pitfalls.  For
example, this description has error during compilation:

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

Indeed, the `"="` operator is defined in both packages, and both
are visible at the place it is used.  The first declaration is an
implicit one, which occurs when the `std_logic_vector` type is
declared and is an element to element comparison, the second one is an
explicit declared function, with the semantic of an unsigned comparison.

With some analyser, the explicit declaration has priority over the implicit
declaration, and this design can be analyzed without error.  However, this
is not the rule given by the VHDL LRM, and since GHDL follows these rules,
it emits an error.

You can force GHDL to use this rule with the *-fexplicit* option.
:ref:`GHDL_options`, for more details.

However it is easy to fix this error, by using a selected name:

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


It is better to only use the standard packages defined by IEEE, which
provides the same functionalities:

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


IEEE math packages
==================

.. index:: Math_Real

.. index:: Math_Complex

The :samp:`ieee` math packages (:samp:`math_real` and
:samp:`math_complex`) provided with `GHDL` are fully compliant with
the `IEEE` standard.
