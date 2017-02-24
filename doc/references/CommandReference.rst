.. _REF:Command:

Command Reference
#################

.. HINT:: The most common commands and options are shown in section :ref:`USING:Invoking`. Here the advanced and experimental features are described.

Misc commands
=============

There are a few GHDL commands which are seldom useful.

.. _Help_command:

Help [:samp:`-h`]
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

Display config [:samp:`--disp-config`]
-------------------

.. index:: --disp-config command

.. index:: display configuration

:samp:`ghdl --disp-config [options]`

Display the program paths and options used by GHDL. This may be useful to track installation errors.

Display standard [:samp:`--disp-standard`]
---------------------

.. index:: --disp-standard command

.. index:: display :samp:`std.standard`

:samp:`ghdl --disp-standard [options]`

Display the :samp:`std.standard` package.

Version [:samp:`--version`]
---------------

.. index:: --version command

.. index:: version

:samp:`ghdl --version`

Display the `GHDL` version and exit.

File commands
=============

The following commands act on one or several files. These are not analyzed, therefore, they work even if a file has semantic errors.

Pretty print [:samp:`--pp-html`]
--------------------

.. index:: --pp-html command

.. index:: pretty printing

.. index:: vhdl to html

:samp:`ghdl --pp-html [options] file...`

The files are just scanned and an html file, with syntax highlighting is generated on standard output. Since the files are not even parsed, erroneous files or incomplete designs can be pretty printed.

The style of the html file can be modified with the :option:`--format=` option:

* By default or when the :option:`--format=html2` option is specified, the output is an HTML 2.0 file, with colours set through `<FONT>` tags.
* When the :option:`--format=css` option is specified, the output is an HTML 4.0 file, with colours set through a CSS file, whose name is :file:`ghdl.css`. See :ref:`Cross-reference_command`, for more details about this CSS file.

Find [:samp:`-f`]
------------

.. index:: -f command

:samp:`ghdl -f file...`

The files are scanned, parsed and the names of design units are displayed. Design units marked with two stars are candidate to be at the apex of a design hierarchy.

Chop [:samp:`--chop`]
------------

.. index:: --chop command

:samp:`ghdl --chop files`

The provided files are read, and a file is written in the current directory for every design unit. Each filename is build according to the type:

* For an entity declaration, a package declaration or a configuration the file name is :file:`NAME.vhdl`, where `NAME` is the name of the design unit.
* For a package body, the filename is :file:`NAME-body.vhdl`.
* Finally, for an architecture `ARCH` of an entity `ENTITY`, the filename is :file:`ENTITY-ARCH.vhdl`.

Since the input files are parsed, this command aborts in case of syntax error. The command aborts too if a file to be written already exists.

Comments between design units are stored into the most adequate files.

This command may be useful to split big files, if your computer has not enough memory to compile such files. The size of the executable is reduced too.

Lines [:samp:`--lines`]
-------------

.. index:: --lines command

:samp:`ghdl --lines files`

Display on the standard output lines of files preceded by line number.

GCC/LLVM only commands
=================

.. _Bind_command:

Bind [:samp:`--bind`]
------------

.. index:: binding

.. index:: --bind command

:samp:`ghdl --bind [options] primary_unit [secondary_unit]`

Performs only the first stage of the elaboration command; the list of objects files is created but the executable is not built.  This command should be used only when the main entry point is not GHDL.

.. _Link_command:

Link [:samp:`--link`]
------------

.. index:: linking

.. index:: --link command

:samp:`ghdl --link [options] primary_unit [secondary_unit]`
  
Performs only the second stage of the elaboration command: the executable is created by linking the files of the object files list. This command is available only for completeness. The elaboration command is equivalent to the bind command followed by the link command.

.. _List_link_command:

List link [:samp:`--list-link`]
-----------------

.. index:: --list-link command

:samp:`ghdl --list-link primary_unit [secondary_unit]`

This command may be used only after a bind command. GHDL displays all the files which will be linked to create an executable. This command is intended to add object files in a link of a foreign program.

GHDL options
=================

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

.. option:: --GHDL1=<COMMAND>

  Use :samp:`COMMAND` as the command name for the compiler.  If :samp:`COMMAND` is
  not a path, then it is searched in the path.

.. option:: --AS=<COMMAND>

  Use :samp:`COMMAND` as the command name for the assembler.  If :samp:`COMMAND` is
  not a path, then it is searched in the path.  The default is :samp:`as`.

.. option:: --LINK=<COMMAND>

  Use :samp:`COMMAND` as the linker driver.  If :samp:`COMMAND` is
  not a path, then it is searched in the path.  The default is :samp:`gcc`.
  
Passing options to other programs
=================================

.. WARNING:: These options are only available with GCC/LLVM.

For many commands, GHDL acts as a driver: it invokes programs to perform the command. You can pass arbitrary options to these programs.

Both the compiler and the linker are in fact GCC programs. See the GCC manual for details on GCC options.

.. option:: -Wc,<OPTION>

  Pass `OPTION` as an option to the compiler.

.. option:: -Wa,<OPTION>

  Pass `OPTION` as an option to the assembler.

.. option:: -Wl,<OPTION>

  Pass `OPTION` as an option to the linker.
