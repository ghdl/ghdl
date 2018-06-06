.. program:: ghdl
.. _REF:Command:

Command Reference
#################

.. HINT:: The most common commands and options are shown in section :ref:`USING:Invoking`. Here the advanced and experimental features are described.

Environment variables
=====================

.. envvar:: GHDL_PREFIX

Misc commands
=============

There are a few GHDL commands which are seldom useful.

.. index:: cmd help

Help [``-h``]
-----------------

.. option:: --help, -h

Display (on the standard output) a short description of the all the commands
available. If the help switch is followed by a command switch, then options
for that second command are displayed::

  ghdl --help
  ghdl -h
  ghdl -h command

.. index:: cmd display configuration

Display config [``--disp-config``]
--------------------------------------

.. option:: --disp-config <[options]>

Display the program paths and options used by GHDL. This may be useful to track installation errors.

.. index:: cmd display standard
.. index:: display ``std.standard``

Display standard [``--disp-standard``]
------------------------------------------

.. option:: --disp-standard <[options]>

Display the ``std.standard`` package.

.. index:: cmd version

Version [``--version``]
---------------------------

.. option:: --version, -v

Display the GHDL version.

File commands
=============

The following commands act on one or several files. These are not analyzed, therefore, they work even if a file has semantic errors.

.. index:: cmd file pretty printing
.. index:: vhdl to html

Pretty print [``--pp-html``]
--------------------------------

.. option:: --pp-html <[options] file...>

The files are just scanned and an html file with syntax highlighting is generated on standard output. Since the files are not even parsed, erroneous files or incomplete designs can be pretty printed.

The style of the html file can be modified with the :option:`--format=` option:

* By default or when the :option:`--format=html2` option is specified, the output is an HTML 2.0 file, with colours set through `<FONT>` tags.
* When the :option:`--format=css` option is specified, the output is an HTML 4.0 file, with colours set through a CSS file, whose name is :file:`ghdl.css`. See :ref:`Cross-reference_command`, for more details about this CSS file.

.. index:: cmd file find

Find [``-f``]
-----------------

.. option:: -f <file...>

The files are scanned, parsed and the names of design units are displayed. Design units marked with two stars are candidates to be at the apex of a design hierarchy.

.. index:: cmd file chop

Chop [``--chop``]
---------------------

.. option:: --chop <files...>

The provided files are read, and a file is written in the current directory for every design unit. Each filename is built according to the type:

* For an entity declaration, a package declaration, or a configuration the file name is :file:`NAME.vhdl`, where `NAME` is the name of the design unit.
* For a package body, the filename is :file:`NAME-body.vhdl`.
* Finally, for an architecture `ARCH` of an entity `ENTITY`, the filename is :file:`ENTITY-ARCH.vhdl`.

Since the input files are parsed, this command aborts in case of syntax error. The command aborts too if a file to be written already exists.

Comments between design units are stored into the most adequate files.

This command may be useful to split big files, if your computer doesn't have enough memory to compile such files. The size of the executable is reduced too.

.. index:: cmd file lines

Lines [``--lines``]
-----------------------

.. option:: --lines <files...>

Display on the standard output lines of files preceded by line number.

GCC/LLVM only commands
======================

.. index:: cmd GCC/LLVM binding

Bind [``--bind``]
---------------------

.. option:: --bind <[options] primary_unit [secondary_unit]>

Performs only the first stage of the elaboration command; the list of object files is created but the executable is not built. This command should be used only when the main entry point is not GHDL.

.. index:: cmd GCC/LLVM linking

Link [``--link``]
---------------------

.. option:: --link <[options] primary_unit [secondary_unit]>

Performs only the second stage of the elaboration command: the executable is created by linking the files of the object files list. This command is available only for completeness. The elaboration command is equivalent to the bind command followed by the link command.

.. index:: cmd GCC/LLVM list link

List link [``--list-link``]
-------------------------------

.. option:: --list-link <primary_unit [secondary_unit]>

This command may be used only after a bind command. GHDL displays all the files which will be linked to create an executable. This command is intended to add object files in a link of a foreign program.

Options
=======

.. option:: --mb-comments, -C

Allow multi-bytes chars in a comment.

.. option:: --syn-binding

Use synthesizer rules for component binding. During elaboration, if a component is not bound to an entity using VHDL LRM rules, try to find in any known library an entity whose name is the same as the component name.

This rule is known as the synthesizer rule.

There are two key points: normal VHDL LRM rules are tried first and entities are searched only in known libraries. A known library is a library which has been named in your design.

This option is only useful during elaboration.

.. option:: --GHDL1<=COMMAND>

Use ``COMMAND`` as the command name for the compiler. If ``COMMAND`` is not a path, then it is searched in the path.

.. option:: --AS<=COMMAND>

Use ``COMMAND`` as the command name for the assembler. If ``COMMAND`` is not a path, then it is searched in the path. The default is ``as``.

.. option:: --LINK<=COMMAND>

Use ``COMMAND`` as the linker driver. If ``COMMAND`` is not a path, then it is searched in the path. The default is ``gcc``.

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
