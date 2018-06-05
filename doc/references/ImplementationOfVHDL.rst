.. _REF:ImplVHDL:

***************************
Implementation of VHDL
***************************

This chapter describes several implementation defined aspects of VHDL in GHDL.

.. _VHDL_standards:

VHDL standards
==============

.. index:: VHDL standards

.. index:: IEEE 1076

.. index:: IEEE 1076a

.. index:: 1076

.. index:: 1076a

.. index:: v87

.. index:: v93

.. index:: v93c

.. index:: v00

.. index:: v02

.. index:: v08

Unfortunately, there are many versions of the VHDL
language, and they aren't backward compatible.

The VHDL language was first standardized in 1987 by IEEE as IEEE 1076-1987, and
is commonly referred as VHDL-87. This is certainly the most important version,
since most of the VHDL tools are still based on this standard.

Various problems of this first standard have been analyzed by experts groups
to give reasonable ways of interpreting the unclear portions of the standard.

VHDL was revised in 1993 by IEEE as IEEE 1076-1993. This revision is still
well-known.

Unfortunately, VHDL-93 is not fully compatible with VHDL-87, i.e. some perfectly
valid VHDL-87 programs are invalid VHDL-93 programs. Here are some of the
reasons:

* the syntax of file declaration has changed (this is the most visible source
  of incompatibility),
* new keywords were introduced (group, impure, inertial, literal,
  postponed, pure, reject, rol, ror, shared, sla, sll, sra, srl,
  unaffected, xnor),
* some dynamic behaviours have changed (the concatenation is one of them),
* rules have been added.

Shared variables were replaced by protected types in the 2000 revision of
the VHDL standard. This modification is also known as 1076a. Note that this
standard is not fully backward compatible with VHDL-93, since the type of a
shared variable must now be a protected type (there was no such restriction
before).

Minor corrections were added by the 2002 revision of the VHDL standard. This
revision is not fully backward compatible with VHDL-00 since, for example,
the value of the `'instance_name` attribute has slightly changed.

The latest version is 2008. Many features have been added, and GHDL
doesn't implement all of them.

You can select the VHDL standard expected by GHDL with the
``--std=VER`` option, where ``VER`` is one of the left column of the
table below:


87
  Select VHDL-87 standard as defined by IEEE 1076-1987. LRM bugs corrected by
  later revisions are taken into account.

93
  Select VHDL-93; VHDL-87 file declarations are not accepted.

93c
  Select VHDL-93 standard with relaxed rules:


  * VHDL-87 file declarations are accepted;

  * default binding indication rules of VHDL-02 are used. Default binding rules
    are often used, but they are particularly obscure before VHDL-02.

00
  Select VHDL-2000 standard, which adds protected types.

02
  Select VHDL-2002 standard.

08
  Select VHDL-2008 standard (partially implemented).

The 93, 93c, 00 and 02 standards are considered compatible: you can
elaborate a design mixing these standards. However, 87, 93 and 08 are
not compatible.

.. _psl_implementation:

PSL implementation
==================

GHDL understands embedded PSL annotations in VHDL files, but not in
separate files.

As PSL annotations are embedded within comments, you must analyze and elaborate
your design with option *-fpsl* to enable PSL annotations.

A PSL assertion statement must appear within a comment that starts
with the `psl` keyword. The keyword must be followed (on the
same line) by a PSL keyword such as `assert` or `default`.
To continue a PSL statement on the next line, just start a new comment.

A PSL statement is considered a process, so it's not allowed within
a process.

All PSL assertions must be clocked (GHDL doesn't support unclocked assertion).
Furthermore only one clock per assertion is allowed.

You can either use a default clock like this:

.. code-block:: VHDL

    -- psl default clock is rising_edge (CLK);
    -- psl assert always
    --   a -> eventually! b;

or use a clocked expression (note the use of parentheses):

.. code-block:: VHDL

    -- psl assert (always a -> next[3](b)) @rising_edge (clk);


Of course only the simple subset of PSL is allowed.

Currently the built-in functions are not implemented.

Source representation
=====================

According to the VHDL standard, design units (i.e. entities,
architectures, packages, package bodies, and configurations) may be
independently analyzed.

Several design units may be grouped into a design file.

In GHDL, a system file represents a design file. That is, a file compiled by
GHDL may contain one or more design units.

It is common to have several design units in a design file.

GHDL does not impose any restriction on the name of a design file
(except that the filename may not contain any control character or
spaces).

GHDL does not keep a binary representation of the design units analyzed like
other VHDL analyzers. The sources of the design units are re-read when
needed (for example, an entity is re-read when one of its architectures is
analyzed). Therefore, if you delete or modify a source file of a unit
analyzed, GHDL will refuse to use it.

.. _Library_database:

Library database
================

Each design unit analyzed is placed into a design library. By default,
the name of this design library is ``work``; however, this can be
changed with the :option:`--work=NAME` option of GHDL.

To keep the list of design units in a design library, GHDL creates
library files. The name of these files is :file:`NAME-objVER.cf`, where
`NAME` is the name of the library, and `VER` the VHDL version (87,
93 or 08) used to analyze the design units.

You don't have to know how to read a library file. You can display it
using the *-d* of `ghdl`. The file contains the name of the
design units, as well as the location and the dependencies.

The format may change with the next version of GHDL.

.. _Top_entity:

Top entity
==========

There are some restrictions on the entity being at the apex of a design
hierarchy:

* The generic must have a default value, and the value of a generic is its
  default value.
* The ports type must be constrained.

Using vendor libraries
======================

Many vendors libraries have been analyzed with GHDL. There are
usually no problems. Be sure to use the :option:`--work=` option.
However, some problems have been encountered.

GHDL follows the VHDL LRM (the manual which defines VHDL) more
strictly than other VHDL tools. You could try to relax the
restrictions by using the :option:`--std=93c`, :option:`-fexplicit`,
:option:`-frelaxed-rules` and :option:`--warn-no-vital-generic`.

Interfacing to other languages
==============================

.. index:: interfacing

.. index:: other languages

.. index:: foreign

.. index:: VHPI

.. index:: VHPIDIRECT

Interfacing with foreign languages is possible only on GNU/Linux systems.

You can define a subprogram in a foreign language (such as `C` or
`Ada`) and import it into a VHDL design.

Foreign declarations
--------------------

Only subprograms (functions or procedures) can be imported, using the foreign
attribute. In this example, the `sin` function is imported:

.. code-block:: VHDL

  package math is
    function sin (v : real) return real;
    attribute foreign of sin : function is "VHPIDIRECT sin";
  end math;

  package body math is
    function sin (v : real) return real is
    begin
      assert false severity failure;
    end sin;
  end math;


A subprogram is made foreign if the `foreign` attribute decorates
it. This attribute is declared in the 1993 revision of the
``std.standard`` package. Therefore, you cannot use this feature in
VHDL 1987.

The decoration is achieved through an attribute specification. The
attribute specification must be in the same declarative part as the
subprogram and must be after it. This is a general rule for specifications.
The value of the specification must be a locally static string.

Even when a subprogram is foreign, its body must be present. However, since
it won't be called, you can make it empty or simply put an assertion.

The value of the attribute must start with ``VHPIDIRECT`` (an
upper-case keyword followed by one or more blanks). The linkage name of the
subprogram follows.

.. _Restrictions_on_foreign_declarations:

Restrictions on foreign declarations
------------------------------------

Any subprogram can be imported. GHDL puts no restrictions on foreign
subprograms. However, the representation of a type or of an interface in a
foreign language may be obscure. Most non-composite types are easily imported:


*integer types*
  They are represented by a 32 bit word. This generally corresponds to
  `int` for `C` or `Integer` for `Ada`.

*physical types*
  They are represented by a 64 bit word. This generally corresponds to the
  `long long` for `C` or `Long_Long_Integer` for `Ada`.

*floating point types*
  They are represented by a 64 bit floating point word. This generally
  corresponds to `double` for `C` or `Long_Float` for `Ada`.

*enumeration types*
  They are represented by an 8 bit word, or, if the number of literals is
  greater than 256, by a 32 bit word. There is no corresponding C type, since arguments are
  not promoted.

Non-composite types are passed by value. For the `in` mode, this
corresponds to the `C` or `Ada` mechanism. The `out` and
`inout` interfaces of non-composite types are gathered in a record
and this record is passed by reference as the first argument to the
subprogram. As a consequence, you shouldn't use `in` and
`inout` modes in foreign subprograms, since they are not portable.

Records are represented like a `C` structure and are passed by reference
to subprograms.

Arrays with static bounds are represented like a `C` array, whose
length is the number of elements, and are passed by reference to subprograms.

Unconstrained arrays are represented by a fat pointer. Do not use unconstrained
arrays in foreign subprograms.

Accesses to an unconstrained array are fat pointers. Other accesses correspond to an address and are passed to a subprogram like other non-composite types.

Files are represented by a 32 bit word, which corresponds to an index
in a table.

.. _Linking_with_foreign_object_files:

Linking with foreign object files
---------------------------------

You may add additional files or options during the link using the
*-Wl,* of `GHDL`, as described in :ref:`Elaboration:command`.
For example::

  ghdl -e -Wl,-lm math_tb

will create the :file:`math_tb` executable with the :file:`lm` (mathematical)
library.

Note the :file:`c` library is always linked with an executable.

.. _Starting_a_simulation_from_a_foreign_program:

Starting a simulation from a foreign program
--------------------------------------------

You may run your design from an external program. You just have to call
the ``ghdl_main`` function which can be defined:

in C:

.. code-block:: C

  extern int ghdl_main (int argc, char **argv);

in Ada:

.. code-block:: Ada

  with System;
  ...
  function Ghdl_Main (Argc : Integer; Argv : System.Address)
    return Integer;
  pragma import (C, Ghdl_Main, "ghdl_main");


This function must be called once, and returns 0 at the end of the simulation.
In case of failure, this function does not return. This has to be fixed.

.. _Linking_with_Ada:

Linking with Ada
----------------

As explained previously in :ref:`Starting_a_simulation_from_a_foreign_program`,
you can start a simulation from an `Ada` program. However the build
process is not trivial: you have to elaborate your `Ada` program and your
`VHDL` design.

First, you have to analyze all your design files. In this example, we
suppose there is only one design file, :file:`design.vhdl`.

::

  $ ghdl -a design.vhdl

Then, bind your design. In this example, we suppose the entity at the
design apex is ``design``.

::

  $ ghdl --bind design

Finally, compile, bind your `Ada` program and link it with your `VHDL`
design::

  $ gnatmake my_prog -largs `ghdl --list-link design`


Using GRT from Ada
------------------

.. warning::
  This topic is only for advanced users who know how to use `Ada`
  and `GNAT`. This is provided only for reference; we have tested
  this once before releasing `GHDL` 0.19, but this is not checked at
  each release.

The simulator kernel of `GHDL` named :dfn:`GRT` is written in
`Ada95` and contains a very light and slightly adapted version
of `VHPI`. Since it is an `Ada` implementation it is
called :dfn:`AVHPI`. Although being tough, you may interface to `AVHPI`.

For using `AVHPI`, you need the sources of `GHDL` and to recompile
them (at least the `GRT` library). This library is usually compiled with
a `No_Run_Time` pragma, so that the user does not need to install the
`GNAT` runtime library. However, you certainly want to use the usual
runtime library and want to avoid this pragma. For this, reset the
`GRT_PRAGMA_FLAG` variable.

::

  $ make GRT_PRAGMA_FLAG= grt-all


Since `GRT` is a self-contained library, you don't want
`gnatlink` to fetch individual object files (furthermore this
doesn't always work due to tricks used in `GRT`). For this,
remove all the object files and make the :file:`.ali` files read-only.

::

  $ rm *.o
  $ chmod -w *.ali


You may then install the sources files and the :file:`.ali` files. I have never
tested this step.

You are now ready to use it.

Here is an example, :file:`test_grt.adb` which displays the top
level design name.

.. code-block:: Ada

  with System; use System;
  with Grt.Avhpi; use Grt.Avhpi;
  with Ada.Text_IO; use Ada.Text_IO;
  with Ghdl_Main;

  procedure Test_Grt is
    --  VHPI handle.
    H : VhpiHandleT;
    Status : Integer;

    --  Name.
    Name : String (1 .. 64);
    Name_Len : Integer;
  begin
    --  Elaborate and run the design.
    Status := Ghdl_Main (0, Null_Address);

    --  Display the status of the simulation.
    Put_Line ("Status is " & Integer'Image (Status));

    --  Get the root instance.
    Get_Root_Inst(H);

    --  Disp its name using vhpi API.
    Vhpi_Get_Str (VhpiNameP, H, Name, Name_Len);
    Put_Line ("Root instance name: " & Name (1 .. Name_Len));
  end Test_Grt;


First, analyze and bind your design::

  $ ghdl -a counter.vhdl
  $ ghdl --bind counter


Then build the whole::

  $ gnatmake test_grt -aL`grt_ali_path` -aI`grt_src_path` -largs
   `ghdl --list-link counter`


Finally, run your design::

  $ ./test_grt
  Status is  0
  Root instance name: counter
