.. program:: ghdl
.. _REF:ImplVHDL:

Implementation of VHDL
######################

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

.. index:: v19

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
before).  This incompatibility can be bypassed with the
:option:`-frelaxed` option.

Minor corrections were added by the 2002 revision of the VHDL standard. This
revision is not fully backward compatible with VHDL-00 since, for example,
the value of the `'instance_name` attribute has slightly changed.

The latest version is 2019. Many features have been added, and GHDL
doesn't implement all of them.

You can select the VHDL standard expected by GHDL with the
:option:`--std=STANDARD <--std>` option, where ``STANDARD`` is one of the list below:


87
  Select VHDL-87 standard as defined by IEEE 1076-1987. LRM bugs corrected by
  later revisions are taken into account.

93
  Select VHDL-93; VHDL-87 file declarations are not accepted.

93c
  Same as 93 and :option:`-frelaxed`.

00
  Select VHDL-2000 standard, which adds protected types.

02
  Select VHDL-2002 standard.

08
  Select VHDL-2008 standard (partially implemented).

19
  Select VHDL-2019 standard (partially implemented).

Multiple standards can be used in a design:

+-----+----------------+
|GROUP|  VHDL Standard |
+=====+================+
|  87 |       87       |
+-----+----------------+
|  93 | 93, 93c, 00, 02|
+-----+----------------+
|  08 |       08       |
+-----+----------------+
|  19 |       19       |
+-----+----------------+

.. note::

   The standards in each group are considered compatible: you can elaborate a design mixing these standards. However, standards of different groups are not compatible.

.. _psl_implementation:

PSL support
===========

GHDL implements a subset of :wikipedia:`PSL <Property_Specification_Language>`.

PSL implementation
------------------

A PSL statement is considered as a process, so it's not allowed within
a process.

All PSL directives (`assert`, `assume`, `restrict`, `cover`) must be clocked (GHDL doesn't support unclocked directives).
Furthermore only one clock per directive is allowed.

You can either use a default clock like this:

.. code-block:: VHDL

   default clock is rising_edge (CLK);
   assert always
     a -> eventually! b;

or use a clocked expression (note the use of parentheses):

.. code-block::

   assert (always a -> next[3](b)) @rising_edge(clk);


Of course only the simple subset of PSL is allowed.

Currently the built-in functions are not implemented, see :ghdlsharp:`662`.
PSL functions `prev()`, `stable()`, `rose()`, `fell()`, `onehot()` and `onehot0()` are supported with GHDL synthesis.

PSL usage
---------

PSL annotations embedded in comments
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GHDL understands embedded PSL annotations in VHDL files:

.. code-block:: VHDL

      -- psl default clock is rising_edge (CLK);
      -- psl assert always
      --   a -> eventually! b;
    end architecture rtl;

* A PSL assertion statement must appear within a comment that starts
  with the `psl` keyword. The keyword must be followed (on the
  same line) by a PSL keyword such as `assert` or `default`.
  To continue a PSL statement on the next line, just start a new comment.

.. HINT::

   As PSL annotations are embedded within comments, you must analyze
   your design with option :option:`-fpsl` to enable PSL annotations:

   .. code-block:: bash

       ghdl -a -fpsl vhdl_design.vhdl
       ghdl -e vhdl_design

PSL annotations (VHDL-2008 and later)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Since VHDL-2008 PSL is integrated in the VHDL language. You can use
PSL in a VHDL(-2008) design without embedding it in comments.

.. code-block:: VHDL

      default clock is rising_edge (CLK);
      assert always
        a -> eventually! b;
    end architecture rtl;

.. HINT::

   You have to use the :option:`--std=08 <--std>` option:

   .. code-block:: bash

       ghdl -a --std=08 vhdl_design.vhdl
       ghdl -e --std=08 vhdl_design

PSL vunit files (VHDL-2008 and later, synthesis only)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GHDL supports vunit (Verification Unit) files.

.. code-block::

    vunit vunit_name (entity_name(architecture_name))
    {
      default clock is rising_edge(clk);
      assert always cnt /= 5 abort rst;
    }

* A vunit can contain PSL and VHDL code.

* It is bound to a VHDL entity or an instance of it.

* The PSL vunit is in the same scope as the VHDL design it is bound
  to. You have access to all objects (ports, types, signals) of the
  VHDL design.

.. HINT::

   The PSL vunit file has to be analyzed together with the VHDL design file, for example:

   .. code-block:: bash

       ghdl -a --std=08 vhdl_design.vhdl vunit.psl

   Or when using the `--synth` command:

   .. code-block:: bash

       ghdl --synth --std=08 vhdl_design.vhdl vunit.psl -e vhdl_design


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
changed with the :option:`--work` option of GHDL.

To keep the list of design units in a design library, GHDL creates
library files. The name of these files is :file:`<LIB_NAME>-obj<GROUP>.cf`, where
`<LIB_NAME>` is the name of the library, and `<GROUP>` the VHDL version (87,
93, 08, or 19) used to analyze the design units.

For details on ``GROUP`` values see section :ref:`VHDL_standards`.

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

Many vendors libraries have been analyzed with `GHDL`. There are usually no problems. Be sure to use the
:option:`--work` option. However, some problems have been encountered. `GHDL` follows the `VHDL` LRM (the manual which
defines `VHDL`) more strictly than other `VHDL` tools. You could try to relax the restrictions by using the
:option:`--std=93c <--std>`, :option:`-fexplicit`, :option:`-frelaxed-rules` and
:option:`--warn-no-vital-generic <--warn-vital-generic>`.
