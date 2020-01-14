.. program:: ghdl

.. _USING:Foreign:

Interfacing to other languages
##############################

.. index:: interfacing

.. index:: other languages

.. index:: foreign

.. index:: VHPI

.. index:: VHPIDIRECT

Interfacing with foreign languages through VHPIDIRECT is possible any platform.
You can define a subprogram in a foreign language (such as `C` or
`Ada`) and import it into a VHDL design.

.. HINT::
   VHPIDIRECT is the simplest way to call C code from VHDL. VHPI is a complex API to interface C and VHDL, which allows to
   inspect the hierarchy, set callbacks and/or assign signals. GHDL does not support VHPI. For these kind of features, it is
   suggested to use VPI instead (see :ref:`VPI_build_commands`).

Foreign declarations
====================

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

Linking foreign object files to GHDL
====================================

You may add additional files or options during the link of `GHDL` using
``-Wl,`` as described in :ref:`passing-options-to-other-programs`.
For example::

  ghdl -e -Wl,-lm math_tb

will create the :file:`math_tb` executable with the :file:`lm` (mathematical)
library.

Note the :file:`c` library is always linked with an executable.

.. _Starting_a_simulation_from_a_foreign_program:

Wrapping and starting a GHDL simulation from a foreign program
==============================================================

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

.. _Linking_with_Ada:

Linking GHDL to Ada/C
=====================

As explained previously in :ref:`Starting_a_simulation_from_a_foreign_program`,
you can start a simulation from an `Ada` or `C` program. However the build
process is not trivial: you have to elaborate your program and your
`VHDL` design.

.. HINT::
   If the foreign language is C, this procedure is equivalent to the one described in
   :ref:`Linking_with_foreign_object_files`, which is easier. Thus, this procedure is
   explained for didactic purposes. When suitable, we suggest to use ``-e`` instead
   of ``--bind`` and ``--list-link``.

First, you have to analyze all your design files. In this example, we
suppose there is only one design file, :file:`design.vhdl`.

::

  $ ghdl -a design.vhdl

Then, bind your design. In this example, we suppose the entity at the
design apex is ``design``.

::

  $ ghdl --bind design

Finally, compile/bind your program and link it with your `VHDL`
design:

in C:

::

  gcc my_prog.c -Wl,`ghdl --list-link design`

in Ada:

::

  $ gnatmake my_prog -largs `ghdl --list-link design`

See :ref:`gccllvm-only-programs` for further details about ``--bind`` and ``--list-link``.

Dynamically loading foreign objects from GHDL
=============================================

Instead of linking and building foreign objects along with GHDL, it is also possible to load foreign resources dinamically.
In order to do so, provide the path and name of the shared library where the resource is to be loaded from. For example:

.. code-block:: VHDL

  attribute foreign of get_rand: function is "VHPIDIRECT ./getrand.so get_rand";

Dynamically loading GHDL
========================

In order to generate a position independent executable (PIE), be it an executable binary
or a shared library, GHDL must be built with config option ``--default-pic``. This will ensure
that all the libraries and sources analyzed by GHDL generate position independent code (PIC).
Furthermore, when the binary is built, argument ``-Wl,-pie`` needs to be provided.

PIE binaries can be loaded and executed from any language that supports C-alike signatures and types
(C, C++, golang, Python, Rust, etc.). For example:

.. code-block:: Python

  import ctypes
  gbin = ctypes.CDLL(bin_path)

  args = ['-gGENA="value"', 'gGENB="value"']

  xargs = (ctypes.POINTER(ctypes.c_char) * (len(args) + 1))()
  for i, arg in enumerate(args):
      xargs[i] = ctypes.create_string_buffer(arg.encode('utf-8'))
  return args[0], xargs

  gbin.main(len(xargv)-1, xargv)

  import _ctypes
  # On GNU/Linux
  _ctypes.dlclose(gbin._handle)
  # On Windows
  #_ctypes.FreeLibrary(gbin._handle)

This allows seamless co-simulation using concurrent/parallel execution features available in each language:
pthreads, goroutines/gochannels, multiprocessing/queues, etc. Moreover, it provides a mechanism to execute multiple
GHDL simulations in parallel.

Using GRT from Ada
==================

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

