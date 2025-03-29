.. program:: ghdl
.. _QuickStart:hello:

`Hello world` program
=====================

To illustrate the general purpose of `VHDL`, the following block is a commented `Hello world` program which is saved in
a file named :file:`hello.vhdl`:

.. literalinclude:: hello.vhdl
   :language: vhdl

.. TIP::

   * Both ``.vhdl`` and ``.vhd`` extensions are used for `VHDL` source files, while ``.v`` is used for Verilog.

      * Since, extension ``.vhd`` is also interpreted as a :wikipedia:`Virtual Hard Disk <VHD_(file_format)>` file
        format, some users prefer ``.vhdl``, to avoid ambiguity.
        This is the case with `GHDL`'s codebase.
        However, in order to maintain :wikipedia:`backward-compatibility <https://en.wikipedia.org/wiki/8.3_filename>`
        with legacy DOS systems, other users prefer ``.vhd``.

   * Unless you use especial characters, either `UTF-8` or `ISO-8859-1` encodings can be used.
     However, if you do, the latter should be used.
     The standard defines ASCII (7-bit encoding) or ISO Latin-1 (ISO-8859-1) as default.
     However, GHDL has a relaxing option, :option:`--mb-comments` (multi byte), to allow UTF-8 or other encodings in
     comments.

* First, you have to compile the file; this is called :ref:`analysis <Analysis:command>` of a design file in `VHDL`
  terms. Run ``ghdl -a hello.vhdl`` in the `shell`.
  This command creates or updates a file :file:`work-obj93.cf`, which describes the library ``work``.
* Then, run ``ghdl -e hello_world`` in the `shell`.
  Command :option:`-e` means :ref:`elaborate <Elaboration:command>`, which is used to build a design, with the
  ``hello_world`` entity at the top of the hierarchy.
* Last, you can directly launch the simulation :ref:`running <Run:command>` ``ghdl -r hello_world`` in the `shell`.
  The result of the simulation will be shown on screen:

.. code-block:: shell

  Hello world!

.. HINT::
   If a GCC/LLVM variant of `GHDL` is used:

   * :ref:`Analysis <Analysis:command>` generates a file, :file:`hello.o`, which is the object file corresponding to
     your `VHDL` program. This is not created with :ref:`mcode <BUILD>`.
     These kind of object files can be compiled into foreign programs (see :ref:`Linking_with_Ada`).
   * The :ref:`elaboration <Elaboration:command>` step is mandatory after running the analysis and prior to launching the
     simulation.
     This will generate an executable binary named :file:`hello_world`.
   * As a result, :option:`-r` is just a passthrough to the binary generated in the `elaboration`.
     Therefore, the executable can be run directly: ``./hello_world``. See :option:`-r` for more informartion.

.. HINT::
  :option:`-e` can be bypassed with :ref:`mcode <BUILD>`, since :option:`-r` actually elaborates the design and saves
  it on memory before running the simulation.
  But you can still use it to check for some elaboration problems.
