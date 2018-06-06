.. program:: ghdl
.. _USING:QuickStart:

Quick Start Guide
#################

In this chapter, you will learn how to use `GHDL` by working on a few examples.

The `'Hello world'` program
===========================

To illustrate the general purpose of `VHDL`, here is a commented `'Hello world'` program which is saved in a file named :file:`hello.vhdl`:

.. code-block:: VHDL

  --  Hello world program
  use std.textio.all; -- Imports the standard textio package.

  --  Defines a design entity, without any ports.
  entity hello_world is
  end hello_world;

  architecture behaviour of hello_world is
  begin
    process
      variable l : line;
    begin
      write (l, String'("Hello world!"));
      writeline (output, l);
      wait;
    end process;
  end behaviour;

.. TIP::

   * Both ``.vhdl`` and ``.vhd`` extensions are used for VHDL source files, while ``.v`` is used for Verilog.
   * Unless you use especial characters, either `UTF-8` or `ISO-8859-1` encodings can be used. However, if you do, the latter should be used. The standard defines ASCII (7-bit encoding) or ISO Latin-1 (ISO-8859-1) as default. However, GHDL has a relaxing option, :option:`--mb-comments` (multi byte), to allow UTF-8 or other encodings in comments.

- First, you have to compile the file; this is called `analysis` of a design file in `VHDL` terms. Run ``ghdl -a hello.vhdl`` in the `shell`. This command creates or updates a file :file:`work-obj93.cf`, which describes the library ``work``.
- Then, run ``ghdl -e hello_world`` in the `shell`. Option :option:`-e` means :dfn:`elaborate`, which is used to build a design, with the ``hello_world`` entity at the top of the hierarchy.
- Last, you can directly launch the simulation running ``ghdl -r hello_world`` in the `shell`. The result of the simulation will be shown on screen:

.. code-block:: shell

  Hello world!

.. HINT::
   If a GCC/LLVM variant of `GHDL` is used:

   * `Analysis` generates a file, :file:`hello.o`, which is the object file corresponding to your `VHDL` program. This is not created with mcode.
   * The elaboration step is mandatory after running the analysis and prior to launching the simulation. This will generate an executable binary named :file:`hello_world`.
   * As a result, :option:`-r` is just a passthrough to the binary generated in the `elaboration`. Therefore, the executable can be run directly, ``./hello_world``. See :option:`-r` for more informartion.

.. HINT:: :option:`-e` can be bypassed with mcode, since :option:`-r` actually elaborates the design and saves it on memory before running the simulation. But you can still use it to check for some elaboration problems.

The `heartbeat` program
=======================

.. code-block:: VHDL

  entity hello_world is
    port ( clk: out std_logic; )
  end hearbeat;

  architecture behaviour of hello_world is
  begin
    -- Clock process definition
    clk_process: process
    begin
      clk <= '0';
      wait for clk_period/2;
      clk <= '1';
      wait for clk_period/2;
    end process;
  end behaviour;

A full adder
============

VHDL is generally used for hardware design. This example starts with a `full adder <https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder>`_ described in a file named :file:`adder.vhdl`:

.. code-block:: VHDL

  entity adder is
    -- `i0`, `i1`, and the carry-in `ci` are inputs of the adder.
    -- `s` is the sum output, `co` is the carry-out.
    port (i0, i1 : in bit; ci : in bit; s : out bit; co : out bit);
  end adder;

  architecture rtl of adder is
  begin
    --  This full-adder architecture contains two concurrent assignments.
    --  Compute the sum.
    s <= i0 xor i1 xor ci;
    --  Compute the carry.
    co <= (i0 and i1) or (i0 and ci) or (i1 and ci);
  end rtl;

You can analyze this design file, ``ghdl -a adder.vhdl``, and try to execute the `adder` design. But this is useless, since nothing externally visible will happen. In order to check this full adder, a :dfn:`testbench` has to be run. This testbench is very simple, since the adder is also simple: it checks exhaustively all inputs. Note that only the behaviour is tested, timing constraints are not checked. A file named :file:`adder_tb.vhdl` contains the testbench for the adder:

.. code-block:: VHDL

  --  A testbench has no ports.
  entity adder_tb is
  end adder_tb;

  architecture behav of adder_tb is
    --  Declaration of the component that will be instantiated.
    component adder
      port (i0, i1 : in bit; ci : in bit; s : out bit; co : out bit);
    end component;

    --  Specifies which entity is bound with the component.
    for adder_0: adder use entity work.adder;
    signal i0, i1, ci, s, co : bit;
  begin
    --  Component instantiation.
    adder_0: adder port map (i0 => i0, i1 => i1, ci => ci,
                             s => s, co => co);

    --  This process does the real job.
    process
      type pattern_type is record
        --  The inputs of the adder.
        i0, i1, ci : bit;
        --  The expected outputs of the adder.
        s, co : bit;
      end record;
      --  The patterns to apply.
      type pattern_array is array (natural range <>) of pattern_type;
      constant patterns : pattern_array :=
        (('0', '0', '0', '0', '0'),
         ('0', '0', '1', '1', '0'),
         ('0', '1', '0', '1', '0'),
         ('0', '1', '1', '0', '1'),
         ('1', '0', '0', '1', '0'),
         ('1', '0', '1', '0', '1'),
         ('1', '1', '0', '0', '1'),
         ('1', '1', '1', '1', '1'));
    begin
      --  Check each pattern.
      for i in patterns'range loop
        --  Set the inputs.
        i0 <= patterns(i).i0;
        i1 <= patterns(i).i1;
        ci <= patterns(i).ci;
        --  Wait for the results.
        wait for 1 ns;
        --  Check the outputs.
        assert s = patterns(i).s
          report "bad sum value" severity error;
        assert co = patterns(i).co
          report "bad carry out value" severity error;
      end loop;
      assert false report "end of test" severity note;
      --  Wait forever; this will finish the simulation.
      wait;
    end process;
  end behav;


As usual, you should analyze the design, ``ghdl -a adder_tb.vhdl``.

.. HINT::
   Then, if required, elaborate the testbench: ``ghdl -e adder_tb``. You do not need to specify which object files are required, since GHDL knows them and automatically adds them.

Now, it is time to run the testbench, ``ghdl -r adder_tb``, and check the result on screen::

  adder_tb.vhdl:52:7:(assertion note): end of test

If your design is rather complex, you'd like to inspect signals. Signal values can be dumped using multiple formats (see section :ref:`export_waves` for more information). The resulting file can be read with a wave viewer such as `GtkWave <http://gtkwave.sourceforge.net/>`_.

As explained in the `manual <http://gtkwave.sourceforge.net/gtkwave.pdf>`_, GtkWave *'relies on a post-mortem approach through the use of dumpfiles'*. Therefore, you should first simulate your design and dump a waveform file, say VCD: ``ghdl -r adder_tb --vcd=adder.vcd``. Then, you can view the dump: ``gtkwave adder.vcd``.

See section :ref:`simulation_options`, for more details on other runtime options.

Starting with a design
======================

Unless you are only studying VHDL, you will work with larger designs than the ones of the previous examples. Let's see how to analyze and run a bigger design, such as the DLX model suite written by Peter Ashenden which is distributed under the terms of the GNU General Public License. A copy is kept on `ghdl.free.fr/dlx.tar.gz <http://ghdl.free.fr/dlx.tar.gz>`_ .

- First, untar the sources: ``tar zxvf dlx.tar.gz``.

.. HINT:: In order not to pollute the sources with the library, it is a good idea to create a :file:`work/` subdirectory for the `WORK` library. To any GHDL commands, we will add the :option:`--workdir=work` option, so that all files generated by the compiler (except the executable) will be placed in this directory.

  .. code-block:: shell

     $ cd dlx
     $ mkdir work

* Then, we will run the ``dlx_test_behaviour`` design. We need to analyze all the design units for the design hierarchy, in the correct order. GHDL provides an easy way to do this, by importing the sources, ``ghdl -i --workdir=work *.vhdl``.

* GHDL knows all the design units of the DLX, but none of them has been analyzed. Run the make option, ``ghdl -m --workdir=work dlx_test_behaviour``, which analyzes and elaborates a design. This creates many files in the :file:`work/` directory, and (GCC/LLVM only) the :file:`dlx_test_behaviour` executable in the current directory.

.. HINT:: The simulation needs to have a DLX program contained in the file :file:`dlx.out`. This memory image will be loaded in the DLX memory. Just take one sample: ``cp test_loop.out dlx.out``.

* Now, you can run the test suite: ``ghdl -r --workdir=work dlx_test_behaviour``. The test bench monitors the bus and displays each instruction executed. It finishes with an assertion of severity level note:

  .. code-block:: shell

     dlx-behaviour.vhdl:395:11:(assertion note): TRAP instruction
      encountered, execution halted

* Lastly, since the clock is still running, you have to manually stop the program with the :kbd:`C-c` key sequence. This behavior prevents you from running the test bench in batch mode. However, you may force the simulator to stop when an assertion above or equal a certain severity level occurs. To do so, call run with this option instead: ``ghdl -r --workdir=work dlx_test_behaviour --assert-level=note```. With this option, the program stops just after the previous message:

  .. code-block:: shell

     dlx-behaviour.vhdl:395:11:(assertion note): TRAP instruction
      encountered, execution halted
     error: assertion failed

.. TIP:: If you want to make room on your hard drive, you can either:

   * Clean the design library with the GHDL command ``ghdl --clean --workdir=work``. This removes the executable and all the object files. If you want to rebuild the design at this point, just do the make command as shown above.
   * Remove the design library with the GHDL command ``ghdl --remove --workdir=work``. This removes the executable, all the object files and the library file. If you want to rebuild the design, you have to import the sources again and make the design.
   * Remove the :file:`work/` directory: ``rm -rf work``. Only the executable is kept. If you want to rebuild the design, create the :file:`work/` directory, import the sources, and make the design.

.. WARNING:: Sometimes, a design does not fully follow the VHDL standards. For example it might use the badly engineered ``std_logic_unsigned`` package. GHDL supports this VHDL dialect through some options: ``--ieee=synopsys -fexplicit``. See section :ref:`IEEE_library_pitfalls`, for more details.
