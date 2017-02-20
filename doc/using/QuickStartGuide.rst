.. _USING:QuickStart:

******************
Quick Start Guide
******************

In this chapter, you will learn how to use `GHDL` by working on a few examples.

The `'Hello world'` program
=======================

To illustrate the large purpose of `VHDL`, here is a commented `'Hello world'` program:

.. code-block:: VHDL

  --  Hello world program.
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

Suppose this program is contained in a file named :file:`hello.vhdl`. First, you have to compile the file; this is called `analysis` of a design file in `VHDL` terms. Run :samp:`ghdl -a hello.vhdl` in the `shell`. This command creates or updates a file :file:`work-obj93.cf`, which describes the library :samp:`work`.

.. TIP:: If a `GCC`/`LLVM` variant of `GHDL` is used, this command generates a file :file:`hello.o`, which is the object file corresponding to your `VHDL` program.  This is not created on Windows.

Then, you have to build an executable file. Again, run :samp:`ghdl -e hello_world` in the `shell`. Option :option:`-e` means :dfn:`elaborate`, which is used to `'build'` a design, with the :samp:`hello_world` entity at the top of the hierarchy.

Last, launch the simulation running :samp:`ghdl -r hello_world` in the `shell`. The result of the simulation will be shown on the screen::

  Hello world!

.. TIP:: If a `GCC`/`LLVM` variant of `GHDL` is used, an executable program called :file:`hello_world` is generated at this step. which can be run


.. code-block:: shell

  $ ghdl -r hello_world

or directly:

.. code-block:: shell

  $ ./hello_world

On Windows or if the GCC backend was not enabled, no file is created.
The simulation is launched using this command:





A full adder
============

VHDL is generally used for hardware design.  This example starts with
a `full adder <https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder>`_ described in the :file:`adder.vhdl` file:

.. code-block:: VHDL

  entity adder is
    -- `i0`, `i1` and the carry-in `ci` are inputs of the adder.
    -- `s` is the sum output, `co` is the carry-out.
    port (i0, i1 : in bit; ci : in bit; s : out bit; co : out bit);
  end adder;

  architecture rtl of adder is
  begin
     --  This full-adder architecture contains two concurrent assignment.
     --  Compute the sum.
     s <= i0 xor i1 xor ci;
     --  Compute the carry.
     co <= (i0 and i1) or (i0 and ci) or (i1 and ci);
  end rtl;


You can analyze this design file:

.. code-block:: shell

  $ ghdl -a adder.vhdl


You can try to execute the `adder` design, but this is useless,
since nothing externally visible will happen.  In order to
check this full adder, a testbench has to be run.  This testbench is
very simple, since the adder is also simple: it checks exhaustively all
inputs.  Note that only the behaviour is tested, timing constraints are
not checked.  The file :file:`adder_tb.vhdl` contains the testbench for
the adder:

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


As usual, you should analyze the design:

.. code-block:: shell

  $ ghdl -a adder_tb.vhdl

And build an executable for the testbench:

.. code-block:: shell

  $ ghdl -e adder_tb

You do not need to specify which object files are required: GHDL knows them
and automatically adds them in the executable.  Now, it is time to run the
testbench:

.. code-block:: shell

  $ ghdl -r adder_tb
  adder_tb.vhdl:52:7:(assertion note): end of test


If your design is rather complex, you'd like to inspect signals.  Signals
value can be dumped using the VCD file format.  The resulting file can be
read with a wave viewer such as GTKWave.  First, you should simulate your
design and dump a waveform file:

.. code-block:: shell

  $ ghdl -r adder_tb --vcd=adder.vcd

Then, you may now view the waves:

.. code-block:: shell

  $ gtkwave adder.vcd

See :ref:`Simulation_options`, for more details on the :option:`--vcd` option and
other runtime options.


Starting with a design
======================

Unless you are only studying VHDL, you will work with bigger designs than
the ones of the previous examples.

Let's see how to analyze and run a bigger design, such as the DLX model
suite written by Peter Ashenden which is distributed under the terms of the
GNU General Public License.  A copy is kept on
http://ghdl.free.fr/dlx.tar.gz

First, untar the sources:

.. code-block:: shell

  $ tar zxvf dlx.tar.gz


In order not to pollute the sources with the library, it is a good idea
to create a :file:`work/` subdirectory for the `WORK` library.  To
any GHDL commands, we will add the :option:`--workdir=work` option, so
that all files generated by the compiler (except the executable) will be
placed in this directory.

.. code-block:: shell

  $ cd dlx
  $ mkdir work


We will run the :samp:`dlx_test_behaviour` design.  We need to analyze
all the design units for the design hierarchy, in the correct order.
GHDL provides an easy way to do this, by importing the sources:

.. code-block:: shell

  $ ghdl -i --workdir=work *.vhdl


and making a design:

.. code-block:: shell

  $ ghdl -m --workdir=work dlx_test_behaviour


Before this second stage, GHDL knows all the design units of the DLX,
but no one have been analyzed.  The make command of GHDL analyzes and
elaborates a design.  This creates many files in the :file:`work/`
directory, and the :file:`dlx_test_behaviour` executable in the current
directory.

The simulation needs to have a DLX program contained in the file
:file:`dlx.out`.  This memory image will be be loaded in the DLX memory.
Just take one sample:

.. code-block:: shell

  $ cp test_loop.out dlx.out


And you can run the test suite:

.. code-block:: shell

  $ ghdl -r --workdir=work dlx_test_behaviour


The test bench monitors the bus and displays each instruction executed.
It finishes with an assertion of severity level note:

.. code-block:: shell

  dlx-behaviour.vhdl:395:11:(assertion note): TRAP instruction
   encountered, execution halted


Since the clock is still running, you have to manually stop the program
with the :kbd:`C-c` key sequence.  This behavior prevents you from running the
test bench in batch mode.  However, you may force the simulator to
stop when an assertion above or equal a certain severity level occurs:

.. code-block:: shell

  $ ghdl -r --workdir=work dlx_test_behaviour --assert-level=note


With this option, the program stops just after the previous message::

  dlx-behaviour.vhdl:395:11:(assertion note): TRAP instruction
   encountered, execution halted
  error: assertion failed


If you want to make room on your hard drive, you can either:

* clean the design library with the GHDL command:

  .. code-block:: shell

    $ ghdl --clean --workdir=work

  This removes the executable and all the object files.  If you want to
  rebuild the design at this point, just do the make command as shown
  above.
  
* remove the design library with the GHDL command:

  .. code-block:: shell

    $ ghdl --remove --workdir=work

  This removes the executable, all the object files and the library file.
  If you want to rebuild the design, you have to import the sources again,
  and to make the design.
  
* remove the :file:`work/` directory:

  .. code-block:: shell

    $ rm -rf work

  Only the executable is kept.  If you want to rebuild the design, create
  the :file:`work/` directory, import the sources, and make the design.

Sometimes, a design does not fully follow the VHDL standards.  For example it
uses the badly engineered :samp:`std_logic_unsigned` package.  GHDL supports
this VHDL dialect through some options::

  --ieee=synopsys -fexplicit

See :ref:`IEEE_library_pitfalls`, for more details.

Further examples
=======================

.. TODO::

  * Add references to examples/tutorials with GHDL.
  * Shall `René Doß <https://mail.gna.org/public/ghdl-discuss/2017-01/msg00000.html>` want to contribute adapting his article to RST?