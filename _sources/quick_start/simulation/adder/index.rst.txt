.. program:: ghdl
.. _QuickStart:adder:

`Full adder` module and testbench
=================================

Unlike :ref:`Heartbeat <QuickStart:heartbeat>`, the target hardware design in this example is written using the
synthesisable subset of `VHDL`. It is a :wikipedia:`full adder <Adder_(electronics)#Full_adder>` described in a file
named :file:`adder.vhdl`:

.. literalinclude:: adder.vhdl
   :language: vhdl

You can :ref:`analyse <Analysis:command>` this design file, ``ghdl -a adder.vhdl``, and try to execute the `adder`
design. But this is useless, since nothing externally visible will happen. In order to check this full adder, a
:dfn:`testbench` has to be run. The :dfn:`testbench` is a description of how to generate inputs and how to check the
outputs of the Unit Under Test (UUT). This one is very simple, since the adder is also simple: it checks exhaustively
all inputs. Note that only the behaviour is tested, timing constraints are not checked. A file named
:file:`adder_tb.vhdl` contains the testbench for the adder:

.. literalinclude:: adder_tb.vhdl
   :language: vhdl

As usual, you should analyze the file, ``ghdl -a adder_tb.vhdl``.

.. HINT::
   Then, if required, :ref:`elaborate <Elaboration:command>` the testbench: ``ghdl -e adder_tb``. You do not need to
   specify which object files are required, since `GHDL` knows them and automatically adds them.

Now, it is time to :ref:`run <Run:command>` the testbench, ``ghdl -r adder_tb``, and check the result on screen::

  adder_tb.vhdl:52:7:(assertion note): end of test

If your design is rather complex, you'd like to inspect signals as explained in :ref:`Heartbeat <QuickStart:heartbeat>`.

See section :ref:`simulation_options`, for more details on other runtime options.
