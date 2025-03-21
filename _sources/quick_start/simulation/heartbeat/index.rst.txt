.. program:: ghdl
.. _QuickStart:heartbeat:

`Heartbeat` module
==================

Although :ref:`Hello world <QuickStart:hello>` illustrates that `VHDL` is supported as a general purpose language, the
main use case of `GHDL` is to simulate hardware descriptions.
The following block, which is saved in a file named :file:`heartbeat.vhdl`, is an example of how to generate a 100 MHz
clock signal with non-synthesisable VHDL:

.. literalinclude:: heartbeat.vhdl
   :language: vhdl

It can be :ref:`analysed <Analysis:command>`, :ref:`elaborated <Elaboration:command>` and :ref:`run <Run:command>`, as
you already know:

.. code-block:: shell

   ghdl -a heartbeat.vhdl
   ghdl -e heartbeat
   ghdl -r heartbeat

However, execution of the design does not terminate. At the same time, no output is shown on screen. This is because,
traditionally, hardware designs are continuously running devices which do not have a screen where to print. In this
context, inspection and verification of the behaviour is done through :wikipedia:`waveforms <Waveform_viewer>`,
which is supported by `GHDL` (see :ref:`export_waves`). You can use either :option:`--wave`, :option:`--vcd`,
:option:`--vcdgz` or :option:`--fst` to save the signals of the simulation to a file. Then, terminate the execution
(:kbd:`C-c`) and you can inspect the wave with a viewer, such as `GtkWave <http://gtkwave.sourceforge.net/>`_. As
explained in the `manual <http://gtkwave.sourceforge.net/gtkwave.pdf>`_, GtkWave *'relies on a post-mortem approach
through the use of dumpfiles'*. Therefore, you should first simulate your design and dump a waveform file, say GHW:

.. code-block:: shell

   ghdl -r heartbeat --wave=wave.ghw

Then, you can view the dump:

.. code-block:: shell

   gtkwave wave.ghw

Of course, manually terminating the simulation is for illustration purposes only.
In :ref:`Full adder <QuickStart:adder>` and :ref:`QuickStart:DLX`, you will see how to write a testbench to terminate
the simulation programmatically.
