.. program:: ghdl
.. _Examples:VHPIDIRECT:Demo:

VHPIDIRECT Demo
===============

The following sources show how to pass different types of data to/from VHDL and C through VHPIDIRECT.

.. NOTE:: :file:`ghdl.h` is a reference of GHDL's ABI, which can be imported to easily convert data types. However, the ABI is not settled, so it might change without prior notice.

.. literalinclude:: run.sh
   :language: bash

.. literalinclude:: tb.vhd
   :language: vhdl

.. literalinclude:: main.c
   :language: c

.. literalinclude:: ghdl.h
   :language: c
