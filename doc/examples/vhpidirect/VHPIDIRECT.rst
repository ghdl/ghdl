.. _Examples:VHPIDIRECT:

Data exchange through VHPIDIRECT
################################

.. toctree::
   :hidden:

   demo/README

See :ref:`Examples:VHPIDIRECT:Demo` for a demo about how to pass different types of data to/from VHDL and C through VHPIDIRECT.

VUnit
=====

`VUnit <https://github.com/VUnit/vunit>`_ is an open source unit testing framework for VHDL/SystemVerilog. Sharing memory buffers between foreign C or Python applications and VHDL testbenches is supported through GHDL's VHPIDIRECT features. Buffers are accessed from VHDL as either strings, arrays of bytes or arrays of 32 bit integers. See VUnit example `external buffer <https://github.com/VUnit/vunit/tree/master/examples/vhdl/external_buffer>`_ for details about the API.

ghdlex and netpp
================

`netpp (network property protocol) <https://section5.ch/index.php/netpp/>`_ is a communication library allowing to expose variables or other properties of an application to the network as abstract 'Properties'. Its basic philosophy is that a device always knows its capabilities. netpp capable devices can be explored by command line, Python scripts or GUI applications. Properties of a device - be it virtual or real - are typically described by a static description in an XML device description language, but they can also be constructed on the fly.

`ghdlex <https://github.com/hackfin/ghdlex>`_ is a set of C extensions to facilitate data exchange between a GHDL simulation and external applications. VHPIDIRECT mechanisms are used to wrap GHDL data types into structures usable from a C library. `ghdlex` uses the `netpp <https://section5.ch/index.php/netpp/>`_ library to expose virtual entities (such as pins or RAM) to the network. It also demonstrates simple data I/O through unix pipes. A few VHDL example entities are provided, such as a virtual console, FIFOs, RAM.

The author of `netpp` and `ghdlex` is also working on `MaSoCist <https://github.com/hackfin/MaSoCist>`_, a linux'ish build system for System on Chip designs, based on GHDL. It allows to handle more complex setup, e.g. how a RISC-V architecture (for example) is regress-tested using a virtual debug interface.
