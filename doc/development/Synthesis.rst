.. _DEV:Synthesis:

Synthesis
#########

There is an experimental command (``--synth``) to generate RTL netlists (the format, VHDL or EDIF, is yet to be defined) from synthesisable code. For command ``--synth`` to be available, GHDL must be configured/built with option ``--enable-synth`` (GCC 8.1>= required, due to some new GNAT features which are only available in recent releases). Since this is a proof-of-concept, the output is mostly a dump of an internal structure for now. Therefore, it is not very useful, except for debugging.

Moreover, `ghdlsynth <https://github.com/tgingold/ghdlsynth-beta>`_ is a complementary repository that lets GHDL to be loaded by `yosys <http://www.clifford.at/yosys/>`_ as a frontend plugin module, in order to generate bitstreams for some FPGA devices.
