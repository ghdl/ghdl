.. program:: ghdl
.. _GHW:

GHDL Waveform (GHW)
###################

The most widespread format for dumping waveforms is Value Change Dump (VCD), defined in the Verilog language (IEEE Std
1364-1995) and extended six years later as Extended VCD (EVCD), defined in IEEE Std 1364-2001.
VCD is simple and compact, which allowed it to be used in fields other than Verilog simulation tools.
For instance, GHDL supports dumping VCD files.

However, VCD/EVCD cannot handle certain signal types from the VHDL language.
There is neither any equivalent in the VHDL LRM.
So, the author of GHDL, Tristan Gingold, implemented an alternative format named GHW, for allowing all VHDL types to be
dumped.
He also contributed a reader to GTKWave based on libghw (see `gtkwave/gtkwave/search?q=libghw <https://github.com/gtkwave/gtkwave/search?q=libghw>`__ and `gtkwave/gtkwave: gtkwave3/src/ghw.c <https://github.com/gtkwave/gtkwave/blob/master/gtkwave3/src/ghw.c>`__),
which allows visualizing GHW waves.

The GHW format is not completely fixed, and it might change slightly as new language features are implemented in GHDL
or as a result of internal tweaks.
Nevertheless, the GHDL codebase (:ghdlsrc:`grt/grt-waves.adb <grt/grt-waves.adb>`) is kept in sync with the utilities in
subdir :ghdlsrc:`ghw <../ghw>`.

.. TIP::
  In `nturley/ghw-notes <https://github.com/nturley/ghw-notes>`__, there is some work for defining the GHW format as a
  Kaitai Struct (see `kaitai.io <https://kaitai.io/>`__).

libghw
======

GHW reading features are provided as a shared library, which is built and installed with GHDL by default.

ghwdump
=======

For debugging and learning purposes, ghwdump uses ghdlib for dumping the content of GHW waves into text files.
ghwdump is also built and installed with GHDL by default.
It's used in the GHDL test suite for catching regressions.
