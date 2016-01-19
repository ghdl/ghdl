# GHDL

This directory contains the sources of GHDL, the VHDL simulator.

GHDL is not an interpreter: it generates machine code from your design,
the only way for high speed simulation.

GHDL fully supports IEEE 1076-1987, IEEE 1076-1993, IEE 1076-2002 and
partially the 1076-2008 version of VHDL.

Main features are:
- available on Linux (x86 and x86-64), Windows and Mac OS x
- handle very large designs like leon3/grlib.
- can write waveforms to a vcd or fst file.
- partial support of PSL
- support vunit (https://github.com/LarsAsplund/vunit)
- support OSVVM (http://osvvm.org)
- support cocotb (https://github.com/potentialventures/cocotb) through the VPI interface

GHDL is free software.  See the file COPYING for copying permission.
The manuals, and some of the runtime libraries, are under different
terms; see the individual source files for details.

Doc is available on http://ghdl.readthedocs.org/en/latest/index.html

## Building GHDL (Short instructions)

If you aren't on linux, see below for full instructions.

You need GNAT GPL 2014 (or later) for x86 (32 or 64 bits).  GNAT is the GNU Ada
compiler and GNAT GPL is very easy to install (download anonymously from
libre.adacore.com, untar and run the doinstall script).  You also need
zlib (for Debian or Ubuntu: install zlib1g-dev package).

In the GHDL source directory, configure and build:
```sh
$ ./configure --prefix=/usr/local
$ make
```

At that place, you can already use the 'ghdl_mcode' built in the directory.
You can also install GHDL (the executable is installed as 'ghdl'):
```sh
$ make install
```

That's all!

## Build GHDL (Long instructions)

See BUILD.txt
