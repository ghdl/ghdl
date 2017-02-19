# GHDL

[![Join the chat at https://gitter.im/ghdl1/Lobby](https://badges.gitter.im/ghdl1/Lobby.svg)](https://gitter.im/ghdl1/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) 

Build status:
	- [![Linux containers at Travis-CI](https://travis-ci.org/tgingold/ghdl.svg?branch=master)](https://travis-ci.org/tgingold/ghdl)
	- [![Windows VMs at AppVeyor](https://ci.appveyor.com/api/projects/status/r5dtv6amsppigpsp/branch/release?svg=true)](https://ci.appveyor.com/project/Paebbels/poc/branch/release)
Distribution:
	- ![Latest tag](https://img.shields.io/github/tag/tgingold/ghdl.svg?style=flat)
	- [![Latest release](https://img.shields.io/github/release/tgingold/ghdl.svg?style=flat)](https://github.com/tgingold/ghdl/releases)
	- ![GNU General Public License 2](https://img.shields.io/github/license/tgingold/ghdl.svg?style=flat)
	
---
	
This directory contains the sources of GHDL, the VHDL simulator.

GHDL is not an interpreter: it generates machine code from your design,
the only way for high speed simulation.

GHDL fully supports IEEE 1076-1987, IEEE 1076-1993, IEEE 1076-2002 and
partially the 1076-2008 version of VHDL.

Main features are:
- available on Linux (x86 and x86-64), Windows and Mac OS x
- handle very large designs like leon3/grlib.
- can write waveforms to a vcd or fst file.
- partial support of PSL
- support vunit (https://vunit.github.io)
- support OSVVM (http://osvvm.org)
- support cocotb (https://github.com/potentialventures/cocotb) through the VPI interface

GHDL is free software.  See the file COPYING for copying permission.
The manuals, and some of the runtime libraries, are under different
terms; see the individual source files for details.

Doc is available on https://ghdl.readthedocs.org/en/latest/index.html

## Building GHDL (Short instructions)

If you aren't on linux, see below for full instructions.

You need GNAT GPL 2014 (or later) for x86 (32 or 64 bits).  GNAT is the GNU Ada
compiler and GNAT GPL is very easy to install (download anonymously from
libre.adacore.com, untar and run the doinstall script).  You also need
zlib (for Debian or Ubuntu: install zlib1g-dev package).

In the GHDL base directory, configure and build:
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

See [BUILD.txt](https://github.com/tgingold/ghdl/blob/master/BUILD.txt)
