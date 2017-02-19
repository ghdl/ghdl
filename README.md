[![Documentation Status](https://readthedocs.org/projects/ghdl/badge/?version=latest)](http://ghdl.readthedocs.io/en/latest/?badge=latest) [![Join the chat at https://gitter.im/ghdl1/Lobby](https://badges.gitter.im/ghdl1/Lobby.svg)](https://gitter.im/ghdl1/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[![Linux containers at Travis-CI](https://travis-ci.org/tgingold/ghdl.svg?branch=master)](https://travis-ci.org/tgingold/ghdl) [![Windows VMs at AppVeyor](https://ci.appveyor.com/api/projects/status/rsq60m5wcly3og8j?svg=true)](https://ci.appveyor.com/project/tgingold/ghdl) ![Latest tag](https://img.shields.io/github/tag/tgingold/ghdl.svg?style=flat) [![Latest release](https://img.shields.io/github/release/tgingold/ghdl.svg?style=flat)](https://github.com/tgingold/ghdl/releases)

# GHDL

This directory contains the sources of GHDL, the open-source compilator and simulator for [VHDL](https://en.wikipedia.org/wiki/VHDL), a Hardware Description Language ([HDL](https://en.wikipedia.org/wiki/Hardware_description_language)). GHDL is not an interpreter: it allows you to analyse and elaborate sources to generate machine code from your design. Native program execution is the only way for high speed simulation.

## Main features

Full support for the [1987](http://ieeexplore.ieee.org/document/26487/), [1993](http://ieeexplore.ieee.org/document/392561/), [2002](http://ieeexplore.ieee.org/document/1003477/) versions of the [IEEE](www.ieee.org) [1076](http://standards.ieee.org/develop/wg/P1076.html) VHDL standard, and partial for the latest [2008](http://ieeexplore.ieee.org/document/4772740/) revision.

Partial support of [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language).

By using a code generator ([LLVM](http://llvm.org/), [GCC](http://gcc.gnu.org/) or, [x86_64](https://en.wikipedia.org/wiki/X86-64)/[i386](https://en.wikipedia.org/wiki/Intel_80386) only, a built-in one), it is much faster than any interpreted simulator. It can handle very large designs, such as [leon3/grlib](http://www.gaisler.com/index.php/downloads/leongrlib).

GHDL runs on [GNU/Linux](http://en.wikipedia.org/wiki/Linux_distribution), [Windows](http://en.wikipedia.org/wiki/Microsoft_Windows) and [macOS](http://en.wikipedia.org/wiki/MacOS), both on `x86` and on `x86_64`. You can freely download a binary distribution for your OS, or try to build it on your own machine (see *'Getting GHDL'* below).

Can write waveforms to a [GHW](http://ghdl.readthedocs.io/en/latest/using/Simulation.html?highlight=GHW#cmdoption-wave), [VCD](https://en.wikipedia.org/wiki/Value_change_dump) or FST file. Combined with a [GUI](http://en.wikipedia.org/wiki/Graphical_user_interface)-based [waveform viewer](https://en.wikipedia.org/wiki/Waveform_viewer) and a good text editor, GHDL is a very powerful tool for writing, testing and simulating your code.

Supported third party projects: [VUnit](https://vunit.github.io), [OSVVM](http://osvvm.org), [cocotb](https://github.com/potentialventures/cocotb) (through the [VPI interface](https://en.wikipedia.org/wiki/Verilog_Procedural_Interface)), ...

GHDL is free software:

- [![GNU General Public License 2](https://img.shields.io/github/license/tgingold/ghdl.svg?style=flat)](https://github.com/tgingold/ghdl/blob/master/COPYING.md)
- Doc is available on [ghdl.readthedocs.org](https://ghdl.readthedocs.org/en/latest/index.html).
- Some of the runtime libraries, are under different terms; see the individual source files for details.

## Getting GHDL

### Pre-built releases

You can download pre-built releases for different platforms, backends and library versions from [GitHub releases](https://github.com/1138-4EB/ghdl/releases).

### Building GHDL

You need the GNU Ada compiler, GNAT GPL, 2014 (or later) for x86 (32 or 64 bits). GNAT GPL can be downloaded anonymously from [libre.adacore.com](http://libre.adacore.com/tools/gnat-gpl-edition/). Then, untar and run the *doinstall* script.

*Depending on the OS and distribution you are using, you will also need to install some toolchain dependencies, such as `zlib`. See '[Building](http://ghdl.readthedocs.io/en/latest/building/index.html)' for specific package names.*

In the GHDL base directory, configure and build:
```sh
$ ./configure --prefix=/usr/local
$ make
```

At that place, you can already use the 'ghdl_mcode' built in the directory. You can also install GHDL:

```sh
$ make install
```

That's all!

*The executable is installed as 'ghdl' in `/usr/local`. To install it to a different path, change the `--prefix` in the call to `configure`. For example, on Windows, you may want to set it to `--prefix=/c/Program Files (x86)/GHDL`.*

---

Furthermore, each supported compiler has its pros and cons. Here is a short comparaison:

| | pros | cons | observations |
|---|---|---|---|
|mcode | very easy to build | x86_64/i386 only | no executable created from your design |
| | very quick analysis time and can analyze very big designs | simulation is slower | |
| GCC | generated code is faster | analyze can take time (particularly for big units) | the output is an executable |
| | generated code can be debugged | build is more complex |
| | many platforms (x86, x86_64, powerpc, sparc) | |

LLVM has the same pros/cons as GCC, but it is easier to build. However, coverage (`gcov`) is unique to GCC.

You can find specific instructions for each of the options in '[Building](http://ghdl.readthedocs.io/en/latest/building/index.html)'.
