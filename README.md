[![Read the Docs](https://img.shields.io/readthedocs/ghdl.svg?longCache=true&style=flat-square&logo=read-the-docs&logoColor=e8ecef)](http://ghdl.readthedocs.io) [![Join the chat at https://gitter.im/ghdl1/Lobby](https://img.shields.io/badge/chat-on%20gitter-4db797.svg?longCache=true&style=flat-square&logo=gitter&logoColor=4db797)](https://gitter.im/ghdl1/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Linux/Mac boxes at Travis-CI](https://img.shields.io/travis/ghdl/ghdl/master.svg?longCache=true&style=flat-square&logo=travis)](https://travis-ci.org/ghdl/ghdl/branches) ![AppVeyor branch](https://img.shields.io/appveyor/ci/tgingold/ghdl-psgys/master.svg?logo=appveyor&logoColor=aab2ab&style=flat-square) [![Commits since latest release](https://img.shields.io/github/commits-since/ghdl/ghdl/latest.svg?longCache=true&style=flat-square)](https://github.com/ghdl/ghdl/releases)

**A new GitHub organization was created (2017-12-20) and the main repo was moved from [github.com/tgingold/ghdl](https://github.com/tgingold/ghdl) to [github.com/ghdl/ghdl](https://github.com/ghdl/ghdl). Old refs will continue working, because permanent redirects are set up. However, we suggest every contributor to update the remote URLs in their local clones. See
[Changing a remote's URL](https://help.github.com/articles/changing-a-remote-s-url/).**

# GHDL

This directory contains the sources of GHDL, the open-source compiler and simulator for [VHDL](https://en.wikipedia.org/wiki/VHDL), a Hardware Description Language ([HDL](https://en.wikipedia.org/wiki/Hardware_description_language)). GHDL is not an interpreter: it allows you to analyse and elaborate sources to generate machine code from your design. Native program execution is the only way for high speed simulation.

## Main features

Full support for the [1987](http://ieeexplore.ieee.org/document/26487/), [1993](http://ieeexplore.ieee.org/document/392561/), [2002](http://ieeexplore.ieee.org/document/1003477/) versions of the [IEEE](www.ieee.org) [1076](http://standards.ieee.org/develop/wg/P1076.html) VHDL standard, and partial for the latest [2008](http://ieeexplore.ieee.org/document/4772740/) revision.

Partial support of [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language).

By using a code generator ([LLVM](http://llvm.org/), [GCC](http://gcc.gnu.org/) or, [x86_64](https://en.wikipedia.org/wiki/X86-64)/[i386](https://en.wikipedia.org/wiki/Intel_80386) only, a built-in one), it is much faster than any interpreted simulator. It can handle very large designs, such as [leon3/grlib](http://www.gaisler.com/index.php/downloads/leongrlib).

GHDL runs on [GNU/Linux](http://en.wikipedia.org/wiki/Linux_distribution), [Windows](http://en.wikipedia.org/wiki/Microsoft_Windows) and [macOS](http://en.wikipedia.org/wiki/MacOS), both on `x86` and on `x86_64`. You can freely download a binary distribution for your OS, or try to build it on your own machine (see *'Getting GHDL'* below).

Can write waveforms to a [GHW](http://ghdl.readthedocs.io/en/latest/using/Simulation.html?highlight=GHW#cmdoption-wave), [VCD](https://en.wikipedia.org/wiki/Value_change_dump) or FST file. Combined with a [GUI](http://en.wikipedia.org/wiki/Graphical_user_interface)-based [waveform viewer](https://en.wikipedia.org/wiki/Waveform_viewer) and a good text editor, GHDL is a very powerful tool for writing, testing and simulating your code.

Supported third party projects: [VUnit](https://vunit.github.io), [OSVVM](http://osvvm.org), [cocotb](https://github.com/potentialventures/cocotb) (through the [VPI interface](https://en.wikipedia.org/wiki/Verilog_Procedural_Interface)), ...

GHDL is free software:

- [![GNU General Public License 2](https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?longCache=true&style=flat-square&label=license&logo=gnu)](https://github.com/ghdl/ghdl/blob/master/COPYING.md)
- [![Creative Commons Attribution-ShareAlike](https://img.shields.io/badge/doc%20license-Creative%20Commons%20Attribution--ShareAlike--4.0-aab2ab.svg?longCache=true&style=flat-square)](https://github.com/ghdl/ghdl/blob/master/doc/COPYING_DOC.md) available at [ghdl.readthedocs.io](https://ghdl.readthedocs.io)
- Some of the runtime libraries, are under different terms; see the individual source files for details.

## Getting GHDL

### Pre-built releases

Periodically (not regularly), several binary distributions are made available through the [releases](https://github.com/ghdl/ghdl/releases) tab. If you can't find the one matching the platform and versions you need, you can build it yourself!

### Building GHDL

In order to follow the traditional way to `configure` and `make`, you need the GNU Ada compiler, GNAT GPL, 2014 (or later) for x86 (32 or 64 bits). GNAT GPL can be downloaded anonymously from [libre.adacore.com](http://libre.adacore.com/tools/gnat-gpl-edition/). Then, untar and run the *doinstall* script.

*Depending on the OS and distribution you are using, you will also need to install some toolchain dependencies, such as `zlib`. See '[Building](http://ghdl.readthedocs.io/en/latest/building/Building.html)' for specific package names.*

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

You can find specific instructions for each of the options in '[Building](http://ghdl.readthedocs.io/en/latest/building/Building.html)'.
