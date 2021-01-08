<p align="center">
  <a title="Documentation" href="https://ghdl.github.io/ghdl"><img src="https://img.shields.io/website.svg?label=ghdl.github.io%2Fghdl&longCache=true&style=flat-square&url=http%3A%2F%2Fghdl.github.io%2Fghdl%2Findex.html"></a><!--
  -->
  <a title="Join the chat at https://gitter.im/ghdl1/Lobby" href="https://gitter.im/ghdl1/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://img.shields.io/badge/chat-on%20gitter-4db797.svg?longCache=true&style=flat-square&logo=gitter&logoColor=e8ecef"></a><!--
  -->
  <a title="Docker Images" href="https://github.com/ghdl/docker"><img src="https://img.shields.io/docker/pulls/ghdl/ghdl.svg?logo=docker&logoColor=e8ecef&style=flat-square&label=docker"></a><!--
  -->
  <a title="Releases" href="https://github.com/ghdl/ghdl/releases"><img src="https://img.shields.io/github/commits-since/ghdl/ghdl/latest.svg?longCache=true&style=flat-square"></a>
</p>

<p align="center">
  <img src="./doc/_static/logo.png"/>
</p>

<p align="center">
  <a title="CII Best Practices" href="https://bestpractices.coreinfrastructure.org/en/projects/3157"><img src="https://img.shields.io/cii/percentage/3157??longCache=true&style=flat-square"></a><!--
  -->
  <a title="'push' workflow Status" href="https://github.com/ghdl/ghdl/actions?query=workflow%3Apush"><img alt="'push' workflow Status" src="https://img.shields.io/github/workflow/status/ghdl/ghdl/push?longCache=true&style=flat-square&label=push&logo=github%20actions&logoColor=fff"></a><!--
  -->
  <a title="'doc' workflow Status" href="https://github.com/ghdl/ghdl/actions?query=workflow%3Adoc"><img alt="'doc' workflow Status" src="https://img.shields.io/github/workflow/status/ghdl/ghdl/doc?longCache=true&style=flat-square&label=doc&logo=github%20actions&logoColor=fff"></a><!--
  -->
  <a title="Linux/Mac boxes at Travis-CI" href="https://travis-ci.org/ghdl/ghdl/branches"><img src="https://img.shields.io/travis/ghdl/ghdl/master.svg?longCache=true&style=flat-square&logo=travis-ci&logoColor=e8ecef"></a><!--
  -->
  <a title="AppVeyor branch" href="https://ci.appveyor.com/project/tgingold/ghdl-psgys/history"><img src="https://img.shields.io/appveyor/ci/tgingold/ghdl-psgys/master.svg?logo=appveyor&logoColor=e8ecef&style=flat-square"></a>
</p>

This directory contains the sources of GHDL, the open-source analyzer, compiler, simulator and (experimental) synthesizer for [VHDL](https://en.wikipedia.org/wiki/VHDL), a Hardware Description Language ([HDL](https://en.wikipedia.org/wiki/Hardware_description_language)). GHDL is not an interpreter: it allows you to analyse and elaborate sources to generate machine code from your design. Native program execution is the only way for high speed simulation.

# Main features

Full support for the [1987](https://ieeexplore.ieee.org/document/26487/), [1993](https://ieeexplore.ieee.org/document/392561/), [2002](https://ieeexplore.ieee.org/document/1003477/) versions of the [IEEE](https://www.ieee.org) [1076](https://standards.ieee.org/develop/wg/P1076.html) VHDL standard, and partial for the latest [2008](https://ieeexplore.ieee.org/document/4772740/) revision.

Partial support of [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language).

By using a code generator ([LLVM](https://llvm.org/), [GCC](https://gcc.gnu.org/) or, [x86_64](https://en.wikipedia.org/wiki/X86-64)/[i386](https://en.wikipedia.org/wiki/Intel_80386) only, a built-in one), it is much faster than any interpreted simulator. It can handle very large designs, such as [leon3/grlib](https://www.gaisler.com/index.php/downloads/leongrlib).

GHDL runs on [GNU/Linux](https://en.wikipedia.org/wiki/Linux_distribution), [Windows](https://en.wikipedia.org/wiki/Microsoft_Windows) and [macOS](https://en.wikipedia.org/wiki/MacOS); on `x86`, `x86_64`, `armv6/armv7/aarch32` and `aarch64`. You can freely [download](https://github.com/ghdl/ghdl/releases) a binary distribution for your OS, use [GHDL Docker images](https://github.com/ghdl/docker), or try to build it on your own machine (see *'Getting GHDL'* below).

Can write waveforms to a [GHW](https://ghdl.github.io/ghdl/using/Simulation.html?highlight=GHW#cmdoption-wave), [VCD](https://en.wikipedia.org/wiki/Value_change_dump) or FST file. Combined with a [GUI](https://en.wikipedia.org/wiki/Graphical_user_interface)-based [waveform viewer](https://en.wikipedia.org/wiki/Waveform_viewer) and a good text editor, GHDL is a very powerful tool for writing, testing and simulating your code.

Supported third party projects: [cocotb](https://github.com/potentialventures/cocotb) (through the [VPI interface](https://en.wikipedia.org/wiki/Verilog_Procedural_Interface)), [OSVVM](https://osvvm.org), [UVVM](https://github.com/UVVM/UVVM), [VUnit](https://vunit.github.io), ...

GHDL is free software:

- [![GNU General Public License 2](https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?longCache=true&style=flat-square&label=license&logo=gnu)](https://github.com/ghdl/ghdl/blob/master/COPYING.md)
- [![Creative Commons Attribution-ShareAlike](https://img.shields.io/badge/doc%20license-Creative%20Commons%20Attribution--ShareAlike--4.0-bf7600.svg?longCache=true&style=flat-square&logo=Creative%20Commons)](https://github.com/ghdl/ghdl/blob/master/doc/COPYING_DOC.md) available at [ghdl.github.io/ghdl](https://ghdl.github.io/ghdl).
- Some of the runtime libraries, are under different terms; see the individual source files for details.

# Getting GHDL

- Pre-built packages:
  - Approximately once a year, tagged binary distributions are made available through the [releases](https://github.com/ghdl/ghdl/releases) tab.
  - After each succesful CI run, [nightly](https://github.com/ghdl/ghdl/releases/tag/nightly) tarballs/zipfiles for Ubuntu Focal and Windows (MSYS2) are updated.
- For using GHDL in CI, [setup-ghdl-ci](https://github.com/ghdl/setup-ghdl-ci) is provided. It is a GitHub Action (see [github.com/features/actions](https://github.com/features/actions)) to setup GHDL in just 3 lines.
- You may use [GHDL Docker images](https://github.com/ghdl/docker) in case your didn't find a suitable release.
- Build GHDL yourself (see below)!

## Building GHDL

GHDL currently supports three different back-ends (code generators). Each has its pros and cons. You can find specific instructions for each of the options in '[Building](https://ghdl.github.io/ghdl/getting/)'.

### TL;DR

In order to follow the traditional way to `configure` and `make`, you need an Ada compiler. Most GNU/Linux package managers provide a package named `gcc-ada` or `gcc-gnat`.

> Alternatively, GNAT GPL can be downloaded anonymously from [libre.adacore.com](https://libre.adacore.com/tools/gnat-gpl-edition/) (later than 2017 is suggested; for x86, 32 or 64 bits). Then, untar and run the *doinstall* script.

> Depending on the OS and distribution you are using, you will also need to install some toolchain dependencies, such as `zlib`. See '[Building](https://ghdl.github.io/ghdl/getting/)' for specific package names.

To use mcode backend (easiest to build), in the GHDL base directory, configure and build:

```sh
$ ./configure --prefix=/usr/local
$ make
```

At that place, you can already use the `ghdl_mcode` built in the directory. You can also install GHDL:

```sh
$ make install
```

That's all!

> The executable is installed as 'ghdl' in `/usr/local`. To install it to a different path, change the `--prefix` in the call to `configure`. For example, on Windows, you may want to set it to `--prefix=/c/Program Files (x86)/GHDL`.

# Project structure

## Regular users

- The 'regular' tool allows analysis, compilation, simulation and (very experimental) synthesis of VHDL 1993 netlists. It is written in Ada and C, and three different backends are supported, which are sometimes named `ghdl_mcode`, `ghdl_gcc` and `ghdl_llvm`. This is the entrypoint for most users.

- `ghdl-ls` implements Language Server Protocol (LSP) in Python. VHDL analysis features provided by GHDL are accessed through `libghdl-py`. This can be integrated in text editors or IDES, such as, Vim, Emacs, Atom or Visual Studio Code.

- [vscode-client](https://github.com/ghdl/ghdl-language-server/tree/master/vscode-client) is an extension for [Visual Studio Code (VSC)](https://code.visualstudio.com/) to provide language support for VHDL by interfacing `ghdl-ls`.

## Advanced users

- `libghdl` is a shared library that includes a subset of the regular features plus some features to be used by extension tools (i.e. `libghdl-py`). This is built along with the regular GHDL and it supports both non-synthesisable and synthesisable code. Nonetheless, this is not for users, but for tools built on top of the core. When configured along with `--enable-synth`, this shared library includes **[experimental]** synthesis features too.

- [pyGHDL](pyGHDL) is a Python interface to `libghdl`. Currently, it is only used by `ghdl-ls`; however, it can be useful for advanced users which are willing to build Python utilities based on GHDL.

<p align="center">
  <a title="Codecov - Branch Coverage" href="https://codecov.io/gh/ghdl/ghdl"><img alt="Codecov - Branch Coverage" src="https://img.shields.io/codecov/c/github/ghdl/ghdl?longCache=true&style=flat-square&logo=Codecov&logoColor=fff"></a><!--
  -->
  <a title="Codacy - Quality" href="https://app.codacy.com/gh/ghdl/ghdl/dashboard"><img alt="Codacy - Quality" src="https://img.shields.io/codacy/grade/dc4f2c76ba7e4f598389ab1f8d7d53e9?longCache=true&style=flat-square&logo=Codacy&logoColor=fff"></a><!--
  -->
  <a title="Codacy - Coverage" href="https://app.codacy.com/gh/ghdl/ghdl/dashboard"><img alt="Codacy - Coverage" src="https://img.shields.io/codacy/coverage/dc4f2c76ba7e4f598389ab1f8d7d53e9?longCache=true&style=flat-square&logo=Codacy&logoColor=fff"></a><!--
  -->
</p>

- **[experimental]** [ghdl-yosys-plugin](https://github.com/ghdl/ghdl-yosys-plugin) is the integration of GHDL as a frontend plugin module for [Yosys Open SYnthesis Suite](http://www.clifford.at/yosys/), which uses the `libghdl` library (built with `--enable-synth`).

- **[deprecated]** `ghdl_simul`, which supports interpreted simulation, is available for historical reasons and for development/debugging only. It is very slow compared to the 'regular' compiled simulation and not all the features are supported.
