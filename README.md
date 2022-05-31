<p align="center">
  <img src="./doc/_static/logo.png"/>
</p>

<p align="center">
  <a title="Documentation" href="https://ghdl.github.io/ghdl"><img src="https://img.shields.io/website.svg?label=ghdl.github.io%2Fghdl&longCache=true&style=flat-square&url=http%3A%2F%2Fghdl.github.io%2Fghdl%2Findex.html&logo=GitHub"></a><!--
  -->
  <a title="Join the chat at https://gitter.im/ghdl1/Lobby" href="https://gitter.im/ghdl1/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://img.shields.io/badge/Chat-on%20gitter-4db797.svg?longCache=true&style=flat-square&logo=gitter&logoColor=e8ecef"></a><!--
  -->
  <a title="Docker Images" href="https://github.com/ghdl/docker"><img src="https://img.shields.io/docker/pulls/ghdl/ghdl.svg?logo=docker&logoColor=e8ecef&style=flat-square&label=Docker"></a><!--
  -->
  <a title="Releases" href="https://github.com/ghdl/ghdl/releases"><img src="https://img.shields.io/github/commits-since/ghdl/ghdl/latest.svg?longCache=true&style=flat-square&logo=GitHub"></a><!--
  -->
</p>
<p align="center">
  <a title="'Test' workflow Status" href="https://github.com/ghdl/ghdl/actions?query=workflow%3ATest"><img alt="'Test' workflow Status" src="https://img.shields.io/github/workflow/status/ghdl/ghdl/Test?longCache=true&style=flat-square&label=Test&logo=github%20actions&logoColor=fff"></a><!--
  -->
  <a title="AppVeyor" href="https://ci.appveyor.com/project/tgingold/ghdl-psgys/history"><img src="https://img.shields.io/appveyor/ci/tgingold/ghdl-psgys/master.svg?logo=appveyor&logoColor=e8ecef&style=flat-square&label=Test"></a><!--
  -->
  <a title="CII Best Practices" href="https://bestpractices.coreinfrastructure.org/en/projects/3157"><img src="https://img.shields.io/cii/percentage/3157??longCache=true&style=flat-square&label=CII&logo=Linux%20Foundation"></a><!--
  -->
</p>

This directory contains the sources of GHDL, the open-source analyzer, compiler, simulator and (experimental) synthesizer for [VHDL](https://en.wikipedia.org/wiki/VHDL), a Hardware Description Language ([HDL](https://en.wikipedia.org/wiki/Hardware_description_language)). GHDL is not an interpreter: it allows you to analyse and elaborate sources for generating machine code from your design. Native program execution is the only way for high speed simulation.

# Main features

Full support for the [1987](https://ieeexplore.ieee.org/document/26487/), [1993](https://ieeexplore.ieee.org/document/392561/), [2002](https://ieeexplore.ieee.org/document/1003477/) versions of the [IEEE](https://www.ieee.org) [1076](https://standards.ieee.org/develop/wg/P1076.html) VHDL standard, and partial for the [2008](https://ieeexplore.ieee.org/document/4772740/) and [2019](https://ieeexplore.ieee.org/document/8938196/) revisions.

Partial support of [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language).

By using a code generator ([LLVM](https://llvm.org/), [GCC](https://gcc.gnu.org/) or, [x86_64](https://en.wikipedia.org/wiki/X86-64)/[i386](https://en.wikipedia.org/wiki/Intel_80386) only, a built-in one), it is much faster than any interpreted simulator. It can handle very large designs, such as [leon3/grlib](https://www.gaisler.com/index.php/downloads/leongrlib).

GHDL runs on [GNU/Linux](https://en.wikipedia.org/wiki/Linux_distribution), [Windows](https://en.wikipedia.org/wiki/Microsoft_Windows) and [macOS](https://en.wikipedia.org/wiki/MacOS); on `x86`, `x86_64`, `armv6/armv7/aarch32`, `aarch64` and `ppc64`. You can freely download [nightly](https://github.com/ghdl/ghdl/releases) assets, use OCI images (aka Docker/Podman containers), or try building it on your own machine (see *'Getting GHDL'* below).

Can write waveforms to [GHW](https://ghdl.github.io/ghdl/using/Simulation.html?highlight=GHW#cmdoption-wave), [VCD](https://en.wikipedia.org/wiki/Value_change_dump) or FST files. Combined with a [GUI](https://en.wikipedia.org/wiki/Graphical_user_interface)-based [waveform viewer](https://en.wikipedia.org/wiki/Waveform_viewer) and a good text editor, GHDL is a very powerful tool for writing, testing and simulating your code.

Co-simulation with foreign applications is supported through Verilog Procedural Interface (VPI) and/or VHPIDIRECT. See [ghdl.github.io/ghdl-cosim](https://ghdl.github.io/ghdl-cosim).

Can synthesize arbitrarily complex VHDL designs into a VHDL 1993 netlist, which can be implicitly or
explicitly used in open source or vendor synthesis frameworks.

Supported third party projects: [Yosys](https://github.com/YosysHQ/yosys) (through [ghdl-yosys-plugin](https://github.com/ghdl/ghdl-yosys-plugin)), [cocotb](https://github.com/potentialventures/cocotb) (through the VPI interface), [OSVVM](https://osvvm.org), [UVVM](https://github.com/UVVM/UVVM), [VUnit](https://vunit.github.io), ... (see [ghdl/extended-tests](https://github.com/ghdl/extended-tests)).

GHDL is free software:

- [![GNU General Public License 2](https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?longCache=true&style=flat-square&label=license&logo=gnu)](https://github.com/ghdl/ghdl/blob/master/COPYING.md)
- [![Creative Commons Attribution-ShareAlike](https://img.shields.io/badge/doc%20license-Creative%20Commons%20Attribution--ShareAlike--4.0-bf7600.svg?longCache=true&style=flat-square&logo=Creative%20Commons)](https://github.com/ghdl/ghdl/blob/master/doc/COPYING_DOC.md) available at [ghdl.github.io/ghdl](https://ghdl.github.io/ghdl).
- Some of the runtime libraries, are under different terms; see the individual source files for details.

# Getting GHDL

- Pre-built packages:
  - GHDL is available through the default package manager on most distributions: Debian/Ubuntu, Fedora, Arch Linux, MSYS2, etc.
  - After each succesful CI run, [nightly](https://github.com/ghdl/ghdl/releases/tag/nightly) tarballs/zipfiles for Ubuntu and Windows (MSYS2) are updated.
  - For using GHDL in CI, [setup-ghdl-ci](https://github.com/ghdl/setup-ghdl-ci) is provided. It is a GitHub Action (see [github.com/features/actions](https://github.com/features/actions)) to setup GHDL in just 3 lines.
- You may use containers from [ghdl/docker](https://github.com/ghdl/docker) or [hdl/containers](https://github.com/hdl/containers), in case your didn't find a suitable release.
- Build GHDL yourself! See [ghdl.github.io/ghdl: Building GHDL](https:///ghdl.github.io/ghdl/development/building/index.html).

# Project structure

## Regular users

- The CLI tool allows analysis, compilation, simulation and (experimental) synthesis for generating VHDL 1993 netlists. It is written in Ada and C, and three different backends are supported, which are sometimes named `ghdl_mcode`, `ghdl_gcc` and `ghdl_llvm`. This is the entrypoint for most users.

- **[experimental]** [ghdl-yosys-plugin](https://github.com/ghdl/ghdl-yosys-plugin) is the integration of GHDL as a frontend plugin module for [Yosys Open SYnthesis Suite](https://yosyshq.net/yosys/), which uses the `libghdl` library (built with `--enable-synth`).

- `ghdl-ls` (part of pyGHDL, see below) implements Language Server Protocol (LSP) in Python. VHDL analysis features provided by GHDL are accessed through `libghdl`. This can be integrated in text editors or IDES, such as, Vim, Emacs, Atom or Visual Studio Code. See [ghdl/ghdl-language-server](https://github.com/ghdl/ghdl-language-server).
  - [vscode-client](https://github.com/ghdl/ghdl-language-server/tree/master/vscode-client) is an extension for [Visual Studio Code (VSC)](https://code.visualstudio.com/) to provide language support for VHDL by interfacing `ghdl-ls`.

## Advanced users

- `libghdl` is a shared library that includes a subset of the regular features plus some features to be used by extension tools (i.e. `pyGHDL`). This is built along with the regular GHDL and it supports both non-synthesisable and synthesisable code. Nonetheless, this is not for users, but for tools built on top of the core. When configured along with `--enable-synth`, this shared library includes synthesis features too.

- [pyGHDL](pyGHDL) is a Python interface to `libghdl`. Currently, it is only used by `ghdl-ls`; however, it can be useful for advanced users which are willing to build Python utilities based on GHDL. There is work in progress for binding libghdl to [pyVHDLModel](https://github.com/vhdl/pyVHDLModel) (see `pyGHDL.dom`).

<p align="center">
  <a title="Codecov - Branch Coverage" href="https://codecov.io/gh/ghdl/ghdl"><img alt="Codecov - Branch Coverage" src="https://img.shields.io/codecov/c/github/ghdl/ghdl?longCache=true&style=flat-square&logo=Codecov&logoColor=fff"></a><!--
  -->
  <a title="Codacy - Quality" href="https://app.codacy.com/gh/ghdl/ghdl/dashboard"><img alt="Codacy - Quality" src="https://img.shields.io/codacy/grade/dc4f2c76ba7e4f598389ab1f8d7d53e9?longCache=true&style=flat-square&logo=Codacy&logoColor=fff"></a><!--
  -->
  <a title="Codacy - Coverage" href="https://app.codacy.com/gh/ghdl/ghdl/dashboard"><img alt="Codacy - Coverage" src="https://img.shields.io/codacy/coverage/dc4f2c76ba7e4f598389ab1f8d7d53e9?longCache=true&style=flat-square&logo=Codacy&logoColor=fff"></a><!--
  -->
</p>

- **[deprecated]** `ghdl_simul`, which supports interpreted simulation, is available for historical reasons and for development/debugging only. It is very slow compared to the 'regular' compiled simulation and not all the features are supported.
