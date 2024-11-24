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
  <a title="'Test' workflow Status" href="https://github.com/ghdl/ghdl/actions/workflows/Test.yml"><img alt="'Test' workflow Status" src="https://img.shields.io/github/actions/workflow/status/ghdl/ghdl/Test.yml?branch=master&longCache=true&style=flat-square&label=Test&logo=github%20actions&logoColor=fff"></a><!--
  -->
  <a title="AppVeyor" href="https://ci.appveyor.com/project/tgingold/ghdl-psgys/history"><img src="https://img.shields.io/appveyor/ci/tgingold/ghdl-psgys/master.svg?logo=appveyor&logoColor=e8ecef&style=flat-square&label=Test"></a><!--
  -->
  <a title="CII Best Practices" href="https://bestpractices.coreinfrastructure.org/en/projects/3157"><img src="https://img.shields.io/cii/percentage/3157??longCache=true&style=flat-square&label=CII&logo=Linux%20Foundation"></a><!--
  -->
</p>

This directory contains the sources of GHDL, the open-source analyzer, compiler, simulator and (experimental) synthesizer for [VHDL](https://en.wikipedia.org/wiki/VHDL), a Hardware Description Language ([HDL](https://en.wikipedia.org/wiki/Hardware_description_language)). GHDL is not an interpreter: it allows you to analyse and elaborate sources for generating machine code from your design. Native program execution is the only way for high-speed simulation.

# Table of Contents

- [Main Features](#main-features) 
- [Third-Party Integrations](#third-party-integrations)
- [Getting GHDL](#getting-ghdl)
- [Benefits of Using GHDL](#benefits-of-using-ghdl)
- [Contributing](#contributing)
- [Project Structure](#project-structure)
  - [Regular Users](#regular-users)
  - [Advanced Users](#advanced-users)

# Main features

Full support for the [1987](https://ieeexplore.ieee.org/document/26487/), [1993](https://ieeexplore.ieee.org/document/392561/), [2002](https://ieeexplore.ieee.org/document/1003477/) versions of the [IEEE](https://www.ieee.org) [1076](https://standards.ieee.org/ieee/1076/10800/) VHDL standard, and partial for the [2008](https://ieeexplore.ieee.org/document/4772740/) and [2019](https://ieeexplore.ieee.org/document/8938196/) revisions.

Partial support of [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language).

By using a code generator ([LLVM](https://llvm.org/), [GCC](https://gcc.gnu.org/) or, [x86_64](https://en.wikipedia.org/wiki/X86-64)/[i386](https://en.wikipedia.org/wiki/Intel_80386) only, a built-in one), it is much faster than any interpreted simulator. It can handle very large designs, such as [leon3/grlib](https://www.gaisler.com/index.php/downloads/leongrlib).

GHDL runs on [GNU/Linux](https://en.wikipedia.org/wiki/Linux_distribution), [Windows](https://en.wikipedia.org/wiki/Microsoft_Windows) and [macOS](https://en.wikipedia.org/wiki/MacOS); on `x86`, `x86_64`, `armv6/armv7/aarch32`, `aarch64` and `ppc64`. You can freely download [nightly](https://github.com/ghdl/ghdl/releases) assets, use OCI images (aka Docker/Podman containers), or try building it on your own machine (see *'Getting GHDL'* below).

Can write waveforms to [GHW](https://ghdl.github.io/ghdl/using/Simulation.html?highlight=GHW#cmdoption-wave), [VCD](https://en.wikipedia.org/wiki/Value_change_dump) or FST files. Combined with a [GUI](https://en.wikipedia.org/wiki/Graphical_user_interface)-based [waveform viewer](https://en.wikipedia.org/wiki/Waveform_viewer) and a good text editor, GHDL is a very powerful tool for writing, testing and simulating your code.

Co-simulation with foreign applications is supported through Verilog Procedural Interface (VPI) and/or VHPIDIRECT. See [ghdl.github.io/ghdl-cosim](https://ghdl.github.io/ghdl-cosim).

Can synthesize arbitrarily complex VHDL designs into a VHDL 1993 netlist, which can be implicitly or
explicitly used in open-source or vendor synthesis frameworks.

GHDL is free software:

- [![GNU General Public License 2](https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?longCache=true&style=flat-square&label=license&logo=gnu)](https://github.com/ghdl/ghdl/blob/master/COPYING.md)
- [![Creative Commons Attribution-ShareAlike](https://img.shields.io/badge/doc%20license-Creative%20Commons%20Attribution--ShareAlike--4.0-bf7600.svg?longCache=true&style=flat-square&logo=Creative%20Commons)](https://github.com/ghdl/ghdl/blob/master/doc/COPYING_DOC.md) available at [ghdl.github.io/ghdl](https://ghdl.github.io/ghdl).
- Some of the runtime libraries, are under different terms; see the individual source files for details.

# Third-Party Integrations

GHDL is designed to integrate seamlessly with several popular open-source tools, allowing users to leverage additional capabilities for both simulation and synthesis workflows.

One of the key integrations is with [Yosys](https://github.com/YosysHQ/yosys), a leading open-source synthesis tool. Using the [ghdl-yosys-plugin](https://github.com/ghdl/ghdl-yosys-plugin), GHDL can be used as a front-end for Yosys, enabling users to synthesize VHDL designs directly. This makes it possible to move from simulation to synthesis without leaving the GHDL environment, streamlining the design flow.

GHDL also integrates with [cocotb](https://github.com/potentialventures/cocotb), a coroutine-based co-simulation framework that allows Python-driven testing of VHDL designs. This is particularly useful for writing complex testbenches in Python and interacting with VHDL simulations in real-time.

For verification, GHDL supports integration with frameworks such as [OSVVM](https://osvvm.org), [UVVM](https://github.com/UVVM/UVVM), and [VUnit](https://vunit.github.io), providing users with a full suite of tools for verifying VHDL designs. These verification libraries enable advanced testing techniques like constrained random verification, functional coverage, and automated regression testing.

Together, these third-party integrations make GHDL a comprehensive tool for digital design, simulation, verification, and synthesis, suitable for both individual developers and large teams.

# Getting GHDL

- Pre-built packages:
  - GHDL is available through the default package manager on most major distributions: Debian/Ubuntu, Fedora, Arch Linux, Guix, and MSYS2 for Windows. You can install GHDL directly through these package managers, making the installation process straightforward for most users.
  - After each successful CI run, [nightly](https://github.com/ghdl/ghdl/releases/tag/nightly) tarballs/zipfiles for Ubuntu and Windows (MSYS2) are updated. These nightly builds provide access to the latest features and updates, which may not yet be available in the package manager repositories.
  - If you need to set up GHDL in a Continuous Integration (CI) pipeline, the [setup-ghdl-ci](https://github.com/ghdl/setup-ghdl-ci) GitHub Action allows you to configure GHDL with minimal effort. It provides a simple and efficient way to integrate GHDL into your CI workflows with just a few lines of configuration.

- **Using Docker**:  
  For those who prefer containerization, GHDL is also available as Docker images. This is especially useful if you want to avoid managing dependencies manually or need a consistent environment across different machines. You can use containers from either [ghdl/docker](https://github.com/ghdl/docker) or [hdl/containers](https://github.com/hdl/containers), both of which offer pre-configured environments with GHDL installed. Running GHDL inside a Docker container ensures that your setup remains isolated from your host system, making it easier to maintain and replicate across different platforms. This option is ideal for users looking for a streamlined, hassle-free installation.

- **Building GHDL from Source**:  
  Advanced users can opt to build GHDL from source, allowing for customization and access to experimental features. Building from source can also be useful for platforms or configurations not covered by pre-built packages. The [Building GHDL guide](https://ghdl.github.io/ghdl/development/building/index.html) provides detailed instructions on how to compile GHDL, including selecting your preferred backend (LLVM, GCC, or mcode). This process gives users full control over their setup and allows them to tailor GHDL to their specific needs.

- **Platform-Specific Notes**:
  - **Linux**: GHDL is supported across a wide range of Linux distributions. It can be installed using common package managers like `apt`, `dnf`, `pacman` or `guix`, depending on the distribution. Installation is typically straightforward, and pre-built packages are updated regularly.
  - **Windows**: For Windows users, GHDL can be installed through MSYS2, a Unix-like environment for Windows. This provides a similar experience to using GHDL on Linux, and MSYS2 ensures that dependencies are properly managed. The [MSYS2 documentation](https://www.msys2.org/) offers detailed guidance on setting up the environment.
  - **macOS**: On macOS, GHDL can be easily installed using Homebrew, a popular package manager. This allows macOS users to get started with GHDL without needing to build from source. Alternatively, for more customization, macOS users can follow the build-from-source instructions.

# Benefits of Using GHDL

GHDL offers several advantages that make it a powerful tool for VHDL simulation and synthesis. One of the key benefits is its native code generation, which allows for faster simulation times compared to interpreted simulators. This performance boost is particularly noticeable in large and complex designs, where simulation speed can be a critical factor.

In addition to performance, GHDL is highly versatile. It supports multiple revisions of the VHDL standard, including the 1987, 1993, 2002, 2008, and 2019 versions. This wide range of compatibility makes GHDL suitable for legacy projects as well as cutting-edge designs. Whether you are working with older VHDL standards or the most recent revisions, GHDL provides the flexibility to handle them all.

Another important benefit is GHDL’s cross-platform support. It runs on major operating systems like Linux, Windows, and macOS, and supports various architectures such as x86, x86_64, ARM, and PPC64. This allows developers to use GHDL on a variety of hardware configurations, making it accessible to a broad range of users.

GHDL’s integration with popular open-source tools such as Yosys for synthesis and cocotb for verification further enhances its utility. These integrations allow users to extend GHDL’s capabilities beyond simulation, making it a comprehensive solution for VHDL design, verification, and synthesis workflows.

# Contributing

GHDL welcomes contributions from the community. Whether you're interested in improving documentation, fixing bugs, or adding new features, there are many ways to get involved. Contributions are a great way to help improve the tool and to learn more about how GHDL works internally.

Here’s how you can start contributing:

1. **Find an issue to work on**: Check out the [open issues](https://github.com/ghdl/ghdl/issues) on the GitHub repository. Issues are often labeled by difficulty or type (e.g., "good first issue" for beginners). You can also propose your own ideas or enhancements.
   
2. **Fork and clone the repository**: Once you’ve chosen an issue to work on, fork the GHDL repository to your own GitHub account. Clone the forked repository to your local machine to make changes.

3. **Create a new branch**: It’s good practice to create a new branch for each issue or feature you’re working on. This helps keep your changes isolated and makes it easier for the maintainers to review your work.

4. **Submit a pull request**: After making your changes and committing them to your branch, submit a pull request (PR) to the main GHDL repository. Be sure to include a clear description of what your PR addresses and any relevant context or testing steps.

For more detailed guidelines, refer to our [Contributing Guide](https://github.com/ghdl/ghdl/blob/master/CONTRIBUTING.md) (when created), which includes coding standards, testing instructions, and best practices for making contributions.

# Project structure

## Regular users

- The CLI tool allows analysis, compilation, simulation and (experimental) synthesis for generating VHDL 1993 netlists. It is written in Ada and C, and three different backends are supported, which are sometimes named `ghdl_mcode`, `ghdl_gcc` and `ghdl_llvm`. This is the entrypoint for most users.

- **[experimental]** [ghdl-yosys-plugin](https://github.com/ghdl/ghdl-yosys-plugin) is the integration of GHDL as a frontend plugin module for [Yosys Open SYnthesis Suite](https://yosyshq.net/yosys/), which uses the `libghdl` library (built with `--enable-synth`).

- `ghdl-ls` (part of pyGHDL, see below) implements Language Server Protocol (LSP) in Python. VHDL analysis features provided by GHDL are accessed through `libghdl`. This can be integrated in text editors or IDES, such as, Vim, Emacs, Atom or Visual Studio Code. See [ghdl/ghdl-language-server](https://github.com/ghdl/ghdl-language-server).
  - [vscode-client](https://github.com/ghdl/ghdl-language-server/tree/master/vscode-client) is an extension for [Visual Studio Code (VSC)](https://code.visualstudio.com/) to provide language support for VHDL by interfacing `ghdl-ls`.

## Advanced users

- `libghdl` is a shared library that includes a subset of the regular features plus some features to be used by extension tools (i.e. `pyGHDL`). This is built along with the regular GHDL and it supports both non-synthesisable and synthesisable code. Nonetheless, this is not for users, but for tools built on top of the core. When configured along with `--enable-synth`, this shared library includes synthesis features too.

- [pyGHDL](pyGHDL) is a Python interface to `libghdl`. Currently, it is only used by `ghdl-ls`; however, it can be useful for advanced users who are willing to build Python utilities based on GHDL. There is work in progress for binding libghdl to [pyVHDLModel](https://github.com/vhdl/pyVHDLModel) (see `pyGHDL.dom`).

<p align="center">
  <a title="Codecov - Branch Coverage" href="https://codecov.io/gh/ghdl/ghdl"><img alt="Codecov - Branch Coverage" src="https://img.shields.io/codecov/c/github/ghdl/ghdl?longCache=true&style=flat-square&logo=Codecov&logoColor=fff"></a><!--
  -->
  <a title="Codacy - Quality" href="https://app.codacy.com/gh/ghdl/ghdl/dashboard"><img alt="Codacy - Quality" src="https://img.shields.io/codacy/grade/dc4f2c76ba7e4f598389ab1f8d7d53e9?longCache=true&style=flat-square&logo=Codacy&logoColor=fff"></a><!--
  -->
  <a title="Codacy - Coverage" href="https://app.codacy.com/gh/ghdl/ghdl/dashboard"><img alt="Codacy - Coverage" src="https://img.shields.io/codacy/coverage/dc4f2c76ba7e4f598389ab1f8d7d53e9?longCache=true&style=flat-square&logo=Codacy&logoColor=fff"></a><!--
  -->
</p>

- **[deprecated]** `ghdl_simul`, which supports interpreted simulation, is available for historical reasons and for development/debugging only. It is very slow compared to the 'regular' compiled simulation and not all the features are supported.
