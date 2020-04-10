.. _DEV:GSOC:

GSOC Ideas
##########

This page contains ideas for enhancing GHDL that can fit internship programs, such as `Google Summer of Code <https://summerofcode.withgoogle.com/>`_.

VHDL frontend for Yosys
=======================

`Yosys <https://github.com/YosysHQ/yosys>`_ is an open-source synthesis tool with built-in Verilog support and partial SystemVerilog support. :ref:`Synth:plugin` is an experimental plugin for Yosys that allows to use GHDL. Although functional, :ref:`USING:Synthesis` is work in progress: multiple features are not supported yet, and others need to be tested for bugs.

.. NOTE::
    * `FOSSI GSOC 2019 | VHDL front-end for Yosys <https://fossi-foundation.org/gsoc19-ideas.html#vhdl-front-end--for-yosys>`_

    * `FOSSI GSOC 2018 | VHDL Frontend for Yosys <https://fossi-foundation.org/gsoc18-ideas.html#vhdl-frontend-for-yosys>`_

Profiling support
=================

Currently, GHDL does not include profiling features, which would allow to speed-up simulations and/or to detect hotspots in user designs.

.. NOTE::
    * `FOSSI GSOC 2018 | Profiling support <https://fossi-foundation.org/gsoc18-ideas.html#ghdl-profiling-support>`_

    * :ghdlsharp:`60`

Improve LLVM backend
====================

There are several possible enhancements to the current implementation of :ref:`BUILD:llvm`

* Debugging is supported with LLVM 3.5 only, although up to version 9.0 is supported for simulation.
* The C++ API of LLVM should be used instead of the C API.
* There was no real try to find the best order of optimization passes. This can significantly improve performance, since GHDL is currently single-threaded and CPU-bound.
* Code coverage is not supported.

.. NOTE::
    * `FOSSI GSOC 2018 | GHDL: Improve LLVM backend <https://fossi-foundation.org/gsoc18-ideas.html#ghdl-improve-llvm-backend>`_

    * :ghdlsharp:`866`, :ghdlsharp:`744`, :ghdlsharp:`286`

Support 64-bit with mcode on Windows
====================================

The built-in in-memory code generator (:ref:`BUILD:mcode`), is supported on 64 bit GNU/Linux, but not on Windows 64 bit. Compared to other backends, this would provide a lightweight and fast analyser, although it doesn't try to optimise.

.. NOTE::
    * `FOSSI GSOC 2018 | Support 64-bit with mcode on Windows <https://fossi-foundation.org/gsoc18-ideas.html#ghdl-support-64-bit-with-mcode-on-windows>`_

    * :ghdlsharp:`657`

Mixed-language (VHDL-Verilog)
=============================

Multiple proofs of concept exist for co-execution of HDL simulators with other tools, such as QEMU. However, there is no open-source solution that allows to co-simulate VHDL and Verilog sources using recent versions of the standards. Some possible approaches for this task are:

* Use procedural interfaces, VPI or VHPIDIRECT (see :ref:`USING:Foreign`).

* Transpile/convert the HDLs into a common intermediate representation.

* Have GHDL use the API of another tool or the other way round.

.. NOTE::
    * `FOSSI GSOC 2018 | Framework for Mixed-Language Simulation <https://fossi-foundation.org/gsoc18-ideas.html#framework-for-mixed-language-simulation>`_

    * `FOSSI GSOC 2017 | Open Source Mixed-Language HDL Simulation <https://fossi-foundation.org/gsoc17-ideas.html#open-source-mixed-language-hdl-simulation>`_

    * :ghdlsharp:`908`, :ghdlsharp:`800`

Mixed-signal (Digital-Analog)
=============================

Thre are three different approaches for mixed-signal simulation with GHDL:

* Built-in VHDL-AMS support. It is currently possible to analyze VHDL-AMS files with GHDL (almost all the features are handled). However, it is analysis only (yet). A DAE solver needs to be pluged into GHDL compute the simulation.

* Co-execution of GHDL and an analog simulator through VPI or VHPIDIRECT (see :ref:`USING:Foreign`).

* Generation of simulation models from VHDL-AMS, like ADMS.

.. NOTE::
    * :ghdlsharp:`1052`, :ghdlsharp:`162`

C APIs
======

Currently, GHDL can be wrapped in a foreign language (such as Ada or C) through VHPIDIRECT (see :ref:`USING:Foreign`). However, runtime management of the simulation is not supported. The API should be enhanced to support stepped execution. Moreover, interfacing with some types is not straightforward. Header files with the definition of those types would simplify data transference between language domains during simulation.

.. NOTE::
    * :ghdlsharp:`1059`, :ghdlsharp:`1053`, :ghdlsharp:`894`, :ghdlsharp:`819`, :ghdlsharp:`803`, :ghdlsharp:`800`

Language server
===============

`ghdl-language-server <https://github.com/ghdl/ghdl-language-server>`_ is an experimental LSP server written in Python (which uses `libghdl-py <https://github.com/ghdl/ghdl#project-structure>`_), along with clients for different editors (e.g. VSCode, Emacs or Vim). Although functional, it is work in progress: multiple features are not supported yet, and others need to be tested for bugs.

Project configuration file format
=================================

`ghdl-language-server <https://github.com/ghdl/ghdl-language-server>`_ supports a configuration file named `hdl-prj.json`. The format of this file is undocumented and lightly defined. This is because it would be desirable to use a configuration format that can be shared with other similar tools, such as `rust_hdl <https://github.com/kraigher/rust_hdl>`_ or `pyVHDLParser <https://github.com/Paebbels/pyVHDLParser>`_. In the context of GHDL, the same configuration file might be used for the language server, simulation, synthesis, etc.

.. NOTE::
    * `ghdl/ghdl-language-server#12 <https://github.com/ghdl/ghdl-language-server/issues/12>`_, `jeremiah-c-leary/vhdl-style-guide#312 <https://github.com/jeremiah-c-leary/vhdl-style-guide/issues/312>`_

Packaging for Windows and/or macOS
==================================

GHDL can be installed with the most known package managers on GNU/Linux distributions (`apt`, `dnf`, `pacman`, etc.). However, this is not the case on Windows and/or macOS.

On Windows, `PKGBUILD` files for MSYS2 are available, but not upstreamed. Nonetheless, it would be desirable to distribute an standalone package that does not depend on a ful MSYS2 installation (see :ref:`BUILD`).

On macOS, a Homebrew formula might be written.

Ideally, these packages would be built/generated and tested in a CI workflow.

.. NOTE::
    * `msys2/MINGW-packages#5757 <https://github.com/msys2/MINGW-packages/pull/5757>`_
    * :ghdlsharp:`744`, `Homebrew/homebrew-cask#47256 <https://github.com/Homebrew/homebrew-cask/pull/47256>`_
