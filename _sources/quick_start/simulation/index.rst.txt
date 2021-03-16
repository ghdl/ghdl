.. program:: ghdl
.. _USING:QuickStart:Simulation:

Simulation
##########

As explained in :ref:`INTRO:GHDL`, `GHDL` is a compiler which translates `VHDL`
files to machine code. Hence, the regular workflow is composed of three steps:

* :ref:`Analysis:command`: convert design units (`VHDL` sources) to an internal
  representation.
* :ref:`Elaboration:command`: generate executable machine code for a target module
  (top-level entity).
* :ref:`Run:command`: execute the design to test the behaviour, generate
  output/waveforms, etc.

The following tips might be useful:

* Don't forget to select the version of the VHDL standard you want to use (see
  :ref:`VHDL_standards`). The default is :option:`--std=93c <--std>`. Use
  :option:`--std=08 <--std>` for VHDL-2008 (albeit not fully implemented).

  * Use :option:`--ieee=synopsys <--ieee>` if your design depends on a non-standard
    implementation of the IEEE library.

  * Use :option:`-fexplicit` and :option:`-frelaxed-rules` if needed. For instance,
    if you would like to use VHDL 2008 and also use shared variables with an
    ordinary type (deprecated in VHDL 2000), you can use ``--std=08 -frelaxed-rules``.

* Use :option:`--work=LIB_NAME <--work>` to analyze files into the ``LIB_NAME`` library.
  To use files analyzed to a different directory, give the path
  to the ``LIB_NAME`` library using :option:`-P/path/to/name/directory/ <-P>`.

* Use the same options for analysis and elaboration. E.g., first analyse with
  ``ghdl -a --std=08 --work=mylib myfile.vhdl``; and then elaborate and run with
  ``ghdl --elab-run --std=08 top``.

Due to the fact that `VHDL` is processed as a general purpose language
(instead of an `HDL`), all the language features are to be supported. I.e., `VHDL`
sources do not need to be limited to the synthesisable subset. However, distinction
between synthesisable and non-synthesisable (simulation-only) subsets is often
misleading for users who are new to the language. Different examples are provided,
in the hope of helping understand the different use cases:

.. toctree::

   hello/index
   heartbeat/index
   adder/index
   DLXModelSuite

.. TIP:: See :ghdlissue:`Learning VHDL with GHDL <1291>`.
