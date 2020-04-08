.. _DEV:Documentation:

Documentation
#############

* Using/Synthesis: formal verification, etc.
* Development/libghdl. How to interact with GHDL through ``libghdl`` and/or ``libghdl-py``.
* Development/Related Projects. Brief discussion about similarities/differences with other open source projects such as rust_hdl or pyVHDLParser.
* Usage/Docker. Probably copy/convert :file:`README.md` and :file:`USE_CASES.md` in ghdl/docker :ghdlsharp:`166`.
* Usage/Language Server.
* Usage/Examples/Coverage. Code coverage in GHDL is a side effect of using GCC as a backend. In the future, GCC backend support might be dropped in favour of mcode and LLVM. To do so, code coverage with LLVM should be supported first. Anyway, comments/bits of info should be gathered somewhere in the docs, along with references to gcov, lcov, etc.
* Usage/Examples/UART. Dossmatik's UART and unisim guides. We have ``*.doc`` sources to be converted to Sphinx.
* Usage/Examples/Free Range VHDL. https://github.com/fabriziotappero/Free-Range-VHDL-book
* It is possible to add waveforms with wavedrom, since there is a sphinx extension available.
