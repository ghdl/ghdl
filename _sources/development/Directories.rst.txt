.. _BUILD:dir_structure:

Directory structure
###################

* ``doc``: `reStructuredText` sources and auxiliary files to build the documentation with `Sphinx <http://www.sphinx-doc.org>`_.
  A continuous integration (CI) workflow is used to automatically build and deploy this site and/or PDF you are reading.

* ``libraries``: mostly third party libraries such as `ieee`, `std`, `synopsys` and `vital`. Except for a few shell and
  `Python` scripts, all the content is written in VHDL.

* ``logo``: Python and Gimp sources of the logo and the banners.

* ``pyGHDL``: sources of the :mod:`Python Interfaces <pyGHDL>`.

* ``scripts``: scripts and auxiliary files:

  * ``scripts/vendors``: Vendors like Altera, Lattice and Xilinx have their own simulation libraries, especially for FPGA
    primitives, soft and hard macros. These libraries cannot be shipped with GHDL, but we offer prepared compile scripts to
    pre-compile the vendor libraries, if the vendor tool is present on the computer. See :ref:`GETTING:PrecompVendor` for
    information on how to use them.

  * ``scripts/gcc``: header and configuration files to build GHDL with GCC (all platforms).

  * ``scripts/msys2-*``: PKGBUILD recipes for building nightly GHDL packages on MSYS2.

  * ``scripts/pnodes*``: Python scripts for automatically generating some of the sources of :mod:`Python Interfaces <pyGHDL>`.

* ``src``: sources of GHDL. Most of them are written in Ada, some in C.

* ``testsuite``: files used for testing.
