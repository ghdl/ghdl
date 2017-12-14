.. _BUILD:dir_structure:

Directory Structure
###################

* ``src``: sources of GHDL, all of them in Ada.

* ``libraries``: mostly third party libraries such as, `ieee`, `mentor`,
  `std`, `synopsys` and `vital`. Except a few shell and `Python` scripts, all
  the content is written in VHDL.

  * Vendors like Altera, Lattice and Xilinx have their own simulation libraries,
    especially for FPGA primitives, soft and hard macros. These libraries can
    not be shipped with GHDL, but we offer prepared compile scripts to
    pre-compile the vendor libraries, if the vendor tool is present on the
    computer. These are located in ``libraries/vendor``.
    See :ref:`GETTING:PrecompVendor` for information on how to
    use them.

* ``dist``: scripts and auxiliar files to build GHDL in different
  environments:

  * ``gcc``: header and configuration files to build GHDL with GCC (all the
    platforms).
  * ``linux``: build and test script written in shell, and other auxiliary
    files used to i) launch docker containers and ii) automate multiple builds
    in `Travis CI <https://travis-ci.org/>`_.

  * ``windows``:

    * ``mcode``:
    * ``appveyor``:

* ``doc``: `Markdown` and `reStructuredText` sources and auxiliary files to
  build the documentation with `Sphinx <http://www.sphinx-doc.org>`_. Indeed,
  `Read the docs <http://readthedocs.org>`_ (RTD) is used to automatically build
  and deploy this site and/or PDF you are reading.

* ``testsuite``: see section :ref:`test_suites`.

* `.yml` configuration files for CI environments (``readthedocs``,
  ``travis`` and ``appveyor``) and `ignore` files for source control
  management tools (``git`` and ``.hg``).

* Files for building GHDL: ``configure`` and ``Makefile.in``.

* Auxiliar files for development: ``.gdbinit`` and ``ghdl.gpr.in``
  (GNAT project file).

* Text files: ``COPYING.md``, ``NEWS.md`` and ``README.md``.
