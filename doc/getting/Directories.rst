.. _BUILD:dir_structure:

Directory structure
###################

* ``src``: sources of GHDL, all of them in Ada.

* ``libraries``: mostly third party libraries such as, `ieee`,
  `std`, `synopsys` and `vital`. Except for a few shell and `Python` scripts, all
  the content is written in VHDL.

  * Vendors like Altera, Lattice and Xilinx have their own simulation libraries,
    especially for FPGA primitives, soft and hard macros. These libraries cannot
    be shipped with GHDL, but we offer prepared compile scripts to
    pre-compile the vendor libraries, if the vendor tool is present on the
    computer. These are located in ``libraries/vendor``.
    See :ref:`GETTING:PrecompVendor` for information on how to
    use them.

* ``dist``: scripts and auxiliary files to build GHDL in different
  environments:

  * ``gcc``: header and configuration files to build GHDL with GCC (all
    platforms).
  * ``linux``: build and test script written in shell, and other auxiliary
    files used to i) launch docker containers and ii) automate multiple builds
    in `Travis CI <https://travis-ci.org/>`_.

  * ``windows``:

    * ``mcode``:
    * ``appveyor``:

* ``doc``: `Markdown` and `reStructuredText` sources and auxiliary files to
  build the documentation with `Sphinx <http://www.sphinx-doc.org>`_. In fact,
  `Read the Docs <http://readthedocs.org>`_ (RTD) is used to automatically build
  and deploy this site and/or PDF you are reading.

* ``testsuite``: files used for testing.

* `.yml` configuration files for CI environments (``readthedocs``,
  ``travis``, and ``appveyor``) and `ignore` files for source control
  management tools (``git`` and ``.hg``).

* Files for building GHDL: ``configure`` and ``Makefile.in``.

* Auxiliary files for development: ``.gdbinit`` and ``ghdl.gpr.in``
  (GNAT project file).

* Text files: ``COPYING.md``, ``NEWS.md``, and ``README.md``.
