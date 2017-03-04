.. _BUILD:dir_structure:

Directory Structure
###################

* :samp:`src`: sources of GHDL, all of them in Ada.

* :samp:`libraries`: mostly third party libraries such as, `ieee`, `mentor`,
  `std`, `synopsys` and `vital`. Except a few shell and `Python` scripts, all
  the content is written in VHDL.

  * Vendors like Altera, Lattice and Xilinx have their own simulation libraries,
    especially for FPGA primitives, soft and hard macros. These libraries can
    not be shipped with GHDL, but we offer prepared compile scripts to
    pre-compile the vendor libraries, if the vendor tool is present on the
    computer. These are located in :samp:`libraries/vendor`.
    See Vendor Primitives <VendorPrimitives.html> for information on how to
    use them.
	
* :samp:`dist`: scripts and auxiliar files to build GHDL in different
  environments:
  
  * :samp:`gcc`: header and configuration files to build GHDL with GCC (all the
    platforms).
  * :samp:`linux`: build and test script written in shell, and other auxiliary
    files used to i) launch docker containers and ii) automate multiple builds
    in `Travis CI <https://travis-ci.org/>`_.
  
  * :samp:`windows`:
    
    * :samp:`mcode`:
    * :samp:`appveyor`: 
  	
* :samp:`doc`: `Markdown` and `reStructuredText` sources and auxiliary files to
  build the documentation with `Sphinx <http://www.sphinx-doc.org>`_. Indeed,
  `Read the docs <http://readthedocs.org>`_ (RTD) is used to automatically build
  and deploy this site and/or PDF you are reading.

* :samp:`testsuite`: see section :ref:`test_suites`.

* `.yml` configuration files for CI environments (:samp:`readthedocs`,
  :samp:`travis` and :samp:`appveyor`) and `ignore` files for source control
  management tools (:samp:`git` and :samp:`.hg`).

* Files for building GHDL: :samp:`configure` and :samp:`Makefile.in`.

* Auxiliar files for development: :samp:`.gdbinit` and :samp:`ghdl.gpr.in`
  (GNAT project file).

* Text files: :samp:`COPYING.md`, :samp:`NEWS.md` and :samp:`README.md`.


.. TODO::

	- [@1138-4EB|@Paebbels] Replace link to VendorPrimitives, when the file is translated from md to rst.
	
	- [@Paebbels] Add brief description of how the content in dist/windows is ordered. From :ghdlsharp:`279`:
		- The content in dist/appveyor is for the AppVeyor flow. AppVeyor is a continuous integration service like Travis-CI, but is offers a Windows Server 2012 R2 machine. The AppVeyor flow has more scripts than the Travis-CI flow, so we put them into a separate directory.
		- The code in dist/windows/mcode is special code needed to build mcode in windows. mcode is supported on Linux (32/64-bit) and Windows (32-bit).
