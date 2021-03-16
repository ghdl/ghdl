.. _PACKAGES:

Getting | Installing
####################

Package managers
****************

Package managers of many popular distributions provide pre-built packages of GHDL. This is the case for `apt`
(Debian/Ubuntu), `dnf` (Fedora), `pacman` (Arch Linux, MSYS2) or `brew` (macOS). Since GHDL supports three different backends
and two library sets (*regular* or *GPL-compatible*), at least six packages with different features might be available in
each package manager.

As a rule of thumb, mcode backend is the fastest for analysis and synthesis. It also allows setting the base simulation time
for speeding up execution. Therefore, it is the recommended pick if available on your platform (x86/amd64, on Windows x86
only). On other platforms, or for using specific features for co-simulation or code coverage, LLVM or GCC need to be used.
See further differences between backends in :ref:`BUILD`.

.. _RELEASE:packages:

Nightly packages
****************

Assets from nightly GHDL builds are available at `github.com/ghdl/ghdl/releases/nightly <https://github.com/ghdl/ghdl/releases/nightly>`__.
These are mostly meant to be used in Continuous Integration (CI) workflows. Precisely, `setup-ghdl-ci <https://github.com/ghdl/setup-ghdl-ci>`__
allows to easily setup nightly assets in GitHub Actions workflows.

However, users on Windows (MSYS2) or Ubuntu might want to download the tarballs/zipfiles and extract/install them locally.

.. _GETTING:PrecompVendor:

Precompile Vendor Primitives
****************************

Vendors like Lattice, Intel (Altera) and Xilinx have their own simulation libraries,
especially for FPGA primitives, soft and hard macros. These libraries cannot
be shipped with GHDL, but GHDL offers prepared compile scripts to pre-compile
these vendor libraries, if the vendor tool is present in the environment. There
are also popular simulation and verification libraries like OSVVM [#f1]_ or
UVVM [#f2]_, which can be pre-compiled, too.

The compilation scripts are writen in the shell languages: *PowerShell* for
*Windows™* and *Bash* for *GNU/Linux*, *MacOS* and *MSYS2*/*MinGW*. The
compile scripts can colorize the GHDL warning and error lines with the help
of ``grc/grcat`` [#f4]_.

.. HINT::
  Vendor precompile scripts for OSVVM and UVVM are tested periodically in `ghdl/extended-tests <https://github.com/ghdl/extended-tests>`__.

Supported Vendors Libraries
===========================

* Lattice (3.6 or later):

  * ``ec``
  * ``ecp``, ``ecp2``, ``ecp3``, ``ecp5u``
  * ``lptm``, ``lptm2``
  * ``machxo``, ``machxo2``, ``machxo3l``, ``machxo3d``
  * ``sc``, ``scm``
  * ``xp``, ``xp2``
  * ...

* Intel (Altera) Quartus (13.0 or later):

  * ``lpm``, ``sgate``
  * ``altera``, ``altera_mf``, ``altera_lnsim``
  * ``arriaii``, ``arriaii_pcie_hip``, ``arriaiigz``
  * ``arriav``, ``arriavgz``, ``arriavgz_pcie_hip``
  * ``cycloneiv``, ``cycloneiv_pcie_hip``, ``cycloneive``
  * ``cyclonev``
  * ``max``, ``maxii``, ``maxv``
  * ``stratixiv``, ``stratixiv_pcie_hip``
  * ``stratixv``, ``stratixv_pcie_hip``
  * ``fiftyfivenm``, ``twentynm``
  * ...

* Xilinx ISE (14.0 or later):

  * ``unisim`` (incl. ``secureip``)
  * ``unimacro``
  * ``simprim`` (incl. ``secureip``)
  * ``xilinxcorelib``

* Xilinx Vivado (2014.1 or later):

  * ``unisim`` (incl. ``secureip``)
  * ``unimacro``

Supported Simulation and Verification Libraries
===============================================

* OSVVM [#f1]_ (for VHDL-2008)
* UVVM [#f2]_ (for VHDL-2008)


---------------------------------------------------------------------

Script Configuration
====================

The vendor library compile scripts need to know where the used / latest vendor
tool chain is installed. Therefore, the scripts implement a default installation
directory search as well as environment variable checks. If a vendor tool cannot
be detected or the script chooses the wrong vendor library source directory,
then it's possible to provide the path via ``--source`` (Bash) or ``-Source``
(PoSh).

The generated output is stored relative to the current working directory. The
scripts create a sub-directory for each vendor. The default output directory can
be overwritten by the parameter ``--output`` (Bash) or ``-Output`` (PoSh).

To compile all source files with GHDL, the simulator executable is searched in
``PATH``. The found default GHDL executable can be overwritten by setting the
environment variable ``GHDL`` or by passing the parameter ``--ghdl`` (Bash) or
``-GHDL`` (PoSh) to the scripts.

If the vendor library compilation is used very often, it's recommend to configure
these parameters in ``config.sh`` (Bash) or ``config.psm1`` (PoSh), so the command
line can be shortened to the essential parts.

---------------------------------------------------------------------

Compiling in Bash
=================

The provided Bash scripts support these environments:

* Linux
* MacOS
* MSYS2 / MinGW
* WSL (Windows Subsystem for Linux)


Follow these steps:

* **Step 0 - Configure the scripts (optional)**

  See the next section for how to configure ``config.sh``.

* **Step 1 - Browse to your simulation working directory**

  .. code-block:: Bash

    $ cd <MySimulationFolder>


* **Step 2 - Start the compilation script(s)**

  Choose one or multiple of the following scripts to run the pre-compilation
  process.

  .. code-block:: Bash

    $ /usr/local/lib/ghdl/vendors/compile-altera.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-intel.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-lattice.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-osvvm.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-uvvm.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-xilinx-ise.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-xilinx-vivado.sh --all


  In most cases GHDL is installed into ``/usr/local/``. The scripts are
  installed into the ``lib\ghdl\vendors`` directory.

* **Step 3 - Viewing the result**

  This creates vendor directories in your current working directory and
  compiles the vendor files into them.


  .. code-block:: Bash

    $ ls -ahl
    ...
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:41 altera
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:42 intel
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:42 lattice
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:48 osvvm
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:58 uvvm
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:58 xilinx-ise
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:48 xilinx-vivado



---------------------------------------------------------------------

Compiling in PowerShell
=======================

The provided PowerShell scripts support these environments:

* Windows™ 10 (PowerShell 5 and PowerShell 6)


Follow these steps:

* **Step 0 - Configure the scripts (optional)**

  See the next section for how to configure ``config.psm1``.

* **Step 1 - Browse to your simulation working directory**

  .. code-block:: PowerShell

     PS> cd <MySimulationFolder>

* **Step 2 - Start the compilation script(s)**

  Choose one or multiple of the following scripts to run the pre-compilation
  process.

  .. code-block:: PowerShell

     PS> <GHDL>\lib\ghdl\vendors\compile-altera.ps1 -All
     PS> <GHDL>\lib\ghdl\vendors\compile-intel.ps1 -All
     PS> <GHDL>\lib\ghdl\vendors\compile-lattice.ps1 -All
     PS> <GHDL>\lib\ghdl\vendors\compile-osvvm.ps1 -All
     PS> <GHDL>\lib\ghdl\vendors\compile-uvvm.ps1 -All
     PS> <GHDL>\lib\ghdl\vendors\compile-xilinx-ise.ps1 -All
     PS> <GHDL>\lib\ghdl\vendors\compile-xilinx-vivado.ps1 -All

  .. # In most cases GHDL is installed into ``/usr/local/``.

  The scripts are installed into the ``lib\ghdl\vendors`` directory.

* **Step 3 - Viewing the result**

  This creates vendor directories in your current working directory and
  compiles the vendor files into them.

  .. code-block::

     PS> dir
         Directory: D:\temp\ghdl

     Mode           LastWriteTime       Length Name
     ----           -------------       ------ ----
     d----    09.03.2018    19:33        <DIR> altera
     d----    09.03.2018    19:38        <DIR> intel
     d----    09.03.2018    19:38        <DIR> lattice
     d----    09.03.2018    19:38        <DIR> osvvm
     d----    09.03.2018    19:45        <DIR> uvvm
     d----    09.03.2018    19:06        <DIR> xilinx-ise
     d----    09.03.2018    19:40        <DIR> xilinx-vivado


---------------------------------------------------------------------

Configuration Files
===================

For Bash: `config.sh`
---------------------

Please open the ``config.sh`` file and set the dictionary entries for the
installed vendor tools to your tool's installation directories. Use an empty
string ``""`` for not installed tools.

``config.sh``:

.. code-block:: Bash

   declare -A InstallationDirectory
   InstallationDirectory[AlteraQuartus]="/opt/Altera/16.0"
   InstallationDirectory[IntelQuartus]="/opt/intelFPGA/20.1"
   InstallationDirectory[LatticeDiamond]="/opt/Diamond/3.10_x64"
   InstallationDirectory[OSVVM]="/home/<user>/git/GitHub/OSVVM"
   InstallationDirectory[UVVM]="/home/<user>/git/GitHub/UVVM"
   InstallationDirectory[XilinxISE]="/opt/Xilinx/14.7"
   InstallationDirectory[XilinxVivado]="/opt/Xilinx/Vivado/2020.2"


For PowerShell: `config.psm1`
-----------------------------

Please open the ``config.psm1`` file and set the dictionary entries for the
installed vendor tools to your tool's installation
folder. Use an empty string ``""`` for not installed tools.

``config.psm1``:

.. code-block:: PowerShell

   $InstallationDirectory = @{
     "AlteraQuartus" =   "C:\Altera\16.0";
     "IntelQuartus" =    "C:\Altera\20.1";
     "LatticeDiamond" =  "C:\Lattice\Diamond\3.10_x64";
     "XilinxISE" =       "C:\Xilinx\14.7\ISE_DS";
     "XilinxVivado" =    "C:\Xilinx\Vivado\2020.2";
     "OSVVM" =           "C:\git\GitHub\OSVVM";
     "UVVM" =            "C:\git\GitHub\UVVM"
   }


Additional Script Parameters
============================

Each script supports partial compilations e.g. of shared packages and
individual parts. In addition, the amount of printout to the console can be
controlled. Some scripts may offer vendor specific options.


For Bash Scripts:
-----------------

* Common parameters to most scripts:

  .. code-block:: none

     --help, -h            Print the embedded help page(s).
     --clean, -c           Cleanup directory before analyzing.
     --no-warnings, -n     Don't show warnings. Report errors only.
     --skip-existing, -s   Skip already compiled files (an *.o file exists).
     --skip-largefiles, -S Don't compile large entities like DSP and PCIe primitives.
     --halt-on-error, -H   Stop compiling if an error occurred.

* ``compile-altera.sh``

  Selectable libraries:

  .. code-block:: none

     --all, -a             Compile all libraries, including common libraries, packages and device libraries.
     --altera              Compile base libraries like 'altera' and 'altera_mf'
     --max                 Compile device libraries for Max CPLDs
     --arria               Compile device libraries for Arria FPGAs
     --cyclone             Compile device libraries for Cyclone FPGAs
     --stratix             Compile device libraries for Stratix FPGAs

  Compile options:

  .. code-block:: none

     --vhdl93              Compile selected libraries with VHDL-93 (default).
     --vhdl2008            Compile selected libraries with VHDL-2008.

* ``compile-xilinx-ise.sh``

  Selectable libraries:

  .. code-block:: none

     --all, -a             Compile all libraries, including common libraries, packages and device libraries.
     --unisim              Compile the unisim primitives
     --unimacro            Compile the unimacro macros
     --simprim             Compile the simprim primitives
     --corelib             Compile the xilinxcorelib macros
     --secureip            Compile the secureip primitives

  Compile options:

  .. code-block:: none

     --vhdl93              Compile selected libraries with VHDL-93 (default).
     --vhdl2008            Compile selected libraries with VHDL-2008.

* ``compile-xilinx-vivado.sh``

  Selectable libraries:

  .. code-block:: none

     --all, -a             Compile all libraries, including common libraries, packages and device libraries.
     --unisim              Compile the unisim primitives
     --unimacro            Compile the unimacro macros
     --secureip            Compile the secureip primitives

  Compile options:

  .. code-block:: none

     --vhdl93              Compile selected libraries with VHDL-93 (default).
     --vhdl2008            Compile selected libraries with VHDL-2008.

* ``compile-osvvm.sh``

  Selectable libraries:

  .. code-block:: none

     --all, -a             Compile all.
     --osvvm               Compile the OSVVM library.

* ``compile-uvvm.sh``

  Selectable libraries:

  .. code-block:: none

     --all, -a             Compile all.
     --uvvm                Compile the UVVM libraries.


For PowerShell Scripts:
-----------------------

* Common parameters to all scripts:

  .. code-block:: none

     -Help                 Print the embedded help page(s).
     -Clean                Cleanup directory before analyzing.
     -SuppressWarnings     Don't show warnings. Report errors only.

* ``compile-altera.ps1``

  Selectable libraries:

  .. code-block:: none

     -All                  Compile all libraries, including common libraries, packages and device libraries.
     -Altera               Compile base libraries like 'altera' and 'altera_mf'
     -Max                  Compile device libraries for Max CPLDs
     -Arria                Compile device libraries for Arria FPGAs
     -Cyclone              Compile device libraries for Cyclone FPGAs
     -Stratix              Compile device libraries for Stratix FPGAs

  Compile options:

  .. code-block:: none

     -VHDL93               Compile selected libraries with VHDL-93 (default).
     -VHDL2008             Compile selected libraries with VHDL-2008.

* ``compile-xilinx-ise.ps1``

  Selectable libraries:

  .. code-block:: none

     -All                  Compile all libraries, including common libraries, packages and device libraries.
     -Unisim               Compile the unisim primitives
     -Unimacro             Compile the unimacro macros
     -Simprim              Compile the simprim primitives
     -CoreLib              Compile the xilinxcorelib macros
     -Secureip             Compile the secureip primitives

  Compile options:

  .. code-block:: none

     -VHDL93               Compile selected libraries with VHDL-93 (default).
     -VHDL2008             Compile selected libraries with VHDL-2008.

* ``compile-xilinx-vivado.ps1``

  Selectable libraries:

  .. code-block:: none

     -All                  Compile all libraries, including common libraries, packages and device libraries.
     -Unisim               Compile the unisim primitives
     -Unimacro             Compile the unimacro macros
     -Secureip             Compile the secureip primitives

  Compile options:

  .. code-block:: none

     -VHDL93               Compile selected libraries with VHDL-93 (default).
     -VHDL2008             Compile selected libraries with VHDL-2008.

* ``compile-osvvm.ps1``

  Selectable libraries:

  .. code-block:: none

     -All                  Compile all.
     -OSVVM                Compile the OSVVM library.

* ``compile-uvvm.ps1``

  Selectable libraries:

  .. code-block:: none

     -All                  Compile all.
     -UVVM                 Compile the UVVM libraries.

--------------------------------------------------------------------------------

.. container:: footnotes

	 .. rubric:: Footnotes

   .. [#f1] OSVVM http://github.com/OSVVM/OSVVM
   .. [#f2] UVVM https://github.com/UVVM/UVVM_All
   .. [#f4] Generic Colourizer http://kassiopeia.juls.savba.sk/~garabik/software/grc.html
