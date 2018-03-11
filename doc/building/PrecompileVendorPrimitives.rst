.. _GETTING:PrecompVendor:

Precompile Vendor Primitives
############################

Vendors like Altera, Lattice and Xilinx have their own simulation libraries,
especially for FPGA primitives, soft and hard macros. These libraries can not
be shipped with *GHDL*, but we offer prepared compile scripts to pre-compile
the vendor libraries, if the vendor tool is present on the computer. There are
also popular simulation and verification libraries like OSVVM [#f1]_ or
UVVM [#f3]_, which can be pre-compiled, too.

The compilation scripts are writen in the shell languages: *PowerShell* for
*Windows* |trade| and *Bash* for *GNU/Linux*. The compile scripts can colorize
the GHDL warning and error lines with the help of `grc/grcat` [#f4]_.

Supported Vendors Libraries
===========================

* Altera/Intel Quartus (13.0 or later):

  * `lpm`, `sgate`
  * `altera`, `altera_mf`, `altera_lnsim`
  * `arriaii`, `arriaii_pcie_hip`, `arriaiigz`
  * `arriav`, `arriavgz`, `arriavgz_pcie_hip`
  * `cycloneiv`, `cycloneiv_pcie_hip`, `cycloneive`
  * `cyclonev`
  * `max`, `maxii`, `maxv`
  * `stratixiv`, `stratixiv_pcie_hip`
  * `stratixv`, `stratixv_pcie_hip`
  * `fiftyfivenm`, `twentynm`
	
* Lattice (3.6 or later):

  * `ec`
  * `ecp`, `ecp2`, `ecp3`, `ecp5u`
  * `lptm`, `lptm2`
  * `machxo`, `machxo2`, `machxo3l`
  * `sc`, `scm`
  * `xp`, `xp2`

* Xilinx ISE (14.0 or later):

  * `unisim` (incl. `secureip`)
  * `unimacro`
  * `simprim` (incl. `secureip`)
  * `xilinxcorelib`

* Xilinx Vivado (2014.1 or later):

  * `unisim` (incl. `secureip`)
  * `unimacro`

Supported Simulation and Verification Libraries
===============================================

* OSVVM (for VHDL-2008)

    * osvvm

* UVVM (for VHDL-2008)

  * uvvm-utilities
  * uvvm-vvc-framework
  * uvvm-vip-avalon_mm
  * uvvm-vip-axi_lite
  * uvvm-vip-axi_stream
  * uvvm-vip-gpio
  * uvvm-vip-i2c
  * uvvm-vip-sbi
  * uvvm-vip-spi
  * uvvm-vip-uart

---------------------------------------------------------------------

Script Configuration
====================

The vendor library compile scripts need to know where the used / latest vendor
tool chain is installed. Therefore, the script implement a default installation
directory search as well as environment variable checks. If a vendor tool could
not be detected or the script choses the wrong vendor library source directory,
then it's possible to provide the path via `--source` or `-Source`.

The generated output is stored relative to the current working directory. The
scripts create a sub-directory for each vendor. The default output directory can
be overwritten by the parameter `--output` or `-Output`.

To compile all source files with GHDL, the simulator executable is searched in
`PATH`. The found default GHDL executable can be overwritten by setting the
environment variable `GHDL` or by passing the parameter `--ghdl` or `-GHDL` to
the scripts.

If the vendor library compilation is used very often, we recommend to configure
these parameters in `config.sh` or `config.psm1`, so the command line can be
shortened to the essential parts.

---------------------------------------------------------------------

Compiling on Linux
==================

* **Step 0 - Configure the scripts (optional)**
    See next section for how to configure `config.sh`.

* **Step 1 - Browse to your simulation working directory**
    
  .. code-block:: Bash

    $ cd <MySimulationFolder>
    ```

* **Step 2 - Start the compilation script(s)**
    
  .. code-block:: Bash

    $ /usr/local/lib/ghdl/vendors/compile-altera.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-lattice.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-xilinx-ise.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-xilinx-vivado.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-osvvm.sh --all
    $ /usr/local/lib/ghdl/vendors/compile-uvvm.sh --all
    ```
    
    In most cases GHDL is installed into `/usr/local/`. The scripts are
    installed into the `lib` directory.

* **Step 3 - Viewing the result**
    This creates vendor directories in your current working directory and
    compiles the vendor files into them.

    
  .. code-block:: Bash

    $ ls -ahl
    ...
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:41 altera
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:42 lattice
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:48 osvvm
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:58 uvvm
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:58 xilinx-ise
    drwxr-xr-x  2 <user> <group>  56K Mar 09 17:48 xilinx-vivado
    ```


---------------------------------------------------------------------

Compiling on Windows
====================

* **Step 0 - Configure the scripts (optional)**

  See next section for how to configure `config.psm1`.

* **Step 1 - Browse to your simulation working directory**
    
  .. code-block:: PowerShell

     PS> cd <MySimulationFolder>

* **Step 2 - Start the compilation script(s)**
    
  .. code-block:: PowerShell

     PS> <GHDL>\libraries\vendors\compile-altera.ps1 -All
     PS> <GHDL>\libraries\vendors\compile-lattice.ps1 -All
     PS> <GHDL>\libraries\vendors\compile-xilinx-ise.ps1 -All
     PS> <GHDL>\libraries\vendors\compile-xilinx-vivado.ps1 -All
     PS> <GHDL>\libraries\vendors\compile-osvvm.ps1 -All
     PS> <GHDL>\libraries\vendors\compile-uvvm.ps1 -All

* **Step 3 - Viewing the result**
  This creates vendor directories in your current working directory and
  compiles the vendor files into them.

  .. code-block:: PowerShell

     PS> dir
         Directory: D:\temp\ghdl

     Mode           LastWriteTime       Length Name
     ----           -------------       ------ ----
     d----    09.03.2018    19:33        <DIR> altera
     d----    09.03.2018    19:38        <DIR> lattice
     d----    09.03.2018    19:38        <DIR> osvvm
     d----    09.03.2018    19:45        <DIR> uvvm
     d----    09.03.2018    19:06        <DIR> xilinx-ise
     d----    09.03.2018    19:40        <DIR> xilinx-vivado


---------------------------------------------------------------------

Configuration Files
======================

For Linux: `config.sh`
----------------------

Please open the `config.sh` file and set the dictionary entries for the
installed vendor tools to the appropriate directory to your tool's installation
directories. Use an empty string `""` for not installed tools.

`config.sh`:

.. code-block:: Bash
   
   declare -A InstallationDirectory
   InstallationDirectory[AlteraQuartus]="/opt/Altera/17.1"
   InstallationDirectory[LatticeDiamond]="/opt/Diamond/3.9_x64"
   InstallationDirectory[OSVVM]="/home/<user>/git/GitHub/osvvm"
   InstallationDirectory[UVVM]="/home/<user>/git/GitHub/uvvm_all"
   InstallationDirectory[XilinxISE]="/opt/Xilinx/14.7"
   InstallationDirectory[XilinxVivado]="/opt/Xilinx/Vivado/2017.4"


For Windows: `config.psm1`
--------------------------

Please open the `config.psm1` file and set the dictionary entries for the
installed vendor tools to the appropriate directory to your tool's installation
folder. Use an empty string `""` for not installed tools.

`config.psm1`:

.. code-block:: PowerShell

   $InstallationDirectory = @{
     "AlteraQuartus" =   "C:\Altera\17.1";
     "LatticeDiamond" =  "C:\Lattice\Diamond\3.9_x64";
     "XilinxISE" =       "C:\Xilinx\14.7\ISE_DS";
     "XilinxVivado" =    "C:\Xilinx\Vivado\2017.4";
     "OSVVM" =           "D:\git\GitHub\osvvm";
     "UVVM" =            "D:\git\GitHub\uvvm_all"
   }


Selectable Options for the Bash Scripts:
----------------------------------------

*First I should translate the scripts before writing the docu...*

* Common parameters to most scripts:
  
  .. code-block:: raw
	
     -h --help             Print the embedded help page(s).
     -c --clean            Cleanup directory before analyzing.
     -n --no-warnings	     Don't show warnings. Report errors only.
     -s --skip-existing    Skip already compiled files (an *.o file exists).
     -S --skip-largefiles  Don't compile large entities like DSP and PCIe primitives.
     -H --halt-on-error    Stop compiling if an error occured.

* `compile-altera.sh`

  Selectable libraries:
  
  .. code-block:: raw

     -a --all              Compile all libraries, including common libraries, packages and device libraries.
        --altera           Compile base libraries like 'altera' and 'altera_mf'
        --max              Compile device libraries for Max CPLDs
        --arria            Compile device libraries for Arria FPGAs
        --cyclone          Compile device libraries for Cyclone FPGAs
        --stratix          Compile device libraries for Stratix FPGAs

   Compile options:
  
   .. code-block:: raw
	

        --vhdl93           Compile selected libraries with VHDL-93 (default).
        --vhdl2008         Compile selected libraries with VHDL-2008.

* `compile-xilinx-ise.sh`

  Selectable libraries:
  
  .. code-block:: raw
	
     -a --all              Compile all libraries, including common libraries, packages and device libraries.
        --unisim           Compile the unisim primitives
        --unimacro         Compile the unimacro macros
        --simprim          Compile the simprim primitives
        --corelib          Compile the xilinxcorelib macros
        --secureip         Compile the secureip primitives

  Compile options:
  
  .. code-block:: raw
	
        --vhdl93           Compile selected libraries with VHDL-93 (default).
        --vhdl2008         Compile selected libraries with VHDL-2008.

* `compile-xilinx-vivado.sh`

  Selectable libraries:
  
  .. code-block:: raw
	
     -a --all              Compile all libraries, including common libraries, packages and device libraries.
        --unisim           Compile the unisim primitives
        --unimacro         Compile the unimacro macros
        --secureip         Compile the secureip primitives

  Compile options:
  
  .. code-block:: raw
	
        --vhdl93           Compile selected libraries with VHDL-93 (default).
        --vhdl2008         Compile selected libraries with VHDL-2008.

* `compile-osvvm.sh`

  Selectable libraries:
  
  .. code-block:: raw
	
     -a --all              Compile all.
        --osvvm            Compile the OSVVM library.

* `compile-uvvm.sh`

  Selectable libraries:
  
  .. code-block:: raw
	
     -a --all              Compile all.
        --uvvm             Compile the UVVM libraries.


Selectable Options for the PowerShell Scripts:
----------------------------------------------

* Common parameters to all scripts:
  
  .. code-block:: raw
	
     -Help                 Print the embedded help page(s).
     -Clean                Cleanup directory before analyzing.
     -SuppressWarnings     Don't show warnings. Report errors only.

* `compile-altera.ps1`

  Selectable libraries:
  
  .. code-block:: raw
	
     -All                  Compile all libraries, including common libraries, packages and device libraries.
     -Altera               Compile base libraries like 'altera' and 'altera_mf'
     -Max                  Compile device libraries for Max CPLDs
     -Arria                Compile device libraries for Arria FPGAs
     -Cyclone              Compile device libraries for Cyclone FPGAs
     -Stratix              Compile device libraries for Stratix FPGAs

  Compile options:
		
  .. code-block:: raw

     -VHDL93               Compile selected libraries with VHDL-93 (default).
     -VHDL2008             Compile selected libraries with VHDL-2008.

* `compile-xilinx-ise.ps1`

  Selectable libraries:
  
  .. code-block:: raw

     -All                  Compile all libraries, including common libraries, packages and device libraries.
     -Unisim               Compile the unisim primitives
     -Unimacro             Compile the unimacro macros
     -Simprim              Compile the simprim primitives
     -CoreLib              Compile the xilinxcorelib macros
     -Secureip             Compile the secureip primitives

  Compile options:
  
  .. code-block:: raw

     -VHDL93               Compile selected libraries with VHDL-93 (default).
     -VHDL2008             Compile selected libraries with VHDL-2008.

* `compile-xilinx-vivado.ps1`

  Selectable libraries:
  
  .. code-block:: raw

     -All                  Compile all libraries, including common libraries, packages and device libraries.
     -Unisim               Compile the unisim primitives
     -Unimacro             Compile the unimacro macros
     -Secureip             Compile the secureip primitives

  Compile options:
  
  .. code-block:: raw

     -VHDL93               Compile selected libraries with VHDL-93 (default).
     -VHDL2008             Compile selected libraries with VHDL-2008.

* `compile-osvvm.ps1`

  Selectable libraries:
  
  .. code-block:: raw

     -All                  Compile all.
     -OSVVM  		           Compile the OSVVM library.

* `compile-uvvm.ps1`

  Selectable libraries:
  
  .. code-block:: raw

     -All                  Compile all.
     -UVVM  		           Compile the UVVM libraries.

--------------------------------------------------------------------------------

.. container:: footnotes

	 .. rubric:: Footnotes

   .. [#f1] OSVVM http://github.com/OSVVM/OSVVM
   .. [#f3] UVVM https://github.com/UVVM/UVVM_All
   .. [#f4] Generic Colourizer http://kassiopeia.juls.savba.sk/~garabik/software/grc.html
