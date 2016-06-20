## Compile Scripts for Vendor VHDL Libraries

Vendors like Altera and Xilinx have there own simulation libraries, especially
for primitives, soft or hard macros. These libraries can not be shipped with
GHDL, but we offer prepared compile scripts to pre-compile a vendor library,
if the vendor tool is present on the computer.

There are also popular simulation and verification libraries like [OSVVM][osvvm]
and [VUnit][vunit], which can be pre-compile.

The compilation scripts are writen in shell languages: PowerShell for Windows
and Bash for Linux. The compile scripts can colorize the GHDL warning and error
lines with the help of grc/grcat ([generic colourizer][grc]).

 [osvvm]: http://osvvm.org/
 [vunit]: https://github.com/LarsAsplund/vunit
 [grc]: http://kassiopeia.juls.savba.sk/~garabik/software/grc.html

##### Supported Vendors Libraries

 - Altera Quartus (&ge;13.0):
     - lpm, sgate
     - altera, altera_mf, altera_lnsim
     - arriaii, arriaii_pcie_hip, arriaiigz
     - arriav, arriavgz, arriavgz_pcie_hip
     - cycloneiv, cycloneiv_pcie_hip, cycloneive
     - cyclonev
     - max, maxii, maxv
     - stratixiv, stratixiv_pcie_hip
     - stratixv, stratixv_pcie_hip
     - fiftyfivenm, twentynm
 - Lattice (&ge;3.6):
     - ec
     - ecp, ecp2, ecp3, ecp5u
     - lptm, lptm2
     - machxo, machxo2, machxo3l
     - sc, scm
     - xp, xp2
 - Xilinx ISE (&ge;14.0):
     - unisim (incl. secureip)
     - unimacro
     - simprim (incl. secureip)
 - Xilinx Vivado (&ge;2014.1):
     - unisim (incl. secureip)
     - unimacro

##### Supported Simulation and Verification Libraries

 - OSVVM (for VHDL-2008)
     - osvvm
 - VUnit (for VHDL-2008)
     - vunit_lib

---------------------------------------------------------------------
### Compiling on Linux

 - **Step 1 - Configure the scripts**
    Please open the `config.sh` file and set the dictionary entries for the installed
    vendor tools to the appropriate directory to your tool's installation folder.

    `config.sh`:
    ```Bash
    declare -A InstallationDirectory
    InstallationDirectory[AlteraQuartusII]="/opt/Altera/15.0"
    InstallationDirectory[XilinxISE]="/opt/Xilinx/14.7"
    InstallationDirectory[XilinxVivado]="/opt/Xilinx/Vivado/2015.2"
    InstallationDirectory[OSVVM]="/home/<user>/git/GitHub/osvvm"
    InstallationDirectory[VUnit]="/home/<user>/git/GitHub/vunit"
    ```

 - **Step 2 - Browse to your simulation working directory**
    ```Bash
    $ cd <MySimulationFolder>
    ```

 - **Step 3 - Start the compilation script(s)**
    ```Bash
    $ <GHDL>\libraries\vendors\compile-altera.sh --all
    $ <GHDL>\libraries\vendors\compile-xilinx-ise.sh --all
    $ <GHDL>\libraries\vendors\compile-xilinx-vivado.sh --all
    $ <GHDL>\libraries\vendors\compile-osvvm.sh --all
    $ <GHDL>\libraries\vendors\compile-vunit.sh --all
    ```

 - **Step 4 - Viewing the result**
    This creates vendor directories in your current working directory and compiles the vendor files into them.

    ```Bash
    $ ls -ahl
    ...
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:41 altera
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:48 osvvm
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:48 vivado
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:58 vunit
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:58 xilinx
    ```

---------------------------------------------------------------------
### Compiling on Windows

 - **Step 1 - Configure the scripts**
    Please open the `config.psm1` file and set the dictionary entries for the installed
    vendor tools to the appropriate directory to your tool's installation folder. Use an
    empty string `""` for not installed tools. 

    `config.psm1`:
    ```PowerShell
    $InstallationDirectory = @{
      "AlteraQuartusII" = "C:\Altera\15.0";
      "XilinxISE" =       "C:\Xilinx\14.7";
      "XilinxVivado" =    "C:\Xilinx\Vivado\2015.4";
      "OSVVM" =           "D:\git\GitHub\osvvm";
      "VUnit" =           "D:\git\GitHub\vunit"
    }
    ```

 - **Step 2 - Browse to your simulation working directory**
    ```PowerShell
    PS> cd <MySimulationFolder>
    ```

 - **Step 3 - Start the compilation script(s)**
    ```PowerShell
    PS> <GHDL>\libraries\vendors\compile-altera.ps1 -All
    PS> <GHDL>\libraries\vendors\compile-xilinx-ise.ps1 -All
    PS> <GHDL>\libraries\vendors\compile-xilinx-vivado.ps1 -All
    PS> <GHDL>\libraries\vendors\compile-osvvm.ps1 -All
    PS> <GHDL>\libraries\vendors\compile-vunit.ps1 -All
    ```

 - **Step 4 - Viewing the result**
    This creates vendor directories in your current working directory and compiles the vendor files into them.

    ```PowerShell
    PS> dir
        Directory: D:\temp\ghdl

    Mode           LastWriteTime       Length Name
    ----           -------------       ------ ----
    d----    20.11.2015    19:33        <DIR> altera
    d----    20.11.2015    19:38        <DIR> osvvm
    d----    20.11.2015    19:45        <DIR> vunit_lib
    d----    20.11.2015    19:06        <DIR> xilinx-ise
    d----    20.11.2015    19:40        <DIR> xilinx-vivado
    ```

### Selectable Options for the Bash Scripts:

*First I should translate the scripts before writing the docu...*

 - Common parameters to all scripts:

        -h --help             Print the embedded help page(s).
        -c --clean            Cleanup directory before analyzing.
        -n --no-warnings	  Don't show warnings. Report errors only.
        -s --skip-existing    Skip already compiled files (an *.o file exists).
        -S --skip-largefiles  Don't compile large entities like DSP and PCIe primitives.
        -H --halt-on-error    Stop compiling if an error occured.
 - `compile-altera.sh`
    Selectable libraries:

        -a --all              Compile all libraries, including common libraries, packages and device libraries.
           --altera           Compile base libraries like 'altera' and 'altera_mf'
           --max              Compile device libraries for Max CPLDs
           --arria            Compile device libraries for Arria FPGAs
           --cyclone          Compile device libraries for Cyclone FPGAs
           --stratix          Compile device libraries for Stratix FPGAs
    Compile options:

           --vhdl93           Compile selected libraries with VHDL-93 (default).
           --vhdl2008         Compile selected libraries with VHDL-2008.
 - `compile-xilinx-ise.sh`
    Selectable libraries:

        -a --all              Compile all libraries, including common libraries, packages and device libraries.
           --unisim           Compile the unisim primitives
           --unimacro         Compile the unimacro macros
           --simprim          Compile the simprim primitives
           --secureip         Compile the secureip primitives
    Compile options:

           --vhdl93           Compile selected libraries with VHDL-93 (default).
           --vhdl2008         Compile selected libraries with VHDL-2008.
 - `compile-xilinx-vivado.sh`
    Selectable libraries:

        -a --all              Compile all libraries, including common libraries, packages and device libraries.
           --unisim           Compile the unisim primitives
           --unimacro         Compile the unimacro macros
           --secureip         Compile the secureip primitives
    Compile options:

           --vhdl93           Compile selected libraries with VHDL-93 (default).
           --vhdl2008         Compile selected libraries with VHDL-2008.
 - `compile-osvvm.sh`
    Selectable libraries:

        -a --all              Compile all.
 - `compile-vunit.sh`
    Selectable libraries:

        -a --all              Compile all.

### Selectable Options for the PowerShell Scripts:

 - Common parameters to all scripts:

        -Help                 Print the embedded help page(s).
        -Clean                Cleanup directory before analyzing.
        -SuppressWarnings     Don't show warnings. Report errors only.
 - `compile-altera.ps1`
    Selectable libraries:

        -All                  Compile all libraries, including common libraries, packages and device libraries.
        -Altera               Compile base libraries like 'altera' and 'altera_mf'
        -Max                  Compile device libraries for Max CPLDs
        -Arria                Compile device libraries for Arria FPGAs
        -Cyclone              Compile device libraries for Cyclone FPGAs
        -Stratix              Compile device libraries for Stratix FPGAs
    Compile options:

        -VHDL93              Compile selected libraries with VHDL-93 (default).
        -VHDL2008            Compile selected libraries with VHDL-2008.
 - `compile-xilinx-ise.ps1`
    Selectable libraries:

        -All                  Compile all libraries, including common libraries, packages and device libraries.
        -Unisim               Compile the unisim primitives
        -Unimacro             Compile the unimacro macros
        -Simprim              Compile the simprim primitives
        -Secureip             Compile the secureip primitives
    Compile options:

        -VHDL93              Compile selected libraries with VHDL-93 (default).
        -VHDL2008            Compile selected libraries with VHDL-2008.
 - `compile-xilinx-vivado.ps1`
    Selectable libraries:

        -All                  Compile all libraries, including common libraries, packages and device libraries.
        -Unisim               Compile the unisim primitives
        -Unimacro             Compile the unimacro macros
        -Secureip             Compile the secureip primitives
    Compile options:

        -VHDL93              Compile selected libraries with VHDL-93 (default).
        -VHDL2008            Compile selected libraries with VHDL-2008.
 - `compile-osvvm.ps1`
    Selectable libraries:

        -All                  Compile all.
 - `compile-vunit.ps1`
    Selectable libraries:

        -All                  Compile all.

------------------------
Author: Patrick Lehmann (30.11.2015)
