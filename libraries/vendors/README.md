## Compile Scripts for Vendor VHDL Libraries

Vendors like Altera and Xilinx have there own simulation libraries, especially for primitives and hard macros. These libraries can not be shipped with GHDL, but we offer prepared compile scripts to pre-compile a vendor library, if the vendor tool is present on the computer.

There are also popular simulation and verification libraries, which can be pre-compile.

The compilation scripts are writen in shell languages: PowerShell for Windows and Bash for Linux. There are no further requirements.

##### Supported Vendors Libraries

 - Altera Quartus-II (15.x):
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
 - Xilinx ISE (14.7):
     - unisim (incl. secureip)
     - unimacro
     - simprim (incl. secureip)
 - Xilinx Vivado (2015.x):
     - unisim
     - unimacro

##### Supported Simulation and Verification Libraries

 - OSVVM (for VHDL-2008)
     - osvvm 
 - VUnit (for VHDL-2008)
     - vunit_lib 
 
---------------------------------------------------------------------
### Compiling on Linux

*First I should translate the scripts before writing the docu...*

---------------------------------------------------------------------
### Compiling on Windows

 - **Step 1 - Configure the scripts**
    Please open the `config.psm1` file and set the dictionary entries for the installed
    vendor tools to the appropriate directory to your tool's installation folder.
    
    `config.psm1`:

        $InstallationDirectory = @{
          "AlteraQuartusII" = "C:\Altera\15.0";
          "XilinxISE" =       "C:\Xilinx\14.7";
          "XilinxVivado" =    "C:\Xilinx\Vivado\2015.3";
          "OSVVM" =           "D:\git\GitHub\osvvm";
          "VUnit" =           "D:\git\GitHub\vunit"
        }

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
    d----    20.11.2015    19:40        <DIR> vivado
    d----    20.11.2015    19:45        <DIR> vunit
    d----    20.11.2015    19:06        <DIR> xilinx
    ```

### Selectable Options for the Bash Scripts:

*First I should translate the scripts before writing the docu...*

 - Common parameters to all scripts:

        --all               Compile all libraries, including common libraries, packages and device libraries.
        --clean				Cleanup directory before analyzing.
        --suppresswarnings	Don't show warnings. Report errors only.
 - `compile-altera.sh`

        --altera            Compile base libraries like 'altera' and 'altera_mf'
        --max               Compile device libraries for Max CPLDs 
        --arria             Compile device libraries for Arria FPGAs
        --cyclone           Compile device libraries for Cyclone FPGAs
        --stratix           Compile device libraries for Stratix FPGAs

 - `compile-xilinx-ise.sh`

        --unisim            Compile the unisim primitives
        --unimacro          Compile the unimacro macros
        --simprim           Compile the simprim primitives
        --secureip          Compile the secureip primitives
 - `compile-xilinx-vivado.sh`

        --unisim            Compile the unisim primitives
        --unimacro          Compile the unimacro macros
        --secureip          Compile the secureip primitives
 - `compile-osvvm.sh`
 - `compile-vunit.sh`

### Selectable Options for the PowerShell Scripts:

 - Common parameters to all scripts:

        -All                Compile all libraries, including common libraries, packages and device libraries.
        -Clean				Cleanup directory before analyzing.
        -SuppressWarnings	Don't show warnings. Report errors only.
 - `compile-altera.ps1`

        -Altera             Compile base libraries like 'altera' and 'altera_mf'
        -Max                Compile device libraries for Max CPLDs 
        -Arria              Compile device libraries for Arria FPGAs
        -Cyclone            Compile device libraries for Cyclone FPGAs
        -Stratix            Compile device libraries for Stratix FPGAs
 - `compile-xilinx-ise.ps1`

        -Unisim             Compile the unisim primitives
        -Unimacro           Compile the unimacro macros
        -Simprim            Compile the simprim primitives
        -Secureip           Compile the secureip primitives
 - `compile-xilinx-vivado.ps1`

        -Unisim             Compile the unisim primitives
        -Unimacro           Compile the unimacro macros
        -Secureip           Compile the secureip primitives
 - `compile-osvvm.ps1`
 - `compile-vunit.ps1`

------------------------
Author: Patrick Lehmann (23.11.2015)
