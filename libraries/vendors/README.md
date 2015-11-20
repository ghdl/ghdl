## Compile Scripts for Vendor Libraries

Vendors like Altera and Xilinx have there own simulation libraries, especially for primitives and hard macros. These libraries can not be shipped with GHDL, but we offer prepared compile scripts to pre-compile a vendor library, if the vendor tool is present on the computer.

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
 - OSVVM
 - *VUnit (planned)*
 - Xilinx ISE (14.7):
     - unisim
     - simprim

##### Supported Operating Systems and Shells

We support Windows (PowerShell) and Linux (Bash).


---------------------------------------------------------------------
### Compiling on Linux

*First I should translate the scripts before writing the docu...*

---------------------------------------------------------------------
### Compiling on Windows

 - **Step 1 - Configure the scripts**
    Please open the provided scripts and set the variable `$SourceDir` to the appropriate directory of your vendors library folder.

 - **Step 2 - Browse to your simulation working directory**
    ```PowerShell
    PS> cd <MySimulationFolder>
    ```

 - **Step 3 - Start the compilation script(s)**
    ```PowerShell
    PS> <GHDL>\libraries\vendors\compile-altera.ps1 -All
    PS> <GHDL>\libraries\vendors\compile-osvvm.ps1 -All
    PS> <GHDL>\libraries\vendors\compile-xilinx.ps1 -All
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
    d----    20.11.2015    19:06        <DIR> xilinx
    ```

### Selectable Options for the Bash Scripts:

*First I should translate the scripts before writing the docu...*

 - compile-altera.sh

        --All       Compile all libraries (base libraries and device libraries)
        --altera    Compile base libraries like 'altera' and 'altera_mf'
        --max       Compile device libraries for Max CPLDs 
        --arria     Compile device libraries for Arria FPGAs
        --cyclone   Compile device libraries for Cyclone FPGAs
        --stratix   Compile device libraries for Stratix FPGAs

 - compile-osvvm.sh

        --All       Compile all packages

 - compile-xilinx.sh

        --All       Compile all libraries
        --unisim    Compile the unisim primitives
        --simprim   Compile the simprim primitives


### Selectable Options for the PowerShell Scripts:

 - compile-altera.ps1

        -All       Compile all libraries (base libraries and device libraries)
        -altera    Compile base libraries like 'altera' and 'altera_mf'
        -max       Compile device libraries for Max CPLDs 
        -arria     Compile device libraries for Arria FPGAs
        -cyclone   Compile device libraries for Cyclone FPGAs
        -stratix   Compile device libraries for Stratix FPGAs

 - compile-osvvm.ps1

        -All       Compile all packages

 - compile-xilinx.ps1

        -All       Compile all libraries
        -unisim    Compile the unisim primitives
        -simprim   Compile the simprim primitives

------------------------
Author: Patrick Lehmann (20.11.2015)
