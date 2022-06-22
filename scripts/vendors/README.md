# Compile Scripts for Vendor VHDL Libraries

Vendors like Altera, Lattice and Xilinx have their own simulation libraries,
especially for FPGA primitives, soft and hard macros. These libraries cannot be
shipped with GHDL, but GHDL offers prepared compile scripts to pre-compile the
vendor libraries, if the vendor tool is present on the computer.

There are also popular simulation and verification libraries like [OSVVM][osvvm]
and [UVVM][uvvm], which can be pre-compile, too.

The compilation scripts are written in the shell languages: PowerShell for Windows
and Bash for Linux, MacOS, MSYS2/MinGW. The compile scripts can colorize the GHDL
warning and error lines with the help of grc/grcat ([generic colourizer][grc]).

 [osvvm]: http://osvvm.org/
 [uvvm]: https://github.com/UVVM/UVVM_All
 [grc]: http://kassiopeia.juls.savba.sk/~garabik/software/grc.html

See the [GHDL Documentation](https://ghdl.github.io/ghdl) for a detailed
documentation on how to use [Precompile Scripts](https://ghdl.github.io/ghdl/getting.html#precompile-vendor-primitives)

---------------------------------------------------------------------

## Quick Start
### Compiling in Bash (e.g. Linux)

 - **Step 1 - Browse to your simulation working directory**
    ```Bash
    $ cd <MySimulationFolder>
    ```

 - **Step 2 - Start the compilation script(s)**
	 Choose one of the provided pre-compilation scripts and run:
    ```Bash
    $ /usr/local/lib/ghdl/vendors/compile-osvvm.sh --all --source ../path/to/osvvm
    ```

    In most cases GHDL is installed into `/usr/local/`. The scripts are
    installed into the `lib/ghdl/vendors` directory.

 - **Step 3 - Viewing the result**
    This creates vendor directories in your current working directory and
    compiles the vendor files into them.

    ```Bash
    $ ls -ahl
    ...
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:48 osvvm
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:58 osvvm_common
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:58 osvvm_axi4
    drwxr-xr-x  2 <user> <group>  56K Nov 30 17:48 osvvm_uart
    ```


---------------------------------------------------------------------
### Compiling in PowerShell (e.g. Windows)

 - **Step 1 - Browse to your simulation working directory**
    ```PowerShell
    PS> cd <MySimulationFolder>
    ```

 - **Step 2 - Start the compilation script(s)**
	 Choose one of the provided pre-compilation scripts and run:
    ```PowerShell
    PS> <GHDL>\lib\ghdl\vendors\compile-osvvm.ps1 -All -Source ..\path\to\osvvm
    ```

 - **Step 3 - Viewing the result**
    This creates vendor directories in your current working directory and
    compiles the vendor files into them.

    ```PowerShell
    PS> dir
        Directory: D:\temp\ghdl

    Mode           LastWriteTime       Length Name
    ----           -------------       ------ ----
    d----    20.11.2019    19:38        <DIR> osvvm
    d----    20.11.2019    19:45        <DIR> osvvm_common
    d----    20.11.2019    19:06        <DIR> osvvm_axi4
    d----    20.11.2019    19:40        <DIR> osvvm_uart
    ```

---------------------------------------------------------------------

## Usage

*TODO*



---------------------------------------------------------------------

## Options

Each script has an integrated help. Use `script.sh --help` (Bash) or
`script.ps1 -Help` (PoSh) to print all options. When setting verbose or debug
mode, scripts will print more information or all commands executed by
the script itself.

With `--source` (Bash) or `-Source` (PoSh) the 3rd party libraries VHDL
directory can be specified. With `-output` (Bash) or `-Output` (PoSh) the
output directory can be specified. If GHDL is not automatically found, then
the path to the GHDL executable can be specified with `--ghdl` (Bash) or
`-GHDL` (PoSh).

For a detailed documentation and all command line options see
[Precompile Scripts](https://ghdl.github.io/ghdl/getting.html#precompile-vendor-primitives)


---------------------------------------------------------------------

## TODOs

- OSVVM
  - switch / search directories if normal OSVVM or OsvvmLibraries is specified as source
- Missing features
  - Implement `-Clean` ```--clean` commands
- describe usage with -P
- document offered procedures and functions
- don't enforce `--output` if `--source` is used.
- UVVM (OSVVM)
  - create a list of components as array and generate from that:
	  - variables
	  - cli options
	  - help text
	  - default values

------------------------
Author: Patrick Lehmann
Last update: 14.01.2020
