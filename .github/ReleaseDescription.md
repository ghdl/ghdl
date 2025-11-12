%%DESCRIPTION%%

# GHDL %ghdl%

GHDL offers the simulator and synthesis tool for VHDL. GHDL can be built for various backends:
* `gcc` - using the GCC compiler framework
* `mcode` - in memory code generation
* `llvm` - using the LLVM compiler framework
* `llvm-jit` - using the LLVM compiler framework, but in memory

The following asset categories are provided for GHDL:
* macOS x64-64 builds as TAR/GZ file
* macOS aarch64 builds as TAR/GZ file
* Ubuntu 24.04 LTS builds as TAR/GZ file
* Windows builds for standalone usage (without MSYS2) as ZIP file
* MSYS2/MinGW64 packages as TAR/ZST file
* MSYS2/UCRT64 packages as TAR/ZST file

## Docker Images

Latest docker images at pushed to [Docker Hub - ghdl/ghdl:latest](https://hub.docker.com/r/ghdl/ghdl/tags).

| Backend  | Ubuntu 22.04                        | Ubuntu 24.04                        |
|----------|-------------------------------------|-------------------------------------|
| mcode    | `ghdl:%ghdl%-mcode-ubuntu-22.04`    | `ghdl:%ghdl%-mcode-ubuntu-24.04`    |
| llvm     | `ghdl:%ghdl%-llvm-ubuntu-22.04`     | `ghdl:%ghdl%-llvm-ubuntu-24.04`     |
| llvm-jit | `ghdl:%ghdl%-llvm-jit-ubuntu-22.04` | `ghdl:%ghdl%-llvm-jit-ubuntu-24.04` |
| gcc      | `ghdl:%ghdl%-gcc-ubuntu-22.04`      | `ghdl:%ghdl%-gcc-ubuntu-24.04`      |


# pyGHDL %ghdl%

The Python package `pyGHDL` offers Python binding (`pyGHDL.libghdl`) to a `libghdl` shared library (`*.so`/`*.dll`).
In addition to the low-level binding layer, pyGHDL offers:
* a Language Server Protocol (LSP) instance for e.g. live code checking by editors
* a Code Document Object Model (CodeDOM) based on [pyVHDLModel](https://github.com/VHDL/pyVHDLModel)

The following asset categories are provided for pyGHDL:
* Platform specific Python wheel package for Ubuntu incl. `pyGHDL...so`
* Platform specific Python wheel package for Windows incl. `pyGHDL...dll`
* Platform specific Python wheel package for Windows + MSYS2 (MinGW64, UCRT64) incl. `pyGHDL...dll`
