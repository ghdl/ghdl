# GHDL Wavefile helpers

This subdir contains C helpers for managing GHDL Wavefiles (GHW).
These sources are used in GTKWave for reading GHW waves (see [gtkwave/gtkwave/search?q=ghwlib](https://github.com/gtkwave/gtkwave/search?q=ghwlib)).

- `ghwlib.c` and `ghwlib.h` provide the reader library.
- `ghwdump.c` uses the library for dumping the content of GHW files in text, for debugging purposes.

A shared library named `ghwlib.[so|dll]` and `ghwdump` are built and installed with GHDL by default.
Furthermore, `ghwdump` is used in the test suite.

These helpers are independent from GHDL's codebase. However, the GHW format is not specified, so it might change as a result of internal tweaks in GHDL. Hence, it is strongly suggested to use the helpers provided in the GHDL installation.
