.. _LIB:Overview:

Overview
########

`GHDL` provides `libghdl` shared library, that is intended to be used by advanced users.
The library includes a subset of the regular features plus some features to be used by extension tools (**TODO**: It is really desirable to list available features).
`libghdl` allows developers to use `GHDL` features in any language they want.
What is needed are only language specific bindings.
If bindings for the language you want to use do not exist, and you want to implement them, please make them raw bindings.
Any additional abstraction or functionality should be based on raw bindings.

Here we should write superficially how `libghdl` works.

* Does it return structures such as AST to the calling code, or does it store them internally?
* Is it thread safe?
* Answers to the questions from https://github.com/ghdl/ghdl/issues/1437 should probably be placed here.
