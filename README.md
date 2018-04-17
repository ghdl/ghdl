[![Documentation Status](https://readthedocs.org/projects/ghdl/badge/?version=latest)](http://ghdl.readthedocs.io) [![Join the chat at https://gitter.im/ghdl1/Lobby](https://img.shields.io/gitter/room/ghdl1/Lobby.svg?colorB=4cb696)](https://gitter.im/ghdl1/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![GNU General Public License 2](https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?style=flat)](https://github.com/ghdl/ghdl/blob/master/COPYING.md) [![Creative Commons Attribution-ShareAlike](https://img.shields.io/badge/doc%20license-CC--BY--SA--4.0-aab2ab.svg?style=flat)](https://github.com/ghdl/ghdl/blob/master/doc/COPYING_DOC.md)
[![Linux containers at Travis-CI](https://img.shields.io/travis/ghdl/ghdl/master.svg?style=flat&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAIP0lEQVR42qVXaUyUWRaFycyP%2BTVLemaSSfrPJN3JZJKOPyY6uBLTQgGlUMVeFHuxFaioKGpcAFFQUaOigAsmo7ghikZtxZVFCCKgqOi4ICoqqLjghqjcOeem%2B5sQFZnuL3l5VV%2B9d%2B9595577iuX%2F%2FcZNWrUX7y8vMyJiYmbpk6d2pKamto5bdq0LnxuT05OPhgUFOQcOXLkP35a7%2Bvr6%2FKLnmXLlun87t27X02YMKFkzpw5va2trXL37l158OCBPHz4UEdXV5fcu3dPbt26Jdu3b%2B%2Fz8%2FNrM5vNf%2BNeAPxlIHAi06xZs7rOnj0rL168kPb2dnn8%2BLE8evToo0EgHPfv35e1a9cKQOT9LKc2m03nyMhIv%2FXr18uzZ8%2F0pDU1NXLz5k0xTzSLycs0cHibBClhBOTJkycKqKmpSZCyGkTv10N27unpqfPw4cNDSkpK5OrVqzSmAHbu3CnXr1%2BXhIQECQkNkeCQYHEkxAPQRHEmJ0taWposWbJEOjs7dX1bW5s0NjYyEtW0OXny5KGBMJlMf1ixYsUHOqYRGmPYi4qK5PLly5KUlCSLFi0S5FqCAaSutk6io6MlMzNTtmzZIkwX9zASdXV10tLSQhALaRtEHdy5iLhYrdbbP53gxo0baqy7u1tWrlxJAErA7sfdYrFYNBINZxv0XUdHh5SXl8uhQ4eMPbW1tfqZqQwODv7rF0%2F%2FPZ6GhoZ%2B5v3AgQNKqqdPnyoBly9fztMxJeSFAeDcuXN8p9Wxf%2F9%2BBfH8%2BXMFcP78eSMlWN%2F8SafDhg0zPqOmG2%2Ffvi137tzRkF%2B6dEny8vLE399fUA1y%2BvRpGtMT%2B1n8JMQWKhUVFQTKiKlzkE7XHjlyRJqbm7lWI3n06NG%2B0aNHfzvAOVDq7Obm9mcIzIbS0lK5cuWKVFVVyerVq2Xv3r2aw5kzZ0pubq6cOnWKNa8njYyKlKRkpyBlPL2yPiUlhfzQVJ04cUI2bdrE1CmI%2Bvp6Wbp06bkRI0ZYBoAA4u9JIIAxGL9jxw5GgeRjCjT8GzdulM2bN2s5btu2TcJsNgmz2yU8IkIBHz58WIm4atUq6enpoS21iagyJbTL1BEYQe8zALi7u%2BdiAxcYg8iZd4a2srJST%2B9wOMTb25tk0gqYiPLjmDRpkkYhJCRE4uPjBeUmBw8e1JJ9%2Ffo108GDGLYJJjw8XAwAY8eOzedLIuZCkoY1Tzbv2rVLEV%2B4cIE5NpSOPCHrORh%2BpoX7GDVqx5kzZ2Tfvn2yZ88eVUVKOG1zMAoRiJoBADnJpEHmiMTh5prqaimHgeLiYs2p0%2BkkETUCXt5eUD8dVDod0A7OjIhGYcGCBVJQUCBlZWVMD21yZjQ1Aj4%2BPv8DMGbMmDCemDJKR%2BPGjRNfhNgeES7hkREML0tIAfiYzWK3h6kA2TBjPUNMkPjdKgmJiRIHdXTEIV0%2B3poWdEqBrGvYs7Oz5dixY0zhfwwAULGvwsLCVMvt4XaJdcRKQlIinahTdEE60FxDyQDOl1Whsrt48WI1Ghpqkxjso3OSkpGaN2%2Be5OfnGwNiRBUlSQnMNqAScEpvi9UiMbGxgj4uqFcl2ty5c2XGjBn6vbCwkGlibbM0WWpaEdAurX0eonhLMbmjJM7JyWFZ0h4JrLYCAwM%2FIJpRsOPqAlTqHPX%2FDXKHU4T2BAYFfsAirQA%2B1HNupNo1NTeR1RzqnJrBCGVkZGiX5MO%2BsBm86evrE9wjpL%2B%2Fn6%2B1R0yfPp0cqfxICRGuDoweEKkxJiaGZaSsfvnypTK8EyLCfK4tLEClPKQ9rqFCsu45U%2FO19gODgmXm7NkQsDICpQ0dGzZsoLBRP%2FrHjx%2F%2Fb5gwnFeDkeunTJnye7J0zZo1zB1DrBtfvXql9ezp4SFubv8Ci09rmeGmQxAsNQWB2xMdajpGurmRI4Zz2KBdKiJ1gNVyLC4uztUFOR6NcJsJBMz%2FHUrwPRWQqnfy5EluplHKL0nIk%2FIdDWl%2Bjx8%2FLu%2Ffv9eajgV33rx5w9Qoy69du8Zy43qt%2B6ysLA7VDcj%2BVj09yuMbzgDBLvhb3AHuV1ZWybp168h8CpPm8O3bt4IOSSlWIwsXLmQFkB%2FMNVlNpSQXWBX8TcO9detW2b17t8yfP5%2BVwshSrlnmjoEXTwWT4moyeda0tl6mkGhIESYC4elY6zCcg%2Fe5MDhPAJZEZYSUgLORd4Cjc%2B4FoBxGjaCQogyWMEVIbYP0f%2FzMXWD8KKvV8i4gIOA8ygcLEzhQQrEwvBgdsBzt%2BSLCeJcRYYulLCvDS3eXwvFSSO4lKF4l0lCB0%2B9CNPORqgpwIq3Fw8PDirT%2F8xOuSUbTgO%2FZ2Yva6uvr0GIb0UrvoSM%2B0ZGenk6RgSL6yAQQc5y7u5ghq%2F4BARJqs1ELlAe9vb1IUTd7B%2FZ3Qaq9nHDugVI3D3YdcwWzv0Y5uqI%2FpOMKTgYbTKYexMXHSaIz6bOD94OJCDfTwT7CywrbMWz24MLbi1R8DdJ%2B5NsQJSxIxeIqCExAVFRU%2B8WLF6WosEiNhIbZ6GRIIzo2WiIiI9gvMEeqPCMFJS6DPUQGyfwNkHZAlhnmtww3DTlTkj%2FliNrPvvHFqFihmC5DfXDyP1nQAZO%2BfFIoXxDb82cdE3gwOiLELuPHxje4c%2Bi1ziZPUzxuPjTyScMcDvCBf1IKCwpBQn90UYfEo2qiYqKVjAGBAc%2B9fXxqQdL0H1V3aBFAGeqMlpnKjmgQD62UTtiy%2FSwWtmYqm5IUXKFeMM%2FsomV2u%2F3vtAEuufysByfgRJX8DshTPU2mPPy7yQBBYzGWgC%2FVkOSXWVmZZHwnSPoD%2FlOmodXSsWFjsOe%2FCAcrQqMD2awAAAAASUVORK5CYII%3D)](https://travis-ci.org/ghdl/ghdl) [![Windows VMs at AppVeyor](https://ci.appveyor.com/api/projects/status/7hx4kfbhidv06sin?svg=true)](https://ci.appveyor.com/project/tgingold/ghdl-psgys) [![Commits since latest release](https://img.shields.io/github/commits-since/ghdl/ghdl/latest.svg?style=flat)](https://github.com/ghdl/ghdl/releases)

**A new GitHub organization was created (2017-12-20) and the main repo was moved from [github.com/tgingold/ghdl](https://github.com/tgingold/ghdl) to [github.com/ghdl/ghdl](https://github.com/ghdl/ghdl). Old refs will continue working, because permanent redirects are set up. However, we suggest every contributor to update the remote URLs in their local clones. See
[Changing a remote's URL](https://help.github.com/articles/changing-a-remote-s-url/).**

# GHDL

This directory contains the sources of GHDL, the open-source compiler and simulator for [VHDL](https://en.wikipedia.org/wiki/VHDL), a Hardware Description Language ([HDL](https://en.wikipedia.org/wiki/Hardware_description_language)). GHDL is not an interpreter: it allows you to analyse and elaborate sources to generate machine code from your design. Native program execution is the only way for high speed simulation.

## Main features

Full support for the [1987](http://ieeexplore.ieee.org/document/26487/), [1993](http://ieeexplore.ieee.org/document/392561/), [2002](http://ieeexplore.ieee.org/document/1003477/) versions of the [IEEE](www.ieee.org) [1076](http://standards.ieee.org/develop/wg/P1076.html) VHDL standard, and partial for the latest [2008](http://ieeexplore.ieee.org/document/4772740/) revision.

Partial support of [PSL](https://en.wikipedia.org/wiki/Property_Specification_Language).

By using a code generator ([LLVM](http://llvm.org/), [GCC](http://gcc.gnu.org/) or, [x86_64](https://en.wikipedia.org/wiki/X86-64)/[i386](https://en.wikipedia.org/wiki/Intel_80386) only, a built-in one), it is much faster than any interpreted simulator. It can handle very large designs, such as [leon3/grlib](http://www.gaisler.com/index.php/downloads/leongrlib).

GHDL runs on [GNU/Linux](http://en.wikipedia.org/wiki/Linux_distribution), [Windows](http://en.wikipedia.org/wiki/Microsoft_Windows) and [macOS](http://en.wikipedia.org/wiki/MacOS), both on `x86` and on `x86_64`. You can freely download a binary distribution for your OS, or try to build it on your own machine (see *'Getting GHDL'* below).

Can write waveforms to a [GHW](http://ghdl.readthedocs.io/en/latest/using/Simulation.html?highlight=GHW#cmdoption-wave), [VCD](https://en.wikipedia.org/wiki/Value_change_dump) or FST file. Combined with a [GUI](http://en.wikipedia.org/wiki/Graphical_user_interface)-based [waveform viewer](https://en.wikipedia.org/wiki/Waveform_viewer) and a good text editor, GHDL is a very powerful tool for writing, testing and simulating your code.

Supported third party projects: [VUnit](https://vunit.github.io), [OSVVM](http://osvvm.org), [cocotb](https://github.com/potentialventures/cocotb) (through the [VPI interface](https://en.wikipedia.org/wiki/Verilog_Procedural_Interface)), ...

GHDL is free software:

- [![GNU General Public License 2](https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?style=flat)](https://github.com/ghdl/ghdl/blob/master/COPYING.md)
- [![Creative Commons Attribution-ShareAlike 4.0](https://img.shields.io/badge/doc%20license-Creative%20Commons%20Attribution--ShareAlike--4.0-aab2ab.svg?style=flat)](https://github.com/ghdl/ghdl/blob/master/doc/COPYING_DOC.md) available at [![ghdl.readthedocs.io](https://img.shields.io/badge/ghdl-.readthedocs.io-2980b9.svg?style=flat)](https://ghdl.readthedocs.io)
- Some of the runtime libraries, are under different terms; see the individual source files for details.

## Getting GHDL

### Pre-built releases

Periodically (not regularly), several binary distributions are made available through the [releases](https://github.com/ghdl/ghdl/releases) tab. If you can't find the one matching the platform and versions you need, you can build it yourself!

### Building GHDL

In order to follow the traditional way to `configure` and `make`, you need the GNU Ada compiler, GNAT GPL, 2014 (or later) for x86 (32 or 64 bits). GNAT GPL can be downloaded anonymously from [libre.adacore.com](http://libre.adacore.com/tools/gnat-gpl-edition/). Then, untar and run the *doinstall* script.

*Depending on the OS and distribution you are using, you will also need to install some toolchain dependencies, such as `zlib`. See '[Building](http://ghdl.readthedocs.io/en/latest/building/Building.html)' for specific package names.*

In the GHDL base directory, configure and build:
```sh
$ ./configure --prefix=/usr/local
$ make
```

At that place, you can already use the 'ghdl_mcode' built in the directory. You can also install GHDL:

```sh
$ make install
```

That's all!

*The executable is installed as 'ghdl' in `/usr/local`. To install it to a different path, change the `--prefix` in the call to `configure`. For example, on Windows, you may want to set it to `--prefix=/c/Program Files (x86)/GHDL`.*

---

Furthermore, each supported compiler has its pros and cons. Here is a short comparaison:

| | pros | cons | observations |
|---|---|---|---|
|mcode | very easy to build | x86_64/i386 only | no executable created from your design |
| | very quick analysis time and can analyze very big designs | simulation is slower | |
| GCC | generated code is faster | analyze can take time (particularly for big units) | the output is an executable |
| | generated code can be debugged | build is more complex |
| | many platforms (x86, x86_64, powerpc, sparc) | |

LLVM has the same pros/cons as GCC, but it is easier to build. However, coverage (`gcov`) is unique to GCC.

You can find specific instructions for each of the options in '[Building](http://ghdl.readthedocs.io/en/latest/building/Building.html)'.
