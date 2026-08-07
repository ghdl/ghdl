# =============================================================================
#               ____ _   _ ____  _
#  _ __  _   _ / ___| | | |  _ \| |
# | '_ \| | | | |  _| |_| | | | | |
# | |_) | |_| | |_| |  _  | |_| | |___
# | .__/ \__, |\____|_| |_|____/|_____|
# |_|    |___/
# =============================================================================
# Authors:
#   Patrick Lehmann
#
# Package module:   Resource package containing GHDL's shared libraries and VHDL libraries.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2026 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
"""
Resource package containing GHDL's binary artifacts.

This package holds no Python code. It is the install location of the files produced when GHDL is
built, so that a pyGHDL wheel is self-contained and :mod:`pyGHDL.libghdl` finds the shared library
next to itself instead of somewhere on the system.

**Contents of an installed package:**

+-------------------------------+--------------------------------------------------------------------+
| File or directory             | Purpose                                                            |
+===============================+====================================================================+
| :file:`libghdl-*.so`,         | GHDL's analyzer and simulator as a shared library, loaded by        |
| :file:`libghdl-*.dll`,        | :mod:`pyGHDL.libghdl` through :mod:`ctypes <python:ctypes>`. The    |
| :file:`libghdl-*.dylib`       | filename carries GHDL's version, e.g.                              |
|                               | :file:`libghdl-7_0_0_dev.so`.                                      |
+-------------------------------+--------------------------------------------------------------------+
| :file:`libgnat-*.so`          | The GNAT runtime ``libghdl`` was linked against, shipped when it is |
|                               | not expected to be present on the target system.                   |
+-------------------------------+--------------------------------------------------------------------+
| :file:`libghdlvpi.*`          | The VPI support library, used by simulations loading VPI modules.  |
+-------------------------------+--------------------------------------------------------------------+
| :file:`ghdl/`                 | The pre-analyzed VHDL libraries - ``std`` and ``ieee`` for each     |
|                               | supported language revision (:file:`v87`, :file:`v93`, :file:`v08`, |
|                               | :file:`v19`) - plus their sources in :file:`ghdl/src`.             |
+-------------------------------+--------------------------------------------------------------------+

.. note::

   The files are build output: none of them is in version control, and the directory contains only
   this module in a source checkout. They appear when GHDL is installed or a wheel is unpacked.
"""
