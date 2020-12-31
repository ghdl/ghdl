# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:          Tristan Gingold
#
# Package package:  Python binding and low-level API for shared library 'libghdl'.
#
# License:
# ============================================================================
# Copyright (C) 2019-2020 Tristan Gingold
#
#	GHDL is free software; you can redistribute it and/or modify it under
#	the terms of the GNU General Public License as published by the Free
#	Software Foundation; either version 2, or (at your option) any later
#	version.
#
#	GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#	WARRANTY; without even the implied warranty of MERCHANTABILITY or
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#	for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with GHDL; see the file COPYING.  If not, write to the Free
#	Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
#
import ctypes
import os
import sys
from os.path import dirname, join, exists, normpath
from shutil import which
from pyGHDL.libghdl.version import __version__


def _to_char_p(arg):
    return ctypes.c_char_p(arg), len(arg)


def _get_libghdl_name():
    """Get the name of the libghdl library (with version and extension)"""
    ver = __version__.replace("-", "_").replace(".", "_")
    ext = {"win32": "dll", "cygwin": "dll", "darwin": "dylib"}.get(sys.platform, "so")
    return "libghdl-" + ver + "." + ext


def _check_libghdl_libdir(libdir, basename):
    """Return libghdl path in :param libdir" if found or None"""
    if libdir is None:
        return None
    # print('libghdl: check in {}'.format(libdir))
    res = join(libdir, basename)
    if exists(res):
        return res
    return None


def _check_libghdl_bindir(bindir, basename):
    if bindir is None:
        return None
    return _check_libghdl_libdir(normpath(join(bindir, "..", "lib")), basename)


def _get_libghdl_path():
    """
    Locate the directory where the shared library is installed.

    Search order:

    1. `GHDL_PREFIX` - directory (prefix) of the vhdl libraries.
    2. `VUNIT_GHDL_PATH` - path of the `ghdl` binary when using VUnit.
    3. `GHDL` - name of, or path to the `ghdl` binary.
    4. Try within `libghdl/` Python installation.
    5. Try when running from the build directory.
    """
    basename = _get_libghdl_name()
    # Try GHDL_PREFIX
    r = os.environ.get("GHDL_PREFIX")
    if r is not None:
        # GHDL_PREFIX is the prefix of the vhdl libraries, so remove the
        # last path component.
        r = _check_libghdl_libdir(dirname(r), basename)
        if r is not None:
            return r
    # Try VUNIT_GHDL_PATH (path of the ghdl binary when using VUnit).
    r = _check_libghdl_bindir(os.environ.get("VUNIT_GHDL_PATH"), basename)
    if r is not None:
        return r
    # Try GHDL (name/path of the ghdl binary)
    r = os.environ.get("GHDL", "ghdl")
    r = which(r)
    if r is not None:
        r = _check_libghdl_bindir(dirname(r), basename)
        if r is not None:
            return r
    # Try within libghdl/ python installation
    r = __file__
    r = _check_libghdl_bindir(dirname(r), basename)
    if r is not None:
        return r
    # Try when running from the build directory
    r = normpath(join(dirname(__file__), "..", "..", "lib"))
    r = _check_libghdl_libdir(r, basename)
    if r is not None:
        return r
    # Failed.
    raise Exception("Cannot find libghdl {}".format(basename))


# Load the shared library
_libghdl_path = _get_libghdl_path()
# print("Load {}".format(_libghdl_path))
libghdl = ctypes.CDLL(_libghdl_path)

# Initialize it.
# First Ada elaboration (must be the first call)
libghdl.libghdl_init()
# Then 'normal' initialization (set hooks)
libghdl.libghdl__set_hooks_for_analysis()

# Set the prefix in order to locate the vhdl libraries.
libghdl.libghdl__set_exec_prefix(
    *_to_char_p(dirname(dirname(_libghdl_path)).encode("utf-8"))
)

def finalize():
    "Free all the memory, be ready for a new initialization"
    libghdl.options__finalize()


def initialize():
    "Initialize or re-initialize the library"
    libghdl.options__initialize()


def set_option(opt):
    "Set option OPT.  Return true iff the option is known and handled"
    return libghdl.libghdl__set_option(*_to_char_p(opt)) == 0


def analyze_init():
    # Deprecated as it may raise an exception.  Use analyze_init_status
    libghdl.libghdl__analyze_init()

def analyze_init_status():
    # Return 0 in case of success
    return libghdl.libghdl__analyze_init_status()

def analyze_file(fname):
    return libghdl.libghdl__analyze_file(*_to_char_p(fname))


def disp_config():
    return libghdl.ghdllocal__disp_config_prefixes()
