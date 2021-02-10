# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
#  Authors:
#    Tristan Gingold
#    Patrick Lehmann
#
# Package package:  Python binding and low-level API for shared library 'libghdl'.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
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

from ctypes import c_char_p, CDLL
import os
import sys
from pathlib import Path
from shutil import which

from pydecor import export

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl.version import __version__


def _get_libghdl_name() -> Path:
    """Get the name of the libghdl library (with version and extension)"""
    ver = __version__.replace("-", "_").replace(".", "_")
    ext = {"win32": "dll", "cygwin": "dll", "darwin": "dylib"}.get(sys.platform, "so")
    return Path("libghdl-{version}.{ext}".format(version=ver, ext=ext))


def _check_libghdl_libdir(libdir: Path, basename: Path) -> Path:
    """Returns libghdl path in :obj:`libdir`, if found."""
    if libdir is None:
        raise ValueError("Parameter 'libdir' is None.")
    # print('libghdl: check in {}'.format(libdir))
    res = libdir / basename
    if res.exists():
        return res

    raise FileNotFoundError(str(res))


def _check_libghdl_bindir(bindir: Path, basename: Path) -> Path:
    if bindir is None:
        raise ValueError("Parameter 'bindir' is None.")

    return _check_libghdl_libdir((bindir / "../lib").resolve(), basename)


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
    # GHDL_PREFIX is the prefix of the vhdl libraries, so remove the
    # last path component.
    r = os.environ.get("GHDL_PREFIX")
    try:
        return _check_libghdl_libdir(Path(r).parent, basename)
    except (TypeError, FileNotFoundError):
        pass

    # Try VUNIT_GHDL_PATH (path of the ghdl binary when using VUnit).
    r = os.environ.get("VUNIT_GHDL_PATH")
    try:
        return _check_libghdl_bindir(Path(r), basename)
    except (TypeError, FileNotFoundError):
        pass

    # Try GHDL (name/path of the ghdl binary)
    r = os.environ.get("GHDL", "ghdl")
    r = which(r)
    try:
        return _check_libghdl_bindir(Path(r).parent, basename)
    except (TypeError, FileNotFoundError):
        pass

    # Try within libghdl/ python installation
    r = Path(__file__)
    try:
        return _check_libghdl_bindir(r.parent, basename)
    except (TypeError, FileNotFoundError):
        pass

    # Try when running from the build directory
    r = (r.parent / "../../lib").resolve()
    try:
        return _check_libghdl_libdir(r, basename)
    except (TypeError, FileNotFoundError):
        pass

    # Failed.
    raise Exception("Cannot find libghdl {}".format(basename))


def _initialize():
    # Load the shared library
    _libghdl_path = _get_libghdl_path()
    # print("Load {}".format(_libghdl_path))
    libghdl = CDLL(str(_libghdl_path))

    # Initialize it.
    # First Ada elaboration (must be the first call)
    libghdl.libghdl_init()
    # Then 'normal' initialization (set hooks)
    libghdl.libghdl__set_hooks_for_analysis()

    # Set the prefix in order to locate the VHDL libraries.
    prefix = str(_libghdl_path.parent.parent).encode("utf-8")
    libghdl.libghdl__set_exec_prefix(c_char_p(prefix), len(prefix))

    return libghdl


# Initialize shared library when package is loaded
libghdl = _initialize()


@export
def finalize() -> None:
    """Free all the memory, be ready for a new initialization."""
    libghdl.options__finalize()


@export
def initialize() -> None:
    """Initialize or re-initialize the shared library."""
    libghdl.options__initialize()


@export
def set_option(Opt: str) -> bool:
    """
    Set option :obj:`opt`.

    :param Opt: Option to set.
    :return:    Return ``True``, if the option is known and handled.
    """
    Opt = Opt.encode("utf-8")
    return libghdl.libghdl__set_option(c_char_p(Opt), len(Opt)) == 0


@export
def analyze_init() -> None:
    """
    Initialize the analyzer.

    .. deprecated:: 1.0.0
       Deprecated as it may raise an exception. Use :func:`analyze_init_status`.
    """
    libghdl.libghdl__analyze_init()


@export
def analyze_init_status() -> int:
    """
    Initialize the analyzer.

    :return: Returns 0 in case of success.
    """
    return libghdl.libghdl__analyze_init_status()


@export
def analyze_file(fname: str) -> Iir:
    """
    Analyze a given filename :obj:`fname`.

    :param fname: File name
    :return:      Internal Intermediate Representation (IIR)
    """
    fname = fname.encode("utf-8")
    return libghdl.libghdl__analyze_file(c_char_p(fname), len(fname))


@export
def disp_config() -> None:
    """Display the configured prefixes for libghdl."""
    libghdl.ghdllocal__disp_config_prefixes()
