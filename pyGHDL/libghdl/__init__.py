# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:
#   Tristan Gingold
#   Patrick Lehmann
#
# License:
# ============================================================================
#  Copyright (C) 2019-2022 Tristan Gingold
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
Python binding and low-level API for shared library ``libghdl``.

In case of an error, a :exc:`LibGHDLException` is raised.
"""
from ctypes import c_char_p, CDLL
from sys import version_info as sys_version_info
from os import environ as os_environ
from pathlib import Path
from shutil import which
from typing import List, Optional

from pyTooling.Decorators import export
from pyTooling.Common import getResourceFile
from pyTooling.Platform import CurrentPlatform
from pyTooling.Exceptions import ToolingException

from pyGHDL import GHDLBaseException
from pyGHDL import __version__ as ghdlVersion

# from pyGHDL.libghdl._decorator import BindToLibGHDL
from pyGHDL.libghdl._types import Iir


Nullable = Optional

__all__ = ["ENCODING"]

ENCODING = "latin-1"


@export
class LibGHDLException(GHDLBaseException):
    _internalErrors: Nullable[List[str]]

    def __init__(self, message: str, errors: List[str] = None):
        super().__init__(message)
        self._internalErrors = errors

    @property
    def InternalErrors(self) -> Nullable[List[str]]:
        return self._internalErrors


@export
def _get_libghdl_name() -> Path:
    """
    Get the filename of the libghdl shared library file (incl. version and file extension).

    :returns: Filename of the shared library file.
    """
    version = ghdlVersion.replace("-", "_").replace(".", "_")

    return Path(f"libghdl-{version}.{CurrentPlatform.DynamicLibraryExtension}")


@export
def _get_libghdl_path() -> Path:
    """\
    Locate the directory where the shared library is installed.

    **Search order:**

    1. `GHDL_PREFIX` - directory (prefix) of the vhdl libraries.
    2. `VUNIT_GHDL_PATH` - path of the `ghdl` binary when using VUnit.
    3. `GHDL` - name of, or path to the `ghdl` binary.
    4. Try within `pyGHDL/lib` Python installation.
    5. Try when running from the build directory.
    """

    def _check_libghdl_libdir(libDirectory: Path, libraryFilename: Path) -> Path:
        libGHDLSharedLibraryFile = libDirectory / libraryFilename
        if not libGHDLSharedLibraryFile.exists():
            raise FileNotFoundError(f"{libGHDLSharedLibraryFile}")

        return libGHDLSharedLibraryFile

    def _check_libghdl_bindir(binDirectory: Path, libraryFilename: Path) -> Path:
        libDirectory = (binDirectory / "../lib").resolve()
        return _check_libghdl_libdir(libDirectory, libraryFilename)

    libGHDLSharedLibraryFile = _get_libghdl_name()
    searchedAt = []

    # Try GHDL_PREFIX
    # GHDL_PREFIX is the prefix of the vhdl libraries, so remove the
    # last path component.
    if (varGHDLPrefix := os_environ.get("GHDL_PREFIX")) is None:
        searchedAt.append(f"  Checked variable 'GHDL_PREFIX':      not set")
    else:
        searchedAt.append(f"  Tried variable 'GHDL_PREFIX':        {varGHDLPrefix}")
        ghdlPrefix = Path(varGHDLPrefix)
        try:
            return _check_libghdl_libdir(ghdlPrefix.parent, libGHDLSharedLibraryFile)
        except FileNotFoundError:
            pass

    # Try GHDL (name/path of the ghdl binary)
    if (varGHDL := os_environ.get("GHDL")) is None:
        searchedAt.append(f"  Checked variable 'GHDL':             not set.")
    else:
        searchedAt.append(f"  Tried variable 'GHDL':               {varGHDL}")
        try:
            ghdl = Path(varGHDL)
            return _check_libghdl_bindir(ghdl.parent, libGHDLSharedLibraryFile)
        except (TypeError, FileNotFoundError):
            pass

    # Try VUNIT_GHDL_PATH (path of the ghdl binary when using VUnit).
    if (varVUnitGHDLPath := os_environ.get("VUNIT_GHDL_PATH")) is None:
        searchedAt.append(f"  Checked variable 'VUNIT_GHDL_PATH':  not set.")
    else:
        searchedAt.append(f"  Tried variable 'VUNIT_GHDL_PATH':    {varVUnitGHDLPath}")
        vunitGHDLPath = Path(varVUnitGHDLPath)
        try:
            return _check_libghdl_bindir(vunitGHDLPath, libGHDLSharedLibraryFile)
        except FileNotFoundError:
            pass

    # TODO: simplify code, see other uses above for find relative to package file
    try:
        from pyGHDL import lib as ResourcePackage

        return getResourceFile(ResourcePackage, libGHDLSharedLibraryFile)
    except ToolingException as ex:
        searchedAt.append(f"  Tried pyGHDL.lib resource directory: {ex.__cause__!s}")

    # Try GHDL (name/path of the ghdl binary)
    try:
        whichGHDL = which(varGHDL)
        searchedAt.append(f"  Tried 'which ghdl':                  {whichGHDL}")
        ghdl = Path(whichGHDL)
        return _check_libghdl_bindir(ghdl.parent, libGHDLSharedLibraryFile)
    except (TypeError, FileNotFoundError):
        pass

    # Try when running from the build directory
    try:
        libDirectory = (Path(__file__).parent / "../../lib").resolve()
        searchedAt.append(f"  Relative to build directory:         {libDirectory}")
        return _check_libghdl_libdir(libDirectory, libGHDLSharedLibraryFile)
    except (TypeError, FileNotFoundError):
        pass

    # Failed.
    ex = LibGHDLException(f"Cannot find pyGHDL shared library '{libGHDLSharedLibraryFile}'.")
    if sys_version_info >= (3, 11):  # pragma: no cover
        for search in searchedAt:
            ex.add_note(search)
    raise ex


@export
def _initialize():
    # Load the shared library
    _libghdl_path = _get_libghdl_path()

    # Load libghdl shared object
    libghdl = CDLL(str(_libghdl_path))

    # Initialize it.
    # First Ada elaboration (must be the first call)
    libghdl.libghdl_init()
    # Then 'normal' initialization (set hooks)
    libghdl.libghdl__set_hooks_for_analysis()

    # Set the prefix in order to locate the VHDL libraries.
    prefix = str(_libghdl_path.parent.parent).encode(ENCODING)
    libghdl.libghdl__set_exec_prefix(c_char_p(prefix), len(prefix))

    return libghdl


# Initialize shared library when package is loaded
libghdl = _initialize()


@export
# @BindToLibGHDL("options__finalize")
def finalize() -> None:
    """Free all the memory, be ready for a new initialization."""
    libghdl.options__finalize()


@export
# @BindToLibGHDL("options__initialize")
def initialize() -> None:
    """Initialize or re-initialize the shared library."""
    libghdl.options__initialize()


@export
# @BindToLibGHDL("libghdl__set_option")
def set_option(opt: str) -> bool:
    """\
    Set option :obj:`opt`.

    :param opt: Option to set.
    :return:    Return ``True``, if the option is known and handled.
    """
    opt = opt.encode(ENCODING)
    return libghdl.libghdl__set_option(c_char_p(opt), len(opt)) == 0


@export
# @BindToLibGHDL("libghdl__analyze_init")
def analyze_init() -> None:
    """\
    Initialize the analyzer.

    .. deprecated:: 1.0.0
       Deprecated as it may raise an exception. Use :func:`analyze_init_status`.
    """
    libghdl.libghdl__analyze_init()


@export
# @BindToLibGHDL("libghdl__analyze_init_status")
def analyze_init_status() -> int:
    """\
    Initialize the analyzer.

    :return: Returns 0 in case of success.
    """
    return libghdl.libghdl__analyze_init_status()


@export
# @BindToLibGHDL("libghdl__analyze_file")
def analyze_file(fname: str) -> Iir:
    """\
    Analyze a given filename :obj:`fname`.

    :param fname: File name
    :return:      Internal Intermediate Representation (IIR)
    """
    fname = fname.encode(ENCODING)
    return libghdl.libghdl__analyze_file(c_char_p(fname), len(fname))


@export
# @BindToLibGHDL("ghdllocal__disp_config_prefixes")
def disp_config() -> None:
    """Display the configured prefixes for libghdl."""
    libghdl.ghdllocal__disp_config_prefixes()
