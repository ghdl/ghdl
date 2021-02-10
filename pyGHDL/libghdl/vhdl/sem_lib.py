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
# Package module:   Python binding and low-level API for shared library 'libghdl'.
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

from pydecor import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import SourceFileEntry


@export
def Load_File(File: SourceFileEntry):
    """
    Start to analyse a file (i.e. load and parse it).

    :param File: File to analyse.
    :return:     Return :attr:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir` in case of parse error. Type: ``Iir_Design_File``
    """
    return libghdl.vhdl__sem_lib__load_file(File)


@export
def Finish_Compilation(Unit, Main: bool = False) -> None:
    """
    Analyze :obj:`Unit`.

    :param Unit: Design unit to analyze.
    :param Main: Is main unit.
    """
    libghdl.vhdl__sem_lib__finish_compilation(Unit, Main)


@export
def Free_Dependence_List(Design) -> None:
    """
    Free the dependence list of :obj:`Design`.

    :param Design: Design unit to free dependencies for.
    """
    libghdl.vhdl__sem_lib__free_dependence_list(Design)
