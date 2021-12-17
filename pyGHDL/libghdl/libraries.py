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

from ctypes import c_int32

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import (
    NameId,
    Iir_Library_Declaration,
    Iir_Design_Unit,
    Iir_Design_File,
    LocationType,
)
from pyGHDL.libghdl._decorator import BindToLibGHDL

__all__ = ["Library_Location", "Work_Library"]

Library_Location: LocationType = c_int32.in_dll(libghdl, "libraries__library_location")
"""
A location for library declarations (such as library WORK). Use ``.value`` to
access this variable inside libghdl.
"""

Work_Library: Iir_Library_Declaration = c_int32.in_dll(libghdl, "libraries__work_library")
"""
Library declaration for the work library. Note: the identifier of the work_library
is ``work_library_name``, which may be different from 'WORK'. Use ``.value`` to
access this variable inside libghdl.
"""


@export
@BindToLibGHDL("libraries__get_libraries_chain")
def Get_Libraries_Chain() -> Iir_Library_Declaration:
    """
    Get the chain of libraries. Can be used only to read (it mustn't be modified).

    :return: undocumented
    """
    return 0


@export
@BindToLibGHDL("libraries__add_design_unit_into_library")
def Add_Design_Unit_Into_Library(Unit: Iir_Design_Unit, Keep_Obsolete: bool) -> None:
    """
    Add or replace an design unit in the work library. DECL must not have a chain
    (because it may be modified).

    If the design_file of UNIT is not already in the library, a new one is created.

    Units are always appended to the design_file. Therefore, the order is kept.

    :param Unit:          undocumented
    :param Keep_Obsolete: If :obj:`Keep_Obsolete` is True, obsoleted units are
                          kept in the library.

                          This is used when a whole design file has to be added
                          in the library and then processed (without that feature,
                          redefined units would disappear).
    """


@export
@BindToLibGHDL("libraries__purge_design_file")
def Purge_Design_File(Design_File: Iir_Design_File) -> None:
    """
    Remove the same file as :obj:`Design_File` from work library and all of its units.

    :param Design_File: undocumented
    """


@export
@BindToLibGHDL("libraries__find_entity_for_component")
def Find_Entity_For_Component(Name: NameId) -> Iir_Design_Unit:
    """
    Find an entity whose name is :obj:`Name` in any library. |br|
    If there is no such entity, return :attr:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`. |br|
    If there are several entities, return :attr:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`;

    :param Name: Entity name to search for.
    :return:     undocumented
    """
    return 0


@export
@BindToLibGHDL("libraries__get_library_no_create")
def Get_Library_No_Create(Ident: NameId) -> Iir_Library_Declaration:
    """
    Get the library named :obj:`Ident`.

    :param Ident: Library to look for.
    :return:      Return :attr:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir` if it doesn't exist.
    """
    return 0


@export
@BindToLibGHDL("libraries__find_primary_unit")
def Find_Primary_Unit(Library: Iir_Library_Declaration, Name: NameId) -> Iir_Design_Unit:
    """
    Just return the design_unit for :obj:`Name`, or ``NULL`` if not found.

    :param Library: Library to look in.
    :param Name:    Primary unit to search for.
    :return:        undocumented
    """
    return 0
