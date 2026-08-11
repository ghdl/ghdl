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
"""
Python binding for the Ada package ``Vhdl.Nodes_Utils`` in *libghdl*.

Helpers that answer questions about a node which are more than a field access, such as stripping a denoting name.
"""

from pyTooling.Decorators import export

from pyGHDL.libghdl._types import Iir
from pyGHDL.libghdl._decorator import BindToLibGHDL


@export
@BindToLibGHDL("vhdl__utils__strip_denoting_name")
def Strip_Denoting_Name(Name: Iir) -> Iir:
    """
    If ``Name`` is a simple or an expanded name, return the denoted declaration.
    Otherwise, return ``Name``.

    :param Name: Simple or an expanded name.
    :returns:    Denoted declaration.
    """
    return 0


@export
@BindToLibGHDL("vhdl__utils__get_entity")
def Get_Entity(Decl: Iir) -> Iir:
    """
    This is a wrapper around ``Get_Entity_Name`` to return the entity declaration
    of the entity name of ``Decl``, or :attr:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir` in case of error.

    :param Decl: The declaration to get the entity of.
    :returns:    Entity
    """
    return 0


@export
@BindToLibGHDL("vhdl__utils__is_second_subprogram_specification")
def Is_Second_Subprogram_Specification(Spec: Iir) -> bool:
    """
    Check if ``Spec`` is the subprogram specification of a subprogram body
    which was previously declared. In that case, the only use of ``Spec``
    is to match the body with its declaration.

    :param Spec: The subprogram specification to check.
    :returns:    ``True`` if subprogram specification and previously declared subprogram body match
    """
    return False


@export
@BindToLibGHDL("vhdl__utils__get_entity_from_entity_aspect")
def Get_Entity_From_Entity_Aspect(Aspect: Iir) -> Iir:
    """
    Extract the entity from ``Aspect``.

    If ``Aspect`` is a component declaration, return ``Aspect``. If it's
    open, return :attr:`~pyGHDL.libghdl.vhdl.nodes.Null_Iir`

    :param Aspect: The entity aspect to extract the entity from.
    :returns:      Entity
    """
    return 0


@export
@BindToLibGHDL("vhdl__utils__get_interface_of_formal")
def Get_Interface_Of_Formal(Formal: Iir) -> Iir:
    """
    Get the interface corresponding to the formal name ``Formal``. This is
    always an interface, even if the formal is a name.

    :param Formal: The formal.
    :returns:      The corresponding interface.
    """
    return 0
