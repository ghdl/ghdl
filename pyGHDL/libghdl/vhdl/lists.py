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

from ctypes import c_int32, c_bool, POINTER, Structure

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._decorator import BindToLibGHDL


@export
class Iterator(Structure):
    _fields_ = [("chunk", c_int32), ("chunk_idx", c_int32), ("remain", c_int32)]


@export
@BindToLibGHDL("vhdl__lists__iterate")
def Iterate(List: int) -> Iterator:
    """
    Create an iterator for a given list.

    The idiomatic way to iterate is:

    .. code-block:: Python

       It = Iterate(List)
       while Is_Valid(It):
         El = Get_Element(It)
           # ...
         Next(It)

    :param List: List to create an iterator from.
    :return:     Iterator structure.
    """


@export
# @BindToLibGHDL("vhdl__lists__is_valid")
def Is_Valid(it: Iterator) -> bool:
    """
    Check if iterator reached the end.

    :param it: Iterator to check.
    :return:   ``False``, if iterator has reached the end.
    """
    func = libghdl.vhdl__lists__is_valid
    func.argstype = [POINTER(Iterator)]
    func.restype = c_bool

    return func(it)


@export
# @BindToLibGHDL("vhdl__lists__next")
def Next(it: Iterator) -> bool:
    """
    Move iterator to the next element.

    :param it: Iterator to increment.
    :return:   ``False``, if iterator has reached the end.
    """
    func = libghdl.vhdl__lists__next
    func.argstype = [POINTER(Iterator)]
    func.restype = None

    return func(it)


@export
# @BindToLibGHDL("vhdl__lists__get_element")
def Get_Element(it: Iterator) -> int:
    """
    Get the current element from iterator.

    :param it: Iterator the get the element from.
    :return:   The current element the iterator points to. Type: ``El_Type``
    """
    func = libghdl.vhdl__lists__get_element
    func.argstype = [POINTER(Iterator)]
    func.restype = c_int32

    return func(it)


@export
@BindToLibGHDL("vhdl__lists__get_nbr_elements")
def Get_Nbr_Elements(List: int) -> int:
    """
    Return the number of elements in the list.

    .. hint:: This is also 1 + the position of the last element.

    :param List: The list to use.
    :return:     Number of list elements.
    """
    return 0


@export
@BindToLibGHDL("vhdl__lists__create_list")
def Create_Iir_List() -> int:
    """
    Create a list.

    :return: undocumented; Type: ``List_Type``
    """
    return 0


@export
@BindToLibGHDL("vhdl__lists__destroy_list")
def Destroy_Iir_List(List: int) -> None:
    """
    Destroy a list.

    :param List: List to destroy.
    """
