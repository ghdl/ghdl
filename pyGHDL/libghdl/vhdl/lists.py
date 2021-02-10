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

from ctypes import c_int32, c_bool, POINTER, Structure

from pydecor import export

from pyGHDL.libghdl import libghdl


@export
class Iterator(Structure):
    _fields_ = [("chunk", c_int32), ("chunk_idx", c_int32), ("remain", c_int32)]


@export
def Iterate(List) -> Iterator:
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
    func = libghdl.vhdl__lists__iterate
    func.argstype = [c_int32]
    func.restype = Iterator

    return func(List)


@export
def Is_Valid(it: Iterator) -> bool:
    """
    Check if iterator reached the end.

    :param Iterator: Iterator to check.
    :return:         False, if iterator has reached the end.
    """
    func = libghdl.vhdl__lists__is_valid
    func.argstype = [POINTER(Iterator)]
    func.restype = c_bool

    return func(it)


@export
def Next(it: Iterator):
    """
    Move iterator to the next element.

    :param Iterator: Iterator to increment.
    :return:         False, if iterator has reached the end.
    """
    func = libghdl.vhdl__lists__next
    func.argstype = [POINTER(Iterator)]
    func.restype = None

    func(it)


@export
def Get_Element(it: Iterator) -> int:
    """
    Get the current element from iterator.

    :param Iterator: Iterator the get the element from.
    :return:         The current element the iterator points to. Type: ``El_Type``
    """
    func = libghdl.vhdl__lists__get_element
    func.argstype = [POINTER(Iterator)]
    func.restype = c_int32

    return func(it)


@export
def Get_Nbr_Elements(List) -> int:
    """
    Return the number of elements in the list.

    .. hint:: This is also 1 + the position of the last element.

    :param List: The list to use.
    :return:     Number of list elements.
    """
    func = libghdl.vhdl__lists__get_nbr_elements
    func.argtype = [c_int32]
    func.restype = c_int32

    return func(List)


@export
def Create_Iir_List() -> int:
    """
    Create a list.

    :return: Type: ``List_Type``
    """
    return libghdl.vhdl__lists__create_list()


@export
def Destroy_Iir_List(List) -> None:
    """
    Destroy a list.

    :param List: List to destroy.
    """
    libghdl.vhdl__lists__destroy_list(List)
