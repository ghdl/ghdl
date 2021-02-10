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

from ctypes import c_void_p

from pydecor import export

from pyGHDL.libghdl import libghdl
from pyGHDL.libghdl._types import NameId, SourceFileEntry

__all__ = [
    "EOT",
    "No_Source_File_Entry",
    "No_Location",
]

EOT = b"\x04"

No_Source_File_Entry = 0

No_Location = 0


@export
def Location_To_File(Location) -> SourceFileEntry:
    """
    Convert :obj:`Location` to a source file.

    :param Location: Location
    :return:         Source file. Return ``No_Source_File_Entry`` if location is incorrect.
    """
    return libghdl.files_map__location_to_file(Location)


@export
def Location_File_To_Pos(Location, File: SourceFileEntry) -> int:
    """
    Convert :obj:`Location` and :obj:`File` to a position (offset) into the source file.

    :param Location: Location
    :param File:     Source file
    :return:         Offset
    """
    return libghdl.files_map__location_file_to_pos(Location, File)


@export
def Location_File_To_Line(Location, File: SourceFileEntry) -> int:
    """
    Convert :obj:`Location` and :obj:`File` to a line number.

    :param Location: Location
    :param File:     Source file
    :return:         Line number
    """
    return libghdl.files_map__location_file_to_line(Location, File)


@export
def Location_File_Line_To_Offset(Location, File: SourceFileEntry, Line: int) -> int:
    """
    Get the offset in :obj:`Line` of :obj:`Location`.

    :param Location: Location
    :param File:     Source file
    :param Line:     Line number
    :return:         Offset
    """
    return libghdl.files_map__location_file_line_to_offset(Location, File, Line)


@export
def Location_File_Line_To_Col(Location, File: SourceFileEntry, Line: int) -> int:
    """
    Get logical column (with HT expanded) from :obj:`Location`, :obj:`File` and
    :obj:`Line`.

    :param Location: Location
    :param File:     Source file
    :param Line:     Line number
    :return:         logical column (horizontal tabs are expanded)
    """
    return libghdl.files_map__location_file_line_to_col(Location, File, Line)


@export
def File_To_Location(File: SourceFileEntry):
    """Convert a :obj:`File` into a location.

    :param File: Source file
    :return:     Location. Type: ``Location_Type``
    """
    return libghdl.files_map__file_to_location(File)


@export
def File_Pos_To_Location(File: SourceFileEntry, Pos: int):
    """
    Convert a :obj:`File` and an offset :obj:`Pos` in the file into a location.

    :param File: Source file
    :param Pos:  Offset in the file
    :return:     Location. Type: ``Location_Type``
    """
    return libghdl.files_map__file_pos_to_location(File, Pos)


@export
def File_Line_To_Position(File: SourceFileEntry, Line: int) -> int:
    """
    Convert a :obj:`File` and :obj:`Line` into a position.

    :param File: Source file
    :param Line: Line number
    :return:     Return ``Source_Ptr_Bad`` in case of error (:obj:`Line` out of bounds).
    """
    return libghdl.files_map__file_line_to_position(File, Line)


@export
def Get_File_Name(File: SourceFileEntry) -> NameId:
    """
    Return the name of the file.

    :param File: Source file to get the filename from.
    :return:     NameId for the filename.
    """
    return libghdl.files_map__get_file_name(File)


@export
def Get_Directory_Name(File: SourceFileEntry) -> NameId:
    """
    Return the directory of the file.

    :param File: Source file to get the directory name from.
    :return:     NameId for the directory.
    """
    return libghdl.files_map__get_directory_name(File)


@export
def Get_File_Buffer(File: SourceFileEntry) -> bytes:
    """
    Return a buffer (access to the contents of the file) for a file entry.

    :param File: Source file to get the buffer from.
    :return:     Type: ``File_Buffer_Ptr``
    """
    func = libghdl.files_map__get_file_buffer
    func.restype = c_void_p

    return func(File)


@export
def Get_File_Length(File: SourceFileEntry) -> int:
    """
    Get the position of the first EOT character.

    :param File: Source file
    :return:     Type: ``Source_Ptr``
    """
    return libghdl.files_map__get_file_length(File)


@export
def Set_File_Length(File: SourceFileEntry, Length: int) -> None:
    """
    Set the length of the file (which is less than the size of the file buffer).

    Set also append two EOT at the end of the file.

    :param File:   Source file
    :param Length: Length for the file. Type: ``Source_Ptr``
    """
    libghdl.files_map__set_file_length(File, Length)


@export
def Read_Source_File(Directory: NameId, Name: NameId) -> SourceFileEntry:
    """
    Return an entry for a filename.

    Load the filename if necessary.

    :param Directory: ``Null_Identifier`` for :obj:`DirectoryId` means current directory.
    :param Name:      File name
    :return:          Return ``No_Source_File_Entry``, if the file does not exist.
    """
    return libghdl.files_map__read_source_file(Directory, Name)


@export
def Reserve_Source_File(Directory: NameId, Name: NameId, Length) -> SourceFileEntry:
    """
    Reserve an entry, but do not read any file.

    The length should includes the two terminal EOT.

    :param Directory: Directory name
    :param Name:      File name
    :param Length:    Length to reserve. Type: ``Source_Ptr``
    :return:          SourceFile
    """
    return libghdl.files_map__reserve_source_file(Directory, Name, Length)


@export
def Discard_Source_File(File: SourceFileEntry) -> None:
    """
    Mark :obj:`File` as unavailable: clear the name and directory.

    .. hint:: This is needed before creating a new source file with the same name.

    :param File: Source file to discard.
    """
    libghdl.files_map__discard_source_file(File)


@export
def Free_Source_File(File: SourceFileEntry) -> None:
    """
    Free resources used by :obj:`File`, but keep the entry.

    .. note:: It could be recycled for files that could fit - not implemented.

    :param File: Source file to free.
    """
    libghdl.files_map__free_source_file(File)


@export
def Get_Last_Source_File_Entry() -> SourceFileEntry:
    """
    Returns the entry of the last known file.

    .. hint:: This allows creating a table of ``SourceFileEntry``.

    :return: Last SourceFileEntry. Type: ``SourceFileEntry``
    """
    return libghdl.files_map__get_last_source_file_entry()
