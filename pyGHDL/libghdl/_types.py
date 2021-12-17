# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
#  Authors:
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
from enum import IntEnum, unique
from pyTooling.Decorators import export
from ctypes import c_int32, c_uint32, c_int64, c_double, c_bool
from typing import TypeVar

__all__ = [
    "ErrorIndex",
    "MessageIdWarnings",
    "NameId",
    "SourceFileEntry",
    "Iir",
    "IirKind",
]


@export
@unique
class TriStateType(IntEnum):
    Unknown = 0
    TFalse = 1
    TTrue = 2


@export
@unique
class DirectionType(IntEnum):
    To = 0
    Downto = 1


# This is an Ada standard type, which can be 1 byte.
Boolean = TypeVar("Boolean", bound=c_bool)

Int32 = TypeVar("Int32", bound=c_int32)
Int64 = TypeVar("Int64", bound=c_int64)
Fp64 = TypeVar("Fp64", bound=c_double)

ErrorIndex = TypeVar("ErrorIndex", bound=c_int32)
MessageIdWarnings = TypeVar("MessageIdWarnings", bound=c_int32)
NameId = TypeVar("NameId", bound=c_int32)

String8Id = TypeVar("String8Id", bound=c_uint32)
FileChecksumId = TypeVar("FileChecksumId", bound=c_uint32)
TimeStampId = TypeVar("TimeStampId", bound=c_uint32)

SourceFileEntry = TypeVar("SourceFileEntry", bound=c_uint32)
SourcePtr = TypeVar("SourcePtr", bound=c_int32)
LocationType = TypeVar("LocationType", bound=c_uint32)

Iir = TypeVar("Iir", bound=c_int32)
IirKind = TypeVar("IirKind", bound=c_int32)

PSLNode = TypeVar("PSLNode", bound=c_int32)
PSLNFA = TypeVar("PSLNFA", bound=c_int32)

Iir_Design_File = TypeVar("Iir_Design_File", bound=c_int32)
Iir_Design_Unit = TypeVar("Iir_Design_Unit", bound=c_int32)
Iir_Library_Declaration = TypeVar("Iir_Library_Declaration", bound=c_int32)
Iir_Package_Declaration = TypeVar("Iir_Package_Declaration", bound=c_int32)
Iir_Enumeration_Type_Definition = TypeVar("Iir_Enumeration_Type_Definition", bound=c_int32)
