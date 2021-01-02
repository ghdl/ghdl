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
from ctypes import c_int8, c_int32, c_char_p, Structure

from pydecor import export

from pyGHDL.libghdl import libghdl


class Error_Message(Structure):
    _fields_ = [
        ("id", c_int8),
        ("group", c_int8),
        ("file", c_int32),
        ("line", c_int32),
        ("offset", c_int32),
        ("length", c_int32),
    ]


# Values for group:
Msg_Single = 0
Msg_Main = 1
Msg_Related = 2
Msg_Last = 3

Install_Handler = libghdl.errorout__memory__install_handler

Get_Nbr_Messages = libghdl.errorout__memory__get_nbr_messages

Get_Error_Record = libghdl.errorout__memory__get_error_record
Get_Error_Record.argstypes = [c_int32]
Get_Error_Record.restype = Error_Message

Get_Error_Message = libghdl.errorout__memory__get_error_message_addr
Get_Error_Message.argstype = [c_int32]
Get_Error_Message.restype = c_char_p

Clear_Errors = libghdl.errorout__memory__clear_errors
