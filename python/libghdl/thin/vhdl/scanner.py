from libghdl import libghdl
from ctypes import c_int, c_bool

Set_File = libghdl.vhdl__scanner__set_file

Close_File = libghdl.vhdl__scanner__close_file

Scan = libghdl.vhdl__scanner__scan

# This is a c_int, so you want to use its .value
Current_Token = c_int.in_dll(libghdl, "vhdl__scanner__current_token")

Flag_Comment = c_bool.in_dll(libghdl, "vhdl__scanner__flag_comment")

Get_Current_Line = libghdl.vhdl__scanner__get_current_line

Get_Token_Offset = libghdl.vhdl__scanner__get_token_offset

Get_Token_Position = libghdl.vhdl__scanner__get_token_position

Get_Position = libghdl.vhdl__scanner__get_position

Current_Identifier = libghdl.vhdl__scanner__current_identifier
