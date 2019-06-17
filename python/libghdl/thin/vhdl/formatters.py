from libghdl import libghdl
from ctypes import c_int32, c_char_p

Indent_String = libghdl.vhdl__formatters__indent_string

Allocate_Handle = libghdl.vhdl__formatters__allocate_handle

Get_Length = libghdl.vhdl__formatters__get_length
Get_Length.restype = c_int32

Get_C_String = libghdl.vhdl__formatters__get_c_string
Get_C_String.restype = c_char_p

Free_Handle = libghdl.vhdl__formatters__free_handle
