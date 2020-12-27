from libghdl import libghdl
from ctypes import c_char_p

Get_Name_Length = libghdl.name_table__get_name_length

Get_Name_Ptr = libghdl.name_table__get_name_ptr
Get_Name_Ptr.restype = c_char_p

_Get_Identifier_With_Len = libghdl.name_table__get_identifier_with_len


def Get_Identifier(s):
    return _Get_Identifier_With_Len(c_char_p(s), len(s))


Null_Identifier = 0
