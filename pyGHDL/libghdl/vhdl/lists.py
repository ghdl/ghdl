from libghdl import libghdl
from ctypes import c_int32, c_bool, POINTER, Structure

List_Type = c_int32


class Iterator(Structure):
    _fields_ = [("chunk", c_int32), ("chunk_idx", c_int32), ("remain", c_int32)]


Iterate = libghdl.vhdl__lists__iterate
Iterate.argstype = [List_Type]
Iterate.restype = Iterator

Is_Valid = libghdl.vhdl__lists__is_valid
Is_Valid.argstype = [POINTER(Iterator)]
Is_Valid.restype = c_bool

Next = libghdl.vhdl__lists__next
Next.argstype = [POINTER(Iterator)]
Next.restype = None

Get_Element = libghdl.vhdl__lists__get_element
Get_Element.argstype = [POINTER(Iterator)]
Get_Element.restype = c_int32

Get_Nbr_Elements = libghdl.vhdl__lists__get_nbr_elements
Get_Nbr_Elements.argtype = [List_Type]
Get_Nbr_Elements.restype = c_int32

Create_Iir_List = libghdl.vhdl__lists__create_list

Destroy_Iir_List = libghdl.vhdl__lists__destroy_list
