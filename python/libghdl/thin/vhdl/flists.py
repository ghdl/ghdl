from libghdl import libghdl
from ctypes import c_int32

Flist_Type = c_int32

Ffirst = 0
Flast = libghdl.vhdl__flists__flast

Length = libghdl.vhdl__flists__length

Get_Nth_Element = libghdl.vhdl__flists__get_nth_element
