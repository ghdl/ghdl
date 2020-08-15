from libghdl import libghdl
from ctypes import c_bool

Parse_Design_File = libghdl.vhdl__parse__parse_design_file

Flag_Parse_Parenthesis = c_bool.in_dll(libghdl, "vhdl__parse__flag_parse_parenthesis")
