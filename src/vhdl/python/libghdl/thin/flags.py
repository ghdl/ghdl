from libghdl import libghdl
from ctypes import c_bool

Flag_Elocations = c_bool.in_dll(libghdl, "flags__flag_elocations")

Verbose = c_bool.in_dll(libghdl, "flags__verbose")

Flag_Elaborate_With_Outdated = c_bool.in_dll(
    libghdl, "flags__flag_elaborate_with_outdated")
