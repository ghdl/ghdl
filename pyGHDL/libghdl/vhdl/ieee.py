from libghdl import libghdl
from ctypes import c_int

Std_Logic_1164_Pkg = c_int.in_dll(
    libghdl, "vhdl__ieee__std_logic_1164__std_logic_1164_pkg"
)

# Get value
Std_Logic_Type = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__std_logic_type")

# Get value
Std_Logic_Vector_Type = c_int.in_dll(
    libghdl, "vhdl__ieee__std_logic_1164__std_logic_vector_type"
)

# Get value
# Rising_Edge = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__rising_edge")

# Get value
# Falling_Edge = c_int.in_dll(libghdl, "vhdl__ieee__std_logic_1164__falling_edge")
