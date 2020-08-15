from libghdl import libghdl
from ctypes import c_int32

# Use .value
Std_Location = c_int32.in_dll(libghdl, "vhdl__std_package__std_location")

# Use .value
Standard_Package = c_int32.in_dll(libghdl, "vhdl__std_package__standard_package")

# Use .value
Character_Type_Definition = c_int32.in_dll(
    libghdl, "vhdl__std_package__character_type_definition"
)
