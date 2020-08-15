from libghdl import libghdl
from ctypes import c_int32

Get_Libraries_Chain = libghdl.libraries__get_libraries_chain

Add_Design_Unit_Into_Library = libghdl.libraries__add_design_unit_into_library

# Use .value
Library_Location = c_int32.in_dll(libghdl, "libraries__library_location")

# Use .value
Work_Library = c_int32.in_dll(libghdl, "libraries__work_library")

Purge_Design_File = libghdl.libraries__purge_design_file

Find_Entity_For_Component = libghdl.libraries__find_entity_for_component

Get_Library_No_Create = libghdl.libraries__get_library_no_create

Find_Primary_Unit = libghdl.libraries__find_primary_unit
