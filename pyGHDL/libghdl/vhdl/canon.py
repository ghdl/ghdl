from libghdl import libghdl
from ctypes import c_bool

Flag_Concurrent_Stmts = c_bool.in_dll(
    libghdl, "vhdl__canon__canon_flag_concurrent_stmts"
)

Flag_Configurations = c_bool.in_dll(libghdl, "vhdl__canon__canon_flag_configurations")

Flag_Associations = c_bool.in_dll(libghdl, "vhdl__canon__canon_flag_associations")

# Extract_Sequential_Statement_Chain_Sensitivity = (
#    libghdl.vhdl__canon__canon_extract_sequential_statement_chain_sensitivity
# )
