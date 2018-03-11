from libghdl import libghdl
from ctypes import (c_char_p, c_int32, c_int, c_bool, sizeof, c_void_p,
                    POINTER, Structure)
import libghdl.iirs as iirs
import libghdl.nodes_meta as nodes_meta
from libghdl.nodes_meta import (Attr, types)
# from libghdl_defs import (fields, Iir_Kind, types, Attr)

assert sizeof(c_bool) == 1

# libghdl

_set_option = libghdl.libghdl__set_option
_analyze_file = libghdl.libghdl__analyze_file


def set_option(opt):
    return _set_option(c_char_p(opt), len(opt))


def analyze_init():
    return libghdl.libghdl__analyze_init()


def analyze_file(filename):
    return _analyze_file(c_char_p(filename), len(filename))


# Lists


class Lists:
    List_Type = c_int32

    class Iterator(Structure):
        _fields_ = [("chunk", c_int32),
                    ("chunk_idx", c_int32),
                    ("remain", c_int32)]

    Iterate = libghdl.lists__iterate
    Iterate.argstype = [List_Type]
    Iterate.restype = Iterator

    Is_Valid = libghdl.lists__is_valid
    Is_Valid.argstype = [POINTER(Iterator)]
    Is_Valid.restype = c_bool

    Next = libghdl.lists__next
    Next.argstype = [POINTER(Iterator)]
    Next.restype = None

    Get_Element = libghdl.lists__get_element
    Get_Element.argstype = [POINTER(Iterator)]
    Get_Element.restype = c_int32

    Get_Nbr_Elements = libghdl.lists__get_nbr_elements
    Get_Nbr_Elements.argtype = [List_Type]
    Get_Nbr_Elements.restype = c_int32

    Create_Iir_List = libghdl.lists__create_list

    Destroy_Iir_List = libghdl.lists__destroy_list


class Flists:
    Flist_Type = c_int32

    Ffirst = 0
    Flast = libghdl.flists__flast

    Length = libghdl.flists__length

    Get_Nth_Element = libghdl.flists__get_nth_element


# Files

Location_To_File = libghdl.files_map__location_to_file

Location_File_To_Pos = libghdl.files_map__location_file_to_pos

Location_File_To_Line = libghdl.files_map__location_file_to_line

Location_File_Line_To_Col = libghdl.files_map__location_file_line_to_col

Get_File_Name = libghdl.files_map__get_file_name

Get_File_Buffer = libghdl.files_map__get_file_buffer
Get_File_Buffer.restype = c_void_p

Get_File_Length = libghdl.files_map__get_file_length

Read_Source_File = libghdl.files_map__read_source_file

No_Source_File_Entry = 0

No_Location = 0

# Names

Get_Name_Length = libghdl.name_table__get_name_length

Get_Name_Ptr = libghdl.name_table__get_name_ptr
Get_Name_Ptr.restype = c_char_p

_Get_Identifier_With_Len = libghdl.name_table__get_identifier_with_len

def Get_Identifier(s):
    return _Get_Identifier_With_Len(c_char_p(s), len(s))

Null_Identifier = 0

# Ieee

class Ieee:
    Std_Logic_1164_Pkg = c_int.in_dll(
        libghdl, "ieee__std_logic_1164__std_logic_1164_pkg")

    # Get value
    Std_Logic_Type = c_int.in_dll(
        libghdl, "ieee__std_logic_1164__std_logic_type")

    # Get value
    Std_Logic_Vector_Type = c_int.in_dll(
        libghdl, "ieee__std_logic_1164__std_logic_vector_type")

    # Get value
    Rising_Edge = c_int.in_dll(libghdl, "ieee__std_logic_1164__rising_edge")

    # Get value
    Falling_Edge = c_int.in_dll(libghdl, "ieee__std_logic_1164__falling_edge")


# Flags
class Flags:
    Flag_Elocations = c_bool.in_dll(libghdl, "flags__flag_elocations")


# Scanner
class Scanner:
    Set_File = libghdl.scanner__set_file

    Close_File = libghdl.scanner__close_file

    Scan = libghdl.scanner__scan

    # This is a c_int, so you want to use its .value
    Current_Token = c_int.in_dll(libghdl, "scanner__current_token")

    Flag_Comment = c_bool.in_dll(libghdl, "scanner__flag_comment")

    Get_Current_Line = libghdl.scanner__get_current_line

    Get_Token_Column = libghdl.scanner__get_token_column

    Get_Token_Position = libghdl.scanner__get_token_position

    Get_Position = libghdl.scanner__get_position

    Current_Identifier = libghdl.scanner__current_identifier


class Parse:
    Parse_Design_File = libghdl.parse__parse_design_file

    Flag_Parse_Parenthesis = c_bool.in_dll(
        libghdl, "parse__flag_parse_parenthesis")


class Canon:
    Flag_Concurrent_Stmts = c_bool.in_dll(
        libghdl, "canon__canon_flag_concurrent_stmts")

    Flag_Configurations = c_bool.in_dll(
        libghdl, "canon__canon_flag_configurations")

    Flag_Associations = c_bool.in_dll(
        libghdl, "canon__canon_flag_associations")

    Extract_Sequential_Statement_Chain_Sensitivity = \
        libghdl.canon__canon_extract_sequential_statement_chain_sensitivity


# std.standard

# Use .value
Standard_Package = c_int32.in_dll(libghdl, "std_package__standard_package")

# Use .value
Character_Type_Definition = c_int32.in_dll(
    libghdl, "std_package__character_type_definition")

# libraries

Get_Libraries_Chain = libghdl.libraries__get_libraries_chain

Add_Design_Unit_Into_Library = libghdl.libraries__add_design_unit_into_library

Finish_Compilation = libghdl.libraries__finish_compilation

# Use .value
Library_Location = c_int32.in_dll(libghdl, "libraries__library_location")

# Use .value
Work_Library = c_int32.in_dll(libghdl, "libraries__work_library")

Purge_Design_File = libghdl.libraries__purge_design_file

# Disp_Tree

Disp_Iir = libghdl.disp_tree__disp_iir

# Iirs_Utils

class Iirs_Utils:
    Strip_Denoting_Name = libghdl.iirs_utils__strip_denoting_name

    Get_Entity = libghdl.iirs_utils__get_entity

    Is_Second_Subprogram_Specification = \
        libghdl.iirs_utils__is_second_subprogram_specification

    Get_Entity_From_Entity_Aspect = \
        libghdl.iirs_utils__get_entity_from_entity_aspect

    Get_Interface_Of_Formal = \
        libghdl.iirs_utils__get_interface_of_formal

Null_Iir = 0
Null_Iir_List = 0
Iir_List_All = 1

Null_Iir_Flist = 0
Iir_Flist_Others = 1
Iir_Flist_All = 2
