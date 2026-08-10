# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
"""
Python binding for the Ada package ``Vhdl.Elocations`` in *libghdl*.

The extended source locations of a node - the position of each keyword and delimiter - kept separately from the
node itself, so an ordinary node does not pay for them.
"""

from pyTooling.Decorators import export

from pyGHDL.libghdl import libghdl


@export
def Get_Start_Location(obj):
    """
    :param obj: The node to read the ``Start_Location`` location of.
    :returns:   The node's ``Start_Location`` location.
    """
    return libghdl.vhdl__elocations__get_start_location(obj)


@export
def Set_Start_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Start_Location`` location of.
    :param value: The location to write into the ``Start_Location`` field.
    """
    libghdl.vhdl__elocations__set_start_location(obj, value)


@export
def Get_Right_Paren_Location(obj):
    """
    :param obj: The node to read the ``Right_Paren_Location`` location of.
    :returns:   The node's ``Right_Paren_Location`` location.
    """
    return libghdl.vhdl__elocations__get_right_paren_location(obj)


@export
def Set_Right_Paren_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Right_Paren_Location`` location of.
    :param value: The location to write into the ``Right_Paren_Location`` field.
    """
    libghdl.vhdl__elocations__set_right_paren_location(obj, value)


@export
def Get_End_Location(obj):
    """
    :param obj: The node to read the ``End_Location`` location of.
    :returns:   The node's ``End_Location`` location.
    """
    return libghdl.vhdl__elocations__get_end_location(obj)


@export
def Set_End_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``End_Location`` location of.
    :param value: The location to write into the ``End_Location`` field.
    """
    libghdl.vhdl__elocations__set_end_location(obj, value)


@export
def Get_Is_Location(obj):
    """
    :param obj: The node to read the ``Is_Location`` location of.
    :returns:   The node's ``Is_Location`` location.
    """
    return libghdl.vhdl__elocations__get_is_location(obj)


@export
def Set_Is_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Is_Location`` location of.
    :param value: The location to write into the ``Is_Location`` field.
    """
    libghdl.vhdl__elocations__set_is_location(obj, value)


@export
def Get_Begin_Location(obj):
    """
    :param obj: The node to read the ``Begin_Location`` location of.
    :returns:   The node's ``Begin_Location`` location.
    """
    return libghdl.vhdl__elocations__get_begin_location(obj)


@export
def Set_Begin_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Begin_Location`` location of.
    :param value: The location to write into the ``Begin_Location`` field.
    """
    libghdl.vhdl__elocations__set_begin_location(obj, value)


@export
def Get_Then_Location(obj):
    """
    :param obj: The node to read the ``Then_Location`` location of.
    :returns:   The node's ``Then_Location`` location.
    """
    return libghdl.vhdl__elocations__get_then_location(obj)


@export
def Set_Then_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Then_Location`` location of.
    :param value: The location to write into the ``Then_Location`` field.
    """
    libghdl.vhdl__elocations__set_then_location(obj, value)


@export
def Get_Use_Location(obj):
    """
    :param obj: The node to read the ``Use_Location`` location of.
    :returns:   The node's ``Use_Location`` location.
    """
    return libghdl.vhdl__elocations__get_use_location(obj)


@export
def Set_Use_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Use_Location`` location of.
    :param value: The location to write into the ``Use_Location`` field.
    """
    libghdl.vhdl__elocations__set_use_location(obj, value)


@export
def Get_Loop_Location(obj):
    """
    :param obj: The node to read the ``Loop_Location`` location of.
    :returns:   The node's ``Loop_Location`` location.
    """
    return libghdl.vhdl__elocations__get_loop_location(obj)


@export
def Set_Loop_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Loop_Location`` location of.
    :param value: The location to write into the ``Loop_Location`` field.
    """
    libghdl.vhdl__elocations__set_loop_location(obj, value)


@export
def Get_Generate_Location(obj):
    """
    :param obj: The node to read the ``Generate_Location`` location of.
    :returns:   The node's ``Generate_Location`` location.
    """
    return libghdl.vhdl__elocations__get_generate_location(obj)


@export
def Set_Generate_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Generate_Location`` location of.
    :param value: The location to write into the ``Generate_Location`` field.
    """
    libghdl.vhdl__elocations__set_generate_location(obj, value)


@export
def Get_Generic_Location(obj):
    """
    :param obj: The node to read the ``Generic_Location`` location of.
    :returns:   The node's ``Generic_Location`` location.
    """
    return libghdl.vhdl__elocations__get_generic_location(obj)


@export
def Set_Generic_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Generic_Location`` location of.
    :param value: The location to write into the ``Generic_Location`` field.
    """
    libghdl.vhdl__elocations__set_generic_location(obj, value)


@export
def Get_Port_Location(obj):
    """
    :param obj: The node to read the ``Port_Location`` location of.
    :returns:   The node's ``Port_Location`` location.
    """
    return libghdl.vhdl__elocations__get_port_location(obj)


@export
def Set_Port_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Port_Location`` location of.
    :param value: The location to write into the ``Port_Location`` field.
    """
    libghdl.vhdl__elocations__set_port_location(obj, value)


@export
def Get_Generic_Map_Location(obj):
    """
    :param obj: The node to read the ``Generic_Map_Location`` location of.
    :returns:   The node's ``Generic_Map_Location`` location.
    """
    return libghdl.vhdl__elocations__get_generic_map_location(obj)


@export
def Set_Generic_Map_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Generic_Map_Location`` location of.
    :param value: The location to write into the ``Generic_Map_Location`` field.
    """
    libghdl.vhdl__elocations__set_generic_map_location(obj, value)


@export
def Get_Port_Map_Location(obj):
    """
    :param obj: The node to read the ``Port_Map_Location`` location of.
    :returns:   The node's ``Port_Map_Location`` location.
    """
    return libghdl.vhdl__elocations__get_port_map_location(obj)


@export
def Set_Port_Map_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Port_Map_Location`` location of.
    :param value: The location to write into the ``Port_Map_Location`` field.
    """
    libghdl.vhdl__elocations__set_port_map_location(obj, value)


@export
def Get_Arrow_Location(obj):
    """
    :param obj: The node to read the ``Arrow_Location`` location of.
    :returns:   The node's ``Arrow_Location`` location.
    """
    return libghdl.vhdl__elocations__get_arrow_location(obj)


@export
def Set_Arrow_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Arrow_Location`` location of.
    :param value: The location to write into the ``Arrow_Location`` field.
    """
    libghdl.vhdl__elocations__set_arrow_location(obj, value)


@export
def Get_Colon_Location(obj):
    """
    :param obj: The node to read the ``Colon_Location`` location of.
    :returns:   The node's ``Colon_Location`` location.
    """
    return libghdl.vhdl__elocations__get_colon_location(obj)


@export
def Set_Colon_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Colon_Location`` location of.
    :param value: The location to write into the ``Colon_Location`` field.
    """
    libghdl.vhdl__elocations__set_colon_location(obj, value)


@export
def Get_Assign_Location(obj):
    """
    :param obj: The node to read the ``Assign_Location`` location of.
    :returns:   The node's ``Assign_Location`` location.
    """
    return libghdl.vhdl__elocations__get_assign_location(obj)


@export
def Set_Assign_Location(obj, value) -> None:
    """
    :param obj:   The node to write the ``Assign_Location`` location of.
    :param value: The location to write into the ``Assign_Location`` field.
    """
    libghdl.vhdl__elocations__set_assign_location(obj, value)
