#!/usr/bin/env python
from sys import argv
from pathlib import Path

import libghdl
from libghdl.thin import name_table
from libghdl.thin import files_map
from libghdl.thin.vhdl import nodes
from libghdl.thin.vhdl import sem_lib
from libghdl.thin.vhdl import pyutils
from libghdl.thin import errorout_console


def get_identifier_ptr(n):
    """Return the python string from node :param n: identifier"""
    return name_table.Get_Name_Ptr(nodes.Get_Identifier(n)).decode("utf-8")


def get_port_mode(port) -> str:
    """Return the Mode of a port, as a string"""
    mode = nodes.Get_Mode(port)
    return (
        "in"
        if mode == nodes.Iir_Mode.In_Mode
        else "out"
        if mode == nodes.Iir_Mode.Out_Mode
        else "inout"
        if mode == nodes.Iir_Mode.Inout_Mode
        else "buffer"
        if mode == nodes.Iir_Mode.Buffer_Mode
        else "linkage"
        if mode == nodes.Iir_Mode.Linkage_Mode
        else "unknown"
    )


def get_port_type(port) -> str:
    "Return the Type of a port, as a string"
    subtype = nodes.Get_Subtype_Indication(port)
    skind = nodes.Get_Kind(subtype)

    if skind == nodes.Iir_Kind.Simple_Name:
        return get_identifier_ptr(subtype)

    if skind == nodes.Iir_Kind.Array_Subtype_Definition:
        mark = get_identifier_ptr(nodes.Get_Subtype_Type_Mark(subtype))

        for rng in pyutils.flist_iter(nodes.Get_Index_Constraint_List(subtype)):
            if nodes.Get_Kind(rng) == nodes.Iir_Kind.Range_Expression:
                return "%s(%d %s %d)" % (
                    mark,
                    nodes.Get_Value(nodes.Get_Left_Limit_Expr(rng)),
                    "downto" if nodes.Get_Direction(rng) else "to",
                    nodes.Get_Value(nodes.Get_Right_Limit_Expr(rng)),
                )
            return "UNSUPPORTED array_subtype_definition"

    return "UNSUPPORTED"


def list_units(filename):
    # Load the file
    file_id = name_table.Get_Identifier(filename.encode("utf_8"))
    sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
    if sfe == files_map.No_Source_File_Entry:
        print("cannot open file '%s'" % filename)
        return

    # Parse
    file = sem_lib.Load_File(sfe)

    # Display all design units
    unit = nodes.Get_First_Design_Unit(file)
    while unit != nodes.Null_Iir:
        lib_unit = nodes.Get_Library_Unit(unit)
        if nodes.Get_Kind(lib_unit) == nodes.Iir_Kind.Entity_Declaration:
            print("  - entity %s" % get_identifier_ptr(lib_unit))
            for port in pyutils.chain_iter(nodes.Get_Port_Chain(lib_unit)):
                print(
                    "    * %s %s %s"
                    % (
                        get_identifier_ptr(port),
                        get_port_mode(port),
                        get_port_type(port),
                    )
                )
        elif nodes.Get_Kind(lib_unit) == nodes.Iir_Kind.Architecture_Body:
            print(
                "  - architecture %s of %s"
                % (
                    get_identifier_ptr(lib_unit),
                    get_identifier_ptr(nodes.Get_Entity_Name(lib_unit)),
                )
            )
        else:
            print("unknown unit!")
        unit = nodes.Get_Chain(unit)


if __name__ == "__main__":
    # Initialization: set options and then load libaries
    errorout_console.Install_Handler()
    libghdl.set_option(b"--std=08")
    libghdl.analyze_init()

    # Recursively find and parse all the files with extension *.vhdl
    if len(argv) > 1:
        for file in Path(argv[1]).glob("**/*.vhdl"):
            print("· %s" % file)
            list_units(str(file))
