#!/usr/bin/env python
import libghdl
import libghdl.thin.name_table as name_table
import libghdl.thin.files_map as files_map
import libghdl.thin.vhdl.nodes as nodes
import libghdl.thin.vhdl.sem_lib as sem_lib

def init():
    """Initialization: set options and then load libaries"""
    libghdl.set_option(b'--std=08')
    libghdl.analyze_init()

def get_identifier_ptr(n):
    """Return the python string from node :param n: identifier"""
    return name_table.Get_Name_Ptr(nodes.Get_Identifier(n)).decode('utf-8')

def list_units(filename):
    # Load the file
    file_id = name_table.Get_Identifier(filename.encode('utf_8'))
    sfe = files_map.Read_Source_File(name_table.Null_Identifier, file_id)
    if sfe == files_map.No_Source_File_Entry:
        print("cannot open file '{}'".format(filename))
        return

    # Parse and analyze
    file = sem_lib.Load_File(sfe)

    # Display all design units
    unit = nodes.Get_First_Design_Unit(file)
    while unit != nodes.Null_Iir:
        lib_unit = nodes.Get_Library_Unit(unit)
        if nodes.Get_Kind(lib_unit) == nodes.Iir_Kind.Entity_Declaration:
            print('entity {}'.format(get_identifier_ptr(lib_unit)))
        elif nodes.Get_Kind(lib_unit) == nodes.Iir_Kind.Architecture_Body:
            print('architecture {}'.format(get_identifier_ptr(lib_unit)))
        else:
            print('unknown unit!')
        unit = nodes.Get_Chain(unit)


def main():
    init()
    list_units('demo.vhdl')

if __name__ == '__main__':
    main()
