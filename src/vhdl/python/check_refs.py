#! /usr/bin/env python

"""Example of Python interface.
   Check that casing of reference is the same as the definition.
"""
import sys
import os.path

import libghdl
import libghdl.thin as thin
import libghdl.iirs as iirs

optind = 0
for i in range(1, len(sys.argv)):
    arg = sys.argv[i]
    if arg[0] != '-':
        optind = i
        break
    if thin.set_option(arg) != 0:
        print 'unknown option {0}'.format(arg)
        sys.exit(2)

if optind == 0:
    print 'no input file'
    sys.exit(2)

thin.analyze_init()

for filename in sys.argv[optind:]:
    basename = os.path.basename(filename)
    file_node = thin.analyze_file(filename)
    if False:
        print file_node
        print thin.kind_image(iirs.Get_Kind(file_node)) + ':'
        for f in thin.fields_iter(file_node):
            print '  ' + thin.fields_image(f)
        for u in thin.chain_iter(iirs.Get_First_Design_Unit(file_node)):
            print thin.kind_image(iirs.Get_Kind(iirs.Get_Library_Unit(u)))
        print 'Dump:'
    standard_sf = thin.Location_To_File(
        iirs.Get_Location(thin.Standard_Package))
    sf = thin.Location_To_File(iirs.Get_Location(file_node))
    file_buf = thin.Get_File_Buffer(sf)
#    print 'std: {0}, src: {1}'.format(standard_sf, sf)
#    libghdl.libghdl.disp_tree__disp_tree(file_node, 0)
#    libghdl.libghdl.files_map__debug_source_files()
    for n in thin.nodes_iter(file_node):
        k = iirs.Get_Kind(n)
        # print thin.kind_image(k)
        if k == iirs.Iir_Kind.Simple_Name:
            loc = iirs.Get_Location(n)
            id = iirs.Get_Identifier(n)
            id_len = thin.Get_Name_Length(id)
            off = thin.Location_File_To_Pos(loc, sf)
            str = file_buf[off:off + id_len]
#                print 'id: at {0}({1}+{2}): {3}'.format(loc, sf, off, str)
            dfn = iirs.Get_Named_Entity(n)
            def_loc = iirs.Get_Location(dfn)
            def_file = thin.Location_To_File(def_loc)
            if def_file == standard_sf:
                # There is no corresponding source file for std.standard
                continue
            def_buf = thin.Get_File_Buffer(def_file)
            def_off = thin.Location_File_To_Pos(def_loc, def_file)
            def_str = def_buf[def_off:def_off + id_len]
            if str != def_str:
                if False:
                    print ' ref: off {0} in {1}, loc: {2} (node {3})'.format(
                        off, sf, loc, n)
                    print ' def: off {0} in {1} ({2})'.format(
                        def_off, def_file, def_str)
                line = thin.Location_File_To_Line(loc, sf)
                col = thin.location_File_Line_To_Col(loc, sf, line)
                print '{0}:{1}:{2}: Bad casing: "{3}" instead of "{4}"'.format(
                    basename, line, col, str, def_str)
