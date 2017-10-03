#!/usr/bin/env python

"""Like pnodes but output for python"""

import sys
sys.path.append("../xtools")

import pnodes

libname = 'libghdl'


def print_enum(name, vals):
    n = 0
    print
    print
    print 'class {0}:'.format(name)
    for k in vals:
        print '    {0} = {1}'.format(k, n)
        n += 1


def do_class_kinds():
    print_enum(pnodes.prefix_name.rstrip('_'), pnodes.kinds)


def do_iirs_subprg():
    classname = pnodes.node_type.lower() + 's'
    print
    print 'Get_Kind = {0}.{1}__get_kind'.format(libname, classname)
    print 'Get_Location = {0}.nodes__get_location'.format(libname, classname)
    for k in pnodes.funcs:
        print
        print 'Get_{0} = {1}.{2}__get_{3}'.format(
            k.name, libname, classname, k.name.lower())
        print
        print 'Set_{0} = {1}.{2}__set_{3}'.format(
            k.name, libname, classname, k.name.lower(), k.pname, k.rname)


def do_class_types():
    print_enum('types', pnodes.get_types())


def do_types_subprg():
    print
    for k in pnodes.get_types():
        print
        print 'Get_{0} = {1}.nodes_meta__get_{2}'.format(
            k, libname, k.lower())


def do_has_subprg():
    print
    for f in pnodes.funcs:
        print
        print 'Has_{0} =\\'.format(f.name)
        print '    {0}.nodes_meta__has_{1}'.format(libname, f.name.lower())


def do_class_field_attributes():
    print_enum('Attr', ['ANone' if a == 'None' else a
                        for a in pnodes.get_attributes()])


def do_class_fields():
    print_enum('fields', [f.name for f in pnodes.funcs])


def do_libghdl_iirs():
    print 'from libghdl import libghdl'
    do_class_kinds()
    do_iirs_subprg()


def do_libghdl_meta():
    print 'from libghdl import libghdl'
    print """

# From nodes_meta
get_fields_first = libghdl.nodes_meta__get_fields_first

get_fields_last = libghdl.nodes_meta__get_fields_last

get_field_by_index = libghdl.nodes_meta__get_field_by_index

get_field_type = libghdl.nodes_meta__get_field_type

get_field_attribute = libghdl.nodes_meta__get_field_attribute"""
    do_class_types()
    do_class_field_attributes()
    do_class_fields()
    do_types_subprg()
    do_has_subprg()


pnodes.actions.update({'class-kinds': do_class_kinds,
                       'libghdl-iirs': do_libghdl_iirs,
                       'libghdl-meta': do_libghdl_meta})


pnodes.main()
