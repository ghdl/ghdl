#!/usr/bin/env python

"""Like pnodes but output for python"""

from __future__ import print_function
import sys
sys.path.append("../xtools")

import pnodes
import re

libname = 'libghdl'


def print_enum(name, vals):
    print()
    print()
    print('class {0}:'.format(name))
    for n, k in enumerate(vals):
        if k == 'None':
            k = 'PNone'
        print('    {0} = {1}'.format(k, n))


def do_class_kinds():
    print_enum(pnodes.prefix_name.rstrip('_'), pnodes.kinds)

    print()
    print()
    print('class Iir_Kinds:')
    for k, v in pnodes.kinds_ranges.items():
        print('    {0} = ['.format(k))
        first = True
        for e in v:
            if first:
                first = False
            else:
                print(',')
            print('        Iir_Kind.{}'.format(e), end='')
        print(']')
        print()

def do_iirs_subprg():
    classname = pnodes.node_type.lower() + 's'
    print()
    print('Get_Kind = {0}.{1}__get_kind'.format(libname, classname))
    print('Get_Location = {0}.nodes__get_location'.format(libname, classname))
    for k in pnodes.funcs:
        print()
        print('Get_{0} = {1}.{2}__get_{3}'.format(
            k.name, libname, classname, k.name.lower()))
        print()
        print('Set_{0} = {1}.{2}__set_{3}'.format(
            k.name, libname, classname, k.name.lower(), k.pname, k.rname))


def do_libghdl_elocations():
    classname = 'elocations'
    print('from libghdl import libghdl')
    print()
    for k in pnodes.funcs:
        print()
        print('Get_{0} = {1}.{2}__get_{3}'.format(
            k.name, libname, classname, k.name.lower()))
        print()
        print('Set_{0} = {1}.{2}__set_{3}'.format(
            k.name, libname, classname, k.name.lower(), k.pname, k.rname))


def do_class_types():
    print_enum('types', pnodes.get_types())


def do_types_subprg():
    print()
    for k in pnodes.get_types():
        print()
        print('Get_{0} = {1}.nodes_meta__get_{2}'.format(
            k, libname, k.lower()))


def do_has_subprg():
    print()
    for f in pnodes.funcs:
        print()
        print('Has_{0} =\\'.format(f.name))
        print('    {0}.nodes_meta__has_{1}'.format(libname, f.name.lower()))


def do_class_field_attributes():
    print_enum('Attr', ['ANone' if a == 'None' else a
                        for a in pnodes.get_attributes()])


def do_class_fields():
    print_enum('fields', [f.name for f in pnodes.funcs])


def read_spec_enum(type_name, prefix, class_name):
    """Read an enumeration declaration from iirs.ads"""
    pat_decl = re.compile(r'   type {0} is$'.format(type_name))
    pat_enum = re.compile(r'      {0}(\w+),?(-- .*)?$'.format(prefix))
    lr = pnodes.linereader(pnodes.kind_file)
    while not pat_decl.match(lr.get()):
        pass
    toks = []
    while True:
        line = lr.get()
        if line == '     );\n':
            break
        m = pat_enum.match(line)
        if m:
            toks.append(m.group(1))
    print_enum(class_name, toks)


def do_libghdl_iirs():
    print('from libghdl import libghdl')
    do_class_kinds()
    read_spec_enum('Iir_Mode', 'Iir_', 'Iir_Mode')
    read_spec_enum('Iir_Staticness', '', 'Iir_Staticness')
    read_spec_enum('Iir_Constraint', '', 'Iir_Constraint')
    read_spec_enum('Iir_Direction', 'Iir_', 'Iir_Direction')
    read_spec_enum('Iir_Delay_Mechanism', 'Iir_', 'Iir_Delay_Mechanism')
    read_spec_enum('Date_State_Type', 'Date_', 'Date_State')
    read_spec_enum('Iir_Predefined_Functions',
                   'Iir_Predefined_', 'Iir_Predefined')
    do_iirs_subprg()


def do_libghdl_meta():
    print('from libghdl import libghdl')
    print("""


# From nodes_meta
get_fields_first = libghdl.nodes_meta__get_fields_first

get_fields_last = libghdl.nodes_meta__get_fields_last

get_field_by_index = libghdl.nodes_meta__get_field_by_index

get_field_type = libghdl.nodes_meta__get_field_type

get_field_attribute = libghdl.nodes_meta__get_field_attribute""")
    do_class_types()
    do_class_field_attributes()
    do_class_fields()
    do_types_subprg()
    do_has_subprg()


def do_libghdl_names():
    pat_name_first = re.compile(
        r'   Name_(\w+)\s+: constant Name_Id := (\d+);')
    pat_name_def = re.compile(
        r'   Name_(\w+)\s+:\s+constant Name_Id :=\s+Name_(\w+)( \+ (\d+))?;')
    dict = {}
    lr = pnodes.linereader('../std_names.ads')
    while True:
        line = lr.get()
        m = pat_name_first.match(line)
        if m:
            name_def = m.group(1)
            val = int(m.group(2))
            dict[name_def] = val
            res = [(name_def, val)]
            break
    val_max = 1
    while True:
        line = lr.get()
        if line == 'end Std_Names;\n':
            break
        if line.endswith(':=\n'):
            line = line.rstrip() + lr.get()
        m = pat_name_def.match(line)
        if m:
            name_def = m.group(1)
            name_ref = m.group(2)
            val = m.group(3)
            if not val:
                val = 0
            val_ref = dict.get(name_ref, None)
            if not val_ref:
                raise pnodes.ParseError(
                    lr, "name {0} not found".format(name_ref))
            val = val_ref + int(val)
            val_max = max(val_max, val)
            dict[name_def] = val
            res.append((name_def, val))
    print('class Name:')
    for n, v in res:
        # Avoid clash with Python names
        if n in ['False', 'True']:
            n = 'N' + n
        print('    {0} = {1}'.format(n, v))


def do_libghdl_tokens():
    pat_token = re.compile(r'       Tok_(\w+),?\s*(--.*)?$')
    lr = pnodes.linereader('tokens.ads')
    toks = []
    while True:
        line = lr.get()
        if line == '      );\n':
            break
        m = pat_token.match(line)
        if m:
            toks.append(m.group(1))
    print_enum("Tok", toks)


pnodes.actions.update({'class-kinds': do_class_kinds,
                       'libghdl-iirs': do_libghdl_iirs,
                       'libghdl-meta': do_libghdl_meta,
                       'libghdl-names': do_libghdl_names,
                       'libghdl-tokens': do_libghdl_tokens,
                       'libghdl-elocs': do_libghdl_elocations})


pnodes.main()
