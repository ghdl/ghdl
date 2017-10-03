from libghdl import libghdl
from ctypes import (c_char_p, c_int32)
import iirs
import nodes_meta
from nodes_meta import (Attr, types)
# from libghdl_defs import (fields, Iir_Kind, types, Attr)

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

Get_Nbr_Elements = libghdl.lists__get_nbr_elements

Get_Nth_Element = libghdl.lists__get_nth_element

# Files

Location_To_File = libghdl.files_map__location_to_file

Location_File_To_Pos = libghdl.files_map__location_file_to_pos

Location_File_To_Line = libghdl.files_map__location_file_to_line

location_File_Line_To_Col = libghdl.files_map__location_file_line_to_col

Get_File_Buffer = libghdl.files_map__get_file_buffer
Get_File_Buffer.restype = c_char_p

# Names

Get_Name_Length = libghdl.name_table__get_name_length

# std.standard

Standard_Package = c_int32.in_dll(libghdl, "std_package__standard_package")


Null_Iir = 0
Null_Iir_List = 0


def _build_enum_image(cls):
    d = [e for e in dir(cls) if e[0] != '_']
    res = [None] * len(d)
    for e in d:
        res[getattr(cls, e)] = e
    return res


_fields_image = _build_enum_image(nodes_meta.fields)


def fields_image(idx):
    """String representation of field idx"""
    return _fields_image[idx]


_kind_image = _build_enum_image(iirs.Iir_Kind)


def kind_image(k):
    """String representation of Iir_Kind k"""
    return _kind_image[k]


_types_image = _build_enum_image(nodes_meta.types)


def types_image(t):
    """String representation of Nodes_Meta.Types t"""
    return _types_image[t]


_attr_image = _build_enum_image(nodes_meta.Attr)


def attr_image(a):
    """String representation of Nodes_Meta.Attr a"""
    return _attr_image[a]


def fields_iter(n):
    """Iterate on fields of node n"""
    if n == Null_Iir:
        return
    k = iirs.Get_Kind(n)
    first = nodes_meta.get_fields_first(k)
    last = nodes_meta.get_fields_last(k)
    for i in range(first, last + 1):
        yield nodes_meta.get_field_by_index(i)


def chain_iter(n):
    """Iterate of a chain headed by node n"""
    while n != Null_Iir:
        yield n
        n = iirs.Get_Chain(n)


def nodes_iter(n):
    """Iterate of all nodes of n, including n.
    Nodes are returned only once."""
    if n == Null_Iir:
        return
#    print 'nodes_iter for {0}'.format(n)
    yield n
    chain_next = None
    for f in fields_iter(n):
        typ = nodes_meta.get_field_type(f)
#        print ' {0}: field {1} (type: {2})'.format(
#            n, fields_image(f), types_image(typ))
        if typ == nodes_meta.types.Iir:
            attr = nodes_meta.get_field_attribute(f)
            if attr == Attr.ANone:
                for n1 in nodes_iter(nodes_meta.Get_Iir(n, f)):
                    yield n1
            elif attr == Attr.Chain:
                n2 = nodes_meta.Get_Iir(n, f)
                while n2 != Null_Iir:
                    for n1 in nodes_iter(n2):
                        yield n1
                    n2 = iirs.Get_Chain(n2)
            elif attr == Attr.Maybe_Ref:
                if not iirs.Get_Is_Ref(n, f):
                    for n1 in nodes_iter(nodes_meta.Get_Iir(n, f)):
                        yield n1
        elif typ == types.Iir_List:
            attr = nodes_meta.get_field_attribute(f)
            if attr == Attr.ANone:
                for n1 in list_iter(nodes_meta.Get_Iir_List(n, f)):
                    yield n1
    if chain_next:
        for n1 in nodes_iter(chain_next):
            yield n1


def list_iter(lst):
    """Iterate of all element of Iir_List lst."""
    if lst == Null_Iir_List:
        return
    for i in range(Get_Nbr_Elements(lst)):
        yield Get_Nth_Element(lst, i)
