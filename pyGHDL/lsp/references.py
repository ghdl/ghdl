import logging
import pyGHDL.libghdl.vhdl.nodes as nodes
import pyGHDL.libghdl.vhdl.nodes_meta as nodes_meta
import pyGHDL.libghdl.name_table as name_table
import pyGHDL.libghdl.utils as pyutils

log = logging.getLogger(__name__)


def find_def_chain(first, loc):
    n1 = first
    while n1 != nodes.Null_Iir:
        res = find_def(n1, loc)
        if res is not None:
            return res
        n1 = nodes.Get_Chain(n1)
    return None


def find_def(n, loc):
    """Return the node at location :param loc:, or None if not under :param n:."""
    if n == nodes.Null_Iir:
        return None
    k = nodes.Get_Kind(n)
    if k in [
        nodes.Iir_Kind.Simple_Name,
        nodes.Iir_Kind.Character_Literal,
        nodes.Iir_Kind.Operator_Symbol,
        nodes.Iir_Kind.Selected_Name,
        nodes.Iir_Kind.Attribute_Name,
        nodes.Iir_Kind.Selected_Element,
    ]:
        n_loc = nodes.Get_Location(n)
        if loc >= n_loc:
            ident = nodes.Get_Identifier(n)
            id_len = name_table.Get_Name_Length(ident)
            if loc < n_loc + id_len:
                return n
        if k == nodes.Iir_Kind.Simple_Name:
            return None
    elif k == nodes.Iir_Kind.Design_File:
        return find_def_chain(nodes.Get_First_Design_Unit(n), loc)
    elif k == nodes.Iir_Kind.Design_Unit:
        # if loc > elocations.Get_End_Location(unit):
        #    return None
        res = find_def_chain(nodes.Get_Context_Items(n), loc)
        if res is not None:
            return res
        unit = nodes.Get_Library_Unit(n)
        return find_def(unit, loc)

    # This is *much* faster than using node_iter!
    for f in pyutils.fields_iter(n):
        typ = nodes_meta.get_field_type(f)
        if typ == nodes_meta.types.Iir:
            attr = nodes_meta.get_field_attribute(f)
            if attr == nodes_meta.Attr.ANone:
                res = find_def(nodes_meta.Get_Iir(n, f), loc)
                if res is not None:
                    return res
            elif attr == nodes_meta.Attr.Chain:
                res = find_def_chain(nodes_meta.Get_Iir(n, f), loc)
                if res is not None:
                    return res
            elif attr == nodes_meta.Attr.Maybe_Ref:
                if not nodes.Get_Is_Ref(n):
                    res = find_def(nodes_meta.Get_Iir(n, f), loc)
                    if res is not None:
                        return res
        elif typ == nodes_meta.types.Iir_List:
            attr = nodes_meta.get_field_attribute(f)
            if attr == nodes_meta.Attr.ANone:
                for n1 in pyutils.list_iter(nodes_meta.Get_Iir_List(n, f)):
                    res = find_def(n1, loc)
                    if res is not None:
                        return res
        elif typ == nodes_meta.types.Iir_Flist:
            attr = nodes_meta.get_field_attribute(f)
            if attr == nodes_meta.Attr.ANone or (attr == nodes_meta.Attr.Of_Maybe_Ref and not nodes.Get_Is_Ref(n)):
                for n1 in pyutils.flist_iter(nodes_meta.Get_Iir_Flist(n, f)):
                    res = find_def(n1, loc)
                    if res is not None:
                        return res

    return None


def goto_definition(n, loc):
    """Return the declaration (as a node) under :param loc: or None."""
    ref = find_def(n, loc)
    log.debug("for loc %u found node %s", loc, ref)
    if ref is None:
        return None
    log.debug(
        "for loc %u id=%s",
        loc,
        name_table.Get_Name_Ptr(nodes.Get_Identifier(ref)),
    )
    ent = nodes.Get_Named_Entity(ref)
    return None if ent == nodes.Null_Iir else ent
