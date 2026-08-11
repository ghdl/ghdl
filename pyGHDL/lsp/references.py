# =============================================================================
#               ____ _   _ ____  _       _
#  _ __  _   _ / ___| | | |  _ \| |     | |___ _ __
# | '_ \| | | | |  _| |_| | | | | |     | / __| '_ \
# | |_) | |_| | |_| |  _  | |_| | |___ _| \__ \ |_) |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|___/ .__/
# |_|    |___/                                |_|
# =============================================================================
# Authors:
#   Tristan Gingold
#
# Package module:   Locating declarations and references in the IIR tree.
#
# License:
# ============================================================================
#  Copyright (C) 2020-2024 Tristan Gingold
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
#
# SPDX-License-Identifier: GPL-2.0-or-later
# ============================================================================
"""
Resolution of a source location to the declaration it refers to, for *go to definition*.
"""

import logging
import pyGHDL.libghdl.vhdl.nodes as nodes
import pyGHDL.libghdl.vhdl.nodes_meta as nodes_meta
import pyGHDL.libghdl.name_table as name_table
import pyGHDL.libghdl.utils as pyutils

log = logging.getLogger(__name__)


def find_def_chain(first, loc):
    """
    Search a chain of nodes for the one a location falls in.

    :param first: The first node of the chain.
    :param loc:   The location to look for.
    :returns:     The node covering that location, or ``None`` if no node of the chain does.
    """
    n1 = first
    while n1 != nodes.Null_Iir:
        res = find_def(n1, loc)
        if res is not None:
            return res
        n1 = nodes.Get_Chain(n1)
    return None


def find_def(n, loc):
    """
    Search a subtree for the name a location falls in.

    A name is matched by its own location and the length of its identifier, as a node records where it starts but
    not where it ends. An operator has no identifier to measure, so the kinds are grouped by the width of the
    symbol that wrote them - ``+`` is one character, ``**`` two, ``and`` three, ``nand`` four.

    The descent reads the fields from the node metadata rather than walking the node's children, which is much
    faster. A field holding a reference is not followed: the node it names is reached through the field that owns
    it, and following both would visit a node twice and could not terminate on a cycle.

    :param n:   The node to search. ``Null_Iir`` searches nothing.
    :param loc: The location to look for.
    :returns:   The name covering that location, or ``None`` if no name in this subtree does.
    """
    if n == nodes.Null_Iir:
        return None
    k = nodes.Get_Kind(n)
    if k in (
        nodes.Iir_Kind.Simple_Name,
        nodes.Iir_Kind.Character_Literal,
        nodes.Iir_Kind.Operator_Symbol,
        nodes.Iir_Kind.Selected_Name,
        nodes.Iir_Kind.Attribute_Name,
        nodes.Iir_Kind.Selected_Element,
    ):
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
    elif k in (
        nodes.Iir_Kind.Identity_Operator,
        nodes.Iir_Kind.Negation_Operator,
        nodes.Iir_Kind.Addition_Operator,
        nodes.Iir_Kind.Substraction_Operator,
        nodes.Iir_Kind.Multiplication_Operator,
        nodes.Iir_Kind.Division_Operator,
        nodes.Iir_Kind.Concatenation_Operator,
        nodes.Iir_Kind.Equality_Operator,
        nodes.Iir_Kind.Less_Than_Operator,
        nodes.Iir_Kind.Greater_Than_Operator,
    ):
        # One character operators
        n_loc = nodes.Get_Location(n)
        if loc == n_loc:
            return n
    elif k in (
        nodes.Iir_Kind.Or_Operator,
        nodes.Iir_Kind.Inequality_Operator,
        nodes.Iir_Kind.Less_Than_Or_Equal_Operator,
        nodes.Iir_Kind.Greater_Than_Or_Equal_Operator,
        nodes.Iir_Kind.Condition_Operator,
        nodes.Iir_Kind.Exponentiation_Operator,
    ):
        # Two characters operators
        n_loc = nodes.Get_Location(n)
        if n_loc <= loc <= n_loc + 1:
            return n
    elif k in (
        nodes.Iir_Kind.Absolute_Operator,
        nodes.Iir_Kind.Not_Operator,
        nodes.Iir_Kind.And_Operator,
        nodes.Iir_Kind.Nor_Operator,
        nodes.Iir_Kind.Xor_Operator,
        nodes.Iir_Kind.Sll_Operator,
        nodes.Iir_Kind.Sla_Operator,
        nodes.Iir_Kind.Srl_Operator,
        nodes.Iir_Kind.Sra_Operator,
        nodes.Iir_Kind.Rol_Operator,
        nodes.Iir_Kind.Ror_Operator,
        nodes.Iir_Kind.Modulus_Operator,
        nodes.Iir_Kind.Remainder_Operator,
    ):
        # Three characters operators
        n_loc = nodes.Get_Location(n)
        if n_loc <= loc <= n_loc + 2:
            return n
    elif k in (
        nodes.Iir_Kind.Nand_Operator,
        nodes.Iir_Kind.Xnor_Operator,
    ):
        # Four characters operators
        n_loc = nodes.Get_Location(n)
        if n_loc <= loc <= n_loc + 3:
            return n

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
            # Only sensitivity lists are interesting.
            if f == nodes_meta.fields.Sensitivity_List:
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


def find_node_by_loc(n, loc):
    """
    Find the name written at a location.

    :param n:   The tree to search, usually a design file.
    :param loc: The location to look for.
    :returns:   The name at that location, or ``None`` if there is no name there.
    """
    ref = find_def(n, loc)
    log.debug("for loc %u found node %s", loc, ref)
    return ref


def find_definition_by_loc(n, loc):
    """
    Find the declaration the name at a location refers to.

    This is the step from the name to what it denotes, which is what *go to definition* needs. A name resolves
    through the entity it was bound to during analysis; anything else - an operator, for one - resolves through the
    subprogram chosen to implement it.

    :param n:   The tree to search, usually a design file.
    :param loc: The location to look for.
    :returns:   The declaration, or ``None`` if there is no name at that location or it was never resolved, which
                is what an unanalyzed or erroneous unit leaves behind.
    """
    ref = find_node_by_loc(n, loc)
    if ref is None:
        return None
    k = nodes.Get_Kind(ref)
    # TODO: character literal, attribute name...
    if k in nodes.Iir_Kinds.Denoting_Name or k == nodes.Iir_Kind.Selected_Element:
        ent = nodes.Get_Named_Entity(ref)
    else:
        ent = nodes.Get_Implementation(ref)
    return None if ent == nodes.Null_Iir else ent
