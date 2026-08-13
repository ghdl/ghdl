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
# Package module:   Translation of IIR nodes into Language Server Protocol symbols.
#
# License:
# ============================================================================
#  Copyright (C) 2020-2023 Tristan Gingold
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
Translation of IIR declarations into the document symbols an editor displays in its outline.
"""

import pyGHDL.libghdl.name_table as name_table
import pyGHDL.libghdl.files_map as files_map
import pyGHDL.libghdl.vhdl.nodes as nodes
import pyGHDL.libghdl.vhdl.nodes_meta as nodes_meta
import pyGHDL.libghdl.vhdl.elocations as elocations
import pyGHDL.libghdl.utils as pyutils

from . import lsp

SYMBOLS_MAP = {
    nodes.Iir_Kind.Package_Declaration: {
        "kind": lsp.SymbolKind.Package,
        "detail": "(declaration)",
    },
    nodes.Iir_Kind.Package_Body: {"kind": lsp.SymbolKind.Package, "detail": "(body)"},
    nodes.Iir_Kind.Entity_Declaration: {"kind": lsp.SymbolKind.Module},
    nodes.Iir_Kind.Architecture_Body: {"kind": lsp.SymbolKind.Module},
    nodes.Iir_Kind.Configuration_Declaration: {"kind": lsp.SymbolKind.Module},
    nodes.Iir_Kind.Package_Instantiation_Declaration: {"kind": lsp.SymbolKind.Module},
    nodes.Iir_Kind.Component_Declaration: {"kind": lsp.SymbolKind.Module},
    nodes.Iir_Kind.Context_Declaration: {"kind": lsp.SymbolKind.Module},
    nodes.Iir_Kind.Use_Clause: {"kind": None},
    nodes.Iir_Kind.Library_Clause: {"kind": None},
    nodes.Iir_Kind.Procedure_Declaration: {"kind": lsp.SymbolKind.Function},
    nodes.Iir_Kind.Function_Declaration: {"kind": lsp.SymbolKind.Function},
    nodes.Iir_Kind.Interface_Procedure_Declaration: {"kind": lsp.SymbolKind.Function},
    nodes.Iir_Kind.Interface_Function_Declaration: {"kind": lsp.SymbolKind.Function},
    nodes.Iir_Kind.Procedure_Body: {
        "kind": lsp.SymbolKind.Function,
        "detail": "(body)",
    },
    nodes.Iir_Kind.Function_Body: {"kind": lsp.SymbolKind.Function, "detail": "(body)"},
    nodes.Iir_Kind.Type_Declaration: {"kind": lsp.SymbolKind.Constructor},
    nodes.Iir_Kind.Subtype_Declaration: {"kind": lsp.SymbolKind.Constructor},
    nodes.Iir_Kind.Attribute_Declaration: {"kind": lsp.SymbolKind.Property},
    nodes.Iir_Kind.Attribute_Specification: {"kind": None},
    nodes.Iir_Kind.Disconnection_Specification: {"kind": None},
    nodes.Iir_Kind.Anonymous_Type_Declaration: {"kind": None},
    nodes.Iir_Kind.Variable_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Constant_Declaration: {"kind": lsp.SymbolKind.Constant},
    nodes.Iir_Kind.Signal_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Attribute_Implicit_Declaration: {"kind": None},
    nodes.Iir_Kind.Interface_Variable_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Interface_Constant_Declaration: {"kind": lsp.SymbolKind.Constant},
    nodes.Iir_Kind.Interface_Signal_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Interface_File_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.File_Declaration: {"kind": lsp.SymbolKind.File},
    nodes.Iir_Kind.Object_Alias_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Non_Object_Alias_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Protected_Type_Body: {"kind": lsp.SymbolKind.Class},
    nodes.Iir_Kind.Group_Template_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Group_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Concurrent_Simple_Signal_Assignment: {"kind": None},
    nodes.Iir_Kind.Concurrent_Conditional_Signal_Assignment: {"kind": None},
    nodes.Iir_Kind.Concurrent_Selected_Signal_Assignment: {"kind": None},
    nodes.Iir_Kind.Concurrent_Procedure_Call_Statement: {"kind": None},
    nodes.Iir_Kind.Concurrent_Assertion_Statement: {"kind": None},
    nodes.Iir_Kind.Component_Instantiation_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Block_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.If_Generate_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.For_Generate_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Case_Generate_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Sensitized_Process_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Process_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Simultaneous_Null_Statement: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Psl_Assert_Directive: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Psl_Assume_Directive: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Psl_Cover_Directive: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Psl_Restrict_Directive: {"kind": lsp.SymbolKind.Method},
    nodes.Iir_Kind.Psl_Endpoint_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Psl_Declaration: {"kind": lsp.SymbolKind.Variable},
    nodes.Iir_Kind.Configuration_Specification: {"kind": None},
}


def location_to_position(fe, loc):
    """
    Convert a *libghdl* location to a position in the protocol's coordinates.

    :param fe:              The source file the location is in.
    :param loc:             The location to convert.
    :returns:               A ``Position``, with its 0-based ``line`` and ``character``.
    :raises AssertionError: If the location is ``No_Location``, which does not name a place in a file.
    """
    assert loc != files_map.No_Location
    line = files_map.Location_File_To_Line(loc, fe)
    off = files_map.Location_File_Line_To_Offset(loc, fe, line)
    return {"line": line - 1, "character": off}


def get_symbols_chain(fe, n):
    """
    Collect the symbols of a chain of nodes.

    A node that is not a symbol - a use clause, or a declaration that is not worth showing - contributes nothing
    rather than an empty entry.

    :param fe: The source file the nodes are in.
    :param n:  The first node of the chain.
    :returns:  One ``DocumentSymbol`` per node that has one.
    """
    res = [get_symbols(fe, el) for el in pyutils.chain_iter(n)]
    return [e for e in res if e is not None]


def get_symbols(fe, n):
    """
    Build the symbol of one node, with the symbols declared inside it as its children.

    The range covers the whole construct where the parser recorded its extent, so folding an architecture in an
    editor folds all of it; elsewhere it covers the declared name alone. A construct whose end was never reached
    is given an empty range rather than an invalid one, which is what a file with a syntax error leaves behind.

    Two kinds of node are dropped: an implicit subprogram, which the user never wrote, and a subprogram
    declaration whose body is in the same file, which would otherwise be listed twice. An anonymous construct is
    kept only if it has children worth showing.

    :param fe:              The source file the node is in.
    :param n:               The node to describe.
    :returns:               A ``DocumentSymbol``, or ``None`` if this node is not shown.
    :raises AssertionError: If the kind of the node is not in :data:`SYMBOLS_MAP`, which means a construct was
                            added to the parser without deciding how it should appear in an outline.
    """
    if n == nodes.Null_Iir:
        return None
    k = nodes.Get_Kind(n)
    if k == nodes.Iir_Kind.Design_Unit:
        return get_symbols(fe, nodes.Get_Library_Unit(n))
    m = SYMBOLS_MAP.get(k, None)
    if m is None:
        raise AssertionError(f"get_symbol: unhandled {pyutils.kind_image(k)}")
    kind = m["kind"]
    if kind is None:
        return None
    if k in [nodes.Iir_Kind.Procedure_Declaration, nodes.Iir_Kind.Function_Declaration]:
        # Discard implicit declarations.
        if nodes.Get_Implicit_Definition(n) < nodes.Iir_Predefined.PNone:
            return None
        if nodes.Get_Has_Body(n):
            # Use the body instead.
            # FIXME: but get interface from the spec!
            return None
    res = {"kind": kind}
    detail = m.get("detail")
    if detail is not None:
        res["detail"] = detail
    # Get the name
    if k in [nodes.Iir_Kind.Function_Body, nodes.Iir_Kind.Procedure_Body]:
        nid = nodes.Get_Identifier(nodes.Get_Subprogram_Specification(n))
    else:
        nid = nodes.Get_Identifier(n)
    if nid == name_table.Null_Identifier:
        name = None
    else:
        name = pyutils.name_image(nid)
    # Get the range.  Use elocations when possible.
    if k in (
        nodes.Iir_Kind.Architecture_Body,
        nodes.Iir_Kind.Entity_Declaration,
        nodes.Iir_Kind.Package_Declaration,
        nodes.Iir_Kind.Package_Body,
        nodes.Iir_Kind.Component_Declaration,
        nodes.Iir_Kind.Process_Statement,
        nodes.Iir_Kind.Sensitized_Process_Statement,
        nodes.Iir_Kind.If_Generate_Statement,
        nodes.Iir_Kind.For_Generate_Statement,
        nodes.Iir_Kind.Block_Statement,
    ):
        start_loc = elocations.Get_Start_Location(n)
        end_loc = elocations.Get_End_Location(n)
        if end_loc == files_map.No_Location:
            # Can happen in case of parse error
            end_loc = start_loc
    else:
        start_loc = nodes.Get_Location(n)
        end_loc = start_loc + name_table.Get_Name_Length(nid)
    res["range"] = {
        "start": location_to_position(fe, start_loc),
        "end": location_to_position(fe, end_loc),
    }

    # Gather children.
    # FIXME: should we use a list of fields to inspect ?
    children = []
    # if nodes_meta.Has_Generic_Chain(k):
    #    children.extend(get_symbols_chain(fe, nodes.Get_Generic_Chain(n)))
    # if nodes_meta.Has_Port_Chain(k):
    #    children.extend(get_symbols_chain(fe, nodes.Get_Port_Chain(n)))
    # if nodes_meta.Has_Interface_Declaration_Chain(k):
    #    children.extend(get_symbols_chain(fe, nodes.Get_Interface_Declaration_Chain(n)))
    if k in (nodes.Iir_Kind.Package_Declaration, nodes.Iir_Kind.Package_Body):
        children.extend(get_symbols_chain(fe, nodes.Get_Declaration_Chain(n)))
    if nodes_meta.Has_Concurrent_Statement_Chain(k):
        children.extend(get_symbols_chain(fe, nodes.Get_Concurrent_Statement_Chain(n)))
    if nodes_meta.Has_Generate_Statement_Body(k):
        children.extend(
            get_symbols_chain(
                fe,
                nodes.Get_Concurrent_Statement_Chain(nodes.Get_Generate_Statement_Body(n)),
            )
        )

    if children:
        res["children"] = children
    else:
        # Discard anonymous symbols without children.
        if name is None:
            return None
    res["name"] = name if name is not None else "<anon>"
    return res
