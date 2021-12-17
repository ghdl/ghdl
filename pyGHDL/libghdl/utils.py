# =============================================================================
#               ____ _   _ ____  _       _ _ _           _         _ _
#  _ __  _   _ / ___| | | |  _ \| |     | (_) |__   __ _| |__   __| | |
# | '_ \| | | | |  _| |_| | | | | |     | | | '_ \ / _` | '_ \ / _` | |
# | |_) | |_| | |_| |  _  | |_| | |___ _| | | |_) | (_| | | | | (_| | |
# | .__/ \__, |\____|_| |_|____/|_____(_)_|_|_.__/ \__, |_| |_|\__,_|_|
# |_|    |___/                                     |___/
# =============================================================================
# Authors:
#   Tristan Gingold
#
# Package module:   Generators/iterators and low-level helpers for pyGHDL.libghdl.
#
# License:
# ============================================================================
#  Copyright (C) 2019-2021 Tristan Gingold
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

from ctypes import byref
from typing import List, Any, Generator

from pyTooling.Decorators import export

from pyGHDL.libghdl._decorator import EnumLookupTable
from pyGHDL.libghdl._types import NameId
import pyGHDL.libghdl.name_table as name_table
import pyGHDL.libghdl.files_map as files_map
import pyGHDL.libghdl.vhdl.nodes as nodes
import pyGHDL.libghdl.vhdl.nodes_meta as nodes_meta
import pyGHDL.libghdl.vhdl.lists as lists
import pyGHDL.libghdl.vhdl.flists as flists


@export
def name_image(Id: NameId) -> str:
    """Lookup a :obj:`Id` and return its string."""
    return name_table.Get_Name_Ptr(Id)


@export
@EnumLookupTable(nodes_meta.fields)
def fields_image(idx: int) -> str:
    """String representation of Nodes_Meta.fields :obj:`idx`."""


@export
@EnumLookupTable(nodes.Iir_Kind)
def kind_image(k: int) -> str:
    """String representation of Nodes.Iir_Kind :obj:`k`."""


@export
@EnumLookupTable(nodes_meta.types)
def types_image(t: int) -> str:
    """String representation of Nodes_Meta.Types :obj:`t`."""


@export
@EnumLookupTable(nodes_meta.Attr)
def attr_image(a: int) -> str:
    """String representation of Nodes_Meta.Attr :obj:`a`."""


@export
def leftest_location(n):
    while True:
        if n == nodes.Null_Iir:
            return files_map.No_Location
        k = nodes.Get_Kind(n)
        if k == nodes.Iir_Kind.Array_Subtype_Definition:
            n = nodes.Get_Subtype_Type_Mark(n)
        else:
            return nodes.Get_Location(n)


@export
def fields_iter(n) -> Generator[Any, None, None]:
    """Iterate on fields of node :obj:`n`."""
    if n == nodes.Null_Iir:
        return
    k = nodes.Get_Kind(n)
    first = nodes_meta.get_fields_first(k)
    last = nodes_meta.get_fields_last(k)
    for i in range(first, last + 1):
        yield nodes_meta.get_field_by_index(i)


@export
def chain_iter(n) -> Generator[Any, None, None]:
    """Iterate of a chain headed by node :obj:`n`."""
    while n != nodes.Null_Iir:
        yield n
        n = nodes.Get_Chain(n)


@export
def chain_to_list(n) -> List[Any]:
    """Convert a chain headed by node :obj:`n` to a Python list."""
    return [e for e in chain_iter(n)]


@export
def nodes_iter(n) -> Generator[Any, None, None]:
    """
    Iterate all nodes of :obj:`n`, including :obj:`n`.
    Nodes are returned only once.
    """
    if n == nodes.Null_Iir:
        return
    #    print 'nodes_iter for {0}'.format(n)
    yield n
    for f in fields_iter(n):
        typ = nodes_meta.get_field_type(f)
        #        print ' {0}: field {1} (type: {2})'.format(
        #            n, fields_image(f), types_image(typ))
        if typ == nodes_meta.types.Iir:
            attr = nodes_meta.get_field_attribute(f)
            if attr == nodes_meta.Attr.ANone:
                for n1 in nodes_iter(nodes_meta.Get_Iir(n, f)):
                    yield n1
            elif attr == nodes_meta.Attr.Chain:
                n2 = nodes_meta.Get_Iir(n, f)
                while n2 != nodes.Null_Iir:
                    for n1 in nodes_iter(n2):
                        yield n1
                    n2 = nodes.Get_Chain(n2)
            elif attr == nodes_meta.Attr.Maybe_Ref:
                if not nodes.Get_Is_Ref(n, f):
                    for n1 in nodes_iter(nodes_meta.Get_Iir(n, f)):
                        yield n1
        elif typ == nodes_meta.types.Iir_List:
            attr = nodes_meta.get_field_attribute(f)
            if attr == nodes_meta.Attr.ANone:
                for n1 in list_iter(nodes_meta.Get_Iir_List(n, f)):
                    for n2 in nodes_iter(n1):
                        yield n2
        elif typ == nodes_meta.types.Iir_Flist:
            attr = nodes_meta.get_field_attribute(f)
            if attr == nodes_meta.Attr.ANone:
                for n1 in flist_iter(nodes_meta.Get_Iir_Flist(n, f)):
                    for n2 in nodes_iter(n1):
                        yield n2


@export
def list_iter(lst) -> Generator[Any, None, None]:
    """Iterate all element of Iir_List :obj:`lst`."""
    if lst <= nodes.Iir_List_All:
        return
    iter = lists.Iterate(lst)
    while lists.Is_Valid(byref(iter)):
        yield lists.Get_Element(byref(iter))
        lists.Next(byref(iter))


@export
def flist_iter(lst) -> Generator[Any, None, None]:
    """Iterate all element of Iir_List :obj:`lst`."""
    if lst <= nodes.Iir_Flist_All:
        return
    for i in range(flists.Flast(lst) + 1):
        yield flists.Get_Nth_Element(lst, i)


@export
def declarations_iter(n) -> Generator[Any, None, None]:
    """Iterate all declarations in node :obj:`n`."""
    k = nodes.Get_Kind(n)
    if nodes_meta.Has_Generic_Chain(k):
        for n1 in chain_iter(nodes.Get_Generic_Chain(n)):
            yield n1
    if nodes_meta.Has_Port_Chain(k):
        for n1 in chain_iter(nodes.Get_Port_Chain(n)):
            yield n1
    if nodes_meta.Has_Interface_Declaration_Chain(k):
        for n1 in chain_iter(nodes.Get_Interface_Declaration_Chain(n)):
            yield n1
    if nodes_meta.Has_Declaration_Chain(k):
        for n1 in chain_iter(nodes.Get_Declaration_Chain(n)):
            k1 = nodes.Get_Kind(n1)
            if k1 in nodes.Iir_Kinds.Specification or k1 == nodes.Iir_Kind.Use_Clause:
                # Not a declaration
                pass
            elif k1 == nodes.Iir_Kind.Signal_Attribute_Declaration:
                # Not a declaration
                pass
            elif k1 in (
                nodes.Iir_Kind.Type_Declaration,
                nodes.Iir_Kind.Anonymous_Type_Declaration,
            ):
                yield n1
                # Handle nested declarations: record elements, physical units,
                # enumeration literals...
                for n2 in declarations_iter(n1):
                    yield n2
            else:
                yield n1
                # There can be nested declarations (subprograms)
                for n2 in declarations_iter(n1):
                    yield n2
    if nodes_meta.Has_Concurrent_Statement_Chain(k):
        for n1 in chain_iter(nodes.Get_Concurrent_Statement_Chain(n)):
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Sequential_Statement_Chain(k):
        for n1 in chain_iter(nodes.Get_Sequential_Statement_Chain(n)):
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Parameter_Specification(k):
        yield nodes.Get_Parameter_Specification(n)
    if nodes_meta.Has_Generate_Statement_Body(k):
        for n1 in declarations_iter(nodes.Get_Generate_Statement_Body(n)):
            yield n1
    if nodes_meta.Has_Else_Clause(k):
        n1 = nodes.Get_Else_Clause(n)
        if n1 != nodes.Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Generate_Else_Clause(k):
        n1 = nodes.Get_Generate_Else_Clause(n)
        if n1 != nodes.Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Block_Header(k):
        n1 = nodes.Get_Block_Header(n)
        if n1 != nodes.Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    # All these nodes are handled:
    if k in (
        nodes.Iir_Kind.Entity_Declaration,
        nodes.Iir_Kind.Architecture_Body,
        nodes.Iir_Kind.Package_Declaration,
        nodes.Iir_Kind.Package_Body,
        nodes.Iir_Kind.Process_Statement,
        nodes.Iir_Kind.Sensitized_Process_Statement,
        nodes.Iir_Kind.Concurrent_Assertion_Statement,
        nodes.Iir_Kind.Concurrent_Simple_Signal_Assignment,
        nodes.Iir_Kind.Concurrent_Selected_Signal_Assignment,
        nodes.Iir_Kind.Concurrent_Conditional_Signal_Assignment,
        nodes.Iir_Kind.Concurrent_Procedure_Call_Statement,
        nodes.Iir_Kind.Block_Statement,
        nodes.Iir_Kind.Block_Header,
        nodes.Iir_Kind.For_Generate_Statement,
        nodes.Iir_Kind.If_Generate_Statement,
        nodes.Iir_Kind.Generate_Statement_Body,
        nodes.Iir_Kind.Assertion_Statement,
        nodes.Iir_Kind.Wait_Statement,
        nodes.Iir_Kind.Simple_Signal_Assignment_Statement,
        nodes.Iir_Kind.Variable_Assignment_Statement,
        nodes.Iir_Kind.For_Loop_Statement,
        nodes.Iir_Kind.While_Loop_Statement,
        nodes.Iir_Kind.Case_Statement,
        nodes.Iir_Kind.Null_Statement,
        nodes.Iir_Kind.Exit_Statement,
        nodes.Iir_Kind.Next_Statement,
        nodes.Iir_Kind.Procedure_Call_Statement,
        nodes.Iir_Kind.Signal_Declaration,
        nodes.Iir_Kind.Constant_Declaration,
        nodes.Iir_Kind.Variable_Declaration,
        nodes.Iir_Kind.File_Declaration,
        nodes.Iir_Kind.Object_Alias_Declaration,
        nodes.Iir_Kind.Attribute_Declaration,
        nodes.Iir_Kind.Component_Declaration,
        nodes.Iir_Kind.Use_Clause,
        nodes.Iir_Kind.If_Statement,
        nodes.Iir_Kind.Elsif,
        nodes.Iir_Kind.Return_Statement,
        nodes.Iir_Kind.Type_Declaration,
        nodes.Iir_Kind.Anonymous_Type_Declaration,
        nodes.Iir_Kind.Subtype_Declaration,
        nodes.Iir_Kind.Function_Declaration,
        nodes.Iir_Kind.Function_Body,
        nodes.Iir_Kind.Procedure_Declaration,
        nodes.Iir_Kind.Procedure_Body,
        nodes.Iir_Kind.Component_Instantiation_Statement,
    ):
        return
    raise Exception("Unknown node of kind {}".format(kind_image(k)))


@export
def concurrent_stmts_iter(n) -> Generator[Any, None, None]:
    """Iterate concurrent statements in node :obj:`n`."""
    k = nodes.Get_Kind(n)
    if k == nodes.Iir_Kind.Design_File:
        for n1 in chain_iter(nodes.Get_First_Design_Unit(n)):
            for n2 in concurrent_stmts_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.Design_Unit:
        for n1 in concurrent_stmts_iter(nodes.Get_Library_Unit(n)):
            yield n1
    elif k in (
        nodes.Iir_Kind.Entity_Declaration,
        nodes.Iir_Kind.Architecture_Body,
        nodes.Iir_Kind.Block_Statement,
    ):
        for n1 in chain_iter(nodes.Get_Concurrent_Statement_Chain(n)):
            yield n1
            for n2 in concurrent_stmts_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.For_Generate_Statement:
        for n1 in concurrent_stmts_iter(nodes.Get_Generate_Statement_Body(n)):
            yield n1
    elif k == nodes.Iir_Kind.If_Generate_Statement:
        while n != nodes.Null_Iir:
            for n1 in concurrent_stmts_iter(nodes.Get_Generate_Statement_Body(n)):
                yield n1
            n = nodes.Get_Generate_Else_Clause(n)
    elif k == nodes.Iir_Kind.Case_Generate_Statement:
        alt = nodes.Get_Case_Statement_Alternative_Chain(n)
        for n1 in chain_iter(alt):
            blk = nodes.Get_Associated_Block(n1)
            if blk != nodes.Null_Iir:
                for n2 in concurrent_stmts_iter(nodes.Get_Generate_Statement_Body(n)):
                    yield n2


@export
def constructs_iter(n) -> Generator[Any, None, None]:
    """
    Iterate library units, concurrent statements and declarations
    that appear directly within a declarative part.
    """
    if n == nodes.Null_Iir:
        return
    k = nodes.Get_Kind(n)
    if k == nodes.Iir_Kind.Design_File:
        for n1 in chain_iter(nodes.Get_First_Design_Unit(n)):
            for n2 in constructs_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.Design_Unit:
        n1 = nodes.Get_Library_Unit(n)
        yield n1
        for n2 in constructs_iter(n1):
            yield n2
    elif k in (
        nodes.Iir_Kind.Entity_Declaration,
        nodes.Iir_Kind.Architecture_Body,
        nodes.Iir_Kind.Block_Statement,
        nodes.Iir_Kind.Generate_Statement_Body,
    ):
        for n1 in chain_iter(nodes.Get_Declaration_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
        for n1 in chain_iter(nodes.Get_Concurrent_Statement_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
    elif k in (
        nodes.Iir_Kind.Configuration_Declaration,
        nodes.Iir_Kind.Package_Declaration,
        nodes.Iir_Kind.Package_Body,
        nodes.Iir_Kind.Function_Body,
        nodes.Iir_Kind.Procedure_Body,
        nodes.Iir_Kind.Protected_Type_Declaration,
        nodes.Iir_Kind.Protected_Type_Body,
        nodes.Iir_Kind.Process_Statement,
        nodes.Iir_Kind.Sensitized_Process_Statement,
    ):
        for n1 in chain_iter(nodes.Get_Declaration_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.For_Generate_Statement:
        n1 = nodes.Get_Generate_Statement_Body(n)
        yield n1
        for n2 in constructs_iter(n1):
            yield n2
    elif k == nodes.Iir_Kind.If_Generate_Statement:
        while n != nodes.Null_Iir:
            n1 = nodes.Get_Generate_Statement_Body(n)
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
            n = nodes.Get_Generate_Else_Clause(n)
    elif k == nodes.Iir_Kind.Case_Generate_Statement:
        alt = nodes.Get_Case_Statement_Alternative_Chain(n)
        for n1 in chain_iter(alt):
            blk = nodes.Get_Associated_Block(n1)
            if blk != nodes.Null_Iir:
                n2 = nodes.Get_Generate_Statement_Body(blk)
                yield n2
                for n3 in constructs_iter(n2):
                    yield n3


@export
def sequential_iter(n) -> Generator[Any, None, None]:
    """
    Iterate sequential statements. The first node must be either
    a process or a subprogram body.
    """
    if n == nodes.Null_Iir:
        return
    k = nodes.Get_Kind(n)
    if k in (
        nodes.Iir_Kind.Process_Statement,
        nodes.Iir_Kind.Sensitized_Process_Statement,
        nodes.Iir_Kind.Function_Body,
        nodes.Iir_Kind.Procedure_Body,
    ):
        for n1 in chain_iter(nodes.Get_Sequential_Statement_Chain(n)):
            yield n1
            for n2 in sequential_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.If_Statement:
        while True:
            n = nodes.Get_Chain(n)
            if n == nodes.Null_Iir:
                break
            yield n
            for n1 in sequential_iter(n):
                yield n1
    elif k == nodes.Iir_Kind.Case_Statement:
        for ch in chain_iter(nodes.Get_Case_Statement_Alternative_Chain(n)):
            stmt = nodes.Get_Associated_Chain(ch)
            if stmt != nodes.Null_Iir:
                for n1 in chain_iter(stmt):
                    yield n1
                    for n2 in sequential_iter(n1):
                        yield n2
    elif k in [nodes.Iir_Kind.For_Loop_Statement, nodes.Iir_Kind.While_Loop_Statement]:
        for n1 in chain_iter(nodes.Get_Sequential_Statement_Chain(n)):
            yield n1
            for n2 in sequential_iter(n1):
                yield n2
    elif k in (
        nodes.Iir_Kind.Assertion_Statement,
        nodes.Iir_Kind.Wait_Statement,
        nodes.Iir_Kind.Null_Statement,
        nodes.Iir_Kind.Exit_Statement,
        nodes.Iir_Kind.Next_Statement,
        nodes.Iir_Kind.Return_Statement,
        nodes.Iir_Kind.Variable_Assignment_Statement,
        nodes.Iir_Kind.Simple_Signal_Assignment_Statement,
        nodes.Iir_Kind.Procedure_Call_Statement,
    ):
        return
    else:
        raise Exception("Unknown node of kind {}".format(kind_image(k)))
