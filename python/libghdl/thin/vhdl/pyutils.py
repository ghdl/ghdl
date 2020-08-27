from ctypes import c_char_p, c_int32, c_int, c_bool, sizeof, c_void_p, byref
import libghdl.thin.name_table as name_table
import libghdl.thin.vhdl.nodes as nodes
import libghdl.thin.vhdl.nodes_meta as nodes_meta
import libghdl.thin.vhdl.lists as lists
import libghdl.thin.vhdl.flists as flists
from libghdl.thin.vhdl.nodes_meta import Attr, types


def name_image(nameid):
    return name_table.Get_Name_Ptr(nameid).decode("utf-8")


def _build_enum_image(cls):
    d = [e for e in dir(cls) if e[0] != "_"]
    res = [None] * len(d)
    for e in d:
        res[getattr(cls, e)] = e
    return res


_fields_image = _build_enum_image(nodes_meta.fields)


def fields_image(idx):
    """String representation of field idx"""
    return _fields_image[idx]


_kind_image = _build_enum_image(nodes.Iir_Kind)


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


def leftest_location(n):
    while True:
        if n == Null_Iir:
            return No_Location
        k = nodes.Get_Kind(n)
        if k == nodes.Iir_Kind.Array_Subtype_Definition:
            n = nodes.Get_Subtype_Type_Mark(n)
        else:
            return nodes.Get_Location(n)


def fields_iter(n):
    """Iterate on fields of node n"""
    if n == nodes.Null_Iir:
        return
    k = nodes.Get_Kind(n)
    first = nodes_meta.get_fields_first(k)
    last = nodes_meta.get_fields_last(k)
    for i in range(first, last + 1):
        yield nodes_meta.get_field_by_index(i)


def chain_iter(n):
    """Iterate of a chain headed by node n"""
    while n != nodes.Null_Iir:
        yield n
        n = nodes.Get_Chain(n)


def chain_to_list(n):
    """Convert a chain headed by node n to a python list"""
    return [e for e in chain_iter(n)]


def nodes_iter(n):
    """Iterate of all nodes of n, including n.
    Nodes are returned only once."""
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
            if attr == Attr.ANone:
                for n1 in nodes_iter(nodes_meta.Get_Iir(n, f)):
                    yield n1
            elif attr == Attr.Chain:
                n2 = nodes_meta.Get_Iir(n, f)
                while n2 != nodes.Null_Iir:
                    for n1 in nodes_iter(n2):
                        yield n1
                    n2 = nodes.Get_Chain(n2)
            elif attr == Attr.Maybe_Ref:
                if not nodes.Get_Is_Ref(n, f):
                    for n1 in nodes_iter(nodes_meta.Get_Iir(n, f)):
                        yield n1
        elif typ == types.Iir_List:
            attr = nodes_meta.get_field_attribute(f)
            if attr == Attr.ANone:
                for n1 in list_iter(nodes_meta.Get_Iir_List(n, f)):
                    for n2 in nodes_iter(n1):
                        yield n2
        elif typ == types.Iir_Flist:
            attr = nodes_meta.get_field_attribute(f)
            if attr == Attr.ANone:
                for n1 in flist_iter(nodes_meta.Get_Iir_Flist(n, f)):
                    for n2 in nodes_iter(n1):
                        yield n2


def list_iter(lst):
    """Iterate of all element of Iir_List lst."""
    if lst <= nodes.Iir_List_All:
        return
    iter = lists.Iterate(lst)
    while lists.Is_Valid(byref(iter)):
        yield lists.Get_Element(byref(iter))
        lists.Next(byref(iter))


def flist_iter(lst):
    """Iterate of all element of Iir_List lst."""
    if lst <= nodes.Iir_Flist_All:
        return
    for i in range(flists.Flast(lst) + 1):
        yield flists.Get_Nth_Element(lst, i)


def declarations_iter(n):
    """Iterator on all declarations in n."""
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
            elif k1 in [
                nodes.Iir_Kind.Type_Declaration,
                nodes.Iir_Kind.Anonymous_Type_Declaration,
            ]:
                yield n1
                # Handle nested declarations: record elements, physical units,
                # enumeration literals...
                typ = nodes.Get_Type_Definition(n1)
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
        if n1 != Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Generate_Else_Clause(k):
        n1 = nodes.Get_Generate_Else_Clause(n)
        if n1 != Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Block_Header(k):
        n1 = nodes.Get_Block_Header(n)
        if n1 != Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    # All these nodes are handled:
    if k in [
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
    ]:
        return
    assert False, "unknown node of kind {}".format(kind_image(k))


def concurrent_stmts_iter(n):
    """Iterator on concurrent statements in n."""
    k = nodes.Get_Kind(n)
    if k == nodes.Iir_Kind.Design_File:
        for n1 in chain_iter(nodes.Get_First_Design_Unit(n)):
            for n2 in concurrent_stmts_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.Design_Unit:
        for n1 in concurrent_stmts_iter(nodes.Get_Library_Unit(n)):
            yield n1
    elif (
        k == nodes.Iir_Kind.Entity_Declaration
        or k == nodes.Iir_Kind.Architecture_Body
        or k == nodes.Iir_Kind.Block_Statement
    ):
        for n1 in chain_iter(nodes.Get_Concurrent_Statement_Chain(n)):
            yield n1
            for n2 in concurrent_stmts_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.For_Generate_Statement:
        for n1 in concurrent_stmts_iter(nodes.Get_Generate_Statement_Body(n)):
            yield n1
    elif k == nodes.Iir_Kind.If_Generate_Statement:
        while n != Null_Iir:
            for n1 in concurrent_stmts_iter(nodes.Get_Generate_Statement_Body(n)):
                yield n1
            n = nodes.Get_Generate_Else_Clause(n)
    elif k == nodes.Iir_Kind.Case_Generate_Statement:
        alt = nodes.Get_Case_Statement_Alternative_Chain(n)
        for n1 in chain_iter(alt):
            blk = nodes.Get_Associated_Block(n1)
            if blk != Null_Iir:
                for n2 in concurrent_stmts_iter(nodes.Get_Generate_Statement_Body(n)):
                    yield n2


def constructs_iter(n):
    """Iterator on library unit, concurrent statements and declarations
    that appear directly within a declarative part."""
    if n == thin.Null_Iir:
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
    elif k in [
        nodes.Iir_Kind.Entity_Declaration,
        nodes.Iir_Kind.Architecture_Body,
        nodes.Iir_Kind.Block_Statement,
        nodes.Iir_Kind.Generate_Statement_Body,
    ]:
        for n1 in chain_iter(nodes.Get_Declaration_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
        for n1 in chain_iter(nodes.Get_Concurrent_Statement_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
    elif k in [
        nodes.Iir_Kind.Configuration_Declaration,
        nodes.Iir_Kind.Package_Declaration,
        nodes.Iir_Kind.Package_Body,
        nodes.Iir_Kind.Function_Body,
        nodes.Iir_Kind.Procedure_Body,
        nodes.Iir_Kind.Protected_Type_Declaration,
        nodes.Iir_Kind.Protected_Type_Body,
        nodes.Iir_Kind.Process_Statement,
        nodes.Iir_Kind.Sensitized_Process_Statement,
    ]:
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
        while n != Null_Iir:
            n1 = nodes.Get_Generate_Statement_Body(n)
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
            n = nodes.Get_Generate_Else_Clause(n)
    elif k == nodes.Iir_Kind.Case_Generate_Statement:
        alt = nodes.Get_Case_Statement_Alternative_Chain(n)
        for n1 in chain_iter(alt):
            blk = nodes.Get_Associated_Block(n1)
            if blk != Null_Iir:
                n2 = nodes.Get_Generate_Statement_Body(blk)
                yield n2
                for n3 in constructs_iter(n2):
                    yield n3


def sequential_iter(n):
    """Iterator on sequential statements.  The first node must be either
    a process or a subprogram body."""
    if n == thin.Null_Iir:
        return
    k = nodes.Get_Kind(n)
    if k in [
        nodes.Iir_Kind.Process_Statement,
        nodes.Iir_Kind.Sensitized_Process_Statement,
        nodes.Iir_Kind.Function_Body,
        nodes.Iir_Kind.Procedure_Body,
    ]:
        for n1 in chain_iter(nodes.Get_Sequential_Statement_Chain(n)):
            yield n1
            for n2 in sequential_iter(n1):
                yield n2
    elif k == nodes.Iir_Kind.If_Statement:
        while True:
            n = nodes.Get_Chain(n)
            if n == thin.Null_Iir:
                break
            yield n
            for n1 in sequential_iter(n):
                yield n1
    elif k == nodes.Iir_Kind.Case_Statement:
        for ch in chain_iter(nodes.Get_Case_Statement_Alternative_Chain(n)):
            stmt = nodes.Get_Associated_Chain(ch)
            if stmt != thin.Null_Iir:
                for n1 in chain_iter(stmt):
                    yield n1
                    for n2 in sequential_iter(n1):
                        yield n2
    elif k in [nodes.Iir_Kind.For_Loop_Statement, nodes.Iir_Kind.While_Loop_Statement]:
        for n1 in chain_iter(nodes.Get_Sequential_Statement_Chain(n)):
            yield n1
            for n2 in sequential_iter(n1):
                yield n2
    elif k in [
        nodes.Iir_Kind.Assertion_Statement,
        nodes.Iir_Kind.Wait_Statement,
        nodes.Iir_Kind.Null_Statement,
        nodes.Iir_Kind.Exit_Statement,
        nodes.Iir_Kind.Next_Statement,
        nodes.Iir_Kind.Return_Statement,
        nodes.Iir_Kind.Variable_Assignment_Statement,
        nodes.Iir_Kind.Simple_Signal_Assignment_Statement,
        nodes.Iir_Kind.Procedure_Call_Statement,
    ]:
        return
    else:
        assert False, "unknown node of kind {}".format(kind_image(k))
