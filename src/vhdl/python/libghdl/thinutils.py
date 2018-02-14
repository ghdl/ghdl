from libghdl import libghdl
from ctypes import (c_char_p, c_int32, c_int, c_bool, sizeof, c_void_p, byref)
import libghdl.iirs as iirs
import libghdl.thin as thin
import libghdl.nodes_meta as nodes_meta
from libghdl.nodes_meta import (Attr, types)
# from libghdl_defs import (fields, Iir_Kind, types, Attr)

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


def leftest_location(n):
    while True:
        if n == Null_Iir:
            return No_Location
        k = iirs.Get_Kind(n)
        if k == iirs.Iir_Kind.Array_Subtype_Definition:
            n = iirs.Get_Subtype_Type_Mark(n)
        else:
            return iirs.Get_Location(n)


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


def chain_to_list(n):
    """Convert a chain headed by node n to a python list"""
    return [e for e in chain_iter(n)]


def nodes_iter(n):
    """Iterate of all nodes of n, including n.
    Nodes are returned only once."""
    if n == Null_Iir:
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
    if lst <= thin.Iir_List_All:
        return
    iter = thin.Lists.Iterate(lst)
    while thin.Lists.Is_Valid(byref(iter)):
        yield thin.Lists.Get_Element(byref(iter))
        thin.Lists.Next(byref(iter))


def flist_iter(lst):
    """Iterate of all element of Iir_List lst."""
    if lst <= thin.Iir_Flist_All:
        return
    for i in range(thin.Flists.Flast(lst) + 1):
        yield thin.Flists.Get_Nth_Element(lst, i)


def declarations_iter(n):
    """Iterator on all declarations in n."""
    k = iirs.Get_Kind(n)
    if nodes_meta.Has_Generic_Chain(k):
        for n1 in chain_iter(iirs.Get_Generic_Chain(n)):
            yield n1
    if nodes_meta.Has_Port_Chain(k):
        for n1 in chain_iter(iirs.Get_Port_Chain(n)):
            yield n1
    if nodes_meta.Has_Interface_Declaration_Chain(k):
        for n1 in chain_iter(iirs.Get_Interface_Declaration_Chain(n)):
            yield n1
    if nodes_meta.Has_Declaration_Chain(k):
        for n1 in chain_iter(iirs.Get_Declaration_Chain(n)):
            k1 = iirs.Get_Kind(n1)
            if k1 in iirs.Iir_Kinds.Specification \
               or k1 == iirs.Iir_Kind.Use_Clause:
                # Not a declaration
                pass
            elif k1 == iirs.Iir_Kind.Signal_Attribute_Declaration:
                # Not a declaration
                pass
            elif k1 in [iirs.Iir_Kind.Type_Declaration,
                        iirs.Iir_Kind.Anonymous_Type_Declaration]:
                yield n1
                # Handle nested declarations: record elements, physical units,
                # enumeration literals...
                typ = iirs.Get_Type_Definition(n1)
                for n2 in declarations_iter(n1):
                    yield n2
            else:
                yield n1
                # There can be nested declarations (subprograms)
                for n2 in declarations_iter(n1):
                    yield n2
    if nodes_meta.Has_Concurrent_Statement_Chain(k):
        for n1 in chain_iter(iirs.Get_Concurrent_Statement_Chain(n)):
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Sequential_Statement_Chain(k):
        for n1 in chain_iter(iirs.Get_Sequential_Statement_Chain(n)):
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Parameter_Specification(k):
        yield iirs.Get_Parameter_Specification(n)
    if nodes_meta.Has_Generate_Statement_Body(k):
        for n1 in declarations_iter(iirs.Get_Generate_Statement_Body(n)):
            yield n1
    if nodes_meta.Has_Else_Clause(k):
        n1 = iirs.Get_Else_Clause(n)
        if n1 != Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Generate_Else_Clause(k):
        n1 = iirs.Get_Generate_Else_Clause(n)
        if n1 != Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    if nodes_meta.Has_Block_Header(k):
        n1 = iirs.Get_Block_Header(n)
        if n1 != Null_Iir:
            for n2 in declarations_iter(n1):
                yield n2
    # All these nodes are handled:
    if k in [iirs.Iir_Kind.Entity_Declaration,
             iirs.Iir_Kind.Architecture_Body,
             iirs.Iir_Kind.Package_Declaration,
             iirs.Iir_Kind.Package_Body,
             iirs.Iir_Kind.Process_Statement,
             iirs.Iir_Kind.Sensitized_Process_Statement,
             iirs.Iir_Kind.Concurrent_Assertion_Statement,
             iirs.Iir_Kind.Concurrent_Simple_Signal_Assignment,
             iirs.Iir_Kind.Concurrent_Selected_Signal_Assignment,
             iirs.Iir_Kind.Concurrent_Conditional_Signal_Assignment,
             iirs.Iir_Kind.Concurrent_Procedure_Call_Statement,
             iirs.Iir_Kind.Block_Statement,
             iirs.Iir_Kind.Block_Header,
             iirs.Iir_Kind.For_Generate_Statement,
             iirs.Iir_Kind.If_Generate_Statement,
             iirs.Iir_Kind.Generate_Statement_Body,
             iirs.Iir_Kind.Assertion_Statement,
             iirs.Iir_Kind.Wait_Statement,
             iirs.Iir_Kind.Simple_Signal_Assignment_Statement,
             iirs.Iir_Kind.Variable_Assignment_Statement,
             iirs.Iir_Kind.For_Loop_Statement,
             iirs.Iir_Kind.While_Loop_Statement,
             iirs.Iir_Kind.Case_Statement,
             iirs.Iir_Kind.Null_Statement,
             iirs.Iir_Kind.Exit_Statement,
             iirs.Iir_Kind.Next_Statement,
             iirs.Iir_Kind.Procedure_Call_Statement,
             iirs.Iir_Kind.Signal_Declaration,
             iirs.Iir_Kind.Constant_Declaration,
             iirs.Iir_Kind.Variable_Declaration,
             iirs.Iir_Kind.File_Declaration,
             iirs.Iir_Kind.Object_Alias_Declaration,
             iirs.Iir_Kind.Attribute_Declaration,
             iirs.Iir_Kind.Component_Declaration,
             iirs.Iir_Kind.Use_Clause,
             iirs.Iir_Kind.If_Statement,
             iirs.Iir_Kind.Elsif,
             iirs.Iir_Kind.Return_Statement,
             iirs.Iir_Kind.Type_Declaration,
             iirs.Iir_Kind.Anonymous_Type_Declaration,
             iirs.Iir_Kind.Subtype_Declaration,
             iirs.Iir_Kind.Function_Declaration,
             iirs.Iir_Kind.Function_Body,
             iirs.Iir_Kind.Procedure_Declaration,
             iirs.Iir_Kind.Procedure_Body,
             iirs.Iir_Kind.Component_Instantiation_Statement,
             ]:
        return
    assert False, "unknown node of kind {}".format(kind_image(k))


def concurrent_stmts_iter(n):
    """Iterator on concurrent statements in n."""
    k = iirs.Get_Kind(n)
    if k == iirs.Iir_Kind.Design_File:
        for n1 in chain_iter(iirs.Get_First_Design_Unit(n)):
            for n2 in concurrent_stmts_iter(n1):
                yield n2
    elif k == iirs.Iir_Kind.Design_Unit:
        for n1 in concurrent_stmts_iter(iirs.Get_Library_Unit(n)):
            yield n1
    elif k == iirs.Iir_Kind.Entity_Declaration \
         or k == iirs.Iir_Kind.Architecture_Body \
         or k == iirs.Iir_Kind.Block_Statement:
        for n1 in chain_iter(iirs.Get_Concurrent_Statement_Chain(n)):
            yield n1
            for n2 in concurrent_stmts_iter(n1):
                yield n2
    elif k == iirs.Iir_Kind.For_Generate_Statement:
        for n1 in concurrent_stmts_iter(iirs.Get_Generate_Statement_Body(n)):
            yield n1
    elif k == iirs.Iir_Kind.If_Generate_Statement:
        while n != Null_Iir:
            for n1 in concurrent_stmts_iter(
                    iirs.Get_Generate_Statement_Body(n)):
                yield n1
            n = iirs.Get_Generate_Else_Clause(n)
    elif k == iirs.Iir_Kind.Case_Generate_Statement:
        alt = iirs.Get_Case_Statement_Alternative_Chain(n)
        for n1 in chain_iter(alt):
            blk = iirs.Get_Associated_Block(n1)
            if blk != Null_Iir:
                for n2 in concurrent_stmts_iter(
                        iirs.Get_Generate_Statement_Body(n)):
                    yield n2


def constructs_iter(n):
    """Iterator on library unit, concurrent statements and declarations
       that appear directly within a declarative part."""
    if n == thin.Null_Iir:
        return
    k = iirs.Get_Kind(n)
    if k == iirs.Iir_Kind.Design_File:
        for n1 in chain_iter(iirs.Get_First_Design_Unit(n)):
            for n2 in constructs_iter(n1):
                yield n2
    elif k == iirs.Iir_Kind.Design_Unit:
        n1 = iirs.Get_Library_Unit(n)
        yield n1
        for n2 in constructs_iter(n1):
            yield n2
    elif k in [iirs.Iir_Kind.Entity_Declaration,
               iirs.Iir_Kind.Architecture_Body,
               iirs.Iir_Kind.Block_Statement,
               iirs.Iir_Kind.Generate_Statement_Body]:
        for n1 in chain_iter(iirs.Get_Declaration_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
        for n1 in chain_iter(iirs.Get_Concurrent_Statement_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
    elif k in [iirs.Iir_Kind.Configuration_Declaration,
               iirs.Iir_Kind.Package_Declaration,
               iirs.Iir_Kind.Package_Body,
               iirs.Iir_Kind.Function_Body,
               iirs.Iir_Kind.Procedure_Body,
               iirs.Iir_Kind.Protected_Type_Declaration,
               iirs.Iir_Kind.Protected_Type_Body,
               iirs.Iir_Kind.Process_Statement,
               iirs.Iir_Kind.Sensitized_Process_Statement]:
        for n1 in chain_iter(iirs.Get_Declaration_Chain(n)):
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
    elif k == iirs.Iir_Kind.For_Generate_Statement:
        n1 = iirs.Get_Generate_Statement_Body(n)
        yield n1
        for n2 in constructs_iter(n1):
            yield n2
    elif k == iirs.Iir_Kind.If_Generate_Statement:
        while n != Null_Iir:
            n1 = iirs.Get_Generate_Statement_Body(n)
            yield n1
            for n2 in constructs_iter(n1):
                yield n2
            n = iirs.Get_Generate_Else_Clause(n)
    elif k == iirs.Iir_Kind.Case_Generate_Statement:
        alt = iirs.Get_Case_Statement_Alternative_Chain(n)
        for n1 in chain_iter(alt):
            blk = iirs.Get_Associated_Block(n1)
            if blk != Null_Iir:
                n2 = iirs.Get_Generate_Statement_Body(blk)
                yield n2
                for n3 in constructs_iter(n2):
                    yield n3

def sequential_iter(n):
    """Iterator on sequential statements.  The first node must be either
       a process or a subprogram body."""
    if n == thin.Null_Iir:
        return
    k = iirs.Get_Kind(n)
    if k in [iirs.Iir_Kind.Process_Statement,
             iirs.Iir_Kind.Sensitized_Process_Statement,
             iirs.Iir_Kind.Function_Body,
             iirs.Iir_Kind.Procedure_Body]:
        for n1 in chain_iter(iirs.Get_Sequential_Statement_Chain(n)):
            yield n1
            for n2 in sequential_iter(n1):
                yield n2
    elif k == iirs.Iir_Kind.If_Statement:
        while True:
            n = iirs.Get_Chain(n)
            if n == thin.Null_Iir:
                break
            yield n
            for n1 in sequential_iter(n):
                yield n1
    elif k == iirs.Iir_Kind.Case_Statement:
        for ch in chain_iter(iirs.Get_Case_Statement_Alternative_Chain(n)):
            stmt = iirs.Get_Associated_Chain(ch)
            if stmt != thin.Null_Iir:
                for n1 in chain_iter(stmt):
                    yield n1
                    for n2 in sequential_iter(n1):
                        yield n2
    elif k in [iirs.Iir_Kind.For_Loop_Statement,
               iirs.Iir_Kind.While_Loop_Statement]:
        for n1 in chain_iter(iirs.Get_Sequential_Statement_Chain(n)):
            yield n1
            for n2 in sequential_iter(n1):
                yield n2
    elif k in [iirs.Iir_Kind.Assertion_Statement,
               iirs.Iir_Kind.Wait_Statement,
               iirs.Iir_Kind.Null_Statement,
               iirs.Iir_Kind.Exit_Statement,
               iirs.Iir_Kind.Next_Statement,
               iirs.Iir_Kind.Return_Statement,
               iirs.Iir_Kind.Variable_Assignment_Statement,
               iirs.Iir_Kind.Simple_Signal_Assignment_Statement,
               iirs.Iir_Kind.Procedure_Call_Statement]:
        return
    else:
        assert False, "unknown node of kind {}".format(kind_image(k))
