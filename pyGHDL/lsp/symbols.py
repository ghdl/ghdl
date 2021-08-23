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
    nodes.Iir_Kind.Signal_Attribute_Declaration: {"kind": None},
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
    assert loc != files_map.No_Location
    line = files_map.Location_File_To_Line(loc, fe)
    off = files_map.Location_File_Line_To_Offset(loc, fe, line)
    return {"line": line - 1, "character": off}


def get_symbols_chain(fe, n):
    res = [get_symbols(fe, el) for el in pyutils.chain_iter(n)]
    return [e for e in res if e is not None]


def get_symbols(fe, n):
    if n == nodes.Null_Iir:
        return None
    k = nodes.Get_Kind(n)
    if k == nodes.Iir_Kind.Design_Unit:
        return get_symbols(fe, nodes.Get_Library_Unit(n))
    m = SYMBOLS_MAP.get(k, None)
    if m is None:
        raise AssertionError("get_symbol: unhandled {}".format(pyutils.kind_image(k)))
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
