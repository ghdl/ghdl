#!/usr/bin/env python3

"""Like pnodes but output for Python."""

from __future__ import print_function

import re
import sys
from textwrap import dedent

try:
    import scripts.pnodes as pnodes
except:
    import pnodes

libname = "libghdl"


def print_enum(name, vals):
    print(dedent("""

        @export
        @unique
        class {0}(IntEnum):
        """).format(name), end=''
    )
    for n, k in enumerate(vals):
        if k == "None":
            k = "PNone"
        print("    {0} = {1}".format(k, n))


def print_file_header(includeIntEnumUnique=True, includeBindToLibGHDL=True):
    print(dedent("""\
            # Auto generated Python source file from Ada sources
            # Call 'make' in 'src/vhdl' to regenerate:
            #
        """) + "{sysImports}from pyTooling.Decorators import export\n{moduleImports}".format(
            sysImports = "from enum import IntEnum, unique\n" if includeIntEnumUnique else "",
            moduleImports = "\nfrom pyGHDL.libghdl._decorator import BindToLibGHDL\n" if includeBindToLibGHDL else "",
        )
    )


def do_class_kinds():
    print_enum(pnodes.prefix_name.rstrip("_"), pnodes.kinds)
    print(dedent("""

        @export
        class Iir_Kinds:
        """), end=''
    )
    for k, v in pnodes.kinds_ranges.items():
        print("    {0} = [".format(k))
        for e in v:
            print("        Iir_Kind.{},".format(e))
        print("    ]")
        print()


def do_iirs_subprg():
    classname = "vhdl__nodes"
    print(dedent("""

        @export
        @BindToLibGHDL("{classname}__get_kind")
        def Get_Kind(node: Iir) -> IirKind:
            \"\"\"Get node kind.\"\"\"
            return 0

        @export
        @BindToLibGHDL("{classname}__get_location")
        def Get_Location(node: Iir) -> LocationType:
            \"\"\"\"\"\"
            return 0
        """).format(libname=libname, classname=classname)
    )
    for k in pnodes.funcs:
        # Don't use the Iir_* subtypes (as they are not described).
        rtype = k.rtype.replace("_", "") if not k.rtype.startswith("Iir_") else "Iir"
        # Exceptions...
        if rtype == "TokenType":
            rtype = "Tok"

        print(dedent("""
            @export
            @BindToLibGHDL("{classname}__get_{kname_lower}")
            def Get_{kname}(obj: Iir) -> {rtype}:
                \"\"\"\"\"\"
                return 0
            @export
            @BindToLibGHDL("{classname}__set_{kname_lower}")
            def Set_{kname}(obj: Iir, value: {rtype}) -> None:
                \"\"\"\"\"\"
            """).format(kname=k.name, kname_lower=k.name.lower(), rtype=rtype,
                         libname=libname, classname=classname)
        )


def do_libghdl_elocations():
    classname = "vhdl__elocations"
    print_file_header(includeIntEnumUnique=False, includeBindToLibGHDL=False)
    print("from pyGHDL.libghdl import libghdl")
    print()
    for k in pnodes.funcs:
        print(dedent("""
            @export
            def Get_{kname}(obj):
                return {libname}.{classname}__get_{kname_lower}(obj)
            @export
            def Set_{kname}(obj, value) -> None:
                {libname}.{classname}__set_{kname_lower}(obj, value)
            """).format(kname=k.name, kname_lower=k.name.lower(), libname=libname, classname=classname)
        )


def do_class_types():
    print_enum("types", pnodes.get_types())


def do_types_subprg():
    print()
    for k in pnodes.get_types():
        print(dedent("""
            def Get_{0}(node, field):
                return {1}.vhdl__nodes_meta__get_{2}(node, field)
            """).format(k, libname, k.lower())
        )


def do_has_subprg():
    print()
    for f in pnodes.funcs:
        print(dedent("""
            @export
            @BindToLibGHDL("vhdl__nodes_meta__has_{fname_lower}")
            def Has_{fname}(kind: IirKind) -> bool:
                \"\"\"\"\"\"
            """).format(fname=f.name, libname=libname, fname_lower=f.name.lower())
        )


def do_class_field_attributes():
    print_enum("Attr", ["ANone" if a == "None" else a for a in pnodes.get_attributes()])


def do_class_fields():
    print_enum("fields", [f.name for f in pnodes.funcs])


def read_enum(filename, type_name, prefix, class_name, g=lambda m: m.group(1)):
    """Read an enumeration declaration from :param filename:."""
    pat_decl = re.compile(r"   type {0} is$".format(type_name))
    pat_enum = re.compile(r"      {0}(\w+),?( *-- .*)?$".format(prefix))
    pat_comment = re.compile(r" *-- .*$")
    lr = pnodes.linereader(filename)
    while not pat_decl.match(lr.get()):
        pass
    line = lr.get()
    if line != "     (\n":
        raise pnodes.ParseError(
            lr, "{}:{}: missing open parenthesis".format(filename, lr.lineno)
        )
    toks = []
    while True:
        line = lr.get()
        if line == "     );\n":
            break
        m = pat_enum.match(line)
        if m:
            toks.append(g(m))
        elif pat_comment.match(line):
            pass
        elif line == "\n":
            pass
        else:
            print(line, file=sys.stderr)
            raise pnodes.ParseError(
                lr,
                "{}:{}: incorrect line in enum {}".format(
                    filename, lr.lineno, type_name
                ),
            )
    print_enum(class_name, toks)


def read_spec_enum(type_name, prefix, class_name):
    """Read an enumeration declaration from iirs.ads."""
    read_enum(pnodes.kind_file, type_name, prefix, class_name)


def do_libghdl_nodes():
    print_file_header()
    print(dedent("""\
        from typing import TypeVar
        from ctypes import c_int32
        from pyGHDL.libghdl._types import (
            Iir,
            IirKind,
            LocationType,
            FileChecksumId,
            TimeStampId,
            SourceFileEntry,
            NameId,
            TriStateType,
            SourcePtr,
            Int32,
            Int64,
            Fp64,
            String8Id,
            Boolean,
            DirectionType,
            PSLNode,
            PSLNFA,
        )
        from pyGHDL.libghdl.vhdl.tokens import Tok

        Null_Iir = 0

        Null_Iir_List = 0
        Iir_List_All = 1

        Null_Iir_Flist = 0
        Iir_Flist_Others = 1
        Iir_Flist_All = 2

        DateType = TypeVar("DateType", bound=c_int32)
        """), end=''
    )

    do_class_kinds()
    read_spec_enum("Iir_Mode", "Iir_", "Iir_Mode")
    read_spec_enum("Scalar_Size", "", "ScalarSize")
    read_spec_enum("Iir_Staticness", "", "Iir_Staticness")
    read_spec_enum("Iir_Constraint", "", "Iir_Constraint")
    read_spec_enum("Iir_Delay_Mechanism", "Iir_", "Iir_Delay_Mechanism")
    read_spec_enum("Date_State_Type", "Date_", "DateStateType")
    read_spec_enum("Number_Base_Type", "", "NumberBaseType")
    read_spec_enum("Iir_Predefined_Functions", "Iir_Predefined_", "Iir_Predefined")
    do_iirs_subprg()


def do_libghdl_meta():
    print_file_header()
    print(dedent("""\
        from pyGHDL.libghdl import libghdl
        from pyGHDL.libghdl._types import IirKind


        # From nodes_meta
        @export
        @BindToLibGHDL("vhdl__nodes_meta__get_fields_first")
        def get_fields_first(K: IirKind) -> int:
            \"\"\"
            Return the list of fields for node :obj:`K`.

            In Ada ``Vhdl.Nodes_Meta.Get_Fields`` returns a ``Fields_Array``. To emulate
            this array access, the API provides ``get_fields_first`` and :func:`get_fields_last`.

            The fields are sorted: first the non nodes/list of nodes, then the
            nodes/lists that aren't reference, and then the reference.

            :param K: Node to get first array index from.
            \"\"\"
            return 0


        @export
        @BindToLibGHDL("vhdl__nodes_meta__get_fields_last")
        def get_fields_last(K: IirKind) -> int:
            \"\"\"
            Return the list of fields for node :obj:`K`.

            In Ada ``Vhdl.Nodes_Meta.Get_Fields`` returns a ``Fields_Array``. To emulate
            this array access, the API provides :func:`get_fields_first` and ``get_fields_last``.

            The fields are sorted: first the non nodes/list of nodes, then the
            nodes/lists that aren't reference, and then the reference.

            :param K: Node to get last array index from.
            \"\"\"
            return 0

        @export
        @BindToLibGHDL("vhdl__nodes_meta__get_field_by_index")
        def get_field_by_index(K: IirKind) -> int:
            \"\"\"\"\"\"
            return 0

        @export
        def get_field_type(*args):
            return libghdl.vhdl__nodes_meta__get_field_type(*args)

        @export
        def get_field_attribute(*args):
            return libghdl.vhdl__nodes_meta__get_field_attribute(*args)
        """), end=''
    )

    do_class_types()
    do_class_field_attributes()
    do_class_fields()
    do_types_subprg()
    do_has_subprg()


def do_libghdl_names():
    pat_name_first = re.compile(r"   Name_(\w+)\s+: constant Name_Id := (\d+);")
    pat_name_def = re.compile(r"   Name_(\w+)\s+:\s+constant Name_Id :=\s+Name_(\w+)( \+ (\d+))?;")
    dict = {}
    lr = pnodes.linereader("../std_names.ads")
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
        if line == "end Std_Names;\n":
            break
        if line.endswith(":=\n"):
            line = line.rstrip() + lr.get()
        m = pat_name_def.match(line)
        if m:
            name_def = m.group(1)
            name_ref = m.group(2)
            val = m.group(4)
            if not val:
                val = 0
            val_ref = dict.get(name_ref, None)
            if not val_ref:
                raise pnodes.ParseError(lr, "name {0} not found".format(name_ref))
            val = val_ref + int(val)
            val_max = max(val_max, val)
            dict[name_def] = val
            res.append((name_def, val))
    print_file_header(includeIntEnumUnique=False, includeBindToLibGHDL=False)
    print(dedent("""

        @export
        class Name:
        """), end=''
    )

    for n, v in res:
        # Avoid clash with Python names
        if n in ["False", "True", "None"]:
            n = "N" + n
        print("    {0} = {1}".format(n, v))


def do_libghdl_tokens():
    print_file_header(includeBindToLibGHDL=False)
    read_enum("vhdl-tokens.ads", "Token_Type", "Tok_", "Tok")


def do_libghdl_errorout():
    print_file_header()
    print(dedent("""\
        @export
        @BindToLibGHDL("errorout__enable_warning")
        def Enable_Warning(Id: int, Enable: bool) -> None:
            \"\"\"\"\"\"
        """), end=''
    )

    read_enum(
        "../errorout.ads",
        "Msgid_Type",
        "(Msgid|Warnid)_",
        "Msgid",
        g=lambda m: m.group(1) + "_" + m.group(2),
    )


pnodes.actions.update(
    {
        "class-kinds": do_class_kinds,
        "libghdl-nodes": do_libghdl_nodes,
        "libghdl-meta": do_libghdl_meta,
        "libghdl-names": do_libghdl_names,
        "libghdl-tokens": do_libghdl_tokens,
        "libghdl-elocs": do_libghdl_elocations,
        "libghdl-errorout": do_libghdl_errorout,
    }
)


def _generateCLIParser():
    return pnodes._generateCLIParser()


if __name__ == "__main__":
    pnodes.main()
