#!/usr/bin/env python3

"""Like pnodes but output for Python"""

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
        class {0}:
        """).format(name), end=''
    )
    for n, k in enumerate(vals):
        if k == "None":
            k = "PNone"
        print("    {0} = {1}".format(k, n))


def print_file_header():
    print(dedent("""\
        # Auto generated Python source file from Ada sources
        # Call 'make' in 'src/vhdl' to regenerate:
        #
        from pydecor import export
    """), end='')


def do_class_kinds():
    print_enum(pnodes.prefix_name.rstrip("_"), pnodes.kinds)
    print(dedent("""

        @export
        class Iir_Kinds:
        """
    ), end='')
    for k, v in pnodes.kinds_ranges.items():
        print("    {0} = [".format(k))
        for e in v:
            print("        Iir_Kind.{},".format(e))
        print("    ]")
        print()


def do_iirs_subprg():
    classname = "vhdl__nodes"
    print(dedent("""
        Get_Kind = {libname}.{classname}__get_kind
        Get_Location = {libname}.{classname}__get_location
        """).format(libname=libname, classname=classname)
    )
    for k in pnodes.funcs:
        print(dedent("""\
            Get_{kname} = {libname}.{classname}__get_{kname_lower}
            Set_{kname} = {libname}.{classname}__set_{kname_lower}
            """).format(kname=k.name, kname_lower=k.name.lower(), libname=libname, classname=classname)
        )


def do_libghdl_elocations():
    classname = "vhdl__elocations"
    print_file_header()
    print("from pyGHDL.libghdl import libghdl")
    print()
    for k in pnodes.funcs:
        print(dedent("""\
            Get_{kname} = {libname}.{classname}__get_{kname_lower}
            Set_{kname} = {libname}.{classname}__set_{kname_lower}
            """).format(kname=k.name, kname_lower=k.name.lower(), libname=libname, classname=classname)
        )


def do_class_types():
    print_enum("types", pnodes.get_types())


def do_types_subprg():
    print()
    for k in pnodes.get_types():
        print()
        print("Get_{0} = {1}.vhdl__nodes_meta__get_{2}".format(k, libname, k.lower()))


def do_has_subprg():
    print()
    for f in pnodes.funcs:
        print()
        print("Has_{0} =\\".format(f.name))
        print("    {0}.vhdl__nodes_meta__has_{1}".format(libname, f.name.lower()))


def do_class_field_attributes():
    print_enum("Attr", ["ANone" if a == "None" else a for a in pnodes.get_attributes()])


def do_class_fields():
    print_enum("fields", [f.name for f in pnodes.funcs])


def read_enum(filename, type_name, prefix, class_name, g=lambda m: m.group(1)):
    """Read an enumeration declaration from :param filename:"""
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
    """Read an enumeration declaration from iirs.ads"""
    read_enum(pnodes.kind_file, type_name, prefix, class_name)


def do_libghdl_nodes():
    print_file_header()
    print(dedent("""\
        from pyGHDL.libghdl import libghdl

        Null_Iir = 0

        Null_Iir_List = 0
        Iir_List_All = 1

        Null_Iir_Flist = 0
        Iir_Flist_Others = 1
        Iir_Flist_All = 2
        """), end='')

    do_class_kinds()
    read_spec_enum("Iir_Mode", "Iir_", "Iir_Mode")
    read_spec_enum("Iir_Staticness", "", "Iir_Staticness")
    read_spec_enum("Iir_Constraint", "", "Iir_Constraint")
    read_spec_enum("Iir_Delay_Mechanism", "Iir_", "Iir_Delay_Mechanism")
    read_spec_enum("Date_State_Type", "Date_", "Date_State")
    read_spec_enum("Iir_Predefined_Functions", "Iir_Predefined_", "Iir_Predefined")
    do_iirs_subprg()


def do_libghdl_meta():
    print_file_header()
    print(dedent("""\
        from pyGHDL.libghdl import libghdl
        from pyGHDL.libghdl._types import IirKind


        # From nodes_meta
        get_fields_first = libghdl.vhdl__nodes_meta__get_fields_first

        get_fields_last = libghdl.vhdl__nodes_meta__get_fields_last

        get_field_by_index = libghdl.vhdl__nodes_meta__get_field_by_index

        get_field_type = libghdl.vhdl__nodes_meta__get_field_type

        get_field_attribute = libghdl.vhdl__nodes_meta__get_field_attribute
        """), end='')

    do_class_types()
    do_class_field_attributes()
    do_class_fields()
    do_types_subprg()
    do_has_subprg()


def do_libghdl_names():
    pat_name_first = re.compile(r"   Name_(\w+)\s+: constant Name_Id := (\d+);")
    pat_name_def = re.compile(
        r"   Name_(\w+)\s+:\s+constant Name_Id :=\s+Name_(\w+)( \+ (\d+))?;"
    )
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
    print_file_header()
    print(dedent("""

        @export
        class Name:
        """), end='')

    for n, v in res:
        # Avoid clash with Python names
        if n in ["False", "True", "None"]:
            n = "N" + n
        print("    {0} = {1}".format(n, v))


def do_libghdl_tokens():
    print_file_header()
    read_enum("vhdl-tokens.ads", "Token_Type", "Tok_", "Tok")


def do_libghdl_errorout():
    print_file_header()
    print(dedent("""\
        from pyGHDL.libghdl import libghdl

        @export
        def Enable_Warning(Id: int, Enable: bool) -> None:
            libghdl.errorout__enable_warning(Id, Enable)
        """), end='')

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
