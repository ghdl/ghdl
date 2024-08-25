#!/usr/bin/env python3

"""Like pnodes but output for Rust."""

import re
import sys
from textwrap import dedent

try:
    import scripts.pnodes as pnodes
except:
    import pnodes

libname = "libghdl"


def print_enum(name, vals):
    print(f"pub enum {name} {{")
    for k in vals:
        print(f"    {k},")
    print(f"}}");

    print(f"")
    print(f"impl {name} {{")
    print(f"    const VALUES: [Self; {len(vals)}] = [")
    for k in vals:
        print(f"        Self::{k},")
    print(f"    ];");
    print(f"")
    print(f"    const IMAGES: [&'static str; {len(vals)}] = [")
    for k in vals:
        print(f"        \"{k.lower()}\",")
    print(f"    ];");
    print(f"}}")

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
    typ = "Kind"
    print(f"#[derive(PartialEq, PartialOrd)]")
    print_enum(typ, pnodes.kinds)
    print(f"impl {typ} {{")
    for k, v in pnodes.kinds_ranges.items():
        print(f"    fn is_{k.lower()}(self: Self) -> bool {{")
        print(f"        self >= {typ}::{v[0]} && self <= {typ}::{v[-1]}")
        print(f"    }}")
        print()
    print(f"}}")

def do_iirs_subprg():
    pfx = "vhdl__nodes"
    print(f'type Iir = u32;')
    print(f'type FileChecksumId = u32;')
    print(f'type TimeStampId = u32;')
    print(f'type SourceFileEntry = u32;')
    print(f'type DateType = u32;')
    print(f'type NameId = u32;')
    print(f'type SourcePtr = u32;')
    print(f'type String8Id = u32;')
    print(f'type PSLNode = u32;')
    print(f'type PSLNFA = u32;')
    print(f'type Tok = u8;') # FIXME
    print(f'pub enum TriStateType {{')
    print(f'   Unknown,')
    print(f'   False,')
    print(f'   True,')
    print(f'}}')
    print(f'pub enum DirectionType {{')
    print(f'   To,')
    print(f'   Downto,')
    print(f'}}')
    print()
    print(f'extern "C" {{')
    print(f'  #[link_name = "{pfx}__get_kind"]')
    print(f"  fn get_kind(n: u32) -> Kind;")
    print()
    print(f'  #[link_name = "{pfx}__get_location"]')
    print(f"  fn get_location(n: u32) -> u32;")
    print()
    typmap = {'TokenType': 'Tok',
              'Boolean' : 'bool',
              'Int32': 'i32',
              'Int64': 'i64',
              'Fp64': 'f64',
              }
    for k in pnodes.funcs:
        # Don't use the Iir_* subtypes (as they are not described).
        rtype = k.rtype.replace("_", "") if not k.rtype.startswith("Iir_") else "Iir"
        # Exceptions...
        rtype = typmap.get(rtype, rtype)

        name = k.name.lower()
        print(f'  #[link_name = "{pfx}__get_{name}"]')
        print(f"  fn get_{name}(n: u32) -> {rtype};")
        print()
        print(f'  #[link_name = "{pfx}__set_{name}"]')
        print(f"  fn set_{name}(n: u32, v: {rtype});")
        print()
    print(f"}}")

def do_libghdl_elocations():
    classname = "vhdl__elocations"
    print_file_header(includeIntEnumUnique=False, includeBindToLibGHDL=False)
    print("from pyGHDL.libghdl import libghdl")
    print()
    for k in pnodes.funcs:
        print(dedent(f"""
            @export
            def Get_{k.name}(obj):
                return {libname}.{classname}__get_{k.name.lower()}(obj)
            @export
            def Set_{k.name}(obj, value) -> None:
                {libname}.{classname}__set_{k.name.lower()}(obj, value)
            """)
        )


def do_class_types():
    print_enum("types", pnodes.get_types())


def do_types_subprg():
    print()
    for k in pnodes.get_types():
        print(dedent(f"""
            def Get_{k}(node, field):
                return {libname}.vhdl__nodes_meta__get_{k.lower()}(node, field)
            """)
        )


def do_has_subprg():
    print()
    for f in pnodes.funcs:
        print(dedent(f"""
            @export
            @BindToLibGHDL("vhdl__nodes_meta__has_{f.name.lower()}")
            def Has_{f.name}(kind: IirKind) -> bool:
                \"\"\"\"\"\"
            """)
        )


def do_class_field_attributes():
    print_enum("Attr", ["ANone" if a == "None" else a for a in pnodes.get_attributes()])


def do_class_fields():
    print_enum("fields", [f.name for f in pnodes.funcs])


def read_enum(filename, type_name, prefix, g=lambda m: m.group(1)):
    """Read an enumeration declaration from :param filename:."""
    pat_decl = re.compile(r"   type {0} is$".format(type_name))
    pat_enum = re.compile(r"      {0}(\w+),?( *-- .*)?$".format(prefix))
    pat_comment = re.compile(r" *-- .*$")
    lr = pnodes.linereader(filename)
    while not pat_decl.match(lr.get()):
        pass
    line = lr.get()
    if line != "     (\n":
        raise pnodes.ParseError(lr, f"{filename}:{lr.lineno}: missing open parenthesis")
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
                f"{filename}:{ lr.lineno}: incorrect line in enum {type_name}"
            )
    return toks


def read_spec_enum(type_name, prefix, class_name):
    """Read an enumeration declaration from iirs.ads."""
    toks = read_enum(pnodes.kind_file, type_name, prefix)
    print_enum(class_name, toks)


def do_nodes():
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
                raise pnodes.ParseError(lr, f"name {name_ref} not found")
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
        print(f"    {n} = {v}")


def do_libghdl_tokens():
    print_file_header(includeBindToLibGHDL=False)
    toks = read_enum("vhdl-tokens.ads", "Token_Type", "Tok_")
    print_enum("Tok", toks)


def do_errorout():
    toks = read_enum(
        "../errorout.ads",
        "Msgid_Type",
        "(Msgid|Warnid)_",
        g=lambda m: m.group(1) + "_" + m.group(2),
    )
    first_warnid = None
    for v, t in enumerate(toks):
        if t.startswith("Msgid_"):
            print(f"pub const {t.upper()}: u8 = {v};")
        elif first_warnid is None:
            first_warnid = t
            print(f"pub const MSGID_FIRST_WARNID: u8 = {v};")

    print("")
    # Remove prefix, remove '_'
    pfx="Warnid_"
    warn_toks = [t[len(pfx):].replace('_', '')
                 for t in toks if t.startswith(pfx)]
    print_enum("Warnid", warn_toks)
    print(f"pub const WARNID_USIZE: usize = {len(warn_toks)};")


pnodes.actions.update(
    {
        "class-kinds": do_class_kinds,
        "nodes": do_nodes,
        "libghdl-meta": do_libghdl_meta,
        "libghdl-names": do_libghdl_names,
        "libghdl-tokens": do_libghdl_tokens,
        "libghdl-elocs": do_libghdl_elocations,
        "errorout": do_errorout,
    }
)


def _generateCLIParser():
    return pnodes._generateCLIParser()


if __name__ == "__main__":
    pnodes.main()
