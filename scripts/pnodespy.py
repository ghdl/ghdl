#!/usr/bin/env python3

"""Like pnodes but output for Python."""

from textwrap import dedent, wrap

try:
    import scripts.pnodes as pnodes
except ImportError:
    import pnodes

libname = "libghdl"

#: Width the generated doc-strings are wrapped to, matching ``[tool.black] line-length``.
LINE_LENGTH = 120


def format_docstring(description, indent, **fields):
    """
    Render a comment block and a field list as a Python doc-string.

    The description is the comment `pnodes` captured from :file:`vhdl-nodes.ads`. It is Ada prose rather
    than ReST, so a paragraph is reflowed while a line that is indented further - the continuation of a
    grammar production, for instance - is kept as written.

    :param description: The captured comment lines, or an empty list.
    :param indent:      Indentation to put in front of every emitted line.
    :param fields:      The ``:param:``/``:returns:`` entries to append, in order.
    :returns:           The doc-string, including its triple quotes, or ``\"\"\"\"\"\"`` if there is nothing to say.
    """
    body = []
    paragraph = []

    def flush():
        if paragraph:
            body.extend(wrap(" ".join(paragraph), width=LINE_LENGTH - len(indent)))
            body.append("")
            paragraph.clear()

    for line in description:
        if not line:
            flush()
        elif line.startswith(" "):
            flush()
            body.append(line)
        else:
            paragraph.append(line)
    flush()

    while body and not body[-1]:
        body.pop()

    if fields:
        labels = {"returns": ":returns:"}
        entries = [(labels.get(name, f":param {name}:"), text) for name, text in fields.items()]
        width = max(len(label) for label, _ in entries)
        if body:
            body.append("")
        body.extend(f"{label.ljust(width)} {text}" for label, text in entries)

    if not body:
        return indent + '"""' + '"""'

    lines = ['"""'] + body + ['"""']
    return "\n".join(indent + l if l else "" for l in lines)


#: What each generated enumeration is, for its doc-string.
ENUM_DESCRIPTIONS = {
    "Iir_Kind": "The kind of an IIR node, which decides what its physical fields mean.",
    "Iir_Kinds": "The ``Iir_Kinds_*`` subtype ranges, grouping consecutive node kinds.",
    "Iir_Mode": "The mode of an interface object: ``in``, ``out``, ``inout``, ``buffer`` or ``linkage``.",
    "ScalarSize": "The storage size of a scalar type.",
    "Iir_Staticness": "How static an expression or a type is: unknown, none, globally or locally.",
    "Iir_Constraint": "How constrained a composite type is: unconstrained, partially or fully.",
    "Iir_Delay_Mechanism": "The delay mechanism of a signal assignment: ``inertial`` or ``transport``.",
    "DateStateType": "How far a design unit has been processed: extern, disk, parse or analyze.",
    "NumberBaseType": "The base a literal was written in.",
    "Iir_Predefined": "The predefined operation an implicit subprogram implements.",
    "types": "The types a field of the meta-model can have.",
    "Attr": "The access attribute of a field: a reference, a chain, or owned.",
    "fields": "Every field of the meta-model, as an enumeration.",
    "Tok": "The VHDL tokens the scanner produces.",
    "Msgid": "The warnings and errors *libghdl* can report.",
    "Name": "The predefined names *libghdl* interns at startup.",
}


def print_enum(name, vals):
    description = ENUM_DESCRIPTIONS.get(name, f"The ``{name}`` enumeration, generated from the Ada sources.")
    print(dedent(f"""

        @export
        @unique
        class {name}(IntEnum):
            \"\"\"
            {description}
            \"\"\"

        """), end=''
    )
    for n, k in enumerate(vals):
        if k == "None":
            k = "PNone"
        print(f"    {k} = {n}")


def print_file_header(includeIntEnumUnique: bool = True, includeBindToLibGHDL: bool = True, description: str = None):
    print(dedent(f"""\
# Auto generated Python source file from Ada sources
# Call 'make' in 'src/vhdl' to regenerate:
#
{'\"\"\"\n' + description + '\n\"\"\"\n\n' if description else ''}\
{'from enum import IntEnum, unique\n\n' if includeIntEnumUnique else ''}\
from pyTooling.Decorators import export
{'\nfrom pyGHDL.libghdl._decorator import BindToLibGHDL\n' if includeBindToLibGHDL else ''}\
    """)
    )


def do_class_kinds():
    print_enum(pnodes.prefix_name.rstrip("_"), pnodes.kinds)
    print(dedent("""

        @export
        class Iir_Kinds:
            \"\"\"
            The ``Iir_Kinds_*`` subtype ranges, each listing the consecutive node kinds it covers.
            \"\"\"

        """), end=''
    )
    for k, v in pnodes.kinds_ranges.items():
        print(f"    {k} = [")
        for e in v:
            print(f"        Iir_Kind.{e},")
        print("    ]")
        print()


def do_iirs_subprg():
    classname = "vhdl__nodes"
    print(dedent(f"""

        @export
        @BindToLibGHDL("{classname}__get_kind")
        def Get_Kind(node: Iir) -> IirKind:
            \"\"\"Get node kind.\"\"\"
            return 0  # pragma: no cover

        @export
        @BindToLibGHDL("{classname}__get_location")
        def Get_Location(node: Iir) -> LocationType:
            \"\"\"
            Get the source location of a node.

            :param node: The node to read the location of.
            :returns:    The node's location, to be resolved with :mod:`pyGHDL.libghdl.files_map`.
            \"\"\"
            return 0  # pragma: no cover
        """)
    )
    for k in pnodes.funcs:
        # Don't use the Iir_* subtypes (as they are not described).
        rtype = k.rtype.replace("_", "") if not k.rtype.startswith("Iir_") else "Iir"
        # Exceptions...
        if rtype == "TokenType":
            rtype = "Tok"

        print()
        print("@export")
        print(f'@BindToLibGHDL("{classname}__get_{k.name.lower()}")')
        print(f"def Get_{k.name}(obj: Iir) -> {rtype}:")
        print(format_docstring(
            k.description,
            "    ",
            obj=f"The node to read the ``{k.name}`` field of.",
            returns=f"The node's ``{k.name}`` field.",
        ))
        print("    return 0  # pragma: no cover")
        print("@export")
        print(f'@BindToLibGHDL("{classname}__set_{k.name.lower()}")')
        print(f"def Set_{k.name}(obj: Iir, value: {rtype}) -> None:")
        print(format_docstring(
            k.description,
            "    ",
            obj=f"The node to write the ``{k.name}`` field of.",
            value=f"The value to write into the ``{k.name}`` field.",
        ))
        print()


def do_libghdl_elocations():
    classname = "vhdl__elocations"
    print_file_header(
            includeIntEnumUnique=False,
            includeBindToLibGHDL=False,
            description="Python binding for the Ada package ``Vhdl.Elocations`` in *libghdl*.\n\n"
            "The extended source locations of a node - the position of each keyword and delimiter - kept separately from the\n"
            "node itself, so an ordinary node does not pay for them.",
        )
    print("from pyGHDL.libghdl import libghdl")
    print()
    for k in pnodes.funcs:
        print()
        print("@export")
        print(f"def Get_{k.name}(obj):")
        print(format_docstring(
            k.description,
            "    ",
            obj=f"The node to read the ``{k.name}`` location of.",
            returns=f"The node's ``{k.name}`` location.",
        ))
        print(f"    return {libname}.{classname}__get_{k.name.lower()}(obj)")
        print("@export")
        print(f"def Set_{k.name}(obj, value) -> None:")
        print(format_docstring(
            k.description,
            "    ",
            obj=f"The node to write the ``{k.name}`` location of.",
            value=f"The location to write into the ``{k.name}`` field.",
        ))
        print(f"    {libname}.{classname}__set_{k.name.lower()}(obj, value)")
        print()


def do_class_types():
    print_enum("types", pnodes.get_types())


def do_types_subprg():
    print()
    for k in pnodes.get_types():
        print(dedent(f"""
            def Get_{k}(node, field):
                \"\"\"
                Read a field of type ``{k}`` from a node, through the meta-model.

                :param node:  The node to read the field of.
                :param field: The field to read, from :class:`fields`.
                :returns:     The field's value.
                \"\"\"
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
                \"\"\"
                Check whether a node of the given kind has a ``{f.name}`` field.

                :param kind: The node kind to check.
                :returns:    ``True`` if a node of that kind has the field.
                \"\"\"
            """)
        )


def do_class_field_attributes():
    print_enum("Attr", ["ANone" if a == "None" else a for a in pnodes.get_attributes()])


def do_class_fields():
    print_enum("fields", [f.name for f in pnodes.funcs])


def read_spec_enum(type_name, prefix, class_name):
    """Read an enumeration declaration from iirs.ads."""
    enum = pnodes.read_enum(pnodes.kind_file, type_name, prefix)
    print_enum(class_name, enum)


def do_libghdl_nodes():
    print_file_header(
            description="Python binding for the Ada package ``Vhdl.Nodes`` in *libghdl*.\n\n"
            "The IIR tree: the node kinds, the enumerations their fields use, and the accessor pair for every field.\n"
            "See :ref:`INT:AST` for what a node is and how the fields are addressed.",
        )
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

        __all__ = [
            "Null_Iir",
            "Null_Iir_List",
            "Iir_List_All",
            "Null_Iir_Flist",
            "Iir_Flist_Others",
            "Iir_Flist_All",
        ]

        Null_Iir = 0
        \"\"\"
        Null element for an IIR node reference.
        \"\"\"

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
    print_file_header(
            description="Python binding for the Ada package ``Vhdl.Nodes_Meta`` in *libghdl*.\n\n"
            "The meta-model: which fields a node kind has, of what type, and with what access attribute. It is what lets an\n"
            "algorithm walk any node without knowing its kind.",
        )
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
            return 0  # pragma: no cover


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
            return 0  # pragma: no cover

        @export
        @BindToLibGHDL("vhdl__nodes_meta__get_field_by_index")
        def get_field_by_index(K: IirKind) -> int:
            \"\"\"
            Get the field at a given index of the fields array.

            :param K: The index into the fields array.
            :returns: The field at that index.
            \"\"\"
            return 0  # pragma: no cover

        @export
        def get_field_type(*args):
            \"\"\"
            Get the type of a field.

            :param args: The field to query, from :class:`fields`.
            :returns:    The field's type, from :class:`types`.
            \"\"\"
            return libghdl.vhdl__nodes_meta__get_field_type(*args)

        @export
        def get_field_attribute(*args):
            \"\"\"
            Get the access attribute of a field.

            :param args: The field to query, from :class:`fields`.
            :returns:    The field's attribute, from :class:`Attr`.
            \"\"\"
            return libghdl.vhdl__nodes_meta__get_field_attribute(*args)
        """), end=''
    )

    do_class_types()
    do_class_field_attributes()
    do_class_fields()
    do_types_subprg()
    do_has_subprg()


def do_libghdl_names():
    res = pnodes.read_std_names()
    print_file_header(
            includeIntEnumUnique=False,
            includeBindToLibGHDL=False,
            description="Python binding for the Ada package ``Std_Names`` in *libghdl*.\n\n"
            "The predefined names *libghdl* interns at startup, so they can be compared by identifier rather than by text.",
        )
    print(dedent("""

        @export
        class Name:
            \"\"\"
            The predefined names *libghdl* interns at startup, as identifier values.
            \"\"\"

        """), end=''
    )

    for n, v in res:
        # Avoid clash with Python names
        if n in ["False", "True", "None"]:
            n = "N" + n
        print(f"    {n} = {v}")


def do_libghdl_tokens():
    print_file_header(
            includeBindToLibGHDL=False,
            description="Python binding for the Ada package ``Vhdl.Tokens`` in *libghdl*.\n\n"
            "The tokens the VHDL scanner produces.",
        )
    enum = pnodes.read_enum("vhdl-tokens.ads", "Token_Type", "Tok_")
    print_enum("Tok", enum)


def do_libghdl_errorout():
    print_file_header(
            description="Python binding for the Ada package ``Errorout`` in *libghdl*.\n\n"
            "The warnings and errors *libghdl* can report, and the subprograms to enable or disable them.",
        )
    print(dedent("""\
        @export
        @BindToLibGHDL("errorout__enable_warning")
        def Enable_Warning(Id: int, Enable: bool) -> None:
            \"\"\"
            Enable or disable a warning.

            :param Id:     The warning to change.
            :param Enable: ``True`` to enable the warning, ``False`` to disable it.
            \"\"\"
        """), end=''
    )

    enum = pnodes.read_enum(
        "../errorout.ads",
        "Msgid_Type",
        "(Msgid|Warnid)_",
        g=lambda m: m.group(1) + "_" + m.group(2),
    )
    print_enum("Msgid", enum)


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
