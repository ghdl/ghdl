#!/usr/bin/env python3

"""Like pnodes but output for Rust."""

import re
import sys
from textwrap import dedent

try:
    import scripts.pnodes as pnodes
except:
    import pnodes


def print_enum(name, vals):
    if len(vals) < 256:
        repr = 8
    elif len(vals) < 65536:
        repr = 16
    else:
        repr = 32
    print(f"#[repr(u{repr})]")
    print(f"pub enum {name} {{")
    for k in vals:
        print(f"    {k},")
    print(f"}}");

    print(f"")
    print(f"impl {name} {{")
    print(f"    pub const VALUES: [Self; {len(vals)}] = [")
    for k in vals:
        print(f"        Self::{k},")
    print(f"    ];");
    print(f"")
    print(f"    pub const IMAGES: [&'static str; {len(vals)}] = [")
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
    print(f"#[derive(Copy, Clone, PartialEq, PartialOrd)]")
    print_enum(typ, pnodes.kinds)
    print(f"impl {typ} {{")
    for k, v in pnodes.kinds_ranges.items():
        print(f"    fn is_{k.lower()}(self: Self) -> bool {{")
        print(f"        self >= {typ}::{v[0]} && self <= {typ}::{v[-1]}")
        print(f"    }}")
        print()
    print(f"}}")

def common_subprg_import_header(pfx, nname):
    print(f'extern "C" {{')
    print(f'    #[link_name = "{pfx}__create_{nname}"]')
    print(f"    fn create(k: Kind) -> Node;")
    print()
    print(f'    #[link_name = "{pfx}__get_kind"]')
    print(f"    fn get_kind(n: Node) -> Kind;")
    print()
    print(f'    #[link_name = "{pfx}__get_location"]')
    print(f"    fn get_location(n: Node) -> Location;")
    print()
    print(f'    #[link_name = "{pfx}__set_location"]')
    print(f"    fn set_location(n: Node, loc: Location);")
    print()

def common_subprg_import(pnodes, pfx, nname, convert):
    common_subprg_import_header(pfx, nname)
    for k in pnodes.funcs:
        rtype = convert(k.rtype)
        name = k.name.lower()
        print(f'    #[link_name = "{pfx}__get_{name}"]')
        print(f"    fn get_{name}(n: Node) -> {rtype};")
        print()
        print(f'    #[link_name = "{pfx}__set_{name}"]')
        print(f"    fn set_{name}(n: Node, v: {rtype});")
        print()


def common_subprg_impl_header():
    print()
    print(f'impl Node {{')
    print(f'    pub const NULL: Self = Node(0);')
    print()
    print(f'    pub fn new(k: Kind) -> Self {{')
    print(f'        unsafe {{ create(k) }}')
    print(f'    }}')
    print()
    print(f'    pub fn kind(self: Self) -> Kind {{')
    print(f'        unsafe {{ get_kind(self) }}')
    print(f'    }}')
    print()
    print(f'    pub fn location(self: Self) -> Location {{')
    print(f'        unsafe {{ get_location(self) }}')
    print(f'    }}')
    print()
    print(f'    pub fn set_location(self: Self, loc: Location) {{')
    print(f'        unsafe {{ set_location(self, loc) }}')
    print(f'    }}')
    print()

def common_subprg_impl(pnodes, convert):
    namemap = {'type': 'typed'}
    common_subprg_impl_header()
    for k in pnodes.funcs:
        rtype = convert(k.rtype)

        name = k.name.lower()
        rname = namemap.get(name, name)
        print(f'    pub fn {rname}(self: Self) -> {rtype} {{')
        print(f'        unsafe {{ get_{name}(self) }}')
        print(f'    }}')
        print()
        print(f'    pub fn set_{rname}(self: Self, v : {rtype}) {{')
        print(f'        unsafe {{ set_{name}(self, v); }}')
        print(f'    }}')
        print()
    print(f"}}")

def convert_vhdl_type_name(rtype):
    typmap = {'TokenType': 'Tok',
              'Boolean' : 'bool',
              'Int32': 'i32',
              'Int64': 'i64',
              'Fp64': 'f64',
              'Iir': 'Node',
              }
    # Don't use the Iir_* subtypes (as they are not described).
    rtype = rtype.replace("Iir_", "")
    rtype = rtype.replace("_", "")
    rtype = rtype if not rtype.startswith("Kind_") else "Iir"
    # Exceptions...
    rtype = typmap.get(rtype, rtype)
    return rtype

def do_vhdl_subprg():
    pfx = "vhdl__nodes"
    print(f'#[repr(transparent)]')
    print(f'#[derive(Copy, Clone, PartialEq)]')
    print(f'pub struct Node(u32);')
    print()
    print(f'#[repr(transparent)]')
    print(f'#[derive(Copy, Clone, PartialEq)]')
    print(f'pub struct Flist(u32);')
#    print(f'type Iir = u32;')
#    print(f'type FileChecksumId = u32;')
#    print(f'type TimeStampId = u32;')
#    print(f'type SourceFileEntry = u32;')
#    print(f'type DateType = u32;')
#    print(f'type NameId = u32;')
#    print(f'type SourcePtr = u32;')
#    print(f'type String8Id = u32;')
    print(f'type PSLNode = u32;')
    print(f'type PSLNFA = u32;')
    print(f'type Tok = u8;') # FIXME
    print(f'type List = u32;')
    print(f'type Index32 = i32;')
    print()
    print(f'#[repr(u8)]')
    print(f'pub enum TriStateType {{')
    print(f'    Unknown,')
    print(f'    False,')
    print(f'    True,')
    print(f'}}')
    print()
    print(f'#[repr(u8)]')
    print(f'pub enum DirectionType {{')
    print(f'    To,')
    print(f'    Downto,')
    print(f'}}')
    print()

    common_subprg_import(pnodes, pfx, "iir", convert_vhdl_type_name)
    print('    #[link_name = "vhdl__flists__create_flist"]')
    print('    fn create_flist(len: u32) -> Flist;')
    print()
    print('    #[link_name = "vhdl__flists__set_nth_element"]')
    print('    fn set_nth_element(flist: Flist, idx: u32, el: Node);')
    print()
    print('    #[link_name = "vhdl__flists__get_nth_element"]')
    print('    fn get_nth_element(flist: Flist, idx: u32) -> Node;')
    print()
    print(f"}}")

    common_subprg_impl(pnodes, convert_vhdl_type_name)

    print('impl Flist {')
    print('    pub fn new(len: u32) -> Self {')
    print('        unsafe { create_flist(len) }')
    print('    }')
    print()
    print('    pub fn set(self: Self, idx: u32, el: Node) {')
    print('        unsafe { set_nth_element(self, idx, el); }')
    print('    }')
    print('}')


def do_class_types():
    print_enum("types", pnodes.get_types())


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


def read_spec_enum(type_name, prefix, class_name):
    """Read an enumeration declaration from kind file."""
    toks = pnodes.read_enum(pnodes.kind_file, type_name, prefix)
    print_enum(class_name, toks)


def do_vhdl_nodes():
    """Convert vhdl-nodes.ads"""
    print("#![allow(non_camel_case_types, dead_code)]")
    print("use crate::types::*;")
    print("use crate::files_map::{Location, SourceFileEntry};")
    print("use crate::NameId;")
    print()
    do_class_kinds()
    read_spec_enum("Iir_Mode", "Iir_", "Mode")
    read_spec_enum("Scalar_Size", "", "ScalarSize")
    read_spec_enum("Iir_Staticness", "", "Staticness")
    read_spec_enum("Iir_Constraint", "", "Iir_Constraint")
    read_spec_enum("Iir_Delay_Mechanism", "Iir_", "DelayMechanism")
    read_spec_enum("Iir_Pure_State", "", "PureState")
    read_spec_enum("Iir_All_Sensitized", "", "AllSensitized")
    read_spec_enum("Iir_Signal_Kind", "Iir_", "SignalKind")
    read_spec_enum("Iir_Constraint", "", "Constraint")
    read_spec_enum("Iir_Force_Mode", "Iir_", "ForceMode")
    read_spec_enum("Date_State_Type", "Date_", "DateStateType")
    read_spec_enum("Number_Base_Type", "", "NumberBaseType")
    read_spec_enum("Iir_Predefined_Functions", "Iir_Predefined_", "PredefinedFunctions")
    do_vhdl_subprg()


def convert_verilog_type_name(rtype):
    typmap = {'Boolean' : 'bool',
              'Int32': 'i32',
              'Int64': 'i64',
              'Fp64': 'f64',
              'Uns32': 'u32',
              }
    # Rename '_' and 'Type'
    rtype = rtype.replace("_", "").replace("Type","")
    # Exceptions...
    rtype = typmap.get(rtype, rtype)
    return rtype


def do_verilog_subprg():
    pfx = "verilog__nodes"
    print()
    common_subprg_import(pnodes, pfx, "node", convert_verilog_type_name)
    print(f"}}")

    common_subprg_impl(pnodes, convert_verilog_type_name)


def do_verilog_nodes():
    """Convert verilog-nodes.ads"""
    print("#![allow(non_camel_case_types, dead_code)]")
    print()
    print("use crate::types::*;")
    print("use crate::verilog::types::*;")
    print("use crate::files_map::Location;")
    print("use crate::NameId;")
    print()
    print("#[repr(transparent)]")
    print("#[derive(Copy, Clone, PartialEq)]")
    print("pub struct Node(u32);")
    print()
    do_class_kinds()
    read_spec_enum("Violation_Type", "Violation_", "Violation")
    read_spec_enum("Visibility_Type", "Visibility_", "Visibility")
    read_spec_enum("DPI_Spec_Type", "DPI_", "DPISpec")
    read_spec_enum("Edge_Type", "Edge_", "Edge")
    read_spec_enum("Base_Type", "Base_", "Base")
    read_spec_enum("Lifetime_Type", "Life_", "Lifetime")
    read_spec_enum("Join_Type", "Join_", "Join")
    read_spec_enum("Udp_Symbol", "", "UdpSymbol")
    read_spec_enum("Polarity_Type", "Polarity_", "Polarity")
    read_spec_enum("Udp_Kind", "Udp_", "UdpKind")
    read_spec_enum("Conv_Ops", "Convop_", "ConvOps")
    read_spec_enum("Binary_Ops", "Binop_", "BinaryOps")
    read_spec_enum("Unary_Ops", "Unop_", "UnaryOps")
    print("pub type SysTfId = u32;")
    do_verilog_subprg()


def do_std_names():
    res = pnodes.read_std_names()
    print('#![allow(dead_code)]')
    print('use crate::NameId;')
    for n, v in res:
#        # Avoid clash with Python names
#        if n in ["False", "True", "None"]:
#            n = "N" + n
        print(f"pub const {n.upper()}: NameId = NameId({v});")


def do_errorout():
    print("#![allow(dead_code)]")
    toks = pnodes.read_enum(
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
        "vhdl-nodes": do_vhdl_nodes,
        "errorout": do_errorout,
        "verilog-nodes": do_verilog_nodes,
        "std_names": do_std_names,
        "class-kinds": do_class_kinds,
    }
)


def _generateCLIParser():
    return pnodes._generateCLIParser()


if __name__ == "__main__":
    pnodes.main()
