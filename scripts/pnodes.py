#!/usr/bin/env python3

import re
import sys
from argparse import ArgumentParser

field_file = "nodes.ads"
kind_file = "iirs.ads"
node_file = "iirs.ads"
template_file = "iirs.adb.in"
meta_base_file = "nodes_meta"
prefix_name = "Iir_Kind_"
prefix_range_name = "Iir_Kinds_"
type_name = "Iir_Kind"
node_type = "Iir"
conversions = ["uc", "pos", "grp"]


class FuncDesc:
    def __init__(self, name, fields, conv, acc, pname, ptype, rname, rtype):
        self.name = name
        self.fields = fields  # List of physical fields used
        self.conv = conv
        self.acc = acc  # access: Chain, Chain_Next, Ref, Of_Ref, Maybe_Ref,
        #                 Forward_Ref, Maybe_Forward_Ref
        self.pname = pname  # Parameter mame
        self.ptype = ptype  # Parameter type
        self.rname = rname  # value name (for procedure)
        self.rtype = rtype  # value type


class NodeDesc:
    def __init__(self, name, format, fields, attrs):
        self.name = name
        self.format = format
        self.fields = fields  # {field: FuncDesc} dict, defined for all fields
        self.attrs = attrs  # A {attr: FuncDesc} dict
        self.order = []  # List of fields name, in order of appearance.


class line:
    def __init__(self, string, no):
        self.l = string
        self.n = no


class EndOfFile(Exception):
    def __init__(self, filename):
        self.filename = filename

    def __str__(self):
        return "end of file " + self.filename


class linereader:
    def __init__(self, filename):
        self.filename = filename
        self.f = open(filename)
        self.lineno = 0
        self.l = ""

    def get(self):
        self.l = self.f.readline()
        if not self.l:
            raise EndOfFile(self.filename)
        self.lineno = self.lineno + 1
        return self.l


class ParseError(Exception):
    def __init__(self, lr, msg):
        self.lr = lr
        self.msg = msg

    def __str__(self):
        return ("Parse error at {lr.filename}:{lr.lineno}: {msg}".format(lr=self.lr, msg=self.msg))


# Return fields description.
# This is a dictionary.  The keys represent the possible format of a node.
# The values are dictionaries representing fields.  Keys are fields name, and
# values are fields type.
def read_fields(file):
    fields = {}
    formats = []
    lr = linereader(file)

    #  Search for 'type Format_Type is'
    while lr.get() != "   type Format_Type is\n":
        pass

    # Skip '('
    if lr.get() != "     (\n":
        raise Exception("no open parenthesis after Format_Type")

    # Read formats
    l = lr.get()
    pat_field_name = re.compile("      Format_(\w+),?\n")
    while l != "     );\n":
        m = pat_field_name.match(l)
        if m is None:
            print(l)
            raise Exception("bad literal within Format_Type")
        name = m.group(1)
        formats.append(name)
        fields[name] = {}
        l = lr.get()

    # Read fields
    l = lr.get()
    pat_fields = re.compile("   -- Fields of Format_(\w+):\n")
    pat_field_desc = re.compile("   --   (\w+) : (\w+).*\n")
    common_desc = {}

    # Read until common fields.
    while l != "   -- Common fields are:\n":
        l = lr.get()
    format_name = "Common"
    nbr_formats = 0

    while True:
        # 1) Read field description
        l = lr.get()
        desc = common_desc.copy()
        while True:
            m = pat_field_desc.match(l)
            if m is None:
                break
            desc[m.group(1)] = m.group(2)
            l = lr.get()
            # print 'For: ' + format_name + ': ' + m.group(1)

        # 2) Disp
        if format_name == "Common":
            common_desc = desc
        else:
            fields[format_name] = desc

        # 3) Read next format
        if l == "\n":
            if nbr_formats == len(fields):
                break
            else:
                l = lr.get()

        # One for a format
        m = pat_fields.match(l)
        if m is not None:
            format_name = m.group(1)
            if format_name not in fields:
                raise ParseError(lr, "Format " + format_name + " is unknown")
            nbr_formats = nbr_formats + 1
        else:
            raise ParseError(lr, "unhandled format line")

    return (formats, fields)


# Read kinds and kinds ranges.
def read_kinds(filename):
    lr = linereader(filename)
    kinds = []
    #  Search for 'type Iir_Kind is'
    while lr.get() != "   type " + type_name + " is\n":
        pass
    # Skip '('
    if lr.get() != "     (\n":
        raise ParseError(lr, 'no open parenthesis after "type ' + type_name + '"')

    # Read literals
    pat_node = re.compile("      " + prefix_name + "(\w+),?( +-- .*)?\n")
    pat_comment = re.compile("( +-- .*)?\n")
    while True:
        l = lr.get()
        if l == "     );\n":
            break
        m = pat_node.match(l)
        if m:
            kinds.append(m.group(1))
            continue
        m = pat_comment.match(l)
        if not m:
            raise ParseError(lr, "Unknown line within kind declaration")

    # Check subtypes
    pat_subtype = re.compile("   subtype " + r"(\w+) is " + type_name + " range\n")
    pat_first = re.compile("     " + prefix_name + r"(\w+) ..\n")
    pat_last = re.compile("     " + prefix_name + r"(\w+);\n")
    pat_middle = re.compile("   --" + prefix_name + r"(\w+)\n")
    kinds_ranges = {}
    while True:
        l = lr.get()
        # Start of methods is also end of subtypes.
        if l == "   -- General methods.\n":
            break
        # Found a subtype.
        m = pat_subtype.match(l)
        if m:
            # Check first bound
            name = m.group(1)
            if not name.startswith(prefix_range_name):
                raise ParseError(lr, "incorrect prefix for subtype")
            name = name[len(prefix_range_name):]
            l = lr.get()
            mf = pat_first.match(l)
            if not mf:
                raise ParseError(lr, "badly formatted first bound of subtype")
            first = kinds.index(mf.group(1))
            idx = first
            has_middle = None
            # Read until last bound
            while True:
                l = lr.get()
                ml = pat_middle.match(l)
                if ml:
                    # Check element in the middle
                    n = ml.group(1)
                    if n not in kinds:
                        raise ParseError(lr, "unknown kind " + n + " in subtype")
                    if kinds.index(n) != idx + 1:
                        raise ParseError(
                            lr, "missing " + kinds[idx + 1] + " in subtype"
                        )
                    has_middle = True
                    idx = idx + 1
                else:
                    # Check last bound
                    ml = pat_last.match(l)
                    if ml:
                        last = kinds.index(ml.group(1))
                        if last != idx + 1 and has_middle:
                            raise ParseError(
                                lr, "missing " + kinds[idx] + " in subtype"
                            )
                        break
                    raise ParseError(lr, "unhandled line in subtype")
            kinds_ranges[name] = kinds[first: last + 1]
    return (kinds, kinds_ranges)


# Read functions
def read_methods(filename):
    lr = linereader(filename)
    # Note: this is a list so that the output is deterministic.
    # Duplicates are not detected, but they will be by the Ada compiler.
    # TODO: use an ordered dict ?
    funcs = []
    pat_field = re.compile(r"   --  Field: ([\w,]+)( \w+)?( \(\w+\))?\n")
    pat_conv = re.compile(r"^ \((\w+)\)$")
    pat_func = re.compile(r"   function Get_(\w+) \((\w+) : (\w+)\) return (\w+);\n")
    pat_proc = re.compile(r"   procedure Set_(\w+) \((\w+) : (\w+); (\w+) : (\w+)\);\n")
    pat_end = re.compile("end [A-Za-z.]+;\n")
    while True:
        l = lr.get()
        # Start of methods
        if l == "   -- General methods.\n":
            break
    while True:
        l = lr.get()
        if pat_end.match(l):
            break
        m = pat_field.match(l)
        if m:
            fields = m.group(1).split(",")
            # Extract access modifier
            acc = m.group(2)
            if acc:
                acc = acc.strip()
            # Extract conversion
            conv = m.group(3)
            if conv:
                mc = pat_conv.match(conv)
                if not mc:
                    raise ParseError(lr, "conversion ill formed")
                conv = mc.group(1)
                if conv not in conversions:
                    raise ParseError(lr, "unknown conversion " + conv)
            else:
                conv = None
            if len(fields) > 1 and conv != "grp":
                raise ParseError(lr, "bad conversion for multiple fields")
            # Read function
            l = lr.get()
            mf = pat_func.match(l)
            if not mf:
                raise ParseError(lr, "function declaration expected after Field")
            # Read procedure
            l = lr.get()
            mp = pat_proc.match(l)
            if not mp:
                raise ParseError(lr, "procedure declaration expected after function")
            # Consistency check between function and procedure
            if mf.group(1) != mp.group(1):
                raise ParseError(lr, "function and procedure name mismatch")
            if mf.group(2) != mp.group(2):
                raise ParseError(lr, "parameter name mismatch with function")
            if mf.group(3) != mp.group(3):
                raise ParseError(lr, "parameter type mismatch with function")
            if mf.group(4) != mp.group(5):
                raise ParseError(lr, "result type mismatch with function")
            funcs.append(
                FuncDesc(
                    mf.group(1),
                    fields,
                    conv,
                    acc,
                    mp.group(2),
                    mp.group(3),
                    mp.group(4),
                    mp.group(5),
                )
            )

    return funcs


# Read description for one node
# LR is the line reader.  NAMES is the list of (node name, format)
#  (one description may describe several nodes).
# A comment start at column 2 or 4 or later.
def read_nodes_fields(lr, names, fields, nodes, funcs_dict):
    pat_only = re.compile("   -- Only for " + prefix_name + "(\w+):\n")
    pat_only_bad = re.compile("   -- *Only for.*\n")
    pat_field = re.compile("   --   Get/Set_(\w+) \((Alias )?([\w,]+)\)\n")
    pat_comment = re.compile("   --(|  [^ ].*|    .*)\n")

    # Create nodes
    cur_nodes = []
    for (nm, fmt) in names:
        if fmt not in fields:
            raise ParseError(lr, 'unknown format "{}"'.format(fmt))
        n = NodeDesc(nm, fmt, {x: None for x in fields[fmt]}, {})
        nodes[nm] = n
        cur_nodes.append(n)

    # Skip comments
    l = lr.l
    while pat_comment.match(l):
        l = lr.get()

    # Look for fields
    while l != "\n":
        # Skip comments
        while pat_comment.match(l):
            l = lr.get()

        # Handle 'Only ...'
        m = pat_only.match(l)
        if m:
            only_nodes = []
            while True:
                name = m.group(1)
                n = nodes.get(name, None)
                if n is None:
                    raise ParseError(lr, "node is unknown")
                if n not in cur_nodes:
                    raise ParseError(lr, "node not currently described")
                only_nodes.append(n)
                l = lr.get()
                m = pat_only.match(l)
                if not m:
                    break
        else:
            # By default a field applies to all nodes.
            only_nodes = cur_nodes

        # Skip comments
        while pat_comment.match(l):
            l = lr.get()

        # Handle field: '--  Get/Set_FUNC (Alias? FIELD)'
        m = pat_field.match(l)
        if not m:
            if pat_only_bad.match(l):
                raise ParseError(lr, "misleading 'Only for' comment")
            else:
                raise ParseError(lr, "bad line in node description")

        func = m.group(1)
        alias = m.group(2)
        fields = m.group(3).split(",")

        # Check the function exists and if the field is correct.
        if func not in funcs_dict:
            raise ParseError(lr, "unknown function")
        func = funcs_dict[func]
        if func.fields != fields:
            raise ParseError(lr, "fields mismatch")

        for c in only_nodes:
            for f in fields:
                if f not in c.fields:
                    raise ParseError(lr, "field " + f + " does not exist in node")
            if not alias:
                for f in fields:
                    if c.fields[f]:
                        raise ParseError(lr, "field " + f + " already used")
                    c.fields[f] = func
                    c.order.append(f)
            c.attrs[func.name] = func

        l = lr.get()


def read_nodes(filename, kinds, kinds_ranges, fields, funcs):
    """Read description for all nodes."""
    lr = linereader(filename)
    funcs_dict = {x.name: x for x in funcs}
    nodes = {}

    # Skip until start
    while lr.get() != "   -- Start of " + type_name + ".\n":
        pass

    pat_decl = re.compile("   -- " + prefix_name + "(\w+) \((\w+)\)\n")
    pat_decls = re.compile("   -- " + prefix_range_name + "(\w+) \((\w+)\)\n")
    pat_comment_line = re.compile("   --+\n")
    pat_comment_box = re.compile("   --(  .*)?\n")
    while True:
        l = lr.get()
        if l == "   -- End of " + type_name + ".\n":
            break
        if l == "\n":
            continue
        m = pat_decl.match(l)
        if m:
            # List of nodes being described by the current description.
            names = []

            # Declaration of the first node
            while True:
                name = m.group(1)
                if name not in kinds:
                    raise ParseError(lr, "unknown node")
                fmt = m.group(2)
                names.append((name, fmt))
                if name in nodes:
                    raise ParseError(lr, "node {} already described".format(name))
                # There might be several nodes described at once.
                l = lr.get()
                m = pat_decl.match(l)
                if not m:
                    break
            read_nodes_fields(lr, names, fields, nodes, funcs_dict)
            continue
        m = pat_decls.match(l)
        if m:
            # List of nodes being described by the current description.
            name = m.group(1)
            fmt = m.group(2)
            names = [(k, fmt) for k in kinds_ranges[name]]
            lr.get()
            read_nodes_fields(lr, names, fields, nodes, funcs_dict)
            continue
        if pat_comment_line.match(l) or pat_comment_box.match(l):
            continue
        raise ParseError(lr, "bad line in node description")

    for k in kinds:
        if k not in nodes:
            raise ParseError(lr, 'no description for "{}"'.format(k))
    return nodes


def gen_choices(choices):
    """Generate a choice 'when A | B ... Z =>' using elements of CHOICES."""
    is_first = True
    for c in choices:
        ch = prefix_name + c
        if is_first:
            is_first = False
            print("         when " + ch, end='')
        else:
            print()
            print("           | " + ch, end='')
    print(" =>")


def gen_get_format(formats, nodes, kinds=None):
    """Generate the Get_Format function."""
    print("   function Get_Format (Kind : " + type_name + ") " + "return Format_Type is")
    print("   begin")
    print("      case Kind is")
    for f in formats:
        choices = [k for k in kinds if nodes[k].format == f]
        gen_choices(choices)
        print("            return Format_" + f + ";")
    print("      end case;")
    print("   end Get_Format;")


def gen_subprg_header(decl):
    if len(decl) < 76:
        print(decl + " is")
    else:
        print(decl)
        print("   is")
    print("   begin")


def gen_assert(func):
    print("      pragma Assert (" + func.pname + " /= Null_" + node_type + ");")
    cond = "(Has_" + func.name + " (Get_Kind (" + func.pname + ")),"
    msg = '"no field ' + func.name + '");'
    if len(cond) < 60:
        print("      pragma Assert " + cond)
        print("                     " + msg)
    else:
        print("      pragma Assert")
        print("         " + cond)
        print("          " + msg)


def get_field_type(fields, f):
    for fld in list(fields.values()):
        if f in fld:
            return fld[f]
    return None


def gen_get_set(func, nodes, fields):
    """Generate Get_XXX/Set_XXX subprograms for FUNC."""
    rtype = func.rtype
    # If the function needs several fields, it must be user defined
    if func.conv == "grp":
        print("   type %s_Conv is record" % rtype)
        for f in func.fields:
            print("      %s: %s;" % (f, get_field_type(fields, f)))
        print("   end record;")
        print("   pragma Pack (%s_Conv);" % rtype)
        print("   pragma Assert (%s_Conv'Size = %s'Size);" % (rtype, rtype))
        print()
    else:
        f = func.fields[0]
        g = "Get_" + f + " (" + func.pname + ")"

    s = func.rname
    if func.conv:
        if func.conv == "uc":
            field_type = get_field_type(fields, f)
            g = field_type + "_To_" + rtype + " (" + g + ")"
            s = rtype + "_To_" + field_type + " (" + s + ")"
        elif func.conv == "pos":
            g = rtype + "'Val (" + g + ")"
            s = rtype + "'Pos (" + s + ")"

    subprg = (
        "   function Get_"
        + func.name
        + " ("
        + func.pname
        + " : "
        + func.ptype
        + ") return "
        + rtype
    )
    if func.conv == "grp":
        print(subprg)
        print("   is")
        print("      function To_%s is new Ada.Unchecked_Conversion" % func.rtype)
        print("         (%s_Conv, %s);" % (rtype, rtype))
        print("      Conv : %s_Conv;" % rtype)
        print("   begin")
    else:
        gen_subprg_header(subprg)
    gen_assert(func)
    if func.conv == "grp":
        for f in func.fields:
            print("      Conv.%s := Get_%s (%s);" % (f, f, func.pname))
        g = "To_%s (Conv)" % rtype
    print("      return " + g + ";")
    print("   end Get_" + func.name + ";")
    print()

    subprg = (
        "   procedure Set_"
        + func.name
        + " ("
        + func.pname
        + " : "
        + func.ptype
        + "; "
        + func.rname
        + " : "
        + func.rtype
        + ")"
    )
    if func.conv == "grp":
        print(subprg)
        print("   is")
        print("      function To_%s_Conv is new Ada.Unchecked_Conversion" % func.rtype)
        print("         (%s, %s_Conv);" % (rtype, rtype))
        print("      Conv : %s_Conv;" % rtype)
        print("   begin")
    else:
        gen_subprg_header(subprg)
    gen_assert(func)
    if func.conv == "grp":
        print("      Conv := To_%s_Conv (%s);" % (rtype, func.rname))
        for f in func.fields:
            print("      Set_%s (%s, Conv.%s);" % (f, func.pname, f))
    else:
        print("      Set_" + f + " (" + func.pname + ", " + s + ");")
    print("   end Set_" + func.name + ";")
    print()


def funcs_of_node(n):
    return sorted([fv.name for fv in list(n.fields.values()) if fv])


def gen_has_func_spec(name, suff):
    spec = "   function Has_" + name + " (K : " + type_name + ")"
    ret = " return Boolean" + suff
    if len(spec) < 60:
        print(spec + ret)
    else:
        print(spec)
        print("     " + ret)


def do_disp_formats():
    for fmt in fields:
        print("Fields of Format_" + fmt)
        fld = fields[fmt]
        for k in fld:
            print("  " + k + " (" + fld[k] + ")")


def do_disp_kinds():
    print("Kinds are:")
    for k in kinds:
        print("  " + prefix_name + k)


def do_disp_funcs():
    print("Functions are:")
    for f in funcs:
        s = "{0} ({1}: {2}".format(f.name, f.fields, f.rtype)
        if f.acc:
            s += " acc:" + f.acc
        if f.conv:
            s += " conv:" + f.conv
        s += ")"
        print(s)


def do_disp_types():
    print("Types are:")
    s = set([])
    for f in funcs:
        s |= {f.rtype}
    for t in sorted(s):
        print("  " + t)


def do_disp_nodes():
    for k in kinds:
        v = nodes[k]
        print(prefix_name + k + " (" + v.format + ")")
        flds = [fk for fk, fv in list(v.fields.items()) if fv]
        for fk in sorted(flds):
            print("  " + fk + ": " + v.fields[fk].name)


def do_get_format():
    gen_get_format(formats, nodes)


def do_body():
    lr = linereader(template_file)
    while True:
        l = lr.get().rstrip()
        print(l)
        if l == "   --  Subprograms":
            gen_get_format(formats, nodes, kinds)
            print()
            for f in funcs:
                gen_get_set(f, nodes, fields)
        if l[0:3] == "end":
            break


def get_types():
    s = set([])
    for f in funcs:
        s |= {f.rtype}
    return [t for t in sorted(s)]


def get_attributes():
    s = set([])
    for f in funcs:
        if f.acc:
            s |= {f.acc}
    res = [t for t in sorted(s)]
    res.insert(0, "None")
    return res


def gen_enum(prefix, vals):
    last = None
    for v in vals:
        if last:
            print(last + ",")
        last = prefix + v
    print(last)


def do_meta_specs():
    lr = linereader(meta_base_file + ".ads.in")
    types = get_types()
    while True:
        l = lr.get().rstrip()
        if l == "      --  TYPES":
            gen_enum("      Type_", types)
        elif l == "      --  FIELDS":
            gen_enum("      Field_", [f.name for f in funcs])
        elif l == "      --  ATTRS":
            gen_enum("      Attr_", get_attributes())
        elif l == "   --  FUNCS":
            for t in types:
                print("   function Get_" + t)
                print("      (N : " + node_type + "; F : Fields_Enum) return " + t + ";")
                print("   procedure Set_" + t)
                print("      (N : " + node_type + "; F : Fields_Enum; V: " + t + ");")
                print()
            for f in funcs:
                gen_has_func_spec(f.name, ";")
        elif l[0:3] == "end":
            print(l)
            break
        else:
            print(l)


def do_meta_body():
    lr = linereader(meta_base_file + ".adb.in")
    while True:
        l = lr.get().rstrip()
        if l == "      --  FIELDS_TYPE":
            last = None
            for f in funcs:
                if last:
                    print(last + ",")
                last = "      Field_" + f.name + " => Type_" + f.rtype
            print(last)
        elif l == "         --  FIELD_IMAGE":
            for f in funcs:
                print("         when Field_" + f.name + " =>")
                print('            return "' + f.name.lower() + '";')
        elif l == "         --  IIR_IMAGE":
            for k in kinds:
                print("         when " + prefix_name + k + " =>")
                print('            return "' + k.lower() + '";')
        elif l == "         --  FIELD_ATTRIBUTE":
            for f in funcs:
                print("         when Field_" + f.name + " =>")
                if f.acc:
                    attr = f.acc
                else:
                    attr = "None"
                print("            return Attr_" + attr + ";")
        elif l == "      --  FIELDS_ARRAY":
            last = None
            nodes_types = [node_type, node_type + "_List", node_type + "_Flist"]
            for k in kinds:
                v = nodes[k]
                if last:
                    print(last + ",")
                last = None
                print("      --  " + prefix_name + k)
                # Get list of physical fields for V, in some order.
                if flag_keep_order:
                    flds = v.order
                else:
                    # First non Iir and no Iir_List.
                    flds = sorted(
                        [
                            fk
                            for fk, fv in list(v.fields.items())
                            if fv and fv.rtype not in nodes_types
                        ]
                    )
                    # Then Iir and Iir_List in order of appearance
                    flds += (fv for fv in v.order if v.fields[fv].rtype in nodes_types)
                # Print the corresponding node field, but remove duplicate due
                # to 'grp'.
                fldsn = []
                for fk in flds:
                    if last:
                        print(last + ",")
                    # Remove duplicate
                    fn = v.fields[fk].name
                    if fn not in fldsn:
                        last = "      Field_" + fn
                        fldsn.append(fn)
                    else:
                        last = None
            if last:
                print(last)
        elif l == "      --  FIELDS_ARRAY_POS":
            pos = -1
            last = None
            for k in kinds:
                v = nodes[k]
                # Create a set to remove duplicate for 'grp'.
                flds = set([fv.name for fk, fv in list(v.fields.items()) if fv])
                pos += len(flds)
                if last:
                    print(last + ",")
                last = "      " + prefix_name + k + " => {}".format(pos)
            print(last)
        elif l == "   --  FUNCS_BODY":
            # Build list of types
            s = set([])
            for f in funcs:
                s |= {f.rtype}
            types = [t for t in sorted(s)]
            for t in types:
                print("   function Get_" + t)
                print("      (N : " + node_type + "; F : Fields_Enum) return " + t + " is")
                print("   begin")
                print("      pragma Assert (Fields_Type (F) = Type_" + t + ");")
                print("      case F is")
                for f in funcs:
                    if f.rtype == t:
                        print("         when Field_" + f.name + " =>")
                        print("            return Get_" + f.name + " (N);")
                print("         when others =>")
                print("            raise Internal_Error;")
                print("      end case;")
                print("   end Get_" + t + ";")
                print()
                print("   procedure Set_" + t)
                print("      (N : " + node_type + "; F : Fields_Enum; V: " + t + ") is")
                print("   begin")
                print("      pragma Assert (Fields_Type (F) = Type_" + t + ");")
                print("      case F is")
                for f in funcs:
                    if f.rtype == t:
                        print("         when Field_" + f.name + " =>")
                        print("            Set_" + f.name + " (N, V);")
                print("         when others =>")
                print("            raise Internal_Error;")
                print("      end case;")
                print("   end Set_" + t + ";")
                print()
            for f in funcs:
                gen_has_func_spec(f.name, " is")
                choices = [k for k in kinds if f.name in nodes[k].attrs]
                if len(choices) == 0:
                    print("      pragma Unreferenced (K);")
                print("   begin")
                if len(choices) == 0:
                    print("      return False;")
                elif len(choices) == 1:
                    print("      return K = " + prefix_name + choices[0] + ";")
                else:
                    print("      case K is")
                    gen_choices(choices)
                    print("            return True;")
                    print("         when others =>")
                    print("            return False;")
                    print("      end case;")
                print("   end Has_" + f.name + ";")
                print()
        elif l[0:3] == "end":
            print(l)
            break
        else:
            print(l)


actions = {
    "disp-nodes": do_disp_nodes,
    "disp-kinds": do_disp_kinds,
    "disp-formats": do_disp_formats,
    "disp-funcs": do_disp_funcs,
    "disp-types": do_disp_types,
    "get_format": do_get_format,
    "body": do_body,
    "meta_specs": do_meta_specs,
    "meta_body": do_meta_body,
}


def _generateCLIParser() -> ArgumentParser:
    """"""
    parser = ArgumentParser(description="Meta-grammar processor")
    parser.add_argument("action", choices=list(actions.keys()), default="disp-nodes")
    parser.add_argument(
        "--field-file",
        dest="field_file",
        default="nodes.ads",
        help="specify file which defines fields",
    )
    parser.add_argument(
        "--kind-file",
        dest="kind_file",
        default="iirs.ads",
        help="specify file which defines nodes kind",
    )
    parser.add_argument(
        "--node-file",
        dest="node_file",
        default="iirs.ads",
        help="specify file which defines nodes and methods",
    )
    parser.add_argument(
        "--template-file",
        dest="template_file",
        default="iirs.adb.in",
        help="specify template body file",
    )
    parser.add_argument(
        "--meta-basename",
        dest="meta_basename",
        default="nodes_meta",
        help="specify base name of meta files",
    )
    parser.add_argument(
        "--kind-type", dest="kind_type", default="Iir_Kind", help="name of kind type"
    )
    parser.add_argument(
        "--kind-prefix",
        dest="kind_prefix",
        default="Iir_Kind_",
        help="prefix for kind literals",
    )
    parser.add_argument(
        "--kind-range-prefix",
        dest="kind_range_prefix",
        default="Iir_Kinds_",
        help="prefix for kind subtype (range)",
    )
    parser.add_argument(
        "--node-type", dest="node_type", default="Iir", help="name of the node type"
    )
    parser.add_argument(
        "--keep-order",
        dest="flag_keep_order",
        action="store_true",
        help="keep field order of nodes",
    )
    parser.set_defaults(flag_keep_order=False)

    return parser


def main():
    parser = _generateCLIParser()
    args = parser.parse_args()

    # At some point, it would be simpler to create a class...
    global formats, fields, nodes, kinds, kinds_ranges, funcs

    global type_name, prefix_name, template_file, node_type, meta_base_file
    global prefix_range_name, flag_keep_order, kind_file

    type_name = args.kind_type
    prefix_name = args.kind_prefix
    prefix_range_name = args.kind_range_prefix
    template_file = args.template_file
    node_type = args.node_type
    meta_base_file = args.meta_basename
    flag_keep_order = args.flag_keep_order

    field_file = args.field_file
    kind_file = args.kind_file
    node_file = args.node_file

    try:
        (formats, fields) = read_fields(field_file)
        (kinds, kinds_ranges) = read_kinds(kind_file)
        funcs = read_methods(node_file)
        nodes = read_nodes(node_file, kinds, kinds_ranges, fields, funcs)

    except ParseError as e:
        print(e, file=sys.stderr)
        print("in {0}:{1}:{2}".format(e.lr.filename, e.lr.lineno, e.lr.l), file=sys.stderr)
        sys.exit(1)

    f = actions.get(args.action, None)
    if not f:
        print("Action {0} is unknown".format(args.action), file=sys.stderr)
        sys.exit(1)
    f()


if __name__ == "__main__":
    main()
