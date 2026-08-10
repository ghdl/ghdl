#!/usr/bin/env python3
"""
Parse the meta-model out of GHDL's Ada sources and generate the code that mirrors it.

:file:`src/vhdl/vhdl-nodes.ads` describes every node kind of the IIR tree in a comment format that the
file itself documents in its header: a kind and its format, the fields it uses, and the accessors that
read and write them. This script reads that description and emits the Ada bodies and specifications
of the accessors and of the meta-model, the Python bindings in :mod:`pyGHDL.libghdl`, and - through
:file:`pnodesrs.py` - their Rust equivalents.

The action to perform is the first command line argument; :data:`actions` lists them. The generated
files are committed, so a user does not need Python to build GHDL; :file:`src/vhdl/Makefile`
regenerates them.
"""


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
    """
    One ``Get_``/``Set_`` accessor pair, as declared in the methods section of the Ada source.
    """
    def __init__(self, name, fields, conv, acc, pname, ptype, rname, rtype, description=None):
        """
        Initializes an accessor description.

        :param name:   The accessor's name, without the ``Get_``/``Set_`` prefix.
        :param fields: The physical fields the accessor reads and writes.
        :param conv:   The conversion applied between the field and the value, or ``None``.
        :param acc:    The access attribute: ``Chain``, ``Chain_Next``, ``Ref``, ``Of_Ref``, ``Maybe_Ref``,
        ``Forward_Ref``, ``Maybe_Forward_Ref``, or ``None`` for an owned field.
        :param pname:       The name of the node parameter in the Ada declaration.
        :param ptype:       The type of that parameter.
        :param rname:       The name of the value parameter of the setter.
        :param rtype:       The type of the value.
        :param description: The comment block above the accessor's ``-- Field:`` line.
        """
        self.name = name
        self.fields = fields  # List of physical fields used
        self.conv = conv
        self.acc = acc  # access: Chain, Chain_Next, Ref, Of_Ref, Maybe_Ref,
        #                 Forward_Ref, Maybe_Forward_Ref
        self.pname = pname  # Parameter mame
        self.ptype = ptype  # Parameter type
        self.rname = rname  # value name (for procedure)
        self.rtype = rtype  # value type
        self.description = description or []  # Comment lines above '-- Field:'


class NodeDesc:
    """
    One node kind, with the fields it uses and the accessors that address them.
    """
    def __init__(self, name, format, fields, attrs, description=None):
        """
        Initializes a node kind description.

        :param name:        The kind's name, without the ``Iir_Kind_`` prefix.
        :param format:      The node format, which decides how many physical fields the node has.
        :param fields:      Mapping from a physical field to the accessor using it, defined for every field.
        :param attrs:       Mapping from an accessor name to its :class:`FuncDesc`.
        :param description: The comment block below the kind header.
        """
        self.name = name
        self.format = format
        self.fields = fields  # {field: FuncDesc} dict, defined for all fields
        self.attrs = attrs  # A {attr: FuncDesc} dict
        self.order = []  # List of fields name, in order of appearance.
        self.description = description or []  # Comment lines below the kind
        self.field_descriptions = {}  # A {attr: comment lines} dict
        self.disabled_fields = []  # (attr, comment lines) of commented out fields


class line:
    """
    A source line together with its line number.
    """
    def __init__(self, string, no):
        """
        Initializes a source line.

        :param string: The text of the line, including its newline.
        :param no:     The line number, counted from 1.
        """
        self.l = string
        self.n = no


class EndOfFile(Exception):
    """
    Raised by :meth:`linereader.get` when there is no line left to read.
    """
    def __init__(self, filename):
        """
        Initializes the exception.

        :param filename: The file that ended.
        """
        self.filename = filename

    def __str__(self):
        """
        Formats the exception as a message naming the file.

        :returns: A string representation of this exception.
        """
        return "end of file " + self.filename


class linereader:
    """
    Reads a file line by line, remembering the current line and its number.
    """
    def __init__(self, filename):
        """
        Opens the file and positions the reader before its first line.

        :param filename: The file to read.
        """
        self.filename = filename
        self.f = open(filename)
        self.lineno = 0
        self.l = ""

    def get(self):
        """
        Read the next line and remember it.

        :returns:          The line that was read, including its newline.
        :raises EndOfFile: If the file has no line left.
        """
        self.l = self.f.readline()
        if not self.l:
            raise EndOfFile(self.filename)
        self.lineno = self.lineno + 1
        return self.l


class ParseError(Exception):
    """
    Raised when the Ada source does not follow the format the header describes.
    """
    def __init__(self, lr, msg):
        """
        Initializes the exception with the position it was detected at.

        :param lr:  The line reader, which carries the file name and the current line.
        :param msg: What is wrong with the line.
        """
        self.lr = lr
        self.msg = msg

    def __str__(self):
        """
        Formats the exception as ``file:line: message``.

        :returns: A string representation of this exception.
        """
        return f"Parse error at {self.lr.filename}:{self.lr.lineno}: {self.msg}"


# Return fields description.
# This is a dictionary.  The keys represent the possible format of a node.
# The values are dictionaries representing fields.  Keys are fields name, and
# values are fields type.
def read_fields(file):
    """
    Read the node formats and their physical fields from the Ada template.

    :param file:        The template file declaring the formats.
    :returns:           A tuple of the format names and a mapping from a format to its fields.
    :raises ParseError: If the declarations do not follow the expected format.
    """
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
    pat_field_name = re.compile(r"      Format_(\w+),?\n")
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
    pat_fields = re.compile(r"   -- Fields of Format_(\w+):\n")
    pat_field_desc = re.compile(r"   --   (\w+) : (\w+).*\n")
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
    """
    Read the ``Iir_Kind`` enumeration and its subtype ranges.

    :param filename: The Ada file declaring the enumeration.
    :returns:        A tuple of the kind names, in declaration order, and a mapping from a range name to the
    kinds it covers.
    :raises ParseError: If the enumeration or a range does not follow the expected format.
    """
    lr = linereader(filename)
    kinds = []
    #  Search for 'type Iir_Kind is'
    while lr.get() != "   type " + type_name + " is\n":
        pass
    # Skip '('
    if lr.get() != "     (\n":
        raise ParseError(lr, 'no open parenthesis after "type ' + type_name + '"')

    # Read literals
    pat_node = re.compile("      " + prefix_name + r"(\w+),?( +-- .*)?\n")
    pat_comment = re.compile(r"( +-- .*)?\n")
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
def strip_comment(l):
    """
    Return the text of comment line L, or None if L is not a comment.

    Only the standard two space prefix is removed, so a line indented further
    than that - the continuation of a grammar production, for instance - keeps
    its relative indentation.
    """
    if not l.startswith("   --"):
        return None
    text = l[5:].rstrip()
    return text[2:] if text.startswith("  ") else text.lstrip()


def trim_description(lines):
    """
    Drop the leading and trailing blank lines of a comment block.
    """
    while lines and not lines[0]:
        lines = lines[1:]
    while lines and not lines[-1]:
        lines = lines[:-1]
    return lines


# A field that was disabled by commenting out its 'Get/Set_' line, e.g.
#   --  -- Get/Set_Signal_Driver (Field7)
pat_disabled_field = re.compile(r"-- Get/Set_(\w+) \((Alias )?([\w,]+)\)$")


def split_last_block(lines):
    """
    Split a comment block at its last blank line.

    :param lines: The comment lines to split.
    :returns:     A tuple of everything before the last blank line and everything after it.
    """
    for i in range(len(lines) - 1, -1, -1):
        if not lines[i]:
            return (lines[:i], lines[i + 1 :])
    return ([], lines)


def split_disabled_fields(lines):
    """
    Split a comment block into (description, disabled fields).

    A field can be disabled by commenting out its 'Get/Set_' line.  The comment
    then still describes that field, so it must not be attributed to the field
    that happens to follow it.
    """
    description = []
    disabled = []
    for l in lines:
        m = pat_disabled_field.match(l)
        if m:
            disabled.append((m.group(1), trim_description(description)))
            description = []
        else:
            description.append(l)
    return (trim_description(description), disabled)


def read_methods(filename):
    """
    Read the accessor declarations from the methods section.

    Each accessor is a ``--  Field:`` line followed by a function and a procedure, and the comment block above the
    ``Field:`` line describes it.

    :param filename:    The Ada file declaring the accessors.
    :returns:           The accessors, in declaration order.
    :raises ParseError: If a declaration is malformed, or a function and its procedure disagree.
    """
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
    description = []
    while True:
        l = lr.get()
        if pat_end.match(l):
            break
        m = pat_field.match(l)
        if not m:
            # Comment lines preceding a '-- Field:' describe the accessor.  Any
            # other line (a declaration, a pragma, a blank line) ends the block.
            comment = strip_comment(l)
            if comment is None:
                description = []
            else:
                description.append(comment)
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
                    description,
                )
            )
            description = []

    return funcs


# Read description for one node
# LR is the line reader.  NAMES is the list of (node name, format)
#  (one description may describe several nodes).
# A comment start at column 2 or 4 or later.
def read_nodes_fields(lr, names, fields, nodes, funcs_dict):
    """
    Read the description of one or more node kinds and attach their fields.

    :param lr:          The line reader, positioned on the line after the kind header.
    :param names:       The ``(kind name, format)`` pairs the description applies to.
    :param fields:      Mapping from a format to its physical fields.
    :param nodes:       Mapping from a kind name to its :class:`NodeDesc`, extended by this function.
    :param funcs_dict:  Mapping from an accessor name to its :class:`FuncDesc`.
    :raises ParseError: If a field line is malformed, names an unknown accessor, or reuses a field.
    """
    pat_only = re.compile(r"   -- Only for " + prefix_name + r"(\w+):\n")
    pat_only_bad = re.compile(r"   -- *Only for.*\n")
    pat_field = re.compile(r"   --   Get/Set_(\w+) \((Alias )?([\w,]+)\)\n")
    pat_comment = re.compile(r"   --(|  [^ ].*|    .*)\n")

    # Create nodes
    cur_nodes = []
    for (nm, fmt) in names:
        if fmt not in fields:
            raise ParseError(lr, f'unknown format "{fmt}"')
        n = NodeDesc(nm, fmt, {x: None for x in fields[fmt]}, {})
        nodes[nm] = n
        cur_nodes.append(n)

    # The comments before the first field describe the node(s), except for the
    # block after the last blank line: a field's description directly precedes
    # its 'Get/Set_' line, so that block belongs to the first field.
    l = lr.l
    comments = []
    while pat_comment.match(l):
        comments.append(strip_comment(l))
        l = lr.get()
    (comments, description) = split_last_block(comments)
    for n in cur_nodes:
        (n.description, disabled) = split_disabled_fields(comments)
        n.disabled_fields.extend(disabled)

    # Look for fields
    while l != "\n":
        # The comments before a field describe that field on this node.
        while pat_comment.match(l):
            description.append(strip_comment(l))
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

        # A field can be preceded by an 'Only for' comment, so continue
        # collecting after it.
        while pat_comment.match(l):
            description.append(strip_comment(l))
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
            (text, disabled) = split_disabled_fields(description)
            c.field_descriptions[func.name] = text
            c.disabled_fields.extend(disabled)

        description = []
        l = lr.get()


def read_nodes(filename, kinds, kinds_ranges, fields, funcs):
    """
    Read description for all nodes.
    """
    lr = linereader(filename)
    funcs_dict = {x.name: x for x in funcs}
    nodes = {}

    # Skip until start
    while lr.get() != "   -- Start of " + type_name + ".\n":
        pass

    pat_decl = re.compile(r"   -- " + prefix_name + r"(\w+) \((\w+)\)\n")
    pat_decls = re.compile(r"   -- " + prefix_range_name + r"(\w+) \((\w+)\)\n")
    pat_comment_line = re.compile(r"   --+\n")
    pat_comment_box = re.compile(r"   --(  .*)?\n")
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
    """
    Generate a choice 'when A | B ... Z =>' using elements of CHOICES.
    """
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
    """
    Generate the Get_Format function.
    """
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
    """
    Print a subprogram header, wrapping it if it does not fit on one line.

    :param decl: The declaration to print, without the trailing ``is``.
    """
    if len(decl) < 76:
        print(decl + " is")
    else:
        print(decl)
        print("   is")
    print("   begin")


def gen_assert(func):
    """
    Print the precondition of an accessor: the node is not null and has the field.

    :param func: The accessor to generate the assertion for.
    """
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
    """
    Look up the type of a physical field.

    :param fields: Mapping from a format to its fields.
    :param f:      The physical field to look up.
    :returns:      The field's type, or ``None`` if no format declares it.
    """
    for fld in list(fields.values()):
        if f in fld:
            return fld[f]
    return None


def gen_get_set(func, nodes, fields):
    """
    Generate Get_XXX/Set_XXX subprograms for FUNC.
    """
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
    """
    Collect the accessors of a node kind, sorted by name.

    :param n: The node kind to collect the accessors of.
    :returns: The accessor names, sorted.
    """
    return sorted([fv.name for fv in list(n.fields.values()) if fv])


def gen_has_func_spec(name, suff):
    """
    Print the specification of a ``Has_*`` predicate, wrapping it if it does not fit.

    :param name: The field the predicate answers for.
    :param suff: What to append, e.g. ``;`` for a specification or ``is`` for a body.
    """
    spec = "   function Has_" + name + " (K : " + type_name + ")"
    ret = " return Boolean" + suff
    if len(spec) < 60:
        print(spec + ret)
    else:
        print(spec)
        print("     " + ret)


def do_disp_formats():
    """
    Display every node format with the physical fields it declares.
    """
    for fmt in fields:
        print("Fields of Format_" + fmt)
        fld = fields[fmt]
        for k in fld:
            print("  " + k + " (" + fld[k] + ")")


def do_disp_kinds():
    """
    Display every node kind, and the subtype ranges over them.
    """
    print("Kinds are:")
    for k in kinds:
        print("  " + prefix_name + k)


def do_disp_funcs():
    """
    Display every accessor with its fields, type and access attribute.
    """
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
    """
    Display the value types used by the accessors.
    """
    print("Types are:")
    s = set([])
    for f in funcs:
        s |= {f.rtype}
    for t in sorted(s):
        print("  " + t)


def do_disp_nodes():
    """
    Display every node kind with the fields it uses.
    """
    for k in kinds:
        v = nodes[k]
        print(prefix_name + k + " (" + v.format + ")")
        flds = [fk for fk, fv in list(v.fields.items()) if fv]
        for fk in sorted(flds):
            print("  " + fk + ": " + v.fields[fk].name)


def do_disp_doc():
    """
    Display the comments attached to every node, field and accessor.
    """
    for f in funcs:
        if f.description:
            print("Get/Set_" + f.name + ":")
            for l in f.description:
                print("  " + l)
    for k in kinds:
        v = nodes[k]
        print(prefix_name + k + " (" + v.format + ")")
        for l in v.description:
            print("  " + l)
        for fk in v.order:
            func = v.fields[fk]
            for l in v.field_descriptions.get(func.name, []):
                print("  " + func.name + ": " + l)
        for name, desc in v.disabled_fields:
            print("  (disabled) " + name + ": " + " ".join(desc))


def do_get_format():
    """
    Generate the Ada ``Get_Format`` function, which maps a kind to its format.
    """
    gen_get_format(formats, nodes)


def do_body():
    """
    Generate the Ada body of the node package from its template.
    """
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
    """
    Collect the value types of all accessors.

    :returns: The type names, sorted.
    """
    s = set([])
    for f in funcs:
        s |= {f.rtype}
    return [t for t in sorted(s)]


def get_attributes():
    """
    Collect the access attributes used by the accessors.

    :returns: The attribute names, sorted, with ``None`` for owned fields first.
    """
    s = set([])
    for f in funcs:
        if f.acc:
            s |= {f.acc}
    res = [t for t in sorted(s)]
    res.insert(0, "None")
    return res


def gen_enum(prefix, vals):
    """
    Print an Ada enumeration literal list, one literal per line.

    :param prefix: The prefix to put in front of every literal.
    :param vals:   The literal names, in declaration order.
    """
    last = None
    for v in vals:
        if last:
            print(last + ",")
        last = prefix + v
    print(last)


def do_meta_specs():
    """
    Generate the Ada specification of the meta-model package from its template.
    """
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
    """
    Generate the Ada body of the meta-model package from its template.
    """
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


def read_enum(filename, type_name, prefix, g=lambda m: m.group(1)):
    """
    Read an enumeration declaration from an Ada file.

    :param filename:    The Ada file declaring the enumeration.
    :param type_name:   The name of the enumeration type.
    :param prefix:      The prefix every literal carries.
    :param g:           Applied to the match, to derive the literal's name.
    :returns:           The literal names, in declaration order.
    :raises ParseError: If the declaration does not follow the expected format.
    """
    pat_decl = re.compile(r"   type {0} is$".format(type_name))
    pat_enum = re.compile(r"      {0}(\w+),?( *-- .*)?$".format(prefix))
    pat_comment = re.compile(r" *-- .*$")
    lr = linereader(filename)
    while not pat_decl.match(lr.get()):
        pass
    line = lr.get()
    if line != "     (\n":
        raise ParseError(lr, f"{filename}:{lr.lineno}: missing open parenthesis")
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
            raise ParseError(
                lr,
                f"{filename}:{ lr.lineno}: incorrect line in enum {type_name}"
            )
    return toks


def read_any_names(filename, prefix, type):
    """
    Read a series of named integer constants from an Ada file.

    :param filename: The Ada file declaring the constants.
    :param prefix:   The prefix every constant carries.
    :param type:     The Ada type of the constants.
    :returns:        The constant names, in declaration order.
    """
    pat_name_first = re.compile(
        r"   {pfx}(\w+)\s+: constant {type} := (\d+);".format(
            pfx=prefix, type=type))
    pat_name_def = re.compile(
        r"   {pfx}(\w+)\s+:\s+constant {type} :=\s+{pfx}(\w+)( \+ (\d+))?;".format(
            pfx=prefix, type=type))
    dict = {}
    res = []
    lr = linereader(filename)
    val_max = 1
    while True:
        line = lr.get()
        if line.startswith("end"):
            break
        if line.endswith(":=\n"):
            line = line.rstrip() + lr.get()

        m = pat_name_first.match(line)
        if m:
            name_def = m.group(1)
            val = int(m.group(2))
            dict[name_def] = val
            res.append((name_def, val))
            val_max = max(val_max, val)
            continue

        m = pat_name_def.match(line)
        if m:
            name_def = m.group(1)
            name_ref = m.group(2)
            val = m.group(4)
            if not val:
                val = 0
            val_ref = dict.get(name_ref, None)
            if not val_ref:
                raise ParseError(lr, f"name {name_ref} not found")
            val = val_ref + int(val)
            val_max = max(val_max, val)
            dict[name_def] = val
            res.append((name_def, val))
    return res


def read_std_names():
    """
    Read the predefined names from :file:`std_names.ads`.

    :returns: The predefined names, in declaration order.
    """
    return read_any_names("../std_names.ads", "Name_", "Name_Id")


actions = {
    "disp-nodes": do_disp_nodes,
    "disp-kinds": do_disp_kinds,
    "disp-formats": do_disp_formats,
    "disp-funcs": do_disp_funcs,
    "disp-types": do_disp_types,
    "disp-doc": do_disp_doc,
    "get_format": do_get_format,
    "body": do_body,
    "meta_specs": do_meta_specs,
    "meta_body": do_meta_body,
}


def _generateCLIParser() -> ArgumentParser:
    """
    Build the command line parser.

    :returns: The parser for this script's arguments.
    """
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


def parse_files(
    node_file_,
    field_file_,
    kind_file_,
    template_file_="iirs.adb.in",
    meta_basename="nodes_meta",
    kind_type="Iir_Kind",
    kind_prefix="Iir_Kind_",
    kind_range_prefix="Iir_Kinds_",
    node_type_="Iir",
    keep_order=False,
):
    """
    Read the Ada sources and return the parsed meta-model.

    This is the entry point for other tools: it sets the module state the
    generators rely on and returns (formats, fields, kinds, kinds_ranges, funcs,
    nodes).  ParseError is raised on a malformed description; callers that want
    the command line behaviour should catch it and report LR themselves.
    """
    # At some point, it would be simpler to create a class...
    global formats, fields, nodes, kinds, kinds_ranges, funcs

    global type_name, prefix_name, template_file, node_type, meta_base_file
    global prefix_range_name, flag_keep_order, kind_file

    type_name = kind_type
    prefix_name = kind_prefix
    prefix_range_name = kind_range_prefix
    template_file = template_file_
    node_type = node_type_
    meta_base_file = meta_basename
    flag_keep_order = keep_order
    kind_file = kind_file_

    (formats, fields) = read_fields(field_file_)
    (kinds, kinds_ranges) = read_kinds(kind_file_)
    funcs = read_methods(node_file_)
    nodes = read_nodes(node_file_, kinds, kinds_ranges, fields, funcs)

    return (formats, fields, kinds, kinds_ranges, funcs, nodes)


def main():
    """
    Parse the command line, read the Ada sources and run the requested action.
    """
    parser = _generateCLIParser()
    args = parser.parse_args()

    try:
        parse_files(
            args.node_file,
            args.field_file,
            args.kind_file,
            args.template_file,
            args.meta_basename,
            args.kind_type,
            args.kind_prefix,
            args.kind_range_prefix,
            args.node_type,
            args.flag_keep_order,
        )

    except ParseError as e:
        print(e, file=sys.stderr)
        print(f"in {e.lr.filename}:{e.lr.lineno}:{e.lr.l}", file=sys.stderr)
        sys.exit(1)

    f = actions.get(args.action, None)
    if not f:
        print(f"Action {args.action} is unknown", file=sys.stderr)
        sys.exit(1)
    f()


if __name__ == "__main__":
    main()
