#!/usr/bin/env python

import re
import sys
import argparse

field_file = "nodes.ads"
spec_file = "iirs.ads"
template_file = "iirs.adb.in"
meta_base_file = "nodes_meta"
prefix_name = "Iir_Kind_"
prefix_range_name = "Iir_Kinds_"
type_name = "Iir_Kind"
node_type = "Iir"
conversions = ['uc', 'pos']

class FuncDesc:
    def __init__(self, name, field, conv, acc,
                 pname, ptype, rname, rtype):
        self.name = name
        self.field = field
        self.conv = conv
        self.acc = acc  # access: Chain, Chain_Next, Ref, Of_Ref, Maybe_Ref
        self.pname = pname # Parameter mame
        self.ptype = ptype # Parameter type
        self.rname = rname # value name (for procedure)
        self.rtype = rtype # value type

class NodeDesc:
    def __init__(self, name, format, fields, attrs):
        self.name = name
        self.format = format
        self.fields = fields # {field: FuncDesc} dict, defined for all fields
        self.attrs = attrs   # A {attr: FuncDesc} dict
        self.order = []      # List of fields name, in order of appearance.

class line:
    def __init__(self, string, no):
        self.l = string
        self.n = no

class EndOfFile(Exception):
    def __init__(self,filename):
        self.filename = filename

    def __str__(self):
        return "end of file " + self.filename

class linereader:
    def __init__(self, filename):
        self.filename = filename
        self.f = open (filename)
        self.lineno = 0
        self.l = ''

    def get(self):
        self.l = self.f.readline()
        if not self.l:
            raise EndOfFile(self.filename)
        self.lineno = self.lineno + 1
        return self.l

class ParseError(Exception):
    def __init__(self, lr, msg):
        self.lr = lr;
        self.msg = msg

    def __str__(self):
        return 'Error: ' + self.msg
        return 'Parse error at ' + self.lr.filname + ':' + self.lr.lineno + \
               ': ' + self.msg

# Return fields description.
# This is a dictionary.  The keys represent the possible format of a node.
# The values are dictionnaries representing fields.  Keys are fields name, and
# values are fields type.
def read_fields(file):
    fields = {}
    formats = []
    lr = linereader(file)

    #  Search for 'type Format_Type is'
    while lr.get() != '   type Format_Type is\n':
        pass

    # Skip '('
    if lr.get() != '     (\n':
        raise 'no open parenthesis after Format_Type';

    # Read formats
    l = lr.get()
    pat_field_name = re.compile('      Format_(\w+),?\n')
    while l != '     );\n':
        m = pat_field_name.match(l)
        if m == None:
            print l
            raise 'bad literal within Format_Type'
        name = m.group(1)
        formats.append(name)
        fields[name] = {}
        l = lr.get()

    # Read fields
    l = lr.get()
    pat_fields = re.compile('   -- Fields of Format_(\w+):\n')
    pat_field_desc = re.compile('   --   (\w+) : (\w+).*\n')
    format_name = ''
    common_desc = {}

    # Read until common fields.
    while l != '   -- Common fields are:\n':
        l = lr.get()
    format_name = 'Common'
    nbr_formats = 0

    while True:
        # 1) Read field description
        l = lr.get()
        desc = common_desc.copy()
        while True:
            m = pat_field_desc.match(l)
            if m == None:
                break
            desc[m.group(1)] = m.group(2)
            l = lr.get()
            # print 'For: ' + format_name + ': ' + m.group(1)

        # 2) Disp
        if format_name == 'Common':
            common_desc = desc
        else:
            fields[format_name] = desc

        # 3) Read next format
        if l == '\n':
            if nbr_formats == len(fields):
                break
            else:
                l = lr.get()

        # One for a format
        m = pat_fields.match(l)
        if m != None:
            format_name = m.group(1)
            if not format_name in fields:
                raise ParseError(
                    lr, 'Format ' + format_name + ' is unknown')
            nbr_formats = nbr_formats + 1
        else:
            raise ParseError(lr, 'unhandled format line')

    return (formats, fields)

# Read kinds, kinds ranges and methods
def read_kinds(filename):
    lr = linereader(filename)
    kinds = []
    #  Search for 'type Iir_Kind is'
    while lr.get() != '   type ' + type_name + ' is\n':
        pass
    # Skip '('
    if lr.get() != '     (\n':
        raise ParseError(lr,
                         'no open parenthesis after "type ' + type_name +'"')

    # Read literals
    pat_node = re.compile('      ' + prefix_name + '(\w+),?( +-- .*)?\n')
    pat_comment = re.compile('( +-- .*)?\n')
    while True:
        l = lr.get()
        if l == '     );\n':
            break
        m = pat_node.match(l)
        if m:
            kinds.append(m.group(1))
            continue
        m = pat_comment.match(l)
        if not m:
            raise ParseError(lr, 'Unknow line within kind declaration')

    # Check subtypes
    pat_subtype = re.compile('   subtype ' + prefix_range_name \
                             + '(\w+) is ' + type_name + ' range\n')
    pat_first = re.compile('     ' + prefix_name + '(\w+) ..\n')
    pat_last = re.compile('     ' + prefix_name + '(\w+);\n')
    pat_middle = re.compile('   --' + prefix_name + '(\w+)\n')
    kinds_ranges={}
    while True:
        l = lr.get()
        # Start of methods is also end of subtypes.
        if l == '   -- General methods.\n':
            break
        # Found a subtype.
        m = pat_subtype.match(l)
        if m:
            # Check first bound
            name = m.group(1)
            l = lr.get()
            mf = pat_first.match(l)
            if not mf:
                raise ParseError(lr, 'badly formated first bound of subtype')
            first = kinds.index(mf.group(1))
            idx = first
            has_middle = None
            # Read until last bound
            while True:
                l = lr.get()
                ml = pat_middle.match(l)
                if ml:
                    # Check element in the middle
                    if kinds.index(ml.group(1)) != idx + 1:
                        raise ParseError(lr,
                            "missing " + kinds[idx] + " in subtype")
                    has_middle = True
                    idx = idx + 1
                else:
                    # Check last bound
                    ml = pat_last.match(l)
                    if ml:
                        last = kinds.index(ml.group(1))
                        if last != idx + 1 and has_middle:
                            raise ParseError(lr,
                                "missing " + kinds[idx] + " in subtype")
                        break
                    raise ParseError(lr,
                                     "unhandled line in subtype")
            kinds_ranges[name] = kinds[first:last+1]

    # Read functions
    funcs = []
    pat_field = re.compile(
        '   --  Field: (\w+)'
        + '( Of_Ref| Ref| Maybe_Ref| Chain_Next| Chain)?( .*)?\n')
    pat_conv = re.compile('^ \((\w+)\)$')
    pat_func = \
      re.compile('   function Get_(\w+) \((\w+) : (\w+)\) return (\w+);\n')
    pat_proc = \
      re.compile('   procedure Set_(\w+) \((\w+) : (\w+); (\w+) : (\w+)\);\n')
    pat_end = re.compile('end [A-Za-z.]+;\n')
    while True:
        l = lr.get()
        if pat_end.match(l):
            break
        m = pat_field.match(l)
        if m:
            # Extract conversion
            acc = m.group(2)
            if acc:
                acc = acc.strip()
            conv = m.group(3)
            if conv:
                mc = pat_conv.match(conv)
                if not mc:
                    raise ParseError(lr, 'conversion ill formed')
                conv = mc.group(1)
                if conv not in conversions:
                    raise ParseError(lr, 'unknown conversion ' + conv)
            else:
                conv = None

            # Read function
            l = lr.get()
            mf = pat_func.match(l)
            if not mf:
                raise ParseError(lr,
                        'function declaration expected after Field')
            # Read procedure
            l = lr.get()
            mp = pat_proc.match(l)
            if not mp:
                raise ParseError(lr,
                        'procedure declaration expected after function')
            # Consistency check between function and procedure
            if mf.group(1) != mp.group(1):
                raise ParseError(lr, 'function and procedure name mismatch')
            if mf.group(2) != mp.group(2):
                raise ParseError(lr, 'parameter name mismatch with function')
            if mf.group(3) != mp.group(3):
                raise ParseError(lr, 'parameter type mismatch with function')
            if mf.group(4) != mp.group(5):
                raise ParseError(lr, 'result type mismatch with function')
            funcs.append(FuncDesc(mf.group(1), m.group(1), conv, acc,
                                  mp.group(2), mp.group(3),
                                  mp.group(4), mp.group(5)))

    return (kinds, kinds_ranges, funcs)

# Read description for one node
# LR is the line reader.  NAMES is the list of (node name, format)
#  (one description may describe several nodes).
def read_nodes_fields(lr, names, fields, nodes, funcs_dict):
    pat_only = re.compile('   -- Only for ' + prefix_name + '(\w+):\n')
    pat_field = re.compile('   --   Get/Set_(\w+) \((Alias )?(\w+)\)\n')
    pat_comment = re.compile('   --.*\n')
    pat_start = re.compile ('   --   \w.*\n')

    # Create nodes
    cur_nodes = []
    for (nm, fmt) in names:
        if fmt not in fields:
            raise ParseError(lr, 'unknown format')
        n = NodeDesc(nm, fmt, {x: None for x in fields[fmt]}, {})
        nodes[nm] = n
        cur_nodes.append(n)

    # Look for fields
    only_nodes = cur_nodes
    l = lr.l
    while l != '\n':
        # Handle 'Only ...'
        while True:
            m = pat_only.match(l)
            if not m:
                break
            name = m.group(1)
            if name not in [x.name for x in cur_nodes]:
                raise ParseError(lr, 'node not currently described')
            if only_nodes == cur_nodes:
                only_nodes = []
            only_nodes.append(nodes[name])
            l = lr.get()
        # Handle field: '--  Get/Set_FUNC (Alias? FIELD)'
        m = pat_field.match(l)
        if m:
            # 1) Check the function exists
            func = m.group(1)
            alias = m.group(2)
            field = m.group(3)
            if func not in funcs_dict:
                raise ParseError(lr, 'unknown function')
            func = funcs_dict[func]
            if func.field != field:
                raise ParseError(lr, 'field mismatch')
            for c in only_nodes:
                if field not in c.fields:
                    raise ParseError(lr, 'field ' + field + \
                                     ' does not exist in node')
                if not alias:
                    if c.fields[field]:
                        raise ParseError(lr, 'field already used')
                    c.fields[field] = func
                    c.order.append(field)
                c.attrs[func.name] = func
            only_nodes = cur_nodes
        elif pat_start.match(l):
            raise ParseError(lr, 'bad line in node description')
        elif not pat_comment.match(l):
            raise ParseError(lr, 'bad line in node description')
        l = lr.get()

# Read description for all nodes
def read_nodes(filename, kinds, kinds_ranges, fields, funcs):
    lr = linereader(filename)
    funcs_dict = {x.name:x for x in funcs}
    nodes = {}

    # Skip until start
    while lr.get() != '   -- Start of ' + type_name + '.\n':
        pass

    pat_decl = re.compile('   -- ' + prefix_name + '(\w+) \((\w+)\)\n')
    pat_decls = re.compile('   -- ' + prefix_range_name + '(\w+) \((\w+)\)\n')
    pat_comment_line = re.compile('   --+\n')
    pat_comment_box = re.compile('   --(  .*)?\n')
    while True:
        l = lr.get()
        if l == '   -- End of ' + type_name + '.\n':
            return nodes
        if l == '\n':
            continue
        m = pat_decl.match(l)
        if m:
            # List of nodes being described by the current description.
            names = []

            # Declaration of the first node
            while True:
                name=m.group(1)
                if not name in kinds:
                    raise ParseError(lr, 'unknown node')
                fmt=m.group(2)
                names.append((name,fmt))
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
            name=m.group(1)
            fmt=m.group(2)
            names = [(k,fmt) for k in kinds_ranges[name]]
            l = lr.get()
            read_nodes_fields(lr, names, fields, nodes, funcs_dict)
            continue
        if pat_comment_line.match(l) or pat_comment_box.match(l):
            continue
        raise ParseError(lr, 'bad line in node description')
    return nodes

# Generate a choice 'when A | B ... Z =>' using elements of CHOICES.
def gen_choices(choices):
    is_first=True
    for c in choices:
        if is_first:
            print '        ',
            print 'when',
        else:
            print
            print '        ',
            print '  |',
        print prefix_name + c,
        is_first=None
    print '=>'

# Generate the Get_Format function.
def gen_get_format(formats, nodes, kinds):
    print '   function Get_Format (Kind : ' + type_name + ') ' + \
          'return Format_Type is'
    print '   begin'
    print '      case Kind is'
    for f in formats:
        choices = [k for k in kinds if nodes[k].format == f]
        gen_choices(choices)
        print '            return Format_' + f + ';'
    print '      end case;'
    print '   end Get_Format;'

def gen_subprg_header(decl):
    if len(decl) < 76:
        print decl + ' is'
    else:
        print decl
        print '   is'
    print '   begin'

def gen_assert(func):
    print '      pragma Assert (' + func.pname + ' /= Null_' + node_type + ');'
    cond = '(Has_' + func.name + ' (Get_Kind (' + func.pname + ')),'
    msg  = '"no field ' + func.name + '");'
    if len (cond) < 60:
        print '      pragma Assert ' + cond
        print '                     ' + msg
    else:
        print '      pragma Assert'
        print '         ' + cond
        print '          ' + msg

# Generate Get_XXX/Set_XXX subprograms for FUNC.
def gen_get_set(func, nodes, fields):
    g = 'Get_' + func.field + ' (' + func.pname + ')'
    s = func.rname
    if func.conv:
        field_type = None
        for fld in fields.values():
            if func.field in fld:
                field_type = fld[func.field]
                break
        if func.conv == 'uc':
            g = field_type + '_To_' + func.rtype + ' (' + g + ')'
            s = func.rtype + '_To_' + field_type + ' (' + s + ')'
        elif func.conv == 'pos':
            g = func.rtype + "'Val (" + g + ')'
            s = func.rtype + "'Pos (" + s + ')'

    subprg = '   function Get_' + func.name + ' (' + func.pname \
          + ' : ' + func.ptype + ') return ' + func.rtype
    gen_subprg_header(subprg)
    gen_assert(func)
    print '      return ' + g + ';'
    print '   end Get_' + func.name + ';'
    print
    subprg =  '   procedure Set_' + func.name + ' (' \
          + func.pname + ' : ' + func.ptype + '; ' \
          + func.rname + ' : ' + func.rtype + ')'
    gen_subprg_header(subprg)
    gen_assert(func)
    print '      Set_' + func.field + ' (' + func.pname + ', ' + s + ');'
    print '   end Set_' + func.name + ';'
    print

def funcs_of_node(n):
    return sorted([fv.name for fv in n.fields.values() if fv])

def gen_has_func_spec(name, suff):
    spec='   function Has_' + f.name + ' (K : ' + type_name + ')'
    ret=' return Boolean' + suff;
    if len(spec) < 60:
        print spec + ret
    else:
        print spec
        print '     ' + ret

parser = argparse.ArgumentParser(description='Meta-grammar processor')
parser.add_argument('action', choices=['disp-nodes', 'disp-kinds',
                                       'disp-formats', 'disp-funcs',
                                       'disp-types',
                                       'get_format', 'body',
                                       'meta_specs', 'meta_body'],
                    default='disp-nodes')
parser.add_argument('--field-file', dest='field_file',
                    default='nodes.ads',
                    help='specify file which defines fields')
parser.add_argument('--spec-file', dest='spec_file',
                    default='iirs.ads',
                    help='specify file which defines nodes')
parser.add_argument('--template-file', dest='template_file',
                    default='iirs.adb.in',
                    help='specify template body file')
parser.add_argument('--meta-basename', dest='meta_basename',
                    default='nodes_meta',
                    help='specify base name of meta files')
parser.add_argument('--kind-type', dest='kind_type',
                    default='Iir_Kind',
                    help='name of kind type')
parser.add_argument('--kind-prefix', dest='kind_prefix',
                    default='Iir_Kind_',
                    help='prefix for kind literals')
parser.add_argument('--node-type', dest='node_type',
                    default='Iir',
                    help='name of the node type')
parser.add_argument('--keep-order', dest='flag_keep_order',
                    action='store_true',
                    help='keep field order of nodes')
parser.set_defaults(flag_keep_order=False)
args = parser.parse_args()

field_file=args.field_file
spec_file=args.spec_file
type_name=args.kind_type
prefix_name=args.kind_prefix
template_file=args.template_file
node_type=args.node_type
meta_base_file=args.meta_basename
flag_keep_order=args.flag_keep_order

try:
    (formats, fields) = read_fields(field_file)
    (kinds, kinds_ranges, funcs) = read_kinds(spec_file)
    nodes = read_nodes(spec_file,kinds,kinds_ranges,fields,funcs)

except ParseError as e:
    print >> sys.stderr, e
    print >> sys.stderr, \
          "in {0}:{1}:{2}".format(e.lr.filename, e.lr.lineno, e.lr.l)
    sys.exit(1)

if args.action == 'disp-formats':
    for fmt in fields:
        print "Fields of Format_"+fmt
        fld=fields[fmt]
        for k in fld:
            print '  ' + k + ' (' + fld[k] + ')'
elif args.action == 'disp-kinds':
    print "Kinds are:"
    for k in kinds:
        print '  ' + prefix_name + k
elif args.action == 'disp-funcs':
    print "Functions are:"
    for f in funcs:
        s = '{0} ({1}: {2}'.format(f.name, f.field, f.rtype)
        if f.acc:
            s += ' acc:' + f.acc
        if f.conv:
            s += ' conv:' + f.conv
        s += ')'
        print s
elif args.action == 'disp-types':
    print "Types are:"
    s = set([])
    for f in funcs:
        s |= set([f.rtype])
    for t in sorted(s):
        print '  ' + t
elif args.action == 'disp-nodes':
    for k in kinds:
        v = nodes[k]
        print prefix_name + k + ' (' + v.format + ')'
        flds = [fk for fk, fv in v.fields.items() if fv]
        for fk in sorted(flds):
            print '  ' + fk + ': '+ v.fields[fk].name
elif args.action == 'get_format':
    gen_get_format(formats, nodes)
elif args.action == 'body':
    lr = linereader(template_file)
    while True:
        l = lr.get().rstrip()
        print l
        if l == '   --  Subprograms':
            gen_get_format(formats, nodes, kinds)
            print
            for f in funcs:
                gen_get_set(f, nodes, fields)
        if l[0:3] == 'end':
            break
elif args.action == 'meta_specs':
    lr = linereader(meta_base_file + '.ads.in')
    # Build list of types
    s = set([])
    for f in funcs:
        s |= set([f.rtype])
    types = [t for t in sorted(s)]
    while True:
        l = lr.get().rstrip()
        if l == '      --  TYPES':
            last = None
            for t in types:
                if last:
                    print last + ','
                last = '      Type_' + t
            print last
        elif l == '      --  FIELDS':
            last = None
            for f in funcs:
                if last:
                    print last + ','
                last = '      Field_' + f.name
            print last
        elif l == '   --  FUNCS':
            for t in types:
                print '   function Get_' + t
                print '      (N : ' + node_type + '; F : Fields_Enum) return ' \
                      + t + ';'
                print '   procedure Set_' + t
                print '      (N : ' + node_type + '; F : Fields_Enum; V: ' \
                      + t + ');'
                print
            for f in funcs:
                gen_has_func_spec(f.name, ';')
        elif l[0:3] == 'end':
            print l
            break
        else:
            print l
elif args.action == 'meta_body':
    lr = linereader(meta_base_file + '.adb.in')
    while True:
        l = lr.get().rstrip()
        if l == '      --  FIELDS_TYPE':
            last = None
            for f in funcs:
                if last:
                    print last + ','
                last = '      Field_' + f.name + ' => Type_' + f.rtype
            print last
        elif l == '         --  FIELD_IMAGE':
            for f in funcs:
                print '         when Field_' + f.name + ' =>'
                print '            return "' + f.name.lower() + '";'
        elif l == '         --  IIR_IMAGE':
            for k in kinds:
                print '         when ' + prefix_name + k + ' =>'
                print '            return "' + k.lower() + '";'
        elif l == '         --  FIELD_ATTRIBUTE':
            for f in funcs:
                print '         when Field_' + f.name + ' =>'
                if f.acc:
                    attr = f.acc
                else:
                    attr = 'None'
                print '            return Attr_' + attr + ';'
        elif l == '      --  FIELDS_ARRAY':
            last = None
            nodes_types = [node_type, node_type + '_List']
            ref_names = ['Ref', 'Of_Ref', 'Maybe_Ref']
            for k in kinds:
                v = nodes[k]
                if last:
                    print last + ','
                last = None
                print '      --  ' + prefix_name + k
                if flag_keep_order:
                    flds = v.order
                else:
                    # Sort fields: first non Iir and non Iir_List,
                    #              then Iir and Iir_List that aren't references
                    #              then Maybe_Ref
                    #              then Ref and Ref_Of
                    flds = sorted([fk for fk, fv in v.fields.items() \
                                   if fv and fv.rtype not in nodes_types])
                    flds += sorted([fk for fk, fv in v.fields.items() \
                                    if fv and fv.rtype in nodes_types \
                                    and fv.acc not in ref_names])
                    flds += sorted([fk for fk, fv in v.fields.items() \
                                    if fv and fv.rtype in nodes_types\
                                    and fv.acc in ['Maybe_Ref']])
                    flds += sorted([fk for fk, fv in v.fields.items() \
                                    if fv and fv.rtype in nodes_types\
                                    and fv.acc in ['Ref', 'Of_Ref']])
                for fk in flds:
                    if last:
                        print last + ','
                    last = '      Field_' + v.fields[fk].name
            if last:
                print last
        elif l == '      --  FIELDS_ARRAY_POS':
            pos = -1
            last = None
            for k in kinds:
                v = nodes[k]
                flds = [fk for fk, fv in v.fields.items() if fv]
                pos += len(flds)
                if last:
                    print last + ','
                last = '      ' + prefix_name + k + ' => {}'.format(pos)
            print last
        elif l == '   --  FUNCS_BODY':
            # Build list of types
            s = set([])
            for f in funcs:
                s |= set([f.rtype])
            types = [t for t in sorted(s)]
            for t in types:
                print '   function Get_' + t
                print '      (N : ' + node_type + '; F : Fields_Enum) return ' \
                      + t + ' is'
                print '   begin'
                print '      pragma Assert (Fields_Type (F) = Type_' + t + ');'
                print '      case F is'
                for f in funcs:
                    if f.rtype == t:
                        print '         when Field_' + f.name + ' =>'
                        print '            return Get_' + f.name + ' (N);';
                print '         when others =>'
                print '            raise Internal_Error;'
                print '      end case;'
                print '   end Get_' + t + ';'
                print
                print '   procedure Set_' + t
                print '      (N : ' + node_type + '; F : Fields_Enum; V: ' \
                      + t + ') is'
                print '   begin'
                print '      pragma Assert (Fields_Type (F) = Type_' + t + ');'
                print '      case F is'
                for f in funcs:
                    if f.rtype == t:
                        print '         when Field_' + f.name + ' =>'
                        print '            Set_' + f.name + ' (N, V);';
                print '         when others =>'
                print '            raise Internal_Error;'
                print '      end case;'
                print '   end Set_' + t + ';'
                print
            for f in funcs:
                gen_has_func_spec(f.name, ' is')
                choices = [k for k in kinds if f.name in nodes[k].attrs]
                if len(choices) == 0:
                    print '      pragma Unreferenced (K);'
                print '   begin'
                if len(choices) == 0:
                    print '      return False;'
                elif len(choices) == 1:
                    print '      return K = ' + prefix_name + choices[0] + ';'
                else:
                    print '      case K is'
                    gen_choices(choices)
                    print '            return True;'
                    print '         when others =>'
                    print '            return False;'
                    print '      end case;'
                print '   end Has_' + f.name + ';'
                print
        elif l[0:3] == 'end':
            print l
            break
        else:
            print l
