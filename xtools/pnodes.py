#!/usr/bin/env python

import re
import sys
import argparse

field_file = "../nodes.ads"
spec_file = "../iirs.ads"
template_file = "../iirs.adb.in"
template_disp_file = "../disp_tree.adb.in"
template_mark_file = "../nodes_gc.adb.in"
prefix_name = "Iir_Kind_"
prefix_range_name = "Iir_Kinds_"
type_name = "Iir_Kind"
conversions = ['uc', 'pos']

class FuncDesc:
    def __init__(self, name, field, conv, acc, display,
                 pname, ptype, rname, rtype):
        self.name = name
        self.field = field
        self.conv = conv
        self.acc = acc
        self.display = display # List of display attributes
        self.pname = pname # Parameter mame
        self.ptype = ptype # Parameter type
        self.rname = rname # value name (for procedure)
        self.rtype = rtype # value type

class NodeDesc:
    def __init__(self, name, format, fields, attrs):
        self.name = name
        self.format = format
        self.fields = fields # {field: FuncDesc} dict, defined for all fields
        self.attrs = attrs # A {attr: FuncDesc} dict

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
    try:
        while True:
            # 1) Search for description
            while True:
                # The common one
                if l == '   -- Common fields are:\n':
                    format_name = 'Common'
                    break
                # One for a format
                m = pat_fields.match(l)
                if m != None:
                    format_name = m.group(1)
                    if not format_name in fields:
                        raise ParseError(
                            lr, 'Format ' + format_name + ' is unknown');
                    break
                l = lr.get()

            # 2) Read field description
            l = lr.get()
            desc = common_desc
            while True:
                m = pat_field_desc.match(l)
                if m == None:
                    break
                desc[m.group(1)] = m.group(2)
                l = lr.get()

            # 3) Disp
            if format_name == 'Common':
                common_desc = desc
            else:
                fields[format_name] = desc
    except EndOfFile:
        pass

    return (formats, fields)

# Read kinds, kinds ranges and methods
def read_kinds(filename):
    lr = linereader(filename)
    kinds = []
    #  Search for 'type Iir_Kind is'
    while lr.get() != '   type ' + type_name + ' is\n':
        pass
    # Skip '('
    if lr.get() != '      (\n':
        raise ParseError(lr,
                         'no open parenthesis after "type ' + type_name +'"')

    # Read literals
    pat_node = re.compile('       ' + prefix_name + '(\w+),?( +-- .*)?\n')
    pat_comment = re.compile('( +-- .*)?\n')
    while True:
        l = lr.get()
        if l == '      );\n':
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
    pat_display = re.compile('   --  Display:(.*)\n')
    pat_field = re.compile('   --  Field: (\w+)'
                           + '( Ref| Chain_Next| Chain)?( .*)?\n')
    pat_conv = re.compile(' \((\w+)\)')
    pat_func = \
      re.compile('   function Get_(\w+) \((\w+) : (\w+)\) return (\w+);\n')
    pat_proc = \
      re.compile('   procedure Set_(\w+) \((\w+) : (\w+); (\w+) : (\w+)\);\n')
    while True:
        l = lr.get()
        if l == 'end Iirs;\n':
            break
        md = pat_display.match(l)
        if md:
            display = md.group(1).split()
            l = lr.get()
            m = pat_field.match(l)
            if not m:
                raise ParseError(lr, 'Field: expected after Display:')
        else:
            display = []
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
            funcs.append(FuncDesc(mf.group(1), m.group(1), conv, acc, display,
                                  mp.group(2), mp.group(3),
                                  mp.group(4), mp.group(5)))

    return (kinds, kinds_ranges, funcs)

# Read description for one node
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
        # Handle field
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
                    raise ParseError(lr, 'field does not exist in node')
                if not alias:
                    if c.fields[field]:
                        raise ParseError(lr, 'field already used')
                    c.fields[field] = func
                c.attrs[func.name] = func
            only_nodes = cur_nodes
        elif pat_start.match(l):
            raise ParseError(lr, 'bad line in node description')
        elif not pat_comment.match(l):
            raise ParseError(lr, 'bad line in node description')
        l = lr.get()

# Read description for all nodes
def read_nodes(filename, kinds_ranges, fields, funcs):
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

# Generate the Check_Kind_For_XXX function
def gen_check_kind(func, nodes, kinds):
    pname = 'Target'
    ptype = 'Iir'
    print '   procedure Check_Kind_For_' + func.name + ' (' + pname \
          + ' : ' + ptype + ') is'
    print '   begin'
    print '      case Get_Kind (' + pname + ') is'
    choices = [k for k in kinds if func.name in nodes[k].attrs]
    gen_choices(choices)
    print '            null;'
    print '         when others =>'
    print '            Failed ("' + func.name + '", ' + pname + ');'
    print '      end case;'
    print '   end Check_Kind_For_' + func.name + ';'
    print

def gen_subprg_header(decl):
    if len(decl) < 76:
        print decl + ' is'
    else:
        print decl
        print '      is'
    print '   begin'

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
    print '      Check_Kind_For_' + func.name + ' (' + func.pname + ');'
    print '      return ' + g + ';'
    print '   end Get_' + func.name + ';'
    print
    subprg =  '   procedure Set_' + func.name + ' (' \
          + func.pname + ' : ' + func.ptype + '; ' \
          + func.rname + ' : ' + func.rtype + ')'
    gen_subprg_header(subprg)
    print '      Check_Kind_For_' + func.name + ' (' + func.pname + ');'
    print '      Set_' + func.field + ' (' + func.pname + ', ' \
          + s + ');'
    print '   end Set_' + func.name + ';'
    print

def gen_image_field(func, param):
    getter = 'Get_' + func.name + ' (' + param + ')'
    if 'Image' in func.display:
        return func.rtype +  '\'Image (' + getter + ')'
    else:
        return 'Image_' + func.rtype + ' (' + getter + ')'

def gen_disp_header(kinds, nodes):
    print '   procedure Disp_Header (N : Iir) is'
    print '   begin'
    print '      if N = Null_Iir then'
    print '         Put_Line ("*null*");'
    print '         return;'
    print '      end if;'
    print
    print '      case Get_Kind (N) is'
    for k in kinds:
        inlines = [f for f in nodes[k].attrs.values() if 'Inline' in f.display]
        if len(inlines) > 1:
            raise Error
        print '         when ' + prefix_name + k + ' =>'
        if inlines:
            print '            Put ("' + k.lower() + ' " &'
            print '                      ' + \
                  gen_image_field(inlines[0], 'N') + ');'
        else:
            print '            Put ("' + k.lower() + '");'
    print '      end case;'
    print '      Put (\' \');'
    print '      Disp_Iir_Number (N);'
    print '      New_Line;'
    print '   end Disp_Header;'
    print

def funcs_of_node(n):
    return sorted([fv.name for fv in n.fields.values() if fv])

def gen_disp(kinds, nodes):
    print '   procedure Disp_Iir (N : Iir;'
    print '                       Indent : Natural := 1;'
    print '                       Flat : Boolean := False)'
    print '   is'
    print '      Sub_Indent : constant Natural := Indent + 1;'
    print '   begin'
    print '      Disp_Header (N);'
    print
    print '      if Flat or else N = Null_Iir then'
    print '         return;'
    print '      end if;'
    print
    print '      Header ("location: ", Indent);'
    print '      Put_Line (Image_Location_Type (Get_Location (N)));'
    print
    print '      --  Protect against infinite recursions.'
    print '      if Indent > 20 then'
    print '         Put_Indent (Indent);'
    print '         Put_Line ("...");'
    print '         return;'
    print '      end if;'
    print
    print '      case Get_Kind (N) is'
    done = []
    for k in kinds:
        if k in done:
            continue
        v = nodes[k]
        # Find other kinds with the same set of functions.
        vfuncs = funcs_of_node(v)
        ks = [k1 for k1 in kinds if \
              k1 not in done and funcs_of_node(nodes[k1]) == vfuncs]
        gen_choices(ks)
        done += ks
        flds = [fk for fk, fv in v.fields.items() if fv]
        if flds:
            for fk in sorted(flds):
                func = v.fields[fk]
                if func.acc == 'Chain_Next':
                    continue
                print '            ' + \
                      'Header ("' + func.name.lower() + ': ", Indent);'
                str = '            '
                if func.acc == 'Chain':
                    str += 'Disp_Chain (Get_' + func.name \
                           + ' (N), Sub_Indent);'
                    print str
                elif func.rtype in [ 'Iir', 'Iir_List', 'PSL_Node', 'PSL_NFA' ]:
                    str += 'Disp_' + func.rtype + \
                          ' (Get_' + func.name + ' (N), Sub_Indent'
                    if func.acc == 'Ref':
                        str += ', True'
                    str += ');'
                    print str
                else:
                    str += 'Put_Line ('
                    if len(func.rtype) <= 20:
                        str += gen_image_field(func, 'N')
                        print str + ');'
                    else:
                        # Inline version due to length
                        str += 'Image_' + func.rtype
                        print str
                        print '                      (' + \
                              'Get_' + func.name + ' (N)));'
        else:
            print '            null;'
    print '      end case;'
    print '   end Disp_Iir;'
    print

def gen_mark(kinds, nodes):
    print '   procedure Mark_Iir (N : Iir) is'
    print '   begin'
    print '      if N = Null_Iir then'
    print '         return;'
    print '      elsif Markers (N) then'
    print '         Already_Marked (N);'
    print '         return;'
    print '      else'
    print '         Markers (N) := True;'
    print '      end if;'
    print
    print '      case Get_Kind (N) is'
    done = []
    for k in kinds:
        if k in done:
            continue
        v = nodes[k]
        # Find other kinds with the same set of functions.
        vfuncs = funcs_of_node(v)
        ks = [k1 for k1 in kinds if \
              k1 not in done and funcs_of_node(nodes[k1]) == vfuncs]
        gen_choices(ks)
        done += ks
        flds = [fk for fk, fv in v.fields.items() if fv]
        empty = True
        for fk in sorted(flds):
            func = v.fields[fk]
            if func.acc in ['Ref', 'Chain_Next']:
                continue
            elif func.acc in [ 'Chain' ]:
                print '            ' + \
                      'Mark_Chain (Get_' + func.name + ' (N));'
                empty = False
            elif func.rtype in [ 'Iir', 'Iir_List', 'PSL_Node', 'PSL_NFA' ]:
                print '            ' + \
                      'Mark_' + func.rtype + ' (Get_' + func.name + ' (N));'
                empty = False
        if empty:
            print '            null;'
    print '      end case;'
    print '   end Mark_Iir;'
    print

parser = argparse.ArgumentParser(description='Meta-grammar processor')
parser.add_argument('action', choices=['disp-nodes', 'disp-kinds',
                                       'disp-fields', 'disp-funcs',
                                       'disp_tree', 'mark_tree',
                                       'get_format', 'body'],
                    default='disp-nodes')
args = parser.parse_args()

try:
    (formats, fields) = read_fields(field_file)
    (kinds, kinds_ranges, funcs) = read_kinds(spec_file)
    nodes = read_nodes(spec_file,kinds_ranges,fields,funcs)

except ParseError as e:
    print >> sys.stderr, e
    print >> sys.stderr, \
          "in {0}:{1}:{2}".format(e.lr.filename, e.lr.lineno, e.lr.l)
    sys.exit(1)

if args.action == 'disp-fields':
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
        s = '{0} ({1}'.format(f.name, f.field)
        if f.acc:
            s += ' acc:' + f.acc
        if f.conv:
            s += ' conv:' + f.conv
        s += ')'
        print s
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
                gen_check_kind(f, nodes, kinds)
                gen_get_set(f, nodes, fields)
        if l[0:3] == 'end':
            break
elif args.action == 'disp_tree':
    lr = linereader(template_disp_file)
    while True:
        l = lr.get().rstrip()
        print l
        if l == '   --  Subprograms':
            gen_disp_header(kinds, nodes)
            gen_disp(kinds, nodes)
        if l[0:3] == 'end':
            break
elif args.action == 'mark_tree':
    lr = linereader(template_mark_file)
    while True:
        l = lr.get().rstrip()
        print l
        if l == '   --  Subprograms':
            gen_mark(kinds,nodes)
        if l[0:3] == 'end':
            break
