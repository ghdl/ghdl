#!/usr/bin/env python
#  Generate the body of ieee.std_logic_1164 from a template.
#  This file is part of GHDL.
#  Copyright (C) 2015 Tristan Gingold
#
#  GHDL is free software; you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2, or (at your option) any later
#  version.
#
#  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#  for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with GCC; see the file COPYING2.  If not see
#  <http://www.gnu.org/licenses/>.

import re
import sys

# Supported versions
V87=0
V93=1
V08=2

binary_funcs = [ "and", "nand", "or", "nor", "xor" ]

# Python modelisation of std_ulogic type.
std_logic = "UX01ZWLH-"

# Normalization map.
ux01_map = { 'U': 'U',
             'X': 'X', 'Z': 'X', 'W': 'X', '-': 'X',
             '0': '0', 'L': '0',
             '1': '1', 'H': '1' }

def sl_and(a,b):
    """and definition"""
    na = ux01_map[a]
    nb = ux01_map[b]
    if na == '0' or nb == '0':
        return '0'
    if na == 'U' or nb == 'U':
        return 'U'
    if na == 'X' or nb == 'X':
        return 'X'
    return '1'

def sl_not(a):
    """not definition"""
    na = ux01_map[a]
    if na == 'U':
        return 'U'
    if na == 'X':
        return 'X'
    if na == '1':
        return '0'
    else:
        return '1'

def sl_or(a,b):
    "or definition"
    return sl_not(sl_and (sl_not(a), sl_not(b)))

def sl_xor(a,b):
    "xor definition"
    return sl_or(sl_and(a, sl_not(b)),
                 sl_and(sl_not(a), b))

# Stream to write.
out=sys.stdout

def w(s):
    "Write S to the output"
    out.write(s)

def gen_log_table2(name, func):
    "Generate a logic table for binary operator NAME using its model FUNC"
    w("""
  constant {0}_table : table_2d :=
  --  UX01ZWLH-
    (""".format(name))
    for a in std_logic:
        w('"')
        for b in std_logic:
            w(func (a, b))
        w('"')
        if a != '-':
            w(',')
        else:
            w(' ')
        w('   -- {}\n'.format(a))
        w('     ')
    w(');\n')

def gen_log_table1(name, func):
    "Generate a logic table for unary operator NAME using its model FUNC"
    w("""
  constant {0}_table : table_1d :=
  --  UX01ZWLH-
     """.format(name))
    w('"')
    for b in std_logic:
        w(func (b))
    w('";\n')

def disp_tables(version):
    "Generate logic tables"
    gen_log_table2("and", sl_and)
    gen_log_table2("nand", lambda a,b : sl_not(sl_and(a, b)))
    gen_log_table2("or", sl_or)
    gen_log_table2("nor", lambda a,b : sl_not(sl_or(a,b)))
    gen_log_table2("xor", sl_xor)
    if version >= V93:
        gen_log_table2("xnor", lambda a,b : sl_not(sl_xor(a,b)))
    gen_log_table1("not", sl_not)

vec_types = ['ulogic', 'logic']

def disp_scalar_binary(fun):
    "Generate scalar binary function body"
    w("""
  function "{0}" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return {0}_table (l, r);
  end "{0}";\n""".format(fun))

def disp_scalar_unary(fun):
    "Generate scalar unary function body"
    w("""
  function "{0}" (l : std_ulogic) return UX01 is
  begin
    return {0}_table (l);
  end "{0}";\n""".format(fun))

def disp_vec_binary(func, typ):
    "Generate vector binary function body"
    w("""
  function "{0}" (l, r : std_{1}_vector) return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_{1}_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded '{0}' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := {0}_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "{0}";\n""".format(func, typ))

def disp_vec_unary(func, typ):
    "Generate vector unary function body"
    w("""
  function "{0}" (l : std_{1}_vector) return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := {0}_table (la (I));
    end loop;
    return res;
  end "{0}";\n""".format(func, typ))

def disp_scal_vec_binary(func, typ):
    "Generate scalar-vector binary function body"
    w("""
  function "{0}" (l : std_{1}_vector; r : std_{1})
    return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := {0}_table (la (I), r);
    end loop;
    return res;
  end "{0}";\n""".format(func, typ))

def disp_vec_scal_binary(func, typ):
    "Generate vector-scalar binary function body"
    w("""
  function "{0}" (l : std_{1}; r : std_{1}_vector)
    return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := {0}_table (l, ra (I));
    end loop;
    return res;
  end "{0}";\n""".format(func, typ))

def disp_vec_reduction(func, typ):
    "Generate reduction function body"
    init = '1' if func in ['and', 'nand'] else '0'
    w("""
  function "{0}" (l : std_{1}_vector) return std_{1}
  is
    variable res : std_{1} := '{2}';
  begin
    for I in l'range loop
      res := {0}_table (l(I), res);
    end loop;
    return res;
  end "{0}";\n""".format(func, typ, init))

def gen_shift(is_left, plus, minus):
    if is_left:
        s = "res (1 to l'length {1} r) := la (r {0} 1 to res'right);\n"
    else:
        s = "res (1 {0} r to res'right) := la (1 to l'length {1} r);\n"
    w("      " + s.format(plus, minus))

def disp_shift_funcs(func, typ):
    "Generate shift functions"
    w("""
  function "{0}" (l : std_{1}_vector; r : integer)
    return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type := (others => '0');
  begin
    if r >= 0 then\n""".format(func, typ))
    gen_shift(func == "sll", '+', '-')
    w("    else\n")
    gen_shift(func != "sll", '-', '+')
    w("""    end if;
    return res;
  end "{0}";\n""".format(func, typ))

def gen_rot(is_left, plus, minus):
    if is_left:
        t = ["res (1 to res'right {1} rm) := la (rm {0} 1 to la'right);",
             "res (res'right {1} rm + 1 to res'right) := la (1 to rm);"]
    else:
        t = ["res (1 {0} rm to res'right) := la (1 to la'right {1} r);",
             "res (1 to rm) := la (la'right {1} rm + 1 to la'right);"]
    for s in t:
        w("      ")
        w(s.format(plus, minus))
        w('\n')

def disp_rot_funcs(func, typ):
    "Generate rotation functions"
    w("""
  function "{0}" (l : std_{1}_vector; r : integer)
    return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
    constant rm : integer := r mod l'length;
  begin
    if r >= 0 then\n""".format(func, typ))
    gen_rot(func == "rol", '+', '-')
    w("    else\n")
    gen_rot(func != "rol", '-', '+')
    w("""    end if;
    return res;
  end "{0}";\n""".format(func, typ))

def disp_all_log_funcs(version):
    "Generate all function bodies for logic operators"
    for f in binary_funcs:
        disp_scalar_binary(f)
    disp_scalar_unary("not")
    for v in vec_types:
        typ = "std_" + v + "_vector"
        for f in binary_funcs:
            disp_vec_binary(f, v)
        disp_vec_unary("not", v)
        if version >= V08:
            for f in binary_funcs:
                disp_scal_vec_binary(f, v)
                disp_vec_scal_binary(f, v)
            for f in binary_funcs:
                disp_vec_reduction(f, v)
            disp_shift_funcs("sll", v)
            disp_shift_funcs("srl", v)
            disp_rot_funcs("rol", v)
            disp_rot_funcs("ror", v)

def disp_sv_to_bv_conv(typ):
    "Generate logic vector to bit vector function body"
    w("""
  function to_bitvector (s : std_{0}_vector; xmap : bit := '0')
    return bit_vector
  is
    subtype res_range is natural range s'length - 1 downto 0;
    alias as : std_{0}_vector (res_range) is s;
    variable res : bit_vector (res_range);
    variable b : bit;
  begin
    for I in res_range loop
      --  Inline for efficiency.
      case as (I) is
        when '0' | 'L' =>
          b := '0';
        when '1' | 'H' =>
          b := '1';
        when others =>
          b := xmap;
      end case;
      res (I) := b;
    end loop;
    return res;
  end to_bitvector;\n""".format(typ))

def disp_bv_to_sv_conv(typ):
    "Generate bit vector to logic vector function body"
    w("""
  function to_std{0}vector (b : bit_vector) return std_{0}_vector is
    subtype res_range is natural range b'length - 1 downto 0;
    alias ab : bit_vector (res_range) is b;
    variable res : std_{0}_vector (res_range);
  begin
    for I in res_range loop
      res (I) := bit_to_std (ab (I));
    end loop;
    return res;
  end to_std{0}vector;\n""".format(typ))

def disp_sv_to_sv_conv(s,d):
    "Generate logic vector to logic vector function body"
    w("""
  function to_std{1}vector (s : std_{0}_vector) return std_{1}_vector
  is
    subtype res_type is std_{1}_vector (s'length - 1 downto 0);
  begin
    return res_type (s);
  end to_std{1}vector;\n""".format(s,d))

def disp_all_conv_funcs(version):
    "Generate conversion function bodies"
    for v in vec_types:
        disp_sv_to_bv_conv(v)
    for v in vec_types:
        disp_bv_to_sv_conv(v)
    if version >= V08:
        disp_bv_to_sv_conv('logic')
    disp_sv_to_sv_conv('logic', 'ulogic')
    disp_sv_to_sv_conv('ulogic', 'logic')

def disp_conv_vec_vec(typ, v):
    "Generate function body for vector conversion"
    utyp = typ.upper();
    w("""
  function to_{1} (s : std_{2}_vector) return std_{2}_vector
  is
    subtype res_type is std_{2}_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_{0} (sa (i));
    end loop;
    return res;
  end to_{1};\n""".format(typ, utyp, v))

def disp_conv_std(typ):
    "Generate function body for scalar conversion"
    utyp = typ.upper();
    w("""
  function to_{1} (s : std_ulogic) return {1} is
  begin
    return std_to_{0} (s);
  end to_{1};\n""".format(typ, utyp))

def disp_conv_bv_vec(typ, v):
    "Generate function body for bit vector conversion"
    utyp = typ.upper();
    w("""
  function to_{1} (b : bit_vector) return std_{2}_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_{2}_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_{1};\n""".format(typ, utyp, v))

def disp_conv_b_t(typ):
    "Generate function body for bit conversion"
    utyp = typ.upper();
    w("""
  function to_{1} (b : bit) return {1} is
  begin
    return bit_to_x01 (b);
  end to_{1};\n""".format(typ, utyp))

def disp_conv_01():
    "Generate to_01 bodies"
    w("""
  function to_01 (s : std_{0}_vector; xmap : std_ulogic := '0')
    return std_{0}_vector
  is
    subtype res_type is std_{0}_vector (s'length - 1 downto 0);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      case sa(i) is
        when '0' | 'L' => res (i) := '0';
        when '1' | 'H' => res (i) := '1';
        when others    => return res_type'(others => xmap);
      end case;
    end loop;
    return res;
  end to_01;\n""".format("ulogic"))
    w("")
    w("""
  function to_01 (s : std_{0}; xmap : std_ulogic := '0')
    return std_{0} is
  begin
    case s is
      when '0' | 'L' => return '0';
      when '1' | 'H' => return '1';
      when others    => return xmap;
    end case;
  end to_01;\n""".format("ulogic"))
    w("")
    w("""
  function to_01 (s : bit_vector; xmap : std_ulogic := '0')
    return std_{0}_vector
  is
    alias sa : bit_vector(s'length - 1 downto 0) is s;
    variable res : std_{0}_vector (s'length - 1 downto 0);
  begin
    for i in sa'range loop
      res (i) := bit_to_std (sa (i));
    end loop;
    return res;
  end to_01;\n""".format("ulogic"))
    w("")
    w("""
  function to_01 (s : bit; xmap : std_ulogic := '0')
    return std_{0} is
  begin
    return bit_to_std(s);
  end to_01;\n""".format("ulogic"))

def disp_cond():
    w("""
  function "??" (l : std_ulogic) return boolean is
  begin
    return l = '1' or l = 'H';
  end "??";\n""")

def disp_all_norm_funcs(version):
    "Generate all function bodies for conversion"
    if version >= V08:
        disp_conv_01()
    for typ in [ "x01", "x01z", "ux01" ]:
        for v in vec_types:
            disp_conv_vec_vec(typ, v)
        disp_conv_std (typ)
        for v in vec_types:
            disp_conv_bv_vec(typ, v)
        disp_conv_b_t(typ)
    if version >= V08:
        disp_cond()

def disp_all_isx_funcs(version):
    "Generate all function bodies for isx functions"
    for v in vec_types:
        w("""
  function is_X (s : std_{0}_vector) return boolean is
  begin
    for i in s'range loop
      if std_x (s (i)) then
        return true;
      end if;
    end loop;
    return false;
  end is_X;\n""".format(v))

    w("""
  function is_X (s : std_ulogic) return boolean is
  begin
    return std_x (s);
  end is_X;\n""")


# Patterns to replace
pats = {'  @TAB\n' : disp_tables,
        '  @LOG\n' : disp_all_log_funcs,
        '  @CONV\n': disp_all_conv_funcs,
        '  @NORM\n': disp_all_norm_funcs,
        '  @ISX\n' : disp_all_isx_funcs }

spec_file='std_logic_1164.proto'
proto_file='std_logic_1164-body.proto'

def gen_body(filename, version):
    global out
    out = open(filename, 'w')
    w('--  This -*- vhdl -*- file was generated from ' + proto_file + '\n')
    keep = True
    for line in open(proto_file):
        if line in pats:
            pats[line](version)
            continue
        if line == '  @BEG V08\n':
            keep = (version == V08)
            continue
        if line == '  @END V08\n':
            keep = True
            continue
        if keep:
            w(line)
    out.close()

VDICT={'--V87':  lambda x: x == V87,
       '--!V87': lambda x: x != V87,
       '--V93':  lambda x: x == V93,
       '--V08':  lambda x: x == V08,
       '--!V08': lambda x: x != V08}

def preprocess_line(line, version):
    for p in VDICT:
        pos = line.find(p)
        if pos >= 0:
            if not VDICT[p](version):
                return None
            l = line[:pos].rstrip() + '\n'
            return l
    return line

def copy_spec(dest, version):
    out=open(dest, 'w')
    for line in open(spec_file):
        l = preprocess_line(line, version)
        if l is not None:
            out.write(l)
    out.close()

# Copy spec
copy_spec('v87/std_logic_1164.vhdl', V87)
copy_spec('v93/std_logic_1164.vhdl', V93)
copy_spec('v08/std_logic_1164.vhdl', V08)

# Generate bodies
gen_body('v87/std_logic_1164-body.vhdl', V87)

binary_funcs.append("xnor")
gen_body('v93/std_logic_1164-body.vhdl', V93)

vec_types = ['ulogic']
gen_body('v08/std_logic_1164-body.vhdl', V08)
