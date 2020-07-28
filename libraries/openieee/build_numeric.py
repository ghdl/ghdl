#!/usr/bin/env python
#  Generate the body of ieee.numeric_std and numeric_bit from a template.
#  The implementation is based only on the specification and on testing (as
#  the specifications are often ambiguous).
#  The algorithms are very simple: carry ripple adder, restoring division.
#  This file is part of GHDL.
#  Both this file and the outputs of this file are copyrighted.
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

# My python 'style' and knowledge is basic...  Do not hesitate to comment.

binary_funcs = [ "and", "nand", "or", "nor", "xor" ]
compare_funcs = [ "=", "/=", ">", ">=", "<", "<=" ]

vec_types = ['UNSIGNED', 'SIGNED']

logics = ['bit', 'std']
logic_types = {'bit' : 'bit', 'std': 'sl_x01' }
logic_undefs = {'bit' : "'0'", 'std': "'X'" }

logic = 'xx' # Current logic, either bit or std

v93=False

# Stream to write.
out=sys.stdout

def w(s):
    "Write S to the output"
    out.write(s)

def logic_type():
    return logic_types[logic]

def logic_undef():
    return logic_undefs[logic]

def disp_vec_binary(func, typ):
    "Generate the body of a vector binary logic function"
    res = """
  function "{0}" (l, r : {1}) return {1}
  is
    subtype res_type is {1} (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : {1} (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""{0}"": arguments are not of the same length"
        severity failure;
      res := (others => """ + logic_undef() + """);
    else
      for I in res_type'range loop
        res (I) := la (I) {0} ra (I);
      end loop;
    end if;
    return res;
  end "{0}";\n"""
    w (res.format(func, typ))

def disp_non_logical_warning(func):
    return """
        assert NO_WARNING
          report "NUMERIC_STD.""{0}"": non logical value detected"
          severity warning;""".format(func)

def conv_bit(expr):
    if logic == 'std':
        return "sl_to_x01 (" + expr + ")"
    else:
        return expr

def extract_bit(name):
    res = "{0}b := " + conv_bit ("{0}a (i)") + ";"
    return res.format(name)

def init_carry(func):
    if func == '+':
        return """
    carry := '0';"""
    else:
        return """
    carry := '1';"""

def extract_extend_bit(name,typ):
    res = """
       if i > {0}a'left then
          {0}b := """
    if typ == 'UNSIGNED':
        res += "'0';"
    else:
        res += "{0} ({0}'left);"
    res += """
       else
          """ + extract_bit(name) + """
       end if;"""
    return res.format(name)

def disp_vec_vec_binary(func, typ):
    "Generate vector binary function body"
    res = """
  function "{0}" (l, r : {1}) return {1}
  is
    constant lft : integer := MAX (l'length, r'length) - 1;
    subtype res_type is {1} (lft downto 0);
    alias la : {1} (l'length - 1 downto 0) is l;
    alias ra : {1} (r'length - 1 downto 0) is r;
    variable res : res_type;
    variable lb, rb, carry : """ + logic_type () + """;
  begin
    if la'left < 0 or ra'left < 0 then
       return null_{1};
    end if;"""

    res += init_carry(func)

    res += """
    for i in 0 to lft loop"""
    res += extract_extend_bit('l', typ)
    res += extract_extend_bit('r', typ)

    if logic == 'std':
        res += """
       if lb = 'X' or rb = 'X' then""" + \
       disp_non_logical_warning(func) + """
          res := (others => 'X');
          exit;
        end if;"""
    if func == '-':
        res += """
      rb := not rb;"""
    res += """
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    return res;
  end "{0}";
"""
    w (res.format (func, typ))

def declare_int_var(name, typ):
    res = """
     variable {0}1, {0}2 : {1};
     variable {0}d : nat1;""";
    if typ == "INTEGER":
        res += """
     constant {0}msb : nat1 := boolean'pos({0} < 0);"""
    return res.format(name, typ)

def init_int_var(name, typ):
    return """
    {0}1 := {0};""".format(name);

def extract_int_lsb(name, typ):
    res = """
       {0}2 := {0}1 / 2;"""
    if typ == "INTEGER":
        res += """
       if {0}1 < 0 then
          {0}d := 2 * {0}2 - {0}1;
          {0}1 := {0}2 - {0}d;
        else
          {0}d := {0}1 - 2 * {0}2;
          {0}1 := {0}2;
        end if;"""
    else:
        res += """
       {0}d := {0}1 - 2 * {0}2;
       {0}1 := {0}2;"""
    res += """
       {0}b := nat1_to_01 ({0}d);"""
    return res.format(name,typ)

def check_int_truncated(func, name, typ):
    if typ == "INTEGER":
        v = "-{0}msb".format(name)
    else:
        v = "0"
    return """
    if {1}1 /= {2} then
      assert NO_WARNING
        report "NUMERIC_STD.""{0}"": vector is truncated"
        severity warning;
    end if;""".format(func, name, v)

def create_vec_int_dict(func, left, right):
    if left in vec_types:
        dic = {'vtype': left,
               'itype': right,
               'vparam': 'l',
               'iparam': 'r'}
    else:
        dic = {'vtype': right,
               'itype': left,
               'vparam': 'r',
               'iparam': 'l'}
    dic.update({'ltype': left,
                'rtype': right,
                'func': func,
                'logic': logic_type()})
    return dic

def disp_vec_int_binary(func, left, right):
    "Generate vector binary function body"
    dic = create_vec_int_dict(func, left, right)
    res = """
  function "{func}" (l : {ltype}; r : {rtype}) return {vtype}
  is
    subtype res_type is {vtype} ({vparam}'length - 1 downto 0);
    alias {vparam}a : res_type is {vparam};""" + \
    declare_int_var (dic["iparam"], dic["itype"]) + """
    variable res : res_type;
    variable lb, rb, carry : {logic};
  begin
    if res'length < 0 then
       return null_{vtype};
    end if;"""

    # Initialize carry.  For subtraction, use 2-complement.
    res += init_carry(func)

    res += init_int_var(dic['iparam'], dic['itype']) + """
    for i in res'reverse_range loop
       """ + extract_bit(dic['vparam']) + "\n" + \
       extract_int_lsb(dic['iparam'], dic['itype']);

    if logic == 'std':
       res += """
       if {vparam}b = 'X' then""" + \
    disp_non_logical_warning(func) + """
        res := (others => 'X');
        {iparam}1 := 0;
        exit;
      end if;"""

    # 2-complement for subtraction
    if func == '-':
        res += """
      rb := not rb;"""

    res += """
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;""" + \
    check_int_truncated(func, dic['iparam'], dic['itype']) + """
    return res;
  end "{func}";\n"""
    w(res.format (**dic))

def disp_vec_int_gcompare(func, left, right):
    "Generate comparison function"
    dic = create_vec_int_dict(func, left, right)
    res = """
  function {func} (l : {ltype}; r : {rtype}) return compare_type
  is
    subtype res_type is {vtype} ({vparam}'length - 1 downto 0);
    alias la : res_type is l;""" + \
    declare_int_var (dic['iparam'], dic['itype']) + """
    variable lb, rb : {logic};
    variable res : compare_type;
  begin
    res := compare_eq;""";

    res += init_int_var(dic['iparam'], dic['itype']) + """
    for i in {vparam}a'reverse_range loop
       """ + extract_bit (dic['vparam']) + \
       extract_int_lsb("r", right)

    if logic == 'std':
        res += """
       if {vparam}b = 'X' then
         return compare_unknown;
       end if;"""

    res += """
       if lb = '1' and rb = '0' then
         res := compare_gt;
       elsif lb = '0' and rb = '1' then
         res := compare_lt;
       end if;
    end loop;"""
    if func == "ucompare":
        res += """
    if r1 /= 0 then
       res := compare_lt;
    end if;"""
    else:
        res += """
    if """ + conv_bit ("l (l'left)") + """ = '1' then
      if r >= 0 then
         res := compare_lt;
      end if;
    else
      if r < 0 then
         res := compare_gt;
      end if;
    end if;"""
    res += """
    return res;
  end {func};
"""
    w(res.format (**dic))

def disp_vec_int_compare(func, left, right):
    "Generate comparison function"
    dic = create_vec_int_dict(func, left, right)
    res = """
  function "{func}" (l : {ltype}; r : {rtype}) return boolean
  is
     subtype res_type is {vtype} ({vparam}'length - 1 downto 0);
     alias {vparam}a : res_type is {vparam};""" + \
    declare_int_var (dic['iparam'], dic['itype']) + """
     variable res : compare_type;
  begin
     if {vparam}'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""{func}"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := """

    if left == "SIGNED" or right == "SIGNED":
        res += "scompare"
    else:
        res += "ucompare"
    if left in vec_types:
        res += " (l, r);"
    else:
        res += " (r, l);"
    if logic == 'std':
        res += """
     if res = compare_unknown then""" + \
    disp_non_logical_warning(func) + """
      return false;
     end if;"""

    if left in vec_types:
        res += """
     return res {func} compare_eq;"""
    else:
        res += """
     return compare_eq {func} res;"""
    res += """
  end "{func}";
"""
    w(res.format (**dic))

def disp_vec_vec_gcompare(func, typ):
    "Generate comparison function"
    res = """
  function {func} (l, r : {typ}) return compare_type
  is
     constant sz : integer := MAX (l'length, r'length) - 1;
     alias la : {typ} (l'length - 1 downto 0) is l;
     alias ra : {typ} (r'length - 1 downto 0) is r;
     variable lb, rb : {logic};
     variable res : compare_type;
  begin"""
    if typ == 'SIGNED':
        res += """
     --  Consider sign bit as S * -(2**N).
     lb := """ + conv_bit ("la (la'left)") + """;
     rb := """ + conv_bit ("ra (ra'left)") + """;
     if lb = '1' and rb = '0' then
        return compare_lt;
     elsif lb = '0' and rb = '1' then
        return compare_gt;
     else
        res := compare_eq;
     end if;"""
    else:
        res += """
     res := compare_eq;"""
    if typ == 'SIGNED':
        res += """
     for i in 0 to sz - 1 loop"""
    else:
        res += """
    for i in 0 to sz loop"""
    res += extract_extend_bit('l', typ)
    res += extract_extend_bit('r', typ)
    if logic == 'std':
        res += """
       if lb = 'X' or rb = 'X' then
         return compare_unknown;
       end if;"""

    res += """
       if lb = '1' and rb = '0' then
         res := compare_gt;
       elsif lb = '0' and rb = '1' then
         res := compare_lt;
       end if;
     end loop;

     return res;
  end {func};\n"""
    w(res.format (func=func, typ=typ, logic=logic_type()))

def disp_vec_vec_compare(func, typ):
    "Generate comparison function"
    res = """
  function "{func}" (l, r : {typ}) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""{func}"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := """
    if typ == "SIGNED":
        res += "scompare"
    else:
        res += "ucompare"
    res += """ (l, r);"""

    if logic == 'std':
        res += """
     if res = compare_unknown then""" + \
    disp_non_logical_warning(func) + """
       return false;
     end if;"""

    res += """
     return res {func} compare_eq;
  end "{func}";\n"""
    w(res.format (func=func, typ=typ))

def disp_vec_not(typ):
    "Generate vector binary function body"
    w("""
  function "not" (l : {0}) return {0}
  is
    subtype res_type is {0} (l'length - 1 downto 0);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := not la (I);
    end loop;
    return res;
  end "not";\n""".format(typ))

def disp_resize(typ):
    res = """
  function resize (ARG : {0}; NEW_SIZE: natural) return {0}
  is
     alias arg1 : {0} (ARG'length - 1 downto 0) is arg;
     variable res : {0} (new_size - 1 downto 0) := (others => '0');
  begin
     if new_size = 0 then
        return null_{0};
     end if;
     if arg1'length = 0 then
        return res;
     end if;
     if arg1'length > new_size then
        --  Reduction."""
    if typ == 'SIGNED':
        res += """
        res (res'left) := arg1 (arg1'left);
        res (res'left - 1 downto 0) := arg1 (res'left - 1 downto 0);"""
    else:
        res += """
        res := arg1 (res'range);"""
    res += """
     else
        --  Expansion
        res (arg1'range) := arg1;"""
    if typ == 'SIGNED':
        res += """
        res (res'left downto arg1'length) := (others => arg1 (arg1'left));"""
    res += """
    end if;
    return res;
  end resize;\n"""
    w(res.format(typ))

def gen_shift(dir, inv):
    if (dir == 'left') ^ inv:
        res = """
       res (res'left downto {opp}count) := arg1 (arg1'left {sub} count downto 0);"""
    else:
        res = """
       res (res'left {sub} count downto 0) := arg1 (arg1'left downto {opp}count);"""
    if inv:
        return res.format(opp="-", sub="+")
    else:
        return res.format(opp="", sub="-")

def disp_shift_op(name, typ, dir):
    res = """
  function {0} (ARG : {1}; COUNT: INTEGER) return {1}
  is
     subtype res_type is {1} (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
  begin
     if res'length = 0 then
        return null_{1};
     end if;
     if count >= 0 and count <= arg1'left then"""
    res += gen_shift(dir, False)
    res += """
     elsif count < 0 and count >= -arg1'left then"""
    res += gen_shift(dir, True)
    res += """
     end if;
     return res;
  end {0};\n"""
    w(res.format(name, typ))

def disp_shift(name, typ, dir):
    res = """
  function {0} (ARG : {1}; COUNT: NATURAL) return {1}
  is
     subtype res_type is {1} (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => """
    if typ == 'SIGNED' and dir == 'right':
        res += "arg1 (arg1'left)"
    else:
        res += "'0'"
    res += """);
  begin
     if res'length = 0 then
        return null_{1};
     end if;
     if count <= arg1'left then"""
    res += gen_shift(dir, False)
    res += """
     end if;
     return res;
  end {0};\n"""
    w(res.format(name, typ))

def disp_rotate(name, typ, dir):
    if 'rotate' in name:
        count_type = 'natural'
        op = 'rem'
    else:
        count_type = 'integer'
        op = 'mod'
    res = """
  function {0} (ARG : {1}; COUNT: {2}) return {1}
  is
     subtype res_type is {1} (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
     variable cnt : natural;
  begin
     if res'length = 0 then
        return null_{1};
     end if;
     cnt := count """ + op + " res'length;"
    if dir == 'left':
        res += """
     res (res'left downto cnt) := arg1 (res'left - cnt downto 0);
     res (cnt - 1 downto 0) := arg1 (res'left downto res'left - cnt + 1);"""
    else:
        res += """
     res (res'left - cnt downto 0) := arg1 (res'left downto cnt);
     res (res'left downto res'left - cnt + 1) := arg1 (cnt - 1 downto 0);"""
    res += """
     return res;
  end {0};\n"""
    w(res.format(name, typ, count_type))

def disp_vec_vec_mul(func, typ):
    res = """
  function "{0}" (L, R : {1}) return {1}
  is
     alias la : {1} (L'Length - 1 downto 0) is l;
     alias ra : {1} (R'Length - 1 downto 0) is r;
     variable res : {1} (L'length + R'Length -1 downto 0) := (others => '0');
     variable rb, lb, vb, carry : """ + logic_type() + """;
  begin
     if la'length = 0 or ra'length = 0 then
        return null_{1};
     end if;
     --  Shift and add L.
     for i in natural range 0 to ra'left """
    if typ == 'SIGNED':
        res += "- 1 "
    res += """loop
       """ + extract_bit ('r') + """
       if rb = '1' then
          --  Compute res := res + shift_left (l, i).
          carry := '0';
          for j in la'reverse_range loop
            lb := la (j);
            vb := res (i + j);
            res (i + j) := compute_sum (carry, vb, lb);
            carry := compute_carry (carry, vb, lb);
          end loop;"""
    if typ == 'UNSIGNED':
        res += """
          --  Propagate carry.
          for j in i + la'length to res'left loop
             exit when carry = '0';
             vb := res (j);
             res (j) := carry xor vb;
             carry := carry and vb;
          end loop;"""
    else:
        res += """
          --  Sign extend and propagate carry.
          lb := la (la'left);
          for j in i + l'length to res'left loop
             vb := res (j);
             res (j) := compute_sum (carry, vb, lb);
             carry := compute_carry (carry, vb, lb);
          end loop;"""
    if logic == 'std':
        res += """
       elsif rb = 'X' then""" + \
        disp_non_logical_warning (func)
    res += """
       end if;
     end loop;"""
    if typ == 'SIGNED':
        res += """
     if ra (ra'left) = '1' then
        --  R is a negative number.  It is considered as:
        --   -2**n + (Rn-1 Rn-2 ... R0).
        --  Compute res := res - 2**n * l.
        carry := '1';
        for i in la'reverse_range loop
          vb := res (ra'length - 1 + i);
          lb := not la (i);
          res (ra'length - 1+ i) := compute_sum (carry, vb, lb);
          carry := compute_carry (carry, vb, lb);
        end loop;
        vb := res (res'left);
        lb := not la (la'left);
        res (res'left) := compute_sum (carry, vb, lb);
     end if;"""
    res += """
     return res;
  end "{0}";\n"""
    w(res.format(func,typ))

def disp_vec_int_mul(left, right):
    res = """
  function "*" (L : {0}; R : {1}) return {0}
  is
     constant size : natural := l'length;
  begin
     if size = 0 then
        return null_{0};
     end if;
     return l * to_{0} (r, size);
  end "*";\n"""
    w (res.format(left,right))

def disp_int_vec_mul(left, right):
    res = """
  function "*" (L : {0}; R : {1}) return {1}
  is
     constant size : natural := r'length;
  begin
     if size = 0 then
        return null_{1};
     end if;
     return r * to_{1} (l, size);
  end "*";\n"""
    w (res.format(left,right))

def disp_neg(func):
    res = """
  function "{func}" (ARG : SIGNED) return SIGNED
  is
     subtype arg_type is SIGNED (ARG'length - 1 downto 0);
     alias arga : arg_type is arg;
     variable res : arg_type;
     variable carry, a : """ + logic_type() + """;
  begin
     if arga'length = 0 then
        return null_signed;
     end if;"""
    if logic == 'std':
        res += """
     if has_0x (arga) = 'X' then""" + \
       disp_non_logical_warning("-") + """
       return arg_type'(others => 'X');
     end if;"""
    if func == 'abs':
        res += """
     if arga (arga'left) = '0' then
        return arga;
     end if;"""
    res += """
     carry := '1';
     for i in arga'reverse_range loop
       a := not arga (i);
       res (i) := carry xor a;
       carry := carry and a;
     end loop;
     return res;
  end "{func}";\n"""
    w(res.format(func=func))

def disp_has_0x(typ):
    res = """
  function has_0x (a : {0}) return {1}
  is
     variable res : {1} := '0';
  begin
     for i in a'range loop"""
    if logic == 'std':
      res += """
      if a (i) = 'X' then
         return 'X';
      end if;"""
    res += """
      res := res or a (i);
    end loop;
    return res;
  end has_0x;\n"""
    w(res.format(typ, logic_type()))

def disp_size():
    w("""
  function size_unsigned (n : natural) return natural
  is
     --  At least one bit (even for 0).
     variable res : natural := 1;
     variable n1 : natural := n;
  begin
     while n1 > 1 loop
        res := res + 1;
        n1 := n1 / 2;
     end loop;
     return res;
  end size_unsigned;\n""")

    w("""
  function size_signed (n : integer) return natural
  is
     variable res : natural := 1;
     variable n1 : natural;
  begin
     if n >= 0 then
        n1 := n;
     else
        -- Use /N = -X -1 = -(X + 1)  (No overflow).
        n1 := -(n + 1);
     end if;
     while n1 /= 0 loop
        res := res + 1;
        n1 := n1 / 2;
     end loop;
     return res;
  end size_signed;\n""")

def disp_divmod():
    w("""
  --  All index range are normalized (N downto 0).
  --  NUM and QUOT have the same range.
  --  DEM and REMAIN have the same range.
  --  No 'X'.
  procedure divmod (num, dem : UNSIGNED; quot, remain : out UNSIGNED)
  is
     --  An extra bit is needed so that it is always possible that DEM >= REG.
     variable reg : unsigned (dem'left + 1 downto 0) := (others => '0');
     variable sub : unsigned (dem'range) := (others => '0');
     variable carry, d : """ + logic_type () + """;
  begin
     for i in num'range loop
        --  Shift to add a new bit from NUM to REG.
        reg (reg'left downto 1) := reg (reg'left - 1 downto 0);
        reg (0) := num (i);
        --  Substract: REG - DEM
        carry := '1';
        for j in dem'reverse_range loop
           d := not dem (j);
           sub (j) := compute_sum (carry, reg (j), d);
           carry := compute_carry (carry, reg (j), d);
        end loop;
        --  Do not forget the extra bit in REG.
        carry := compute_carry (carry, reg (reg'left), '1');
        --  Test
        if carry = '0' then
           --  REG < DEM
           quot (i) := '0';
        else
           --  REG >= DEM: do the substraction
           quot (i) := '1';
           reg (reg'left) := '0';
           reg (sub'range) := sub;
        end if;
     end loop;
     remain := reg (dem'range);
  end divmod;
""")

def disp_vec_vec_udiv(func):
    res = """
  function "{func}" (L, R : UNSIGNED) return UNSIGNED
  is
     subtype l_type is UNSIGNED (L'length - 1 downto 0);
     subtype r_type is UNSIGNED (R'length - 1 downto 0);
     alias la : l_type is l;
     alias ra : r_type is r;
     variable quot : l_type;
     variable rema : r_type;
     variable r0 : """ + logic_type() + """ := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_unsigned;
     end if;"""
    if logic == 'std':
        res += """
     if has_0x (l) = 'X' or r0 = 'X' then""" + \
        disp_non_logical_warning ('/') + """
        return l_type'(others => 'X');
     end if;"""
    res += """
     assert r0 /= '0'
        report "NUMERIC_STD.""{func}"": division by 0"
        severity error;
     divmod (la, ra, quot, rema);"""
    if func == '/':
       res += """
     return quot;"""
    else:
       res += """
     return rema;"""
    res += """
  end "{func}";\n"""
    w(res.format(func=func))

def disp_vec_int_udiv(func):
    res = """
  function "{func}" (L : UNSIGNED; R : NATURAL) return UNSIGNED
  is
     constant r_size : natural := size_unsigned (r);
  begin
     if l'length = 0 then
        return null_unsigned;
     end if;"""
    if func in ['mod', 'rem']:
        res += """
     return resize (l {func} to_unsigned (r, r_size), l'length);"""
    else:
        res += """
     return l {func} to_unsigned (r, r_size);"""
    res += """
  end "{func}";\n"""
    w(res.format(func=func))

    res = """
  function "{func}" (L : NATURAL; R : UNSIGNED) return UNSIGNED
  is
     constant l_size : natural := size_unsigned (l);
  begin
     if r'length = 0 then
        return null_unsigned;
     end if;"""
    if func == '/':
        res += """
     return resize (to_unsigned (l, l_size) {func} r, r'length);"""
    else:
        res += """
     return to_unsigned (l, l_size) {func} r;"""
    res += """
  end "{func}";\n"""
    w(res.format(func=func))

def disp_vec_vec_sdiv(func):
    res = """
  function "{func}" (L, R : SIGNED) return SIGNED
  is
     subtype l_type is SIGNED (L'length - 1 downto 0);
     subtype r_type is SIGNED (R'length - 1 downto 0);
     alias la : l_type is l;
     alias ra : r_type is r;
     subtype l_utype is UNSIGNED (l_type'range);
     subtype r_utype is UNSIGNED (r_type'range);
     variable lu : l_utype;
     variable ru : r_utype;
     variable quot : l_utype;
     variable rema : r_utype;
     variable r0 : """ + logic_type() + """ := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_signed;
     end if;"""
    if logic == 'std':
        res += """
     if has_0x (l) = 'X' or r0 = 'X' then""" + \
        disp_non_logical_warning (func) + """
        return l_type'(others => 'X');
     end if;"""
    res += """
     assert r0 /= '0'
        report "NUMERIC_STD.""{func}"": division by 0"
        severity error;"""
    res += """
     if la (la'left) = '1' then
        lu := unsigned (-la);
     else
        lu := unsigned (la);
     end if;
     if ra (ra'left) = '1' then
        ru := unsigned (-ra);
     else
        ru := unsigned (ra);
     end if;
     divmod (lu, ru, quot, rema);"""
    if func == '/':
        res += """
     if (ra (ra'left) xor la (la'left)) = '1' then
       return -signed (quot);
     else
       return signed (quot);
     end if;"""
    elif func == 'rem':
        res += """
     --  Result of rem has the sign of the dividend.
     if la (la'left) = '1' then
       return -signed (rema);
     else
       return signed (rema);
     end if;"""
    elif func == 'mod':
        res += """
     --  Result of mod has the sign of the divisor.
     if rema = r_utype'(others => '0') then
        --  If the remainder is 0, then the modulus is 0.
        return signed (rema);
     else
        if ra (ra'left) = '1' then
           if la (la'left) = '1' then
              return -signed (rema);
           else
              return ra + signed (rema);
           end if;
        else
           if la (la'left) = '1' then
              return ra - signed (rema);
           else
              return signed (rema);
           end if;
        end if;
     end if;"""
    res += """
  end "{func}";\n"""
    w(res.format(func=func))

def disp_vec_int_sdiv(func):
    res = """
  function "{func}" (L : SIGNED; R : INTEGER) return SIGNED
  is
     constant r_size : natural := size_signed (r);
  begin
     if l'length = 0 then
        return null_signed;
     end if;"""
    if func == '/':
         res += """
     return l {func} to_signed (r, r_size);"""
    else:
         res += """
     return resize (l {func} to_signed (r, r_size), l'length);"""
    res += """
  end "{func}";\n"""
    w(res.format(func=func))

    res = """
  function "{func}" (L : INTEGER; R : SIGNED) return SIGNED
  is
     constant l_size : natural := size_signed (l);
  begin
     if r'length = 0 then
        return null_signed;
     end if;"""
    if func == '/':
        res += """
     return resize (to_signed (l, max (l_size, r'length)) {func} r, r'length);"""
    else:
        res += """
     return to_signed (l, l_size) {func} r;"""
    res += """
  end "{func}";\n"""
    w(res.format(func=func))

def disp_all_log_funcs():
    "Generate all function bodies for logic operators"
    for t in vec_types:
        disp_resize(t)
    for v in vec_types:
        disp_vec_not(v)
    for f in binary_funcs:
        for v in vec_types:
            disp_vec_binary(f, v)
    disp_vec_vec_gcompare("ucompare", "UNSIGNED")
    disp_vec_vec_gcompare("scompare", "SIGNED")
    disp_vec_int_gcompare("ucompare", "UNSIGNED", "NATURAL")
    disp_vec_int_gcompare("scompare", "SIGNED", "INTEGER")
    for f in compare_funcs:
        disp_vec_vec_compare(f, "UNSIGNED")
        disp_vec_vec_compare(f, "SIGNED")
        disp_vec_int_compare(f, "UNSIGNED", "NATURAL")
        disp_vec_int_compare(f, "NATURAL", "UNSIGNED")
        disp_vec_int_compare(f, "SIGNED", "INTEGER")
        disp_vec_int_compare(f, "INTEGER", "SIGNED")
    for t in vec_types:
        for d in ['left', 'right']:
            disp_shift('shift_' + d, t, d);
        for d in ['left', 'right']:
            disp_rotate('rotate_' + d, t, d);
        if v93:
            disp_shift_op('"sll"', t, 'left')
            disp_shift_op('"srl"', t, 'right')
            disp_rotate('"rol"', t, 'left')
            disp_rotate('"ror"', t, 'right')

def disp_match(typ):
    res = """
  function std_match (l, r : {0}) return boolean
  is
     alias la : {0} (l'length downto 1) is l;
     alias ra : {0} (r'length downto 1) is r;
  begin
     if la'left = 0 or ra'left = 0 then
        assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: null argument, returning false"
          severity warning;
        return false;
     elsif la'left /= ra'left then
        assert NO_WARNING
          report "NUMERIC_STD.STD_MATCH: args length mismatch, returning false"
          severity warning;
        return false;
     else
        for i in la'range loop
           if not match_table (la (i), ra (i)) then
             return false;
           end if;
        end loop;
        return true;
     end if;
  end std_match;\n"""
    w(res.format(typ))


def disp_all_match_funcs():
    disp_match('std_ulogic_vector');
    disp_match('std_logic_vector');
    disp_match('UNSIGNED');
    disp_match('SIGNED');

def disp_all_arith_funcs():
    "Generate all function bodies for logic operators"
    for op in ['+', '-']:
        disp_vec_vec_binary(op, "UNSIGNED")
        disp_vec_vec_binary(op, "SIGNED")
        disp_vec_int_binary(op, "UNSIGNED", "NATURAL")
        disp_vec_int_binary(op, "NATURAL", "UNSIGNED")
        disp_vec_int_binary(op, "SIGNED", "INTEGER")
        disp_vec_int_binary(op, "INTEGER", "SIGNED")
    disp_vec_vec_mul('*', 'UNSIGNED')
    disp_vec_vec_mul('*', 'SIGNED')
    disp_vec_int_mul('UNSIGNED', 'NATURAL')
    disp_vec_int_mul('SIGNED', 'INTEGER')
    disp_int_vec_mul('NATURAL', 'UNSIGNED')
    disp_int_vec_mul('INTEGER', 'SIGNED')
    disp_has_0x('UNSIGNED')
    disp_divmod()
    disp_size()
    disp_vec_vec_udiv('/')
    disp_vec_int_udiv('/')
    disp_vec_vec_udiv('rem')
    disp_vec_int_udiv('rem')
    disp_vec_vec_udiv('mod')
    disp_vec_int_udiv('mod')
    disp_has_0x('SIGNED')
    disp_neg("-")
    disp_neg("abs")
    disp_vec_vec_sdiv('/')
    disp_vec_int_sdiv('/')
    disp_vec_vec_sdiv('rem')
    disp_vec_int_sdiv('rem')
    disp_vec_vec_sdiv('mod')
    disp_vec_int_sdiv('mod')

# Patterns to replace
pats = {'  @LOG\n' : disp_all_log_funcs,
        '  @ARITH\n' : disp_all_arith_funcs,
        '  @MATCH\n' : disp_all_match_funcs }

spec_file='numeric_std.vhdl'
#proto_file='numeric_std-body.proto'

def gen_body(proto_file):
    w('--  This -*- vhdl -*- file was generated from ' + proto_file + '\n')
    for line in open(proto_file):
        if line in pats:
            pats[line]()
            continue
        w(line)

# Copy spec
for log in logics:
    for std in ['87', '93']:
        out=open('v' + std + '/numeric_' + log + '.vhdl', 'w')
        for line in open('numeric_' + log + '.proto'):
            if line == '  @COMMON\n':
                for lcom in open('numeric_common.proto'):
                    if lcom[0:2] == '--':
                        pass
                    elif std == '87' and ('"xnor"' in lcom
                                          or '"sll"' in lcom
                                          or '"srl"' in lcom
                                          or '"rol"' in lcom
                                          or '"ror"' in lcom):
                        w("--" + lcom[2:])
                    else:
                        w(lcom)
            else:
                w(line)
        out.close()

# Generate bodies
v93=False
for l in logics:
    logic = l
    out=open('v87/numeric_{0}-body.vhdl'.format(l), 'w')
    gen_body('numeric_{0}-body.proto'.format(l))
    out.close()

v93=True
binary_funcs.append("xnor")
for l in logics:
    logic = l
    out=open('v93/numeric_{0}-body.vhdl'.format(l), 'w')
    gen_body('numeric_{0}-body.proto'.format(l))
    out.close()
