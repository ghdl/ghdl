--  This -*- vhdl -*- file was generated from numeric_bit-body.proto
--  This -*- vhdl -*- file is part of GHDL.
--  IEEE 1076.3 compliant numeric bit package body.
--  The implementation is based only on the specifications.
--  Copyright (C) 2015 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING2.  If not see
--  <http://www.gnu.org/licenses/>.

package body NUMERIC_BIT is
  constant NO_WARNING : Boolean := False;

  constant null_unsigned : unsigned (0 downto 1) := (others => '0');
  constant null_signed   :   signed (0 downto 1) := (others => '0');

  subtype nat1 is natural range 0 to 1;

  type nat1_to_sl_type is array (nat1) of bit;
  constant nat1_to_01 : nat1_to_sl_type := (0 => '0', 1 => '1');

  subtype sl_01 is bit;

  type carry_array is array (sl_01, sl_01, sl_01) of sl_01;
  constant compute_carry : carry_array :=
    ('0' => ('0' => ('0' => '0', '1' => '0'),
             '1' => ('0' => '0', '1' => '1')),
     '1' => ('0' => ('0' => '0', '1' => '1'),
             '1' => ('0' => '1', '1' => '1')));
  constant compute_sum : carry_array :=
    ('0' => ('0' => ('0' => '0', '1' => '1'),
             '1' => ('0' => '1', '1' => '0')),
     '1' => ('0' => ('0' => '1', '1' => '0'),
             '1' => ('0' => '0', '1' => '1')));

  type compare_type is (compare_unknown,
                        compare_lt,
                        compare_eq,
                        compare_gt);

  function MAX (L, R : natural) return natural is
  begin
    if L > R then
      return L;
    else
      return R;
    end if;
  end MAX;

  function TO_INTEGER (ARG : UNSIGNED) return NATURAL
  is
    variable res : natural := 0;
  begin
    if arg'length = 0 then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_INTEGER: null array detected, returning 0"
        severity warning;
      return 0;
    end if;

    for i in arg'range loop
      res := res + res;
      if arg (i) = '1' then
        res := res + 1;
      end if;
    end loop;

    return res;
  end TO_INTEGER;

  function TO_INTEGER (ARG :   SIGNED) return INTEGER
  is
    alias argn : SIGNED (ARG'Length -1 downto 0) is arg;
    variable res : integer := 0;
    variable b : bit;
  begin
    if argn'length = 0 then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_INTEGER: null array detected, returning 0"
        severity warning;
      return 0;
    end if;
    if argn (argn'left) = '1' then
      --  Negative value
      b := '0';
    else
      b := '1';
    end if;

    for i in argn'range loop
      res := res + res;
      if argn (i) = b then
        res := res + 1;
      end if;
    end loop;

    if b = '0' then
      --  Avoid overflow.
      res := -res - 1;
    end if;

    return res;
  end TO_INTEGER;

  function TO_UNSIGNED (ARG, SIZE : NATURAL) return UNSIGNED
  is
    variable res : UNSIGNED (SIZE - 1 downto 0);
    variable a : natural := arg;
    variable d : nat1;
  begin
    if size = 0 then
      return null_unsigned;
    end if;
    for i in res'reverse_range loop
      d := a rem 2;
      res (i) := nat1_to_01 (d);
      a := a / 2;
    end loop;
    if a /= 0 then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_UNSIGNED: vector is truncated"
        severity warning;
    end if;
    return res;
  end TO_UNSIGNED;

  function TO_SIGNED (ARG : INTEGER; SIZE : NATURAL) return SIGNED
  is
    variable res : SIGNED (SIZE - 1 downto 0);
    variable v : integer := arg;
    variable b0, b1 : bit;
    variable d : nat1;
  begin
    if size = 0 then
      return null_signed;
    end if;
    if arg < 0 then
      --  Use one complement to avoid overflow:
      --   -v = (not v) + 1
      --   not v = -v - 1
      --   not v = -(v + 1)
      v := -(arg + 1);
      b0 := '1';
      b1 := '0';
    else
      v := arg;
      b0 := '0';
      b1 := '1';
    end if;

    for i in res'reverse_range loop
      d := v rem 2;
      v := v / 2;
      if d = 0 then
        res (i) := b0;
      else
        res (i) := b1;
      end if;
    end loop;
    if v /= 0 or res (res'left) /= b0 then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_SIGNED: vector is truncated"
        severity warning;
    end if;
    return res;
  end TO_SIGNED;


  function "+" (l, r : UNSIGNED) return UNSIGNED
  is
    constant lft : integer := MAX (l'length, r'length) - 1;
    subtype res_type is UNSIGNED (lft downto 0);
    alias la : UNSIGNED (l'length - 1 downto 0) is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if la'left < 0 or ra'left < 0 then
       return null_UNSIGNED;
    end if;
    carry := '0';
    for i in 0 to lft loop
       if i > la'left then
          lb := '0';
       else
          lb := la (i);
       end if;
       if i > ra'left then
          rb := '0';
       else
          rb := ra (i);
       end if;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    return res;
  end "+";

  function "+" (l, r : SIGNED) return SIGNED
  is
    constant lft : integer := MAX (l'length, r'length) - 1;
    subtype res_type is SIGNED (lft downto 0);
    alias la : SIGNED (l'length - 1 downto 0) is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if la'left < 0 or ra'left < 0 then
       return null_SIGNED;
    end if;
    carry := '0';
    for i in 0 to lft loop
       if i > la'left then
          lb := l (l'left);
       else
          lb := la (i);
       end if;
       if i > ra'left then
          rb := r (r'left);
       else
          rb := ra (i);
       end if;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    return res;
  end "+";

  function "+" (l : UNSIGNED; r : NATURAL) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_UNSIGNED;
    end if;
    carry := '0';
    r1 := r;
    for i in res'reverse_range loop
       lb := la (i);

       r2 := r1 / 2;
       rd := r1 - 2 * r2;
       r1 := r2;
       rb := nat1_to_01 (rd);
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if r1 /= 0 then
      assert NO_WARNING
        report "NUMERIC_STD.""+"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "+";

  function "+" (l : NATURAL; r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (r'length - 1 downto 0);
    alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_UNSIGNED;
    end if;
    carry := '0';
    l1 := l;
    for i in res'reverse_range loop
       rb := ra (i);

       l2 := l1 / 2;
       ld := l1 - 2 * l2;
       l1 := l2;
       lb := nat1_to_01 (ld);
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if l1 /= 0 then
      assert NO_WARNING
        report "NUMERIC_STD.""+"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "+";

  function "+" (l : SIGNED; r : INTEGER) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_SIGNED;
    end if;
    carry := '0';
    r1 := r;
    for i in res'reverse_range loop
       lb := la (i);

       r2 := r1 / 2;
       if r1 < 0 then
          rd := 2 * r2 - r1;
          r1 := r2 - rd;
        else
          rd := r1 - 2 * r2;
          r1 := r2;
        end if;
       rb := nat1_to_01 (rd);
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if r1 /= -rmsb then
      assert NO_WARNING
        report "NUMERIC_STD.""+"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "+";

  function "+" (l : INTEGER; r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (r'length - 1 downto 0);
    alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_SIGNED;
    end if;
    carry := '0';
    l1 := l;
    for i in res'reverse_range loop
       rb := ra (i);

       l2 := l1 / 2;
       if l1 < 0 then
          ld := 2 * l2 - l1;
          l1 := l2 - ld;
        else
          ld := l1 - 2 * l2;
          l1 := l2;
        end if;
       lb := nat1_to_01 (ld);
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if l1 /= -lmsb then
      assert NO_WARNING
        report "NUMERIC_STD.""+"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "+";

  function "-" (l, r : UNSIGNED) return UNSIGNED
  is
    constant lft : integer := MAX (l'length, r'length) - 1;
    subtype res_type is UNSIGNED (lft downto 0);
    alias la : UNSIGNED (l'length - 1 downto 0) is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if la'left < 0 or ra'left < 0 then
       return null_UNSIGNED;
    end if;
    carry := '1';
    for i in 0 to lft loop
       if i > la'left then
          lb := '0';
       else
          lb := la (i);
       end if;
       if i > ra'left then
          rb := '0';
       else
          rb := ra (i);
       end if;
      rb := not rb;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    return res;
  end "-";

  function "-" (l, r : SIGNED) return SIGNED
  is
    constant lft : integer := MAX (l'length, r'length) - 1;
    subtype res_type is SIGNED (lft downto 0);
    alias la : SIGNED (l'length - 1 downto 0) is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if la'left < 0 or ra'left < 0 then
       return null_SIGNED;
    end if;
    carry := '1';
    for i in 0 to lft loop
       if i > la'left then
          lb := l (l'left);
       else
          lb := la (i);
       end if;
       if i > ra'left then
          rb := r (r'left);
       else
          rb := ra (i);
       end if;
      rb := not rb;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    return res;
  end "-";

  function "-" (l : UNSIGNED; r : NATURAL) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_UNSIGNED;
    end if;
    carry := '1';
    r1 := r;
    for i in res'reverse_range loop
       lb := la (i);

       r2 := r1 / 2;
       rd := r1 - 2 * r2;
       r1 := r2;
       rb := nat1_to_01 (rd);
      rb := not rb;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if r1 /= 0 then
      assert NO_WARNING
        report "NUMERIC_STD.""-"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "-";

  function "-" (l : NATURAL; r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (r'length - 1 downto 0);
    alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_UNSIGNED;
    end if;
    carry := '1';
    l1 := l;
    for i in res'reverse_range loop
       rb := ra (i);

       l2 := l1 / 2;
       ld := l1 - 2 * l2;
       l1 := l2;
       lb := nat1_to_01 (ld);
      rb := not rb;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if l1 /= 0 then
      assert NO_WARNING
        report "NUMERIC_STD.""-"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "-";

  function "-" (l : SIGNED; r : INTEGER) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_SIGNED;
    end if;
    carry := '1';
    r1 := r;
    for i in res'reverse_range loop
       lb := la (i);

       r2 := r1 / 2;
       if r1 < 0 then
          rd := 2 * r2 - r1;
          r1 := r2 - rd;
        else
          rd := r1 - 2 * r2;
          r1 := r2;
        end if;
       rb := nat1_to_01 (rd);
      rb := not rb;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if r1 /= -rmsb then
      assert NO_WARNING
        report "NUMERIC_STD.""-"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "-";

  function "-" (l : INTEGER; r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (r'length - 1 downto 0);
    alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
    variable res : res_type;
    variable lb, rb, carry : bit;
  begin
    if res'length < 0 then
       return null_SIGNED;
    end if;
    carry := '1';
    l1 := l;
    for i in res'reverse_range loop
       rb := ra (i);

       l2 := l1 / 2;
       if l1 < 0 then
          ld := 2 * l2 - l1;
          l1 := l2 - ld;
        else
          ld := l1 - 2 * l2;
          l1 := l2;
        end if;
       lb := nat1_to_01 (ld);
      rb := not rb;
      res (i) := compute_sum (carry, rb, lb);
      carry := compute_carry (carry, rb, lb);
    end loop;
    if l1 /= -lmsb then
      assert NO_WARNING
        report "NUMERIC_STD.""-"": vector is truncated"
        severity warning;
    end if;
    return res;
  end "-";

  function "*" (L, R : UNSIGNED) return UNSIGNED
  is
     alias la : UNSIGNED (L'Length - 1 downto 0) is l;
     alias ra : UNSIGNED (R'Length - 1 downto 0) is r;
     variable res : UNSIGNED (L'length + R'Length -1 downto 0) := (others => '0');
     variable rb, lb, vb, carry : bit;
  begin
     if la'length = 0 or ra'length = 0 then
        return null_UNSIGNED;
     end if;
     --  Shift and add L.
     for i in natural range 0 to ra'left loop
       rb := ra (i);
       if rb = '1' then
          --  Compute res := res + shift_left (l, i).
          carry := '0';
          for j in la'reverse_range loop
            lb := la (j);
            vb := res (i + j);
            res (i + j) := compute_sum (carry, vb, lb);
            carry := compute_carry (carry, vb, lb);
          end loop;
          --  Propagate carry.
          for j in i + la'length to res'left loop
             exit when carry = '0';
             vb := res (j);
             res (j) := carry xor vb;
             carry := carry and vb;
          end loop;
       end if;
     end loop;
     return res;
  end "*";

  function "*" (L, R : SIGNED) return SIGNED
  is
     alias la : SIGNED (L'Length - 1 downto 0) is l;
     alias ra : SIGNED (R'Length - 1 downto 0) is r;
     variable res : SIGNED (L'length + R'Length -1 downto 0) := (others => '0');
     variable rb, lb, vb, carry : bit;
  begin
     if la'length = 0 or ra'length = 0 then
        return null_SIGNED;
     end if;
     --  Shift and add L.
     for i in natural range 0 to ra'left - 1 loop
       rb := ra (i);
       if rb = '1' then
          --  Compute res := res + shift_left (l, i).
          carry := '0';
          for j in la'reverse_range loop
            lb := la (j);
            vb := res (i + j);
            res (i + j) := compute_sum (carry, vb, lb);
            carry := compute_carry (carry, vb, lb);
          end loop;
          --  Sign extend and propagate carry.
          lb := la (la'left);
          for j in i + l'length to res'left loop
             vb := res (j);
             res (j) := compute_sum (carry, vb, lb);
             carry := compute_carry (carry, vb, lb);
          end loop;
       end if;
     end loop;
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
     end if;
     return res;
  end "*";

  function "*" (L : UNSIGNED; R : NATURAL) return UNSIGNED
  is
     constant size : natural := l'length;
  begin
     if size = 0 then
        return null_UNSIGNED;
     end if;
     return l * to_UNSIGNED (r, size);
  end "*";

  function "*" (L : SIGNED; R : INTEGER) return SIGNED
  is
     constant size : natural := l'length;
  begin
     if size = 0 then
        return null_SIGNED;
     end if;
     return l * to_SIGNED (r, size);
  end "*";

  function "*" (L : NATURAL; R : UNSIGNED) return UNSIGNED
  is
     constant size : natural := r'length;
  begin
     if size = 0 then
        return null_UNSIGNED;
     end if;
     return r * to_UNSIGNED (l, size);
  end "*";

  function "*" (L : INTEGER; R : SIGNED) return SIGNED
  is
     constant size : natural := r'length;
  begin
     if size = 0 then
        return null_SIGNED;
     end if;
     return r * to_SIGNED (l, size);
  end "*";

  function has_0x (a : UNSIGNED) return bit
  is
     variable res : bit := '0';
  begin
     for i in a'range loop
      res := res or a (i);
    end loop;
    return res;
  end has_0x;

  --  All index range are normalized (N downto 0).
  --  NUM and QUOT have the same range.
  --  DEM and REMAIN have the same range.
  --  No 'X'.
  procedure divmod (num, dem : UNSIGNED; quot, remain : out UNSIGNED)
  is
     variable reg : unsigned (dem'left + 1 downto 0) := (others => '0');
     variable sub : unsigned (dem'range) := (others => '0');
     variable carry, d : bit;
  begin
     for i in num'range loop
        --  Shift
        reg (reg'left downto 1) := reg (reg'left - 1 downto 0);
        reg (0) := num (i);
        --  Substract
        carry := '1';
        for j in dem'reverse_range loop
           d := not dem (j);
           sub (j) := compute_sum (carry, reg (j), d);
           carry := compute_carry (carry, reg (j), d);
        end loop;
        carry := compute_carry (carry, reg (reg'left), '1');
        --  Test
        if carry = '0' then
           --  Greater than
           quot (i) := '0';
        else
           quot (i) := '1';
           reg (reg'left) := '0';
           reg (sub'range) := sub;
        end if;
     end loop;
     remain := reg (dem'range);
  end divmod;

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
  end size_unsigned;

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
  end size_signed;

  function "/" (L, R : UNSIGNED) return UNSIGNED
  is
     subtype l_type is UNSIGNED (L'length - 1 downto 0);
     subtype r_type is UNSIGNED (R'length - 1 downto 0);
     alias la : l_type is l;
     alias ra : r_type is r;
     variable quot : l_type;
     variable rema : r_type;
     variable r0 : bit := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_unsigned;
     end if;
     assert r0 /= '0'
        report "NUMERIC_STD.""/"": division by 0"
        severity error;
     divmod (la, ra, quot, rema);
     return quot;
  end "/";

  function "/" (L : UNSIGNED; R : NATURAL) return UNSIGNED
  is
     constant r_size : natural := size_unsigned (r);
  begin
     if l'length = 0 then
        return null_unsigned;
     end if;
     return l / to_unsigned (r, r_size);
  end "/";

  function "/" (L : NATURAL; R : UNSIGNED) return UNSIGNED
  is
     constant l_size : natural := size_unsigned (l);
  begin
     if r'length = 0 then
        return null_unsigned;
     end if;
     return resize (to_unsigned (l, l_size) / r, r'length);
  end "/";

  function "rem" (L, R : UNSIGNED) return UNSIGNED
  is
     subtype l_type is UNSIGNED (L'length - 1 downto 0);
     subtype r_type is UNSIGNED (R'length - 1 downto 0);
     alias la : l_type is l;
     alias ra : r_type is r;
     variable quot : l_type;
     variable rema : r_type;
     variable r0 : bit := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_unsigned;
     end if;
     assert r0 /= '0'
        report "NUMERIC_STD.""rem"": division by 0"
        severity error;
     divmod (la, ra, quot, rema);
     return rema;
  end "rem";

  function "rem" (L : UNSIGNED; R : NATURAL) return UNSIGNED
  is
     constant r_size : natural := size_unsigned (r);
  begin
     if l'length = 0 then
        return null_unsigned;
     end if;
     return resize (l rem to_unsigned (r, r_size), l'length);
  end "rem";

  function "rem" (L : NATURAL; R : UNSIGNED) return UNSIGNED
  is
     constant l_size : natural := size_unsigned (l);
  begin
     if r'length = 0 then
        return null_unsigned;
     end if;
     return to_unsigned (l, l_size) rem r;
  end "rem";

  function "mod" (L, R : UNSIGNED) return UNSIGNED
  is
     subtype l_type is UNSIGNED (L'length - 1 downto 0);
     subtype r_type is UNSIGNED (R'length - 1 downto 0);
     alias la : l_type is l;
     alias ra : r_type is r;
     variable quot : l_type;
     variable rema : r_type;
     variable r0 : bit := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_unsigned;
     end if;
     assert r0 /= '0'
        report "NUMERIC_STD.""mod"": division by 0"
        severity error;
     divmod (la, ra, quot, rema);
     return rema;
  end "mod";

  function "mod" (L : UNSIGNED; R : NATURAL) return UNSIGNED
  is
     constant r_size : natural := size_unsigned (r);
  begin
     if l'length = 0 then
        return null_unsigned;
     end if;
     return resize (l mod to_unsigned (r, r_size), l'length);
  end "mod";

  function "mod" (L : NATURAL; R : UNSIGNED) return UNSIGNED
  is
     constant l_size : natural := size_unsigned (l);
  begin
     if r'length = 0 then
        return null_unsigned;
     end if;
     return to_unsigned (l, l_size) mod r;
  end "mod";

  function has_0x (a : SIGNED) return bit
  is
     variable res : bit := '0';
  begin
     for i in a'range loop
      res := res or a (i);
    end loop;
    return res;
  end has_0x;

  function "-" (ARG : SIGNED) return SIGNED
  is
     subtype arg_type is SIGNED (ARG'length - 1 downto 0);
     alias arga : arg_type is arg;
     variable res : arg_type;
     variable carry, a : bit;
  begin
     if arga'length = 0 then
        return null_signed;
     end if;
     carry := '1';
     for i in arga'reverse_range loop
       a := not arga (i);
       res (i) := carry xor a;
       carry := carry and a;
     end loop;
     return res;
  end "-";

  function "abs" (ARG : SIGNED) return SIGNED
  is
     subtype arg_type is SIGNED (ARG'length - 1 downto 0);
     alias arga : arg_type is arg;
     variable res : arg_type;
     variable carry, a : bit;
  begin
     if arga'length = 0 then
        return null_signed;
     end if;
     if arga (arga'left) = '0' then
        return arga;
     end if;
     carry := '1';
     for i in arga'reverse_range loop
       a := not arga (i);
       res (i) := carry xor a;
       carry := carry and a;
     end loop;
     return res;
  end "abs";

  function "/" (L, R : SIGNED) return SIGNED
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
     variable r0 : bit := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_signed;
     end if;
     assert r0 /= '0'
        report "NUMERIC_STD.""/"": division by 0"
        severity error;
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
     divmod (lu, ru, quot, rema);
     if (ra (ra'left) xor la (la'left)) = '1' then
       return -signed (quot);
     else
       return signed (quot);
     end if;
  end "/";

  function "/" (L : SIGNED; R : INTEGER) return SIGNED
  is
     constant r_size : natural := size_signed (r);
  begin
     if l'length = 0 then
        return null_signed;
     end if;
     return l / to_signed (r, r_size);
  end "/";

  function "/" (L : INTEGER; R : SIGNED) return SIGNED
  is
     constant l_size : natural := size_signed (l);
  begin
     if r'length = 0 then
        return null_signed;
     end if;
     return resize (to_signed (l, max (l_size, r'length)) / r, r'length);
  end "/";

  function "rem" (L, R : SIGNED) return SIGNED
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
     variable r0 : bit := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_signed;
     end if;
     assert r0 /= '0'
        report "NUMERIC_STD.""rem"": division by 0"
        severity error;
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
     divmod (lu, ru, quot, rema);
     --  Result of rem has the sign of the dividend.
     if la (la'left) = '1' then
       return -signed (rema);
     else
       return signed (rema);
     end if;
  end "rem";

  function "rem" (L : SIGNED; R : INTEGER) return SIGNED
  is
     constant r_size : natural := size_signed (r);
  begin
     if l'length = 0 then
        return null_signed;
     end if;
     return resize (l rem to_signed (r, r_size), l'length);
  end "rem";

  function "rem" (L : INTEGER; R : SIGNED) return SIGNED
  is
     constant l_size : natural := size_signed (l);
  begin
     if r'length = 0 then
        return null_signed;
     end if;
     return to_signed (l, l_size) rem r;
  end "rem";

  function "mod" (L, R : SIGNED) return SIGNED
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
     variable r0 : bit := has_0x (r);
  begin
     if la'length = 0 or ra'length = 0 then
        return null_signed;
     end if;
     assert r0 /= '0'
        report "NUMERIC_STD.""mod"": division by 0"
        severity error;
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
     divmod (lu, ru, quot, rema);
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
     end if;
  end "mod";

  function "mod" (L : SIGNED; R : INTEGER) return SIGNED
  is
     constant r_size : natural := size_signed (r);
  begin
     if l'length = 0 then
        return null_signed;
     end if;
     return resize (l mod to_signed (r, r_size), l'length);
  end "mod";

  function "mod" (L : INTEGER; R : SIGNED) return SIGNED
  is
     constant l_size : natural := size_signed (l);
  begin
     if r'length = 0 then
        return null_signed;
     end if;
     return to_signed (l, l_size) mod r;
  end "mod";


  function resize (ARG : UNSIGNED; NEW_SIZE: natural) return UNSIGNED
  is
     alias arg1 : UNSIGNED (ARG'length - 1 downto 0) is arg;
     variable res : UNSIGNED (new_size - 1 downto 0) := (others => '0');
  begin
     if new_size = 0 then
        return null_UNSIGNED;
     end if;
     if arg1'length = 0 then
        return res;
     end if;
     if arg1'length > new_size then
        --  Reduction.
        res := arg1 (res'range);
     else
        --  Expansion
        res (arg1'range) := arg1;
    end if;
    return res;
  end resize;

  function resize (ARG : SIGNED; NEW_SIZE: natural) return SIGNED
  is
     alias arg1 : SIGNED (ARG'length - 1 downto 0) is arg;
     variable res : SIGNED (new_size - 1 downto 0) := (others => '0');
  begin
     if new_size = 0 then
        return null_SIGNED;
     end if;
     if arg1'length = 0 then
        return res;
     end if;
     if arg1'length > new_size then
        --  Reduction.
        res (res'left) := arg1 (arg1'left);
        res (res'left - 1 downto 0) := arg1 (res'left - 1 downto 0);
     else
        --  Expansion
        res (arg1'range) := arg1;
        res (res'left downto arg1'length) := (others => arg1 (arg1'left));
    end if;
    return res;
  end resize;

  function "not" (l : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := not la (I);
    end loop;
    return res;
  end "not";

  function "not" (l : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := not la (I);
    end loop;
    return res;
  end "not";

  function "and" (l, r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""and"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) and ra (I);
      end loop;
    end if;
    return res;
  end "and";

  function "and" (l, r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""and"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) and ra (I);
      end loop;
    end if;
    return res;
  end "and";

  function "nand" (l, r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""nand"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) nand ra (I);
      end loop;
    end if;
    return res;
  end "nand";

  function "nand" (l, r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""nand"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) nand ra (I);
      end loop;
    end if;
    return res;
  end "nand";

  function "or" (l, r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""or"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) or ra (I);
      end loop;
    end if;
    return res;
  end "or";

  function "or" (l, r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""or"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) or ra (I);
      end loop;
    end if;
    return res;
  end "or";

  function "nor" (l, r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""nor"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) nor ra (I);
      end loop;
    end if;
    return res;
  end "nor";

  function "nor" (l, r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""nor"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) nor ra (I);
      end loop;
    end if;
    return res;
  end "nor";

  function "xor" (l, r : UNSIGNED) return UNSIGNED
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : UNSIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""xor"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) xor ra (I);
      end loop;
    end if;
    return res;
  end "xor";

  function "xor" (l, r : SIGNED) return SIGNED
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
    alias ra : SIGNED (r'length - 1 downto 0) is r;
    variable res : res_type;
  begin
    if la'left /= ra'left then
      assert false
        report "NUMERIC_STD.""xor"": arguments are not of the same length"
        severity failure;
      res := (others => '0');
    else
      for I in res_type'range loop
        res (I) := la (I) xor ra (I);
      end loop;
    end if;
    return res;
  end "xor";

  function ucompare (l, r : UNSIGNED) return compare_type
  is
     constant sz : integer := MAX (l'length, r'length) - 1;
     alias la : UNSIGNED (l'length - 1 downto 0) is l;
     alias ra : UNSIGNED (r'length - 1 downto 0) is r;
     variable lb, rb : bit;
     variable res : compare_type;
  begin
     res := compare_eq;
    for i in 0 to sz loop
       if i > la'left then
          lb := '0';
       else
          lb := la (i);
       end if;
       if i > ra'left then
          rb := '0';
       else
          rb := ra (i);
       end if;
       if lb = '1' and rb = '0' then
         res := compare_gt;
       elsif lb = '0' and rb = '1' then
         res := compare_lt;
       end if;
     end loop;

     return res;
  end ucompare;

  function scompare (l, r : SIGNED) return compare_type
  is
     constant sz : integer := MAX (l'length, r'length) - 1;
     alias la : SIGNED (l'length - 1 downto 0) is l;
     alias ra : SIGNED (r'length - 1 downto 0) is r;
     variable lb, rb : bit;
     variable res : compare_type;
  begin
     --  Consider sign bit as S * -(2**N).
     lb := la (la'left);
     rb := ra (ra'left);
     if lb = '1' and rb = '0' then
        return compare_lt;
     elsif lb = '0' and rb = '1' then
        return compare_gt;
     else
        res := compare_eq;
     end if;
     for i in 0 to sz - 1 loop
       if i > la'left then
          lb := l (l'left);
       else
          lb := la (i);
       end if;
       if i > ra'left then
          rb := r (r'left);
       else
          rb := ra (i);
       end if;
       if lb = '1' and rb = '0' then
         res := compare_gt;
       elsif lb = '0' and rb = '1' then
         res := compare_lt;
       end if;
     end loop;

     return res;
  end scompare;

  function ucompare (l : UNSIGNED; r : NATURAL) return compare_type
  is
    subtype res_type is UNSIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
    variable lb, rb : bit;
    variable res : compare_type;
  begin
    res := compare_eq;
    r1 := r;
    for i in la'reverse_range loop
       lb := la (i);
       r2 := r1 / 2;
       rd := r1 - 2 * r2;
       r1 := r2;
       rb := nat1_to_01 (rd);
       if lb = '1' and rb = '0' then
         res := compare_gt;
       elsif lb = '0' and rb = '1' then
         res := compare_lt;
       end if;
    end loop;
    if r1 /= 0 then
       res := compare_lt;
    end if;
    return res;
  end ucompare;

  function scompare (l : SIGNED; r : INTEGER) return compare_type
  is
    subtype res_type is SIGNED (l'length - 1 downto 0);
    alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
    variable lb, rb : bit;
    variable res : compare_type;
  begin
    res := compare_eq;
    r1 := r;
    for i in la'reverse_range loop
       lb := la (i);
       r2 := r1 / 2;
       if r1 < 0 then
          rd := 2 * r2 - r1;
          r1 := r2 - rd;
        else
          rd := r1 - 2 * r2;
          r1 := r2;
        end if;
       rb := nat1_to_01 (rd);
       if lb = '1' and rb = '0' then
         res := compare_gt;
       elsif lb = '0' and rb = '1' then
         res := compare_lt;
       end if;
    end loop;
    if l (l'left) = '1' then
      if r >= 0 then
         res := compare_lt;
      end if;
    else
      if r < 0 then
         res := compare_gt;
      end if;
    end if;
    return res;
  end scompare;

  function "=" (l, r : UNSIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res = compare_eq;
  end "=";

  function "=" (l, r : SIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res = compare_eq;
  end "=";

  function "=" (l : UNSIGNED; r : NATURAL) return boolean
  is
     subtype res_type is UNSIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res = compare_eq;
  end "=";

  function "=" (l : NATURAL; r : UNSIGNED) return boolean
  is
     subtype res_type is UNSIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (r, l);
     return compare_eq = res;
  end "=";

  function "=" (l : SIGNED; r : INTEGER) return boolean
  is
     subtype res_type is SIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res = compare_eq;
  end "=";

  function "=" (l : INTEGER; r : SIGNED) return boolean
  is
     subtype res_type is SIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (r, l);
     return compare_eq = res;
  end "=";

  function "/=" (l, r : UNSIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""/="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res /= compare_eq;
  end "/=";

  function "/=" (l, r : SIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""/="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res /= compare_eq;
  end "/=";

  function "/=" (l : UNSIGNED; r : NATURAL) return boolean
  is
     subtype res_type is UNSIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""/="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res /= compare_eq;
  end "/=";

  function "/=" (l : NATURAL; r : UNSIGNED) return boolean
  is
     subtype res_type is UNSIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""/="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (r, l);
     return compare_eq /= res;
  end "/=";

  function "/=" (l : SIGNED; r : INTEGER) return boolean
  is
     subtype res_type is SIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""/="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res /= compare_eq;
  end "/=";

  function "/=" (l : INTEGER; r : SIGNED) return boolean
  is
     subtype res_type is SIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""/="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (r, l);
     return compare_eq /= res;
  end "/=";

  function ">" (l, r : UNSIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res > compare_eq;
  end ">";

  function ">" (l, r : SIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res > compare_eq;
  end ">";

  function ">" (l : UNSIGNED; r : NATURAL) return boolean
  is
     subtype res_type is UNSIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res > compare_eq;
  end ">";

  function ">" (l : NATURAL; r : UNSIGNED) return boolean
  is
     subtype res_type is UNSIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (r, l);
     return compare_eq > res;
  end ">";

  function ">" (l : SIGNED; r : INTEGER) return boolean
  is
     subtype res_type is SIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res > compare_eq;
  end ">";

  function ">" (l : INTEGER; r : SIGNED) return boolean
  is
     subtype res_type is SIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (r, l);
     return compare_eq > res;
  end ">";

  function ">=" (l, r : UNSIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res >= compare_eq;
  end ">=";

  function ">=" (l, r : SIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res >= compare_eq;
  end ">=";

  function ">=" (l : UNSIGNED; r : NATURAL) return boolean
  is
     subtype res_type is UNSIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res >= compare_eq;
  end ">=";

  function ">=" (l : NATURAL; r : UNSIGNED) return boolean
  is
     subtype res_type is UNSIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (r, l);
     return compare_eq >= res;
  end ">=";

  function ">=" (l : SIGNED; r : INTEGER) return boolean
  is
     subtype res_type is SIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res >= compare_eq;
  end ">=";

  function ">=" (l : INTEGER; r : SIGNED) return boolean
  is
     subtype res_type is SIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD."">="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (r, l);
     return compare_eq >= res;
  end ">=";

  function "<" (l, r : UNSIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res < compare_eq;
  end "<";

  function "<" (l, r : SIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res < compare_eq;
  end "<";

  function "<" (l : UNSIGNED; r : NATURAL) return boolean
  is
     subtype res_type is UNSIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res < compare_eq;
  end "<";

  function "<" (l : NATURAL; r : UNSIGNED) return boolean
  is
     subtype res_type is UNSIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (r, l);
     return compare_eq < res;
  end "<";

  function "<" (l : SIGNED; r : INTEGER) return boolean
  is
     subtype res_type is SIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res < compare_eq;
  end "<";

  function "<" (l : INTEGER; r : SIGNED) return boolean
  is
     subtype res_type is SIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<"": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (r, l);
     return compare_eq < res;
  end "<";

  function "<=" (l, r : UNSIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res <= compare_eq;
  end "<=";

  function "<=" (l, r : SIGNED) return boolean
  is
     variable res : compare_type;
  begin
     if l'length = 0 or r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res <= compare_eq;
  end "<=";

  function "<=" (l : UNSIGNED; r : NATURAL) return boolean
  is
     subtype res_type is UNSIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : NATURAL;
     variable rd : nat1;
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (l, r);
     return res <= compare_eq;
  end "<=";

  function "<=" (l : NATURAL; r : UNSIGNED) return boolean
  is
     subtype res_type is UNSIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : NATURAL;
     variable ld : nat1;
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := ucompare (r, l);
     return compare_eq <= res;
  end "<=";

  function "<=" (l : SIGNED; r : INTEGER) return boolean
  is
     subtype res_type is SIGNED (l'length - 1 downto 0);
     alias la : res_type is l;
     variable r1, r2 : INTEGER;
     variable rd : nat1;
     constant rmsb : nat1 := boolean'pos(r < 0);
     variable res : compare_type;
  begin
     if l'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (l, r);
     return res <= compare_eq;
  end "<=";

  function "<=" (l : INTEGER; r : SIGNED) return boolean
  is
     subtype res_type is SIGNED (r'length - 1 downto 0);
     alias ra : res_type is r;
     variable l1, l2 : INTEGER;
     variable ld : nat1;
     constant lmsb : nat1 := boolean'pos(l < 0);
     variable res : compare_type;
  begin
     if r'length = 0 then
        assert NO_WARNING
           report "NUMERIC_STD.""<="": null argument, returning FALSE"
           severity warning;
        return false;
     end if;

     res := scompare (r, l);
     return compare_eq <= res;
  end "<=";

  function shift_left (ARG : UNSIGNED; COUNT: NATURAL) return UNSIGNED
  is
     subtype res_type is UNSIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
  begin
     if res'length = 0 then
        return null_UNSIGNED;
     end if;
     if count <= arg1'left then
       res (res'left downto count) := arg1 (arg1'left - count downto 0);
     end if;
     return res;
  end shift_left;

  function shift_right (ARG : UNSIGNED; COUNT: NATURAL) return UNSIGNED
  is
     subtype res_type is UNSIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
  begin
     if res'length = 0 then
        return null_UNSIGNED;
     end if;
     if count <= arg1'left then
       res (res'left - count downto 0) := arg1 (arg1'left downto count);
     end if;
     return res;
  end shift_right;

  function rotate_left (ARG : UNSIGNED; COUNT: natural) return UNSIGNED
  is
     subtype res_type is UNSIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
     variable cnt : natural;
  begin
     if res'length = 0 then
        return null_UNSIGNED;
     end if;
     cnt := count rem res'length;
     res (res'left downto cnt) := arg1 (res'left - cnt downto 0);
     res (cnt - 1 downto 0) := arg1 (res'left downto res'left - cnt + 1);
     return res;
  end rotate_left;

  function rotate_right (ARG : UNSIGNED; COUNT: natural) return UNSIGNED
  is
     subtype res_type is UNSIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
     variable cnt : natural;
  begin
     if res'length = 0 then
        return null_UNSIGNED;
     end if;
     cnt := count rem res'length;
     res (res'left - cnt downto 0) := arg1 (res'left downto cnt);
     res (res'left downto res'left - cnt + 1) := arg1 (cnt - 1 downto 0);
     return res;
  end rotate_right;

  function shift_left (ARG : SIGNED; COUNT: NATURAL) return SIGNED
  is
     subtype res_type is SIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
  begin
     if res'length = 0 then
        return null_SIGNED;
     end if;
     if count <= arg1'left then
       res (res'left downto count) := arg1 (arg1'left - count downto 0);
     end if;
     return res;
  end shift_left;

  function shift_right (ARG : SIGNED; COUNT: NATURAL) return SIGNED
  is
     subtype res_type is SIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => arg1 (arg1'left));
  begin
     if res'length = 0 then
        return null_SIGNED;
     end if;
     if count <= arg1'left then
       res (res'left - count downto 0) := arg1 (arg1'left downto count);
     end if;
     return res;
  end shift_right;

  function rotate_left (ARG : SIGNED; COUNT: natural) return SIGNED
  is
     subtype res_type is SIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
     variable cnt : natural;
  begin
     if res'length = 0 then
        return null_SIGNED;
     end if;
     cnt := count rem res'length;
     res (res'left downto cnt) := arg1 (res'left - cnt downto 0);
     res (cnt - 1 downto 0) := arg1 (res'left downto res'left - cnt + 1);
     return res;
  end rotate_left;

  function rotate_right (ARG : SIGNED; COUNT: natural) return SIGNED
  is
     subtype res_type is SIGNED (ARG'length - 1 downto 0);
     alias arg1 : res_type is arg;
     variable res : res_type := (others => '0');
     variable cnt : natural;
  begin
     if res'length = 0 then
        return null_SIGNED;
     end if;
     cnt := count rem res'length;
     res (res'left - cnt downto 0) := arg1 (res'left downto cnt);
     res (res'left downto res'left - cnt + 1) := arg1 (cnt - 1 downto 0);
     return res;
  end rotate_right;

  function rising_edge (signal s : bit) return boolean is
  begin
    return s'event and s = '1';
  end rising_edge;

  function falling_edge (signal s : bit) return boolean is
  begin
    return s'event and s = '0';
  end falling_edge;
end NUMERIC_BIT;
