--  This -*- vhdl -*- file was generated from std_logic_1164-body.proto
--  This is an implementation of -*- vhdl -*- ieee.std_logic_1164 based only
--  on the specifications.  This file is part of GHDL.
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

--  This is a template file.  To avoid errors and duplication, the python
--  script build.py generate most of the bodies.

package body std_logic_1164 is

  type table_1d is array (std_ulogic) of std_ulogic;
  type table_2d is array (std_ulogic, std_ulogic) of std_ulogic;

  constant resolution : table_2d :=
  --  UX01ZWLH-
    ("UUUUUUUUU",   --  U
     "UXXXXXXXX",   --  X
     "UX0X0000X",   --  0
     "UXX11111X",   --  1
     "UX01ZWLHX",   --  Z
     "UX01WWWWX",   --  W
     "UX01LWLWX",   --  L
     "UX01HWWHX",   --  H
     "UXXXXXXXX"    --  -
     );

  function resolved (s : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := 'Z';
  begin
    for I in s'range loop
      res := resolution (res, s (I));
    end loop;
    return res;
  end resolved;


  constant and_table : table_2d :=
  --  UX01ZWLH-
    ("UU0UUU0UU",   -- U
     "UX0XXX0XX",   -- X
     "000000000",   -- 0
     "UX01XX01X",   -- 1
     "UX0XXX0XX",   -- Z
     "UX0XXX0XX",   -- W
     "000000000",   -- L
     "UX01XX01X",   -- H
     "UX0XXX0XX"    -- -
     );

  constant nand_table : table_2d :=
  --  UX01ZWLH-
    ("UU1UUU1UU",   -- U
     "UX1XXX1XX",   -- X
     "111111111",   -- 0
     "UX10XX10X",   -- 1
     "UX1XXX1XX",   -- Z
     "UX1XXX1XX",   -- W
     "111111111",   -- L
     "UX10XX10X",   -- H
     "UX1XXX1XX"    -- -
     );

  constant or_table : table_2d :=
  --  UX01ZWLH-
    ("UUU1UUU1U",   -- U
     "UXX1XXX1X",   -- X
     "UX01XX01X",   -- 0
     "111111111",   -- 1
     "UXX1XXX1X",   -- Z
     "UXX1XXX1X",   -- W
     "UX01XX01X",   -- L
     "111111111",   -- H
     "UXX1XXX1X"    -- -
     );

  constant nor_table : table_2d :=
  --  UX01ZWLH-
    ("UUU0UUU0U",   -- U
     "UXX0XXX0X",   -- X
     "UX10XX10X",   -- 0
     "000000000",   -- 1
     "UXX0XXX0X",   -- Z
     "UXX0XXX0X",   -- W
     "UX10XX10X",   -- L
     "000000000",   -- H
     "UXX0XXX0X"    -- -
     );

  constant xor_table : table_2d :=
  --  UX01ZWLH-
    ("UUUUUUUUU",   -- U
     "UXXXXXXXX",   -- X
     "UX01XX01X",   -- 0
     "UX10XX10X",   -- 1
     "UXXXXXXXX",   -- Z
     "UXXXXXXXX",   -- W
     "UX01XX01X",   -- L
     "UX10XX10X",   -- H
     "UXXXXXXXX"    -- -
     );

  constant xnor_table : table_2d :=
  --  UX01ZWLH-
    ("UUUUUUUUU",   -- U
     "UXXXXXXXX",   -- X
     "UX10XX10X",   -- 0
     "UX01XX01X",   -- 1
     "UXXXXXXXX",   -- Z
     "UXXXXXXXX",   -- W
     "UX10XX10X",   -- L
     "UX01XX01X",   -- H
     "UXXXXXXXX"    -- -
     );

  constant not_table : table_1d :=
  --  UX01ZWLH-
     "UX10XX10X";


  function "and" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return and_table (l, r);
  end "and";

  function "nand" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return nand_table (l, r);
  end "nand";

  function "or" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return or_table (l, r);
  end "or";

  function "nor" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return nor_table (l, r);
  end "nor";

  function "xor" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return xor_table (l, r);
  end "xor";

  function "xnor" (l : std_ulogic; r : std_ulogic) return UX01 is
  begin
    return xnor_table (l, r);
  end "xnor";

  function "not" (l : std_ulogic) return UX01 is
  begin
    return not_table (l);
  end "not";

  function "and" (l, r : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_ulogic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'and' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := and_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "and";

  function "nand" (l, r : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_ulogic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'nand' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := nand_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "nand";

  function "or" (l, r : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_ulogic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'or' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := or_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "or";

  function "nor" (l, r : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_ulogic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'nor' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := nor_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "nor";

  function "xor" (l, r : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_ulogic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'xor' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := xor_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "xor";

  function "xnor" (l, r : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_ulogic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'xnor' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := xnor_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "xnor";

  function "not" (l : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := not_table (la (I));
    end loop;
    return res;
  end "not";

  function "and" (l, r : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_logic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'and' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := and_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "and";

  function "nand" (l, r : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_logic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'nand' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := nand_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "nand";

  function "or" (l, r : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_logic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'or' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := or_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "or";

  function "nor" (l, r : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_logic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'nor' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := nor_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "nor";

  function "xor" (l, r : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_logic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'xor' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := xor_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "xor";

  function "xnor" (l, r : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    alias ra : std_logic_vector (1 to r'length) is r;
    variable res : res_type;
  begin
    if la'length /= ra'length then
      assert false
        report "arguments of overloaded 'xnor' operator are not of the same length"
        severity failure;
    else
      for I in res_type'range loop
        res (I) := xnor_table (la (I), ra (I));
      end loop;
    end if;
    return res;
  end "xnor";

  function "not" (l : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := not_table (la (I));
    end loop;
    return res;
  end "not";

  --  Conversion functions.
  --  The result range (for vectors) is S'Length - 1 downto 0.
  --  XMAP is return for values not in '0', '1', 'L', 'H'.
  function to_bit (s : std_ulogic; xmap : bit := '0') return bit is
  begin
    case s is
      when '0' | 'L' =>
        return '0';
      when '1' | 'H' =>
        return '1';
      when others =>
        return xmap;
    end case;
  end to_bit;

  type bit_to_std_table is array (bit) of std_ulogic;
  constant bit_to_std : bit_to_std_table := "01";


  function to_bitvector (s : std_ulogic_vector; xmap : bit := '0')
    return bit_vector
  is
    subtype res_range is natural range s'length - 1 downto 0;
    alias as : std_ulogic_vector (res_range) is s;
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
  end to_bitvector;

  function to_bitvector (s : std_logic_vector; xmap : bit := '0')
    return bit_vector
  is
    subtype res_range is natural range s'length - 1 downto 0;
    alias as : std_logic_vector (res_range) is s;
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
  end to_bitvector;

  function to_stdulogicvector (b : bit_vector) return std_ulogic_vector is
    subtype res_range is natural range b'length - 1 downto 0;
    alias ab : bit_vector (res_range) is b;
    variable res : std_ulogic_vector (res_range);
  begin
    for I in res_range loop
      res (I) := bit_to_std (ab (I));
    end loop;
    return res;
  end to_stdulogicvector;

  function to_stdlogicvector (b : bit_vector) return std_logic_vector is
    subtype res_range is natural range b'length - 1 downto 0;
    alias ab : bit_vector (res_range) is b;
    variable res : std_logic_vector (res_range);
  begin
    for I in res_range loop
      res (I) := bit_to_std (ab (I));
    end loop;
    return res;
  end to_stdlogicvector;

  function to_stdulogicvector (s : std_logic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (s'length - 1 downto 0);
  begin
    return res_type (s);
  end to_stdulogicvector;

  function to_stdlogicvector (s : std_ulogic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (s'length - 1 downto 0);
  begin
    return res_type (s);
  end to_stdlogicvector;

  function to_stdulogic (b : bit) return std_ulogic is
  begin
    return bit_to_std (b);
  end to_stdulogic;

  --  Normalization.
  type table_std_x01 is array (std_ulogic) of X01;
  constant std_to_x01 : table_std_x01 := ('U' | 'X' | 'Z' | 'W' | '-' => 'X',
                                          '0' | 'L'                   => '0',
                                          '1' | 'H'                   => '1');

  type table_bit_x01 is array (bit) of X01;
  constant bit_to_x01 : table_bit_x01 := ('0' => '0',
                                          '1' => '1');


  type table_std_x01z is array (std_ulogic) of X01Z;
  constant std_to_x01z : table_std_x01z := ('U' | 'X' | 'W' | '-' => 'X',
                                            '0' | 'L'             => '0',
                                            '1' | 'H'             => '1',
                                            'Z'                   => 'Z');

  type table_std_ux01 is array (std_ulogic) of UX01;
  constant std_to_ux01 : table_std_ux01 := ('U'                   => 'U',
                                            'X' | 'Z' | 'W' | '-' => 'X',
                                            '0' | 'L'             => '0',
                                            '1' | 'H'             => '1');


  function to_X01 (s : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_x01 (sa (i));
    end loop;
    return res;
  end to_X01;

  function to_X01 (s : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_x01 (sa (i));
    end loop;
    return res;
  end to_X01;

  function to_X01 (s : std_ulogic) return X01 is
  begin
    return std_to_x01 (s);
  end to_X01;

  function to_X01 (b : bit_vector) return std_ulogic_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_ulogic_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_X01;

  function to_X01 (b : bit_vector) return std_logic_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_logic_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_X01;

  function to_X01 (b : bit) return X01 is
  begin
    return bit_to_x01 (b);
  end to_X01;

  function to_X01Z (s : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_x01z (sa (i));
    end loop;
    return res;
  end to_X01Z;

  function to_X01Z (s : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_x01z (sa (i));
    end loop;
    return res;
  end to_X01Z;

  function to_X01Z (s : std_ulogic) return X01Z is
  begin
    return std_to_x01z (s);
  end to_X01Z;

  function to_X01Z (b : bit_vector) return std_ulogic_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_ulogic_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_X01Z;

  function to_X01Z (b : bit_vector) return std_logic_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_logic_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_X01Z;

  function to_X01Z (b : bit) return X01Z is
  begin
    return bit_to_x01 (b);
  end to_X01Z;

  function to_UX01 (s : std_ulogic_vector) return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_ux01 (sa (i));
    end loop;
    return res;
  end to_UX01;

  function to_UX01 (s : std_logic_vector) return std_logic_vector
  is
    subtype res_type is std_logic_vector (1 to s'length);
    alias sa : res_type is s;
    variable res : res_type;
  begin
    for i in res_type'range loop
      res (i) := std_to_ux01 (sa (i));
    end loop;
    return res;
  end to_UX01;

  function to_UX01 (s : std_ulogic) return UX01 is
  begin
    return std_to_ux01 (s);
  end to_UX01;

  function to_UX01 (b : bit_vector) return std_ulogic_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_ulogic_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_UX01;

  function to_UX01 (b : bit_vector) return std_logic_vector
  is
    subtype res_range is natural range 1 to b'length;
    alias ba : bit_vector (res_range) is b;
    variable res : std_logic_vector (res_range);
  begin
    for i in res_range loop
      res (i) := bit_to_x01 (ba (i));
    end loop;
    return res;
  end to_UX01;

  function to_UX01 (b : bit) return UX01 is
  begin
    return bit_to_x01 (b);
  end to_UX01;

  function rising_edge (signal s : std_ulogic) return boolean is
  begin
    return s'event
      and to_x01 (s'last_value) = '0'
      and to_x01 (s) = '1';
  end rising_edge;

  function falling_edge (signal s : std_ulogic) return boolean is
  begin
    return s'event
      and to_x01 (s'last_value) = '1'
      and to_x01 (s) = '0';
  end falling_edge;

  type std_x_array is array (std_ulogic) of boolean;
  constant std_x : std_x_array := ('U' | 'X' | 'Z' | 'W' | '-' => true,
                                   '0' | '1' | 'L' | 'H' => false);


  function is_X (s : std_ulogic_vector) return boolean is
  begin
    for i in s'range loop
      if std_x (s (i)) then
        return true;
      end if;
    end loop;
    return false;
  end is_X;

  function is_X (s : std_logic_vector) return boolean is
  begin
    for i in s'range loop
      if std_x (s (i)) then
        return true;
      end if;
    end loop;
    return false;
  end is_X;

  function is_X (s : std_ulogic) return boolean is
  begin
    return std_x (s);
  end is_X;
end std_logic_1164;
