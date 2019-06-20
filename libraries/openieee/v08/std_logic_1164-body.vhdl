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

  function "and" (l : std_ulogic_vector; r : std_ulogic)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := and_table (la (I), r);
    end loop;
    return res;
  end "and";

  function "and" (l : std_ulogic; r : std_ulogic_vector)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := and_table (l, ra (I));
    end loop;
    return res;
  end "and";

  function "nand" (l : std_ulogic_vector; r : std_ulogic)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := nand_table (la (I), r);
    end loop;
    return res;
  end "nand";

  function "nand" (l : std_ulogic; r : std_ulogic_vector)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := nand_table (l, ra (I));
    end loop;
    return res;
  end "nand";

  function "or" (l : std_ulogic_vector; r : std_ulogic)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := or_table (la (I), r);
    end loop;
    return res;
  end "or";

  function "or" (l : std_ulogic; r : std_ulogic_vector)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := or_table (l, ra (I));
    end loop;
    return res;
  end "or";

  function "nor" (l : std_ulogic_vector; r : std_ulogic)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := nor_table (la (I), r);
    end loop;
    return res;
  end "nor";

  function "nor" (l : std_ulogic; r : std_ulogic_vector)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := nor_table (l, ra (I));
    end loop;
    return res;
  end "nor";

  function "xor" (l : std_ulogic_vector; r : std_ulogic)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := xor_table (la (I), r);
    end loop;
    return res;
  end "xor";

  function "xor" (l : std_ulogic; r : std_ulogic_vector)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := xor_table (l, ra (I));
    end loop;
    return res;
  end "xor";

  function "xnor" (l : std_ulogic_vector; r : std_ulogic)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := xnor_table (la (I), r);
    end loop;
    return res;
  end "xnor";

  function "xnor" (l : std_ulogic; r : std_ulogic_vector)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to r'length);
    alias ra : res_type is r;
    variable res : res_type;
  begin
    for I in res_type'range loop
      res (I) := xnor_table (l, ra (I));
    end loop;
    return res;
  end "xnor";

  function "and" (l : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := '1';
  begin
    for I in l'range loop
      res := and_table (l(I), res);
    end loop;
    return res;
  end "and";

  function "nand" (l : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := '1';
  begin
    for I in l'range loop
      res := nand_table (l(I), res);
    end loop;
    return res;
  end "nand";

  function "or" (l : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := '0';
  begin
    for I in l'range loop
      res := or_table (l(I), res);
    end loop;
    return res;
  end "or";

  function "nor" (l : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := '0';
  begin
    for I in l'range loop
      res := nor_table (l(I), res);
    end loop;
    return res;
  end "nor";

  function "xor" (l : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := '0';
  begin
    for I in l'range loop
      res := xor_table (l(I), res);
    end loop;
    return res;
  end "xor";

  function "xnor" (l : std_ulogic_vector) return std_ulogic
  is
    variable res : std_ulogic := '0';
  begin
    for I in l'range loop
      res := xnor_table (l(I), res);
    end loop;
    return res;
  end "xnor";

  function "sll" (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type := (others => '0');
  begin
    if r >= 0 then
      res (1 to l'length - r) := la (r + 1 to res'right);
    else
      res (1 - r to res'right) := la (1 to l'length + r);
    end if;
    return res;
  end "sll";

  function "srl" (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type := (others => '0');
  begin
    if r >= 0 then
      res (1 + r to res'right) := la (1 to l'length - r);
    else
      res (1 to l'length + r) := la (r - 1 to res'right);
    end if;
    return res;
  end "srl";

  function "rol" (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
    constant rm : integer := r mod l'length;
  begin
    if r >= 0 then
      res (1 to res'right - rm) := la (rm + 1 to la'right);
      res (res'right - rm + 1 to res'right) := la (1 to rm);
    else
      res (1 - rm to res'right) := la (1 to la'right + r);
      res (1 to rm) := la (la'right + rm + 1 to la'right);
    end if;
    return res;
  end "rol";

  function "ror" (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (1 to l'length);
    alias la : res_type is l;
    variable res : res_type;
    constant rm : integer := r mod l'length;
  begin
    if r >= 0 then
      res (1 + rm to res'right) := la (1 to la'right - r);
      res (1 to rm) := la (la'right - rm + 1 to la'right);
    else
      res (1 to res'right + rm) := la (rm - 1 to la'right);
      res (res'right + rm + 1 to res'right) := la (1 to rm);
    end if;
    return res;
  end "ror";

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


  function to_01 (s : std_ulogic_vector; xmap : std_ulogic := '0')
    return std_ulogic_vector
  is
    subtype res_type is std_ulogic_vector (s'length - 1 downto 0);
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
  end to_01;

  function to_01 (s : std_ulogic; xmap : std_ulogic := '0')
    return std_ulogic is
  begin
    case s is
      when '0' | 'L' => return '0';
      when '1' | 'H' => return '1';
      when others    => return xmap;
    end case;
  end to_01;

  function to_01 (s : bit_vector; xmap : std_ulogic := '0')
    return std_ulogic_vector
  is
    alias sa : bit_vector(s'length - 1 downto 0) is s;
    variable res : std_ulogic_vector (s'length - 1 downto 0);
  begin
    for i in sa'range loop
      res (i) := bit_to_std (sa (i));
    end loop;
    return res;
  end to_01;

  function to_01 (s : bit; xmap : std_ulogic := '0')
    return std_ulogic is
  begin
    return bit_to_std(s);
  end to_01;

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

  function to_UX01 (b : bit) return UX01 is
  begin
    return bit_to_x01 (b);
  end to_UX01;

  function "??" (l : std_ulogic) return boolean is
  begin
    return l = '1' or l = 'H';
  end "??";

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

  function is_X (s : std_ulogic) return boolean is
  begin
    return std_x (s);
  end is_X;

  function to_ostring (value : std_ulogic_vector) return string
  is
    alias avalue : std_ulogic_vector(value'length downto 1) is value;
    constant len : natural := (value'length + 2) / 3;
    variable padded : std_ulogic_vector (len * 3 downto 1);
    variable pad : std_ulogic;
    variable res : string (len downto 1);
    variable c : character;
    subtype res_type is string (1 to len);
  begin
    pad := 'Z' when value (value'left) = 'Z' else '0';
    padded (avalue'range) := to_x01z (value);
    padded (padded'left downto avalue'left + 1) := (others => pad);
    for i in res'range loop
      case padded(i * 3 downto i * 3 - 2) is
        when "000" =>  c := '0';
        when "001" =>  c := '1';
        when "010" =>  c := '2';
        when "011" =>  c := '3';
        when "100" =>  c := '4';
        when "101" =>  c := '5';
        when "110" =>  c := '6';
        when "111" =>  c := '7';
        when "ZZZ" =>  c := 'Z';
        when others => c := 'X';
      end case;
      res (i) := c;
    end loop;
    return res_type (res);
  end to_ostring;

  function to_hstring (value : std_ulogic_vector) return string
  is
    alias avalue : std_ulogic_vector(value'length downto 1) is value;
    constant len : natural := (value'length + 3) / 4;
    variable padded : std_ulogic_vector (len * 4 downto 1);
    variable pad : std_ulogic;
    variable res : string (len downto 1);
    variable c : character;
    subtype res_type is string (1 to len);
  begin
    pad := 'Z' when value (value'left) = 'Z' else '0';
    padded (avalue'range) := to_x01z (value);
    padded (padded'left downto avalue'left + 1) := (others => pad);
    for i in res'range loop
      case padded(i * 4 downto i * 4 - 3) is
        when "0000" =>  c := '0';
        when "0001" =>  c := '1';
        when "0010" =>  c := '2';
        when "0011" =>  c := '3';
        when "0100" =>  c := '4';
        when "0101" =>  c := '5';
        when "0110" =>  c := '6';
        when "0111" =>  c := '7';
        when "1000" =>  c := '8';
        when "1001" =>  c := '9';
        when "1010" =>  c := 'A';
        when "1011" =>  c := 'B';
        when "1100" =>  c := 'C';
        when "1101" =>  c := 'D';
        when "1110" =>  c := 'E';
        when "1111" =>  c := 'F';
        when "ZZZZ" =>  c := 'Z';
        when others => c := 'X';
      end case;
      res (i) := c;
    end loop;
    return res_type (res);
  end to_hstring;

  type sl_to_char_array is array (std_ulogic) of character;
  constant sl_to_char : sl_to_char_array := "UX01ZWLH-";

  procedure write (l : inout line; value : std_ulogic;
                   justified : side := right; field : width := 0) is
  begin
    write (l, sl_to_char (value), justified, field);
  end write;

  procedure write (l : inout line; value : std_ulogic_vector;
                   justified : side := right; field : width := 0) is
  begin
    write (l, to_string (value), justified, field);
  end write;

  procedure owrite (l : inout line; value : std_ulogic_vector;
                    justified : side := right; field : width := 0) is
  begin
    write (l, to_ostring (value), justified, field);
  end owrite;

  procedure hwrite (l : inout line; value : std_ulogic_vector;
                    justified : side := right; field : width := 0) is
  begin
    write (l, to_hstring (value), justified, field);
  end hwrite;

  constant nbsp : character := character'val (160);

  procedure trim (l : inout line; left : natural)
  is
    variable nl : line;
  begin
    if l'ascending then
      nl := new string(left to l'right);
      nl.all := l(left to l'right);
    else
      nl := new string(left downto l'right);
      nl.all := l(left downto l'right);
    end if;
    deallocate(l);
    l := nl;
  end trim;

  procedure read (l: inout line; value: out std_ulogic; good: out boolean)
  is
    variable p : positive;
    variable dir : integer;
  begin
    good := false;
    value := 'U';
    if l = null or l'length = 0 then
      --  Return now for empty line.
      return;
    end if;

    if l'ascending then
      dir := 1;
    else
      dir := -1;
    end if;
    p := l'left;
    loop
      case l(p) is
        when ' ' | NBSP | HT =>
          --  Skip blanks.
          null;
        when 'U' => value := 'U'; exit;
        when 'X' => value := 'X'; exit;
        when '0' => value := '0'; exit;
        when '1' => value := '1'; exit;
        when 'Z' => value := 'Z'; exit;
        when 'W' => value := 'W'; exit;
        when 'L' => value := 'L'; exit;
        when 'H' => value := 'H'; exit;
        when '-' => value := '-'; exit;
        when others =>
          trim (l, p);
          return;
      end case;
      if p = l'right then
        --  End of string.
        deallocate(l);
        l := new string'("");
        return;
      end if;
      p := p + dir;
    end loop;

    good := true;
    trim (l, p + dir);
  end read;

  procedure read (l : inout line; value : out std_ulogic)
  is
    variable good : boolean;
  begin
    read (l, value, good);
    assert good report "std_logic_1164.read(std_ulogic) cannot read value"
      severity error;
  end read;

  procedure read (l : inout line;
                  value : out std_ulogic_vector; good : out boolean)
  is
    variable p : positive;
    variable i : natural;
    variable dir : integer;
    alias av : std_ulogic_vector(1 to value'length) is value;
    variable allow_underscore : boolean;
    variable c : character;
    variable d : std_ulogic;
  begin
    good := value'length = 0;
    value := (value'range => 'U');
    if l = null or l'length = 0 then
      --  Return now for empty line.
      return;
    end if;

    if l'ascending then
      dir := 1;
    else
      dir := -1;
    end if;
    p := l'left;

    --  Skip blanks.
    p := l'left;
    loop
      case l(p) is
        when ' ' | NBSP | HT =>
          null;
        when others =>
          exit;
      end case;
      if p = l'right then
        --  End of string.
        deallocate(l);
        l := new string'("");
        return;
      end if;
      p := p + dir;
    end loop;

    if value'length = 0 then
      --  Nothing to read.
      trim (l, p);
      return;
    end if;

    --  Extract value
    i := 1;
    allow_underscore := False;
    good := false;
    loop
      c := l(p);
      case c is
        when 'U' => d := 'U';
        when 'X' => d := 'X';
        when '0' => d := '0';
        when '1' => d := '1';
        when 'Z' => d := 'Z';
        when 'W' => d := 'W';
        when 'L' => d := 'L';
        when 'H' => d := 'H';
        when '-' => d := '-';
        when others =>
          if c = '_' and allow_underscore then
            allow_underscore := false;
          else
            --  Invalid character, double or leading '_'.
            trim (l, p);
            value := (value'range => 'U');
            return;
          end if;
      end case;
      if c /= '_' then
        av (i) := d;
        allow_underscore := true;
        if i = av'right then
          --  Done.
          good := true;
          trim (l, p + dir);
          return;
        end if;
        i := i + 1;
      end if;
      if p = l'right then
        --  End of string.
        trim (l, p + dir);
        deallocate(l);
        l := new string'("");
        value := (value'range => 'U');
        return;
      end if;
      p := p + dir;
    end loop;
  end read;

  procedure read (l : inout line; value : out std_ulogic_vector)
  is
    variable good : boolean;
  begin
    read (l, value, good);
    assert good
      report "std_logic_1164.read(std_ulogic_vector) cannot read value"
      severity error;
  end read;

  procedure hread (l : inout line;
                   value : out std_ulogic_vector; good : out boolean)
  is
    variable p : positive;
    variable i : natural;
    variable dir : integer;
    constant ndigits : natural := (value'length + 3) / 4;
    variable v : std_ulogic_vector(1 to ndigits * 4);
    variable allow_underscore : boolean;
    variable c : character;
    variable d : std_ulogic_vector (3 downto 0);
  begin
    good := value'length = 0;
    value := (value'range => 'U');
    if l = null or l'length = 0 then
      --  Return now for empty line.
      return;
    end if;

    if l'ascending then
      dir := 1;
    else
      dir := -1;
    end if;
    p := l'left;

    --  Skip blanks.
    p := l'left;
    loop
      case l(p) is
        when ' ' | NBSP | HT =>
          null;
        when others =>
          exit;
      end case;
      if p = l'right then
        --  End of string.
        deallocate(l);
        l := new string'("");
        return;
      end if;
      p := p + dir;
    end loop;

    if value'length = 0 then
      --  Nothing to read.
      trim (l, p);
      return;
    end if;

    --  Extract value
    i := 0;
    allow_underscore := False;
    good := false;
    loop
      c := l(p);
      case c is
        when '0'       => d := "0000";
        when '1'       => d := "0001";
        when '2'       => d := "0010";
        when '3'       => d := "0011";
        when '4'       => d := "0100";
        when '5'       => d := "0101";
        when '6'       => d := "0110";
        when '7'       => d := "0111";
        when '8'       => d := "1000";
        when '9'       => d := "1001";
        when 'A' | 'a' => d := "1010";
        when 'B' | 'b' => d := "1011";
        when 'C' | 'c' => d := "1100";
        when 'D' | 'd' => d := "1101";
        when 'E' | 'e' => d := "1110";
        when 'F' | 'f' => d := "1111";
        when 'Z'       => d := "ZZZZ";
        when 'X'       => d := "XXXX";
        when others =>
          if c = '_' and allow_underscore then
            allow_underscore := false;
          else
            --  Invalid character, double or leading '_'.
            trim (l, p);
            return;
          end if;
      end case;
      if c /= '_' then
        allow_underscore := true;
        v (i * 4 + 1 to i * 4 + 4) := d;
        i := i + 1;
        if i = ndigits then
          --  Done.
          if or (v(1 to ndigits * 4 - value'length)) /= '1' then
            --  No truncated digit is a '1'.
            value := v (ndigits * 4 - value'length + 1 to v'right);
            good := true;
          end if;
          trim (l, p + dir);
          return;
        end if;
      end if;
      if p = l'right then
        --  End of string.
        trim (l, p + dir);
        deallocate(l);
        l := new string'("");
        return;
      end if;
      p := p + dir;
    end loop;
  end hread;

  procedure hread (l : inout line; value : out std_ulogic_vector)
  is
    variable good : boolean;
  begin
    hread (l, value, good);
    assert good
      report "std_logic_1164.hread(std_ulogic_vector) cannot read value"
      severity error;
  end hread;

  procedure oread (l : inout line;
                   value : out std_ulogic_vector; good : out boolean)
  is
    variable p : positive;
    variable i : natural;
    variable dir : integer;
    constant ndigits : natural := (value'length + 2) / 3;
    variable v : std_ulogic_vector(1 to ndigits * 3);
    variable allow_underscore : boolean;
    variable c : character;
    variable d : std_ulogic_vector (2 downto 0);
  begin
    good := value'length = 0;
    value := (value'range => 'U');
    if l = null or l'length = 0 then
      --  Return now for empty line.
      return;
    end if;

    if l'ascending then
      dir := 1;
    else
      dir := -1;
    end if;
    p := l'left;

    --  Skip blanks.
    p := l'left;
    loop
      case l(p) is
        when ' ' | NBSP | HT =>
          null;
        when others =>
          exit;
      end case;
      if p = l'right then
        --  End of string.
        deallocate(l);
        l := new string'("");
        return;
      end if;
      p := p + dir;
    end loop;

    if value'length = 0 then
      --  Nothing to read.
      trim (l, p);
      return;
    end if;

    --  Extract value
    i := 0;
    allow_underscore := False;
    good := false;
    loop
      c := l(p);
      case c is
        when '0'       => d := "000";
        when '1'       => d := "001";
        when '2'       => d := "010";
        when '3'       => d := "011";
        when '4'       => d := "100";
        when '5'       => d := "101";
        when '6'       => d := "110";
        when '7'       => d := "111";
        when 'Z'       => d := "ZZZ";
        when 'X'       => d := "XXX";
        when others =>
          if c = '_' and allow_underscore then
            allow_underscore := false;
          else
            --  Invalid character, double or leading '_'.
            trim (l, p);
            return;
          end if;
      end case;
      if c /= '_' then
        allow_underscore := true;
        v (i * 3 + 1 to i * 3 + 3) := d;
        i := i + 1;
        if i = ndigits then
          --  Done.
          if or (v(1 to ndigits * 3 - value'length)) /= '1' then
            --  No truncated digit is a '1'.
            value := v (ndigits * 3 - value'length + 1 to v'right);
            good := true;
          end if;
          trim (l, p + dir);
          return;
        end if;
      end if;
      if p = l'right then
        --  End of string.
        trim (l, p + dir);
        deallocate(l);
        l := new string'("");
        return;
      end if;
      p := p + dir;
    end loop;
  end oread;

  procedure oread (l : inout line; value : out std_ulogic_vector)
  is
    variable good : boolean;
  begin
    oread (l, value, good);
    assert good
      report "std_logic_1164.oread(std_ulogic_vector) cannot read value"
      severity error;
  end oread;

end std_logic_1164;
