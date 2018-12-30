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

use std.textio.all;

package std_logic_1164 is

  --  Unresolved logic state.
  type std_ulogic is
    (
     'U',  --  Uninitialized, this is also the default value.
     'X',  --  Unknown / conflict value (forcing level).
     '0',  --  0 (forcing level).
     '1',  --  1 (forcing level).
     'Z',  --  High impedance.
     'W',  --  Unknown / conflict (weak level).
     'L',  --  0 (weak level).
     'H',  --  1 (weak level).
     '-'   --  Don't care.
    );

  --  Vector of logic state.
  type std_ulogic_vector is array (natural range <>) of std_ulogic;

  --  Resolution function.
  --  If S is empty, returns 'Z'.
  --  If S has one element, return the element.
  --  Otherwise, 'U' is the strongest.
  --       then  'X'
  --       then  '0' and '1'
  --       then  'W'
  --       then  'H' and 'L'
  --       then  'Z'.
  function resolved (s : std_ulogic_vector) return std_ulogic;

  --  Resolved logic state.
  subtype std_logic is resolved std_ulogic;

  --  Vector of std_logic.
  subtype std_logic_vector is (resolved) std_ulogic_vector;

  --  Subtypes of std_ulogic.  The names give the values.
  subtype X01   is resolved std_ulogic range 'X' to '1';
  subtype X01Z  is resolved std_ulogic range 'X' to 'Z';
  subtype UX01  is resolved std_ulogic range 'U' to '1';
  subtype UX01Z is resolved std_ulogic range 'U' to 'Z';

  --  Logical operators.
  --  For logical operations, the inputs are first normalized to UX01:
  --  0 and L are normalized to 0, 1 and 1 are normalized to 1, U isnt changed,
  --  all other states are normalized to X.
  --  Then the classical electric rules are followed.
  function "and"  (l : std_ulogic; r : std_ulogic) return UX01;
  function "nand" (l : std_ulogic; r : std_ulogic) return UX01;
  function "or"   (l : std_ulogic; r : std_ulogic) return UX01;
  function "nor"  (l : std_ulogic; r : std_ulogic) return UX01;
  function "xor"  (l : std_ulogic; r : std_ulogic) return UX01;
  function "xnor" (l : std_ulogic; r : std_ulogic) return UX01;
  function "not"  (l : std_ulogic) return UX01;

  --  Logical operators for vectors.
  --  An assertion of severity failure fails if the length of L and R aren't
  --  equal.  The result range is 1 to L'Length.

  function "and"  (l, r : std_ulogic_vector) return std_ulogic_vector;
  function "nand" (l, r : std_ulogic_vector) return std_ulogic_vector;
  function "or"   (l, r : std_ulogic_vector) return std_ulogic_vector;
  function "nor"  (l, r : std_ulogic_vector) return std_ulogic_vector;
  function "xor"  (l, r : std_ulogic_vector) return std_ulogic_vector;
  function "xnor" (l, r : std_ulogic_vector) return std_ulogic_vector;
  function "not"  (l : std_ulogic_vector) return std_ulogic_vector;

  function "and"  (l : std_ulogic_vector; r : std_ulogic       )
    return std_ulogic_vector;
  function "and"  (l : std_ulogic;        r : std_ulogic_vector)
    return std_ulogic_vector;
  function "nand" (l : std_ulogic_vector; r : std_ulogic       )
    return std_ulogic_vector;
  function "nand" (l : std_ulogic;        r : std_ulogic_vector)
    return std_ulogic_vector;
  function "or"   (l : std_ulogic_vector; r : std_ulogic       )
    return std_ulogic_vector;
  function "or"   (l : std_ulogic;        r : std_ulogic_vector)
    return std_ulogic_vector;
  function "nor"  (l : std_ulogic_vector; r : std_ulogic       )
    return std_ulogic_vector;
  function "nor"  (l : std_ulogic;        r : std_ulogic_vector)
    return std_ulogic_vector;
  function "xor"  (l : std_ulogic_vector; r : std_ulogic       )
    return std_ulogic_vector;
  function "xor"  (l : std_ulogic;        r : std_ulogic_vector)
    return std_ulogic_vector;
  function "xnor" (l : std_ulogic_vector; r : std_ulogic       )
    return std_ulogic_vector;
  function "xnor" (l : std_ulogic;        r : std_ulogic_vector)
    return std_ulogic_vector;

  function "and"  (l : std_ulogic_vector) return std_ulogic;
  function "nand" (l : std_ulogic_vector) return std_ulogic;
  function "or"   (l : std_ulogic_vector) return std_ulogic;
  function "nor"  (l : std_ulogic_vector) return std_ulogic;
  function "xor"  (l : std_ulogic_vector) return std_ulogic;
  function "xnor" (l : std_ulogic_vector) return std_ulogic;

  function "sll"  (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector;
  function "srl"  (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector;
  function "rol"  (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector;
  function "ror"  (l : std_ulogic_vector; r : integer)
    return std_ulogic_vector;

  --  Conversion functions.
  --  The result range (for vectors) is S'Length - 1 downto 0.
  --  XMAP is return for values not in '0', '1', 'L', 'H'.
  function to_bit (s : std_ulogic; xmap : bit := '0') return bit;
  function to_bitvector (s : std_ulogic_vector; xmap : bit := '0')
    return bit_vector;

  function to_stdulogic (b : bit) return std_ulogic;
  function to_stdlogicvector (b : bit_vector) return std_logic_vector;
  function to_stdlogicvector (s : std_ulogic_vector) return std_logic_vector;
  function to_stdulogicvector (b : bit_vector) return std_ulogic_vector;
  function to_stdulogicvector (s : std_logic_vector) return std_ulogic_vector;

  alias to_bit_vector is
    to_bitvector[std_ulogic_vector, bit return bit_vector];
  alias to_bv is
    to_bitvector[std_ulogic_vector, bit return bit_vector];

  alias to_std_logic_vector is
    to_stdlogicvector[bit_vector return std_logic_vector];
  alias to_slv is
    to_stdlogicvector[bit_vector return std_logic_vector];

  alias to_std_logic_vector is
    to_stdlogicvector[std_ulogic_vector return std_logic_vector];
  alias to_slv is
    to_stdlogicvector[std_ulogic_vector return std_logic_vector];

  alias to_std_ulogic_vector is
    to_stdulogicvector[bit_vector return std_ulogic_vector];
  alias to_sulv is
    to_stdulogicvector[bit_vector return std_ulogic_vector];

  alias to_std_ulogic_vector is
    to_stdulogicvector[std_logic_vector return std_ulogic_vector];
  alias to_sulv is
    to_stdulogicvector[std_logic_vector return std_ulogic_vector];

  --  Normalization.
  --  The result range (for vectors) is 1 to S'Length.
  function to_01  (s : std_ulogic_vector; xmap : std_ulogic := '0')
    return std_ulogic_vector;
  function to_01  (s : std_ulogic;        xmap : std_ulogic := '0')
    return std_ulogic;
  function to_01  (s : bit_vector;        xmap : std_ulogic := '0')
    return std_ulogic_vector;
  function to_01  (s : bit;               xmap : std_ulogic := '0')
    return std_ulogic;

  function to_X01 (s : std_ulogic_vector) return std_ulogic_vector;
  function to_X01 (s : std_ulogic) return X01;
  function to_X01 (b : bit_vector) return std_ulogic_vector;
  function to_X01 (b : bit) return X01;

  function to_X01Z (s : std_ulogic_vector) return std_ulogic_vector;
  function to_X01Z (s : std_ulogic) return X01Z;
  function to_X01Z (b : bit_vector) return std_ulogic_vector;
  function to_X01Z (b : bit) return X01Z;

  function to_UX01 (s : std_ulogic_vector) return std_ulogic_vector;
  function to_UX01 (s : std_ulogic) return UX01;
  function to_UX01 (b : bit_vector) return std_ulogic_vector;
  function to_UX01 (b : bit) return UX01;

  function "??" (l : std_ulogic) return boolean;

  --  Edge detection.
  --  An edge is detected in case of event on s, and X01 normalized value
  --  rises from 0 to 1 or falls from 1 to 0.
  function rising_edge (signal s : std_ulogic) return boolean;
  function falling_edge (signal s : std_ulogic) return boolean;

  --  Test for unknown.  Only 0, 1, L and H are known values.
  function is_X (s : std_ulogic_vector) return boolean;
  function is_X (s : std_ulogic) return boolean;

  --  String conversion

  alias to_bstring is to_string [std_ulogic_vector return string];
  alias to_binary_string is to_string [std_ulogic_vector return string];

  function to_ostring (value : std_ulogic_vector) return string;
  alias to_octal_string is to_ostring [std_ulogic_vector return string];

  function to_hstring (value : std_ulogic_vector) return string;
  alias to_hex_string is to_hstring [std_ulogic_vector return string];

  --  Input/output

  procedure write (l : inout line; value : std_ulogic;
                   justified : side := right; field : width := 0);

  procedure write (l : inout line; value : std_ulogic_vector;
                   justified : side := right; field : width := 0);
  alias bwrite is write [line, std_ulogic_vector, side, width];
  alias binary_write is write [line, std_ulogic_vector, side, width];

  procedure owrite (l : inout line; value : std_ulogic_vector;
                    justified : side := right; field : width := 0);
  alias octal_write is owrite [line, std_ulogic_vector, side, width];

  procedure hwrite (l : inout line; value : std_ulogic_vector;
                    justified : side := right; field : width := 0);
  alias hex_write is hwrite [line, std_ulogic_vector, side, width];

  procedure read (l : inout line;
                  value : out std_ulogic; good : out boolean);
  procedure read (l : inout line; value : out std_ulogic);

  procedure read (l : inout line;
                  value : out std_ulogic_vector; good : out boolean);
  procedure read (l : inout line; value : out std_ulogic_vector);
  alias bread is read [line, std_ulogic_vector, boolean];
  alias bread is read [line, std_ulogic_vector];
  alias binary_read is read [line, std_ulogic_vector, boolean];
  alias binary_read is read [line, std_ulogic_vector];

  procedure hread (l : inout line;
                   value : out std_ulogic_vector; good : out boolean);
  procedure hread (l : inout line; value : out std_ulogic_vector);
  alias hex_read is read [line, std_ulogic_vector, boolean];
  alias hex_read is read [line, std_ulogic_vector];

  procedure oread (l : inout line;
                   value : out std_ulogic_vector; good : out boolean);
  procedure oread (l : inout line; value : out std_ulogic_vector);
  alias octal_read is read [line, std_ulogic_vector, boolean];
  alias octal_read is read [line, std_ulogic_vector];
end std_logic_1164;
