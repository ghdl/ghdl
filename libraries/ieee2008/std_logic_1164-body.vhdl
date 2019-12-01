-- -----------------------------------------------------------------
-- 
-- Copyright 2019 IEEE P1076 WG Authors
-- 
-- See the LICENSE file distributed with this work for copyright and
-- licensing information and the AUTHORS file.
-- 
-- This file to you under the Apache License, Version 2.0 (the "License").
-- You may obtain a copy of the License at
-- 
--     http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
--
--   Title     :  Standard multivalue logic package
--             :  (STD_LOGIC_1164 package body)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  IEEE model standards group (PAR 1164),
--             :  Accellera VHDL-TC, and IEEE P1076 Working Group
--             :
--   Purpose   :  This packages defines a standard for designers
--             :  to use in describing the interconnection data types
--             :  used in vhdl modeling.
--             :
--   Limitation:  The logic system defined in this package may
--             :  be insufficient for modeling switched transistors,
--             :  since such a requirement is out of the scope of this
--             :  effort. Furthermore, mathematics, primitives,
--             :  timing standards, etc. are considered orthogonal
--             :  issues as it relates to this package and are therefore
--             :  beyond the scope of this effort.
--             :
--   Note      :  This package may be modified to include additional data
--             :  required by tools, but it must in no way change the
--             :  external interfaces or simulation behavior of the
--             :  description. It is permissible to add comments and/or
--             :  attributes to the package declarations, but not to change
--             :  or delete any original lines of the package declaration.
--             :  The package body may be changed only in accordance with
--             :  the terms of Clause 16 of this standard.
--             :
-- --------------------------------------------------------------------
-- $Revision: 1220 $
-- $Date: 2008-04-10 17:16:09 +0930 (Thu, 10 Apr 2008) $
-- --------------------------------------------------------------------

package body std_logic_1164 is
  -------------------------------------------------------------------
  -- local types
  -------------------------------------------------------------------
  type stdlogic_1d is array (STD_ULOGIC) of STD_ULOGIC;
  type stdlogic_table is array(STD_ULOGIC, STD_ULOGIC) of STD_ULOGIC;

  -------------------------------------------------------------------
  -- resolution function
  -------------------------------------------------------------------
  constant resolution_table : stdlogic_table := (
    --      ---------------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -        |   |
    --      ---------------------------------------------------------
             ('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U'),  -- | U |
             ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'),  -- | X |
             ('U', 'X', '0', 'X', '0', '0', '0', '0', 'X'),  -- | 0 |
             ('U', 'X', 'X', '1', '1', '1', '1', '1', 'X'),  -- | 1 |
             ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', 'X'),  -- | Z |
             ('U', 'X', '0', '1', 'W', 'W', 'W', 'W', 'X'),  -- | W |
             ('U', 'X', '0', '1', 'L', 'W', 'L', 'W', 'X'),  -- | L |
             ('U', 'X', '0', '1', 'H', 'W', 'W', 'H', 'X'),  -- | H |
             ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X')   -- | - |
             );

  function resolved (s : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := 'Z';  -- weakest state default
  begin
    -- the test for a single driver is essential otherwise the
    -- loop would return 'X' for a single driver of '-' and that
    -- would conflict with the value of a single driver unresolved
    -- signal.
    if (s'length = 1) then return s(s'low);
    else
      for i in s'range loop
        result := resolution_table(result, s(i));
      end loop;
    end if;
    return result;
  end function resolved;

  -------------------------------------------------------------------
  -- tables for logical operations
  -------------------------------------------------------------------

  -- truth table for "and" function
  constant and_table : stdlogic_table := (
    --      ----------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -         |   |
    --      ----------------------------------------------------
             ('U', 'U', '0', 'U', 'U', 'U', '0', 'U', 'U'),  -- | U |
             ('U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X'),  -- | X |
             ('0', '0', '0', '0', '0', '0', '0', '0', '0'),  -- | 0 |
             ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'),  -- | 1 |
             ('U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X'),  -- | Z |
             ('U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X'),  -- | W |
             ('0', '0', '0', '0', '0', '0', '0', '0', '0'),  -- | L |
             ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'),  -- | H |
             ('U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X')   -- | - |
             );

  -- truth table for "or" function
  constant or_table : stdlogic_table := (
    --      ----------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -         |   |
    --      ----------------------------------------------------
             ('U', 'U', 'U', '1', 'U', 'U', 'U', '1', 'U'),  -- | U |
             ('U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X'),  -- | X |
             ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'),  -- | 0 |
             ('1', '1', '1', '1', '1', '1', '1', '1', '1'),  -- | 1 |
             ('U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X'),  -- | Z |
             ('U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X'),  -- | W |
             ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'),  -- | L |
             ('1', '1', '1', '1', '1', '1', '1', '1', '1'),  -- | H |
             ('U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X')   -- | - |
             );

  -- truth table for "xor" function
  constant xor_table : stdlogic_table := (
    --      ----------------------------------------------------
    --      |  U    X    0    1    Z    W    L    H    -         |   |
    --      ----------------------------------------------------
             ('U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U'),  -- | U |
             ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'),  -- | X |
             ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'),  -- | 0 |
             ('U', 'X', '1', '0', 'X', 'X', '1', '0', 'X'),  -- | 1 |
             ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'),  -- | Z |
             ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X'),  -- | W |
             ('U', 'X', '0', '1', 'X', 'X', '0', '1', 'X'),  -- | L |
             ('U', 'X', '1', '0', 'X', 'X', '1', '0', 'X'),  -- | H |
             ('U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X')   -- | - |
             );

  -- truth table for "not" function
  constant not_table : stdlogic_1d :=
    --  -------------------------------------------------
    --  |   U    X    0    1    Z    W    L    H    -   |
    --  -------------------------------------------------
          ('U', 'X', '1', '0', 'X', 'X', '1', '0', 'X');

  -------------------------------------------------------------------
  -- overloaded logical operators ( with optimizing hints )
  -------------------------------------------------------------------

  function "and" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (and_table(l, r));
  end function "and";

  function "nand" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (not_table (and_table(l, r)));
  end function "nand";

  function "or" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (or_table(l, r));
  end function "or";

  function "nor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (not_table (or_table(l, r)));
  end function "nor";

  function "xor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (xor_table(l, r));
  end function "xor";

  function "xnor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return not_table(xor_table(l, r));
  end function "xnor";

  function "not" (l : STD_ULOGIC) return UX01 is
  begin
    return (not_table(l));
  end function "not";

  -------------------------------------------------------------------
  -- and
  -------------------------------------------------------------------
  function "and" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD_LOGIC_1164.""and"": "
        & "arguments of overloaded 'and' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := and_table (lv(i), rv(i));
      end loop;
    end if;
    return result;
  end function "and";
  -------------------------------------------------------------------
  -- nand
  -------------------------------------------------------------------
  function "nand" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD_LOGIC_1164.""nand"": "
        & "arguments of overloaded 'nand' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := not_table(and_table (lv(i), rv(i)));
      end loop;
    end if;
    return result;
  end function "nand";
  -------------------------------------------------------------------
  -- or
  -------------------------------------------------------------------
  function "or" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD_LOGIC_1164.""or"": "
        & "arguments of overloaded 'or' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := or_table (lv(i), rv(i));
      end loop;
    end if;
    return result;
  end function "or";
  -------------------------------------------------------------------
  -- nor
  -------------------------------------------------------------------
  function "nor" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD_LOGIC_1164.""nor"": "
        & "arguments of overloaded 'nor' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := not_table(or_table (lv(i), rv(i)));
      end loop;
    end if;
    return result;
  end function "nor";
  ---------------------------------------------------------------------
  -- xor
  -------------------------------------------------------------------
  function "xor" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD_LOGIC_1164.""xor"": "
        & "arguments of overloaded 'xor' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := xor_table (lv(i), rv(i));
      end loop;
    end if;
    return result;
  end function "xor";
  -------------------------------------------------------------------
  -- xnor
  -------------------------------------------------------------------
  function "xnor" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    if (l'length /= r'length) then
      assert false
        report "STD_LOGIC_1164.""xnor"": "
        & "arguments of overloaded 'xnor' operator are not of the same length"
        severity failure;
    else
      for i in result'range loop
        result(i) := not_table(xor_table (lv(i), rv(i)));
      end loop;
    end if;
    return result;
  end function "xnor";
  -------------------------------------------------------------------
  -- not
  -------------------------------------------------------------------
  function "not" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => 'X');
  begin
    for i in result'range loop
      result(i) := not_table(lv(i));
    end loop;
    return result;
  end function "not";

  -------------------------------------------------------------------
  -- and
  -------------------------------------------------------------------
  function "and" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := and_table (lv(i), r);
    end loop;
    return result;
  end function "and";
  -------------------------------------------------------------------
  function "and" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := and_table (l, rv(i));
    end loop;
    return result;
  end function "and";

  -------------------------------------------------------------------
  -- nand
  -------------------------------------------------------------------
  function "nand" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := not_table(and_table (lv(i), r));
    end loop;
    return result;
  end function "nand";
  -------------------------------------------------------------------
  function "nand" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := not_table(and_table (l, rv(i)));
    end loop;
    return result;
  end function "nand";

  -------------------------------------------------------------------
  -- or
  -------------------------------------------------------------------
  function "or" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := or_table (lv(i), r);
    end loop;
    return result;
  end function "or";
  -------------------------------------------------------------------
  function "or" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := or_table (l, rv(i));
    end loop;
    return result;
  end function "or";

  -------------------------------------------------------------------
  -- nor
  -------------------------------------------------------------------
  function "nor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := not_table(or_table (lv(i), r));
    end loop;
    return result;
  end function "nor";
  -------------------------------------------------------------------
  function "nor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := not_table(or_table (l, rv(i)));
    end loop;
    return result;
  end function "nor";

  -------------------------------------------------------------------
  -- xor
  -------------------------------------------------------------------
  function "xor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := xor_table (lv(i), r);
    end loop;
    return result;
  end function "xor";
  -------------------------------------------------------------------
  function "xor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := xor_table (l, rv(i));
    end loop;
    return result;
  end function "xor";

  -------------------------------------------------------------------
  -- xnor
  -------------------------------------------------------------------
  function "xnor" (l : STD_ULOGIC_VECTOR; r : STD_ULOGIC)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
  begin
    for i in result'range loop
      result(i) := not_table(xor_table (lv(i), r));
    end loop;
    return result;
  end function "xnor";
  -------------------------------------------------------------------
  function "xnor" (l : STD_ULOGIC; r : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias rv        : STD_ULOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_ULOGIC_VECTOR (1 to r'length);
  begin
    for i in result'range loop
      result(i) := not_table(xor_table (l, rv(i)));
    end loop;
    return result;
  end function "xnor";

  -------------------------------------------------------------------
  -- and
  -------------------------------------------------------------------
  function "and" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '1';
  begin
    for i in l'reverse_range loop
      result := and_table (l(i), result);
    end loop;
    return result;
  end function "and";

  -------------------------------------------------------------------
  -- nand
  -------------------------------------------------------------------
  function "nand" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '1';
  begin
    for i in l'reverse_range loop
      result := and_table (l(i), result);
    end loop;
    return not_table(result);
  end function "nand";

  -------------------------------------------------------------------
  -- or
  -------------------------------------------------------------------
  function "or" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '0';
  begin
    for i in l'reverse_range loop
      result := or_table (l(i), result);
    end loop;
    return result;
  end function "or";

  -------------------------------------------------------------------
  -- nor
  -------------------------------------------------------------------
  function "nor" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '0';
  begin
    for i in l'reverse_range loop
      result := or_table (l(i), result);
    end loop;
    return not_table(result);
  end function "nor";

  -------------------------------------------------------------------
  -- xor
  -------------------------------------------------------------------
  function "xor" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '0';
  begin
    for i in l'reverse_range loop
      result := xor_table (l(i), result);
    end loop;
    return result;
  end function "xor";

  -------------------------------------------------------------------
  -- xnor
  -------------------------------------------------------------------
  function "xnor" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC is
    variable result : STD_ULOGIC := '0';
  begin
    for i in l'reverse_range loop
      result := xor_table (l(i), result);
    end loop;
    return not_table(result);
  end function "xnor";

  -------------------------------------------------------------------
  -- shift operators
  -------------------------------------------------------------------

  -------------------------------------------------------------------
  -- sll
  -------------------------------------------------------------------
  function "sll" (l : STD_ULOGIC_VECTOR; r : INTEGER)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => '0');
  begin
    if r >= 0 then
      result(1 to l'length - r) := lv(r + 1 to l'length);
    else
      result := l srl -r;
    end if;
    return result;
  end function "sll";

  -------------------------------------------------------------------
  -- srl
  -------------------------------------------------------------------
  function "srl" (l : STD_ULOGIC_VECTOR; r : INTEGER)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => '0');
  begin
    if r >= 0 then
      result(r + 1 to l'length) := lv(1 to l'length - r);
    else
      result := l sll -r;
    end if;
    return result;
  end function "srl";

  -------------------------------------------------------------------
  -- rol
  -------------------------------------------------------------------
  function "rol" (l : STD_ULOGIC_VECTOR; r : INTEGER)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length);
    constant rm     : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(1 to l'length - rm)            := lv(rm + 1 to l'length);
      result(l'length - rm + 1 to l'length) := lv(1 to rm);
    else
      result := l ror -r;
    end if;
    return result;
  end function "rol";

  -------------------------------------------------------------------
  -- ror
  -------------------------------------------------------------------
  function "ror" (l : STD_ULOGIC_VECTOR; r : INTEGER)
    return STD_ULOGIC_VECTOR
  is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => '0');
    constant rm     : INTEGER := r mod l'length;
  begin
    if r >= 0 then
      result(rm + 1 to l'length) := lv(1 to l'length - rm);
      result(1 to rm)            := lv(l'length - rm + 1 to l'length);
    else
      result := l rol -r;
    end if;
    return result;
  end function "ror";

  -------------------------------------------------------------------
  -- conversion tables
  -------------------------------------------------------------------
  type logic_x01_table is array (STD_ULOGIC'low to STD_ULOGIC'high) of X01;
  type logic_x01z_table is array (STD_ULOGIC'low to STD_ULOGIC'high) of X01Z;
  type logic_ux01_table is array (STD_ULOGIC'low to STD_ULOGIC'high) of UX01;
  ----------------------------------------------------------
  -- table name : cvt_to_x01
  --
  -- parameters :
  --        in  :  std_ulogic  -- some logic value
  -- returns    :  x01         -- state value of logic value
  -- purpose    :  to convert state-strength to state only
  --
  -- example    : if (cvt_to_x01 (input_signal) = '1' ) then ...
  --
  ----------------------------------------------------------
  constant cvt_to_x01 : logic_x01_table := (
    'X',                                -- 'U'
    'X',                                -- 'X'
    '0',                                -- '0'
    '1',                                -- '1'
    'X',                                -- 'Z'
    'X',                                -- 'W'
    '0',                                -- 'L'
    '1',                                -- 'H'
    'X'                                 -- '-'
    );

  ----------------------------------------------------------
  -- table name : cvt_to_x01z
  --
  -- parameters :
  --        in  :  std_ulogic  -- some logic value
  -- returns    :  x01z        -- state value of logic value
  -- purpose    :  to convert state-strength to state only
  --
  -- example    : if (cvt_to_x01z (input_signal) = '1' ) then ...
  --
  ----------------------------------------------------------
  constant cvt_to_x01z : logic_x01z_table := (
    'X',                                -- 'U'
    'X',                                -- 'X'
    '0',                                -- '0'
    '1',                                -- '1'
    'Z',                                -- 'Z'
    'X',                                -- 'W'
    '0',                                -- 'L'
    '1',                                -- 'H'
    'X'                                 -- '-'
    );

  ----------------------------------------------------------
  -- table name : cvt_to_ux01
  --
  -- parameters :
  --        in  :  std_ulogic  -- some logic value
  -- returns    :  ux01        -- state value of logic value
  -- purpose    :  to convert state-strength to state only
  --
  -- example    : if (cvt_to_ux01 (input_signal) = '1' ) then ...
  --
  ----------------------------------------------------------
  constant cvt_to_ux01 : logic_ux01_table := (
    'U',                                -- 'U'
    'X',                                -- 'X'
    '0',                                -- '0'
    '1',                                -- '1'
    'X',                                -- 'Z'
    'X',                                -- 'W'
    '0',                                -- 'L'
    '1',                                -- 'H'
    'X'                                 -- '-'
    );

  -------------------------------------------------------------------
  -- conversion functions
  -------------------------------------------------------------------
  function To_bit (s : STD_ULOGIC; xmap : BIT := '0') return BIT is
  begin
    case s is
      when '0' | 'L' => return ('0');
      when '1' | 'H' => return ('1');
      when others    => return xmap;
    end case;
  end function To_bit;
  --------------------------------------------------------------------
  function To_bitvector (s : STD_ULOGIC_VECTOR; xmap : BIT := '0')
    return BIT_VECTOR
  is
    alias sv        : STD_ULOGIC_VECTOR (s'length-1 downto 0) is s;
    variable result : BIT_VECTOR (s'length-1 downto 0);
  begin
    for i in result'range loop
      case sv(i) is
        when '0' | 'L' => result(i) := '0';
        when '1' | 'H' => result(i) := '1';
        when others    => result(i) := xmap;
      end case;
    end loop;
    return result;
  end function To_bitvector;
  --------------------------------------------------------------------
  function To_StdULogic (b : BIT) return STD_ULOGIC is
  begin
    case b is
      when '0' => return '0';
      when '1' => return '1';
    end case;
  end function To_StdULogic;
  --------------------------------------------------------------------
  function To_StdLogicVector (b : BIT_VECTOR)
    return STD_LOGIC_VECTOR
  is
    alias bv        : BIT_VECTOR (b'length-1 downto 0) is b;
    variable result : STD_LOGIC_VECTOR (b'length-1 downto 0);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end function To_StdLogicVector;
  --------------------------------------------------------------------
  function To_StdLogicVector (s : STD_ULOGIC_VECTOR)
    return STD_LOGIC_VECTOR
  is
    alias sv        : STD_ULOGIC_VECTOR (s'length-1 downto 0) is s;
    variable result : STD_LOGIC_VECTOR (s'length-1 downto 0);
  begin
    for i in result'range loop
      result(i) := sv(i);
    end loop;
    return result;
  end function To_StdLogicVector;
  --------------------------------------------------------------------
  function To_StdULogicVector (b : BIT_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias bv        : BIT_VECTOR (b'length-1 downto 0) is b;
    variable result : STD_ULOGIC_VECTOR (b'length-1 downto 0);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end function To_StdULogicVector;
  --------------------------------------------------------------------
  function To_StdULogicVector (s : STD_LOGIC_VECTOR)
    return STD_ULOGIC_VECTOR
  is
    alias sv        : STD_LOGIC_VECTOR (s'length-1 downto 0) is s;
    variable result : STD_ULOGIC_VECTOR (s'length-1 downto 0);
  begin
    for i in result'range loop
      result(i) := sv(i);
    end loop;
    return result;
  end function To_StdULogicVector;

  -------------------------------------------------------------------
  -- strength strippers and type convertors
  -------------------------------------------------------------------
  -- to_01
  -------------------------------------------------------------------
  function TO_01 (s : STD_ULOGIC_VECTOR; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC_VECTOR
  is
    variable RESULT      : STD_ULOGIC_VECTOR(s'length-1 downto 0);
    variable BAD_ELEMENT : BOOLEAN := false;
    alias XS             : STD_ULOGIC_VECTOR(s'length-1 downto 0) is s;
  begin
    for I in RESULT'range loop
      case XS(I) is
        when '0' | 'L' => RESULT(I)   := '0';
        when '1' | 'H' => RESULT(I)   := '1';
        when others    => BAD_ELEMENT := true;
      end case;
    end loop;
    if BAD_ELEMENT then
      for I in RESULT'range loop
        RESULT(I) := xmap;              -- standard fixup
      end loop;
    end if;
    return RESULT;
  end function TO_01;
  -------------------------------------------------------------------
  function TO_01 (s : STD_ULOGIC; xmap : STD_ULOGIC := '0') return STD_ULOGIC is
  begin
    case s is
      when '0' | 'L' => RETURN '0';
      when '1' | 'H' => RETURN '1';
      when others    => return xmap;
    end case;
  end function TO_01;
  -------------------------------------------------------------------
  function TO_01 (s : BIT_VECTOR; xmap : STD_ULOGIC := '0')
    return STD_ULOGIC_VECTOR
  is
    variable RESULT : STD_ULOGIC_VECTOR(s'length-1 downto 0);
    alias XS        : BIT_VECTOR(s'length-1 downto 0) is s;
  begin
    for I in RESULT'range loop
      case XS(I) is
        when '0' => RESULT(I) := '0';
        when '1' => RESULT(I) := '1';
      end case;
    end loop;
    return RESULT;
  end function TO_01;
  -------------------------------------------------------------------
  function TO_01 (s : BIT; xmap : STD_ULOGIC := '0') return STD_ULOGIC is
  begin
    case s is
      when '0' => RETURN '0';
      when '1' => RETURN '1';
    end case;
  end function TO_01;
  -------------------------------------------------------------------
  -- to_x01
  -------------------------------------------------------------------
  function To_X01 (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias sv        : STD_ULOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_ULOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_x01 (sv(i));
    end loop;
    return result;
  end function To_X01;
  --------------------------------------------------------------------
  function To_X01 (s : STD_ULOGIC) return X01 is
  begin
    return (cvt_to_x01(s));
  end function To_X01;
  --------------------------------------------------------------------
  function To_X01 (b : BIT_VECTOR) return STD_ULOGIC_VECTOR is
    alias bv        : BIT_VECTOR (1 to b'length) is b;
    variable result : STD_ULOGIC_VECTOR (1 to b'length);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end function To_X01;
  --------------------------------------------------------------------
  function To_X01 (b : BIT) return X01 is
  begin
    case b is
      when '0' => return('0');
      when '1' => return('1');
    end case;
  end function To_X01;
  --------------------------------------------------------------------
  -- to_x01z
  -------------------------------------------------------------------
  function To_X01Z (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias sv        : STD_ULOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_ULOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_x01z (sv(i));
    end loop;
    return result;
  end function To_X01Z;
  --------------------------------------------------------------------
  function To_X01Z (s : STD_ULOGIC) return X01Z is
  begin
    return (cvt_to_x01z(s));
  end function To_X01Z;
  --------------------------------------------------------------------
  function To_X01Z (b : BIT_VECTOR) return STD_ULOGIC_VECTOR is
    alias bv        : BIT_VECTOR (1 to b'length) is b;
    variable result : STD_ULOGIC_VECTOR (1 to b'length);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end function To_X01Z;
  --------------------------------------------------------------------
  function To_X01Z (b : BIT) return X01Z is
  begin
    case b is
      when '0' => return('0');
      when '1' => return('1');
    end case;
  end function To_X01Z;
  --------------------------------------------------------------------
  -- to_ux01
  -------------------------------------------------------------------
  function To_UX01 (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias sv        : STD_ULOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_ULOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_ux01 (sv(i));
    end loop;
    return result;
  end function To_UX01;
  --------------------------------------------------------------------
  function To_UX01 (s : STD_ULOGIC) return UX01 is
  begin
    return (cvt_to_ux01(s));
  end function To_UX01;
  --------------------------------------------------------------------
  function To_UX01 (b : BIT_VECTOR) return STD_ULOGIC_VECTOR is
    alias bv        : BIT_VECTOR (1 to b'length) is b;
    variable result : STD_ULOGIC_VECTOR (1 to b'length);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end function To_UX01;
  --------------------------------------------------------------------
  function To_UX01 (b : BIT) return UX01 is
  begin
    case b is
      when '0' => return('0');
      when '1' => return('1');
    end case;
  end function To_UX01;

  function "??" (l : STD_ULOGIC) return BOOLEAN is
  begin
    return l = '1' or l = 'H';
  end function "??";

  -------------------------------------------------------------------
  -- edge detection
  -------------------------------------------------------------------
  function rising_edge (signal s : STD_ULOGIC) return BOOLEAN is
  begin
    return (s'event and (To_X01(s) = '1') and
            (To_X01(s'last_value) = '0'));
  end function rising_edge;

  function falling_edge (signal s : STD_ULOGIC) return BOOLEAN is
  begin
    return (s'event and (To_X01(s) = '0') and
            (To_X01(s'last_value) = '1'));
  end function falling_edge;

  -------------------------------------------------------------------
  -- object contains an unknown
  -------------------------------------------------------------------
  function Is_X (s : STD_ULOGIC_VECTOR) return BOOLEAN is
  begin
    for i in s'range loop
      case s(i) is
        when 'U' | 'X' | 'Z' | 'W' | '-' => return true;
        when others                      => null;
      end case;
    end loop;
    return false;
  end function Is_X;
  --------------------------------------------------------------------
  function Is_X (s : STD_ULOGIC) return BOOLEAN is
  begin
    case s is
      when 'U' | 'X' | 'Z' | 'W' | '-' => return true;
      when others                      => null;
    end case;
    return false;
  end function Is_X;

  -------------------------------------------------------------------
  -- string conversion and write operations
  -------------------------------------------------------------------

  function TO_OSTRING (value : STD_ULOGIC_VECTOR) return STRING is
    constant result_length : NATURAL := (value'length+2)/3;
    variable pad           : STD_ULOGIC_VECTOR(1 to result_length*3 - value'length);
    variable padded_value  : STD_ULOGIC_VECTOR(1 to result_length*3);
    variable result        : STRING(1 to result_length);
    variable tri           : STD_ULOGIC_VECTOR(1 to 3);
  begin
    if value (value'left) = 'Z' then
      pad := (others => 'Z');
    else
      pad := (others => '0');
    end if;
    padded_value := pad & value;
    for i in 1 to result_length loop
      tri := To_X01Z(padded_value(3*i-2 to 3*i));
      case tri is
        when o"0"   => result(i) := '0';
        when o"1"   => result(i) := '1';
        when o"2"   => result(i) := '2';
        when o"3"   => result(i) := '3';
        when o"4"   => result(i) := '4';
        when o"5"   => result(i) := '5';
        when o"6"   => result(i) := '6';
        when o"7"   => result(i) := '7';
        when "ZZZ"  => result(i) := 'Z';
        when others => result(i) := 'X';
      end case;
    end loop;
    return result;
  end function TO_OSTRING;

  function TO_HSTRING (value : STD_ULOGIC_VECTOR) return STRING is
    constant result_length : NATURAL := (value'length+3)/4;
    variable pad           : STD_ULOGIC_VECTOR(1 to result_length*4 - value'length);
    variable padded_value  : STD_ULOGIC_VECTOR(1 to result_length*4);
    variable result        : STRING(1 to result_length);
    variable quad          : STD_ULOGIC_VECTOR(1 to 4);
  begin
    if value (value'left) = 'Z' then
      pad := (others => 'Z');
    else
      pad := (others => '0');
    end if;
    padded_value := pad & value;
    for i in 1 to result_length loop
      quad := To_X01Z(padded_value(4*i-3 to 4*i));
      case quad is
        when x"0"   => result(i) := '0';
        when x"1"   => result(i) := '1';
        when x"2"   => result(i) := '2';
        when x"3"   => result(i) := '3';
        when x"4"   => result(i) := '4';
        when x"5"   => result(i) := '5';
        when x"6"   => result(i) := '6';
        when x"7"   => result(i) := '7';
        when x"8"   => result(i) := '8';
        when x"9"   => result(i) := '9';
        when x"A"   => result(i) := 'A';
        when x"B"   => result(i) := 'B';
        when x"C"   => result(i) := 'C';
        when x"D"   => result(i) := 'D';
        when x"E"   => result(i) := 'E';
        when x"F"   => result(i) := 'F';
        when "ZZZZ" => result(i) := 'Z';
        when others => result(i) := 'X';
      end case;
    end loop;
    return result;
  end function TO_HSTRING;

  -- Type and constant definitions used to map STD_ULOGIC values
  -- into/from character values.
  type MVL9plus is ('U', 'X', '0', '1', 'Z', 'W', 'L', 'H', '-', error);
  type char_indexed_by_MVL9 is array (STD_ULOGIC) of CHARACTER;
  type MVL9_indexed_by_char is array (CHARACTER) of STD_ULOGIC;
  type MVL9plus_indexed_by_char is array (CHARACTER) of MVL9plus;
  constant MVL9_to_char : char_indexed_by_MVL9 := "UX01ZWLH-";
  constant char_to_MVL9 : MVL9_indexed_by_char :=
    ('U' => 'U', 'X' => 'X', '0' => '0', '1' => '1', 'Z' => 'Z',
     'W' => 'W', 'L' => 'L', 'H' => 'H', '-' => '-', others => 'U');
  constant char_to_MVL9plus : MVL9plus_indexed_by_char :=
    ('U' => 'U', 'X' => 'X', '0' => '0', '1' => '1', 'Z' => 'Z',
     'W' => 'W', 'L' => 'L', 'H' => 'H', '-' => '-', others => error);

  constant NBSP : CHARACTER := CHARACTER'val(160);  -- space character

  -- purpose: Skips white space
  procedure skip_whitespace (
    L : inout LINE) is
    variable c : CHARACTER;
    variable left : positive;
  begin
    while L /= null and L.all'length /= 0 loop
      left := L.all'left;
      c := L.all(left);
      if (c = ' ' or c = NBSP or c = HT) then
        read (L, c);
      else
        exit;
      end if;
    end loop;
  end procedure skip_whitespace;

  procedure READ (L    : inout LINE; VALUE : out STD_ULOGIC;
                  GOOD : out   BOOLEAN) is
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
  begin
    VALUE := 'U';                       -- initialize to a "U"
    skip_whitespace (L);
    read (L, c, readOk);
    if not readOk then
      GOOD := false;
    else
      if char_to_MVL9plus(c) = error then
        GOOD := false;
      else
        VALUE := char_to_MVL9(c);
        GOOD  := true;
      end if;
    end if;
  end procedure READ;

  procedure READ (L    : inout LINE; VALUE : out STD_ULOGIC_VECTOR;
                  GOOD : out   BOOLEAN) is
    variable c      : CHARACTER;
    variable mv     : STD_ULOGIC_VECTOR(0 to VALUE'length-1);
    variable readOk : BOOLEAN;
    variable i      : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    skip_whitespace (L);
    if VALUE'length > 0 then
      read (L, c, readOk);
      i := 0;
      GOOD := true;
      while i < VALUE'length loop
        if not readOk then     -- Bail out if there was a bad read
          GOOD := false;
          return;
        elsif c = '_' then
          if i = 0 then
            GOOD := false;                -- Begins with an "_"
            return;
          elsif lastu then
            GOOD := false;                -- "__" detected
            return;
          else
            lastu := true;
          end if;
        elsif (char_to_MVL9plus(c) = error) then
          GOOD := false;                  -- Illegal character
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i + 1;
          if i > mv'high then             -- reading done
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    else
      GOOD := true;                   -- read into a null array
    end if;
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC) is
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
  begin
    VALUE := 'U';                       -- initialize to a "U"
    skip_whitespace (L);
    read (L, c, readOk);
    if not readOk then
      report "STD_LOGIC_1164.READ(STD_ULOGIC) "
        & "End of string encountered"
        severity error;
      return;
    elsif char_to_MVL9plus(c) = error then
      report
        "STD_LOGIC_1164.READ(STD_ULOGIC) Error: Character '" &
        c & "' read, expected STD_ULOGIC literal."
        severity error;
    else
      VALUE := char_to_MVL9(c);
    end if;
  end procedure READ;

  procedure READ (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR) is
    variable c      : CHARACTER;
    variable readOk : BOOLEAN;
    variable mv     : STD_ULOGIC_VECTOR(0 to VALUE'length-1);
    variable i      : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    skip_whitespace (L);
    if VALUE'length > 0 then            -- non Null input string
      read (L, c, readOk);
      i := 0;
      while i < VALUE'length loop
        if readOk = false then              -- Bail out if there was a bad read
          report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
            & "End of string encountered"
            severity error;
          return;
        elsif c = '_' then
          if i = 0 then
            report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        elsif char_to_MVL9plus(c) = error then
          report
            "STD_LOGIC_1164.READ(STD_ULOGIC_VECTOR) Error: Character '" &
            c & "' read, expected STD_ULOGIC literal."
            severity error;
          return;
        else
          mv(i) := char_to_MVL9(c);
          i := i + 1;
          if i > mv'high then
            VALUE := mv;
            return;
          end if;
          lastu := false;
        end if;
        read(L, c, readOk);
      end loop;
    end if;
  end procedure READ;

  procedure WRITE (L         : inout LINE; VALUE : in STD_ULOGIC;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write(L, MVL9_to_char(VALUE), JUSTIFIED, FIELD);
  end procedure WRITE;

  procedure WRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
    variable s : STRING(1 to VALUE'length);
    alias m    : STD_ULOGIC_VECTOR(1 to VALUE'length) is VALUE;
  begin
    for i in 1 to VALUE'length loop
      s(i) := MVL9_to_char(m(i));
    end loop;
    write(L, s, JUSTIFIED, FIELD);
  end procedure WRITE;

  procedure Char2TriBits (C           : in  CHARACTER;
                          RESULT      : out STD_ULOGIC_VECTOR(2 downto 0);
                          GOOD        : out BOOLEAN;
                          ISSUE_ERROR : in  BOOLEAN) is
  begin
    case C is
      when '0' => RESULT := o"0"; GOOD := true;
      when '1' => RESULT := o"1"; GOOD := true;
      when '2' => RESULT := o"2"; GOOD := true;
      when '3' => RESULT := o"3"; GOOD := true;
      when '4' => RESULT := o"4"; GOOD := true;
      when '5' => RESULT := o"5"; GOOD := true;
      when '6' => RESULT := o"6"; GOOD := true;
      when '7' => RESULT := o"7"; GOOD := true;
      when 'Z' => RESULT := "ZZZ"; GOOD := true;
      when 'X' => RESULT := "XXX"; GOOD := true;
      when others =>
        assert not ISSUE_ERROR
          report
          "STD_LOGIC_1164.OREAD Error: Read a '" & C &
          "', expected an Octal character (0-7)."
          severity error;
        GOOD := false;
    end case;
  end procedure Char2TriBits;

  procedure OREAD (L    : inout LINE; VALUE : out STD_ULOGIC_VECTOR;
                   GOOD : out   BOOLEAN) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER := (VALUE'length+2)/3;
    constant pad : INTEGER := ne*3 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*3 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    skip_whitespace (L);
    if VALUE'length > 0 then
      read (L, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          GOOD := false;
          return;
        elsif c = '_' then
          if i = 0 then
            GOOD := false;                -- Begins with an "_"
            return;
          elsif lastu then
            GOOD := false;                -- "__" detected
            return;
          else
            lastu := true;
          end if;
        else
          Char2TriBits(c, sv(3*i to 3*i+2), ok, false);
          if not ok then
            GOOD := false;
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or (sv (0 to pad-1)) = '1' then
        GOOD := false;                           -- vector was truncated.
      else
        GOOD  := true;
        VALUE := sv (pad to sv'high);
      end if;
    else
      GOOD := true;                  -- read into a null array
    end if;
  end procedure OREAD;

  procedure OREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR) is
    variable c   : CHARACTER;
    variable ok  : BOOLEAN;
    constant ne  : INTEGER := (VALUE'length+2)/3;
    constant pad : INTEGER := ne*3 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*3 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    skip_whitespace (L);
    if VALUE'length > 0 then
      read (L, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          report "STD_LOGIC_1164.OREAD "
            & "End of string encountered"
            severity error;
          return;
        elsif c = '_' then
          if i = 0 then
            report "STD_LOGIC_1164.OREAD "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report "STD_LOGIC_1164.OREAD "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        else
          Char2TriBits(c, sv(3*i to 3*i+2), ok, true);
          if not ok then
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or (sv (0 to pad-1)) = '1' then
        report "STD_LOGIC_1164.OREAD Vector truncated"
          severity error;
      else
        VALUE := sv (pad to sv'high);
      end if;
    end if;
  end procedure OREAD;

  procedure Char2QuadBits (C           :     CHARACTER;
                           RESULT      : out STD_ULOGIC_VECTOR(3 downto 0);
                           GOOD        : out BOOLEAN;
                           ISSUE_ERROR : in  BOOLEAN) is
  begin
    case C is
      when '0'       => RESULT := x"0"; GOOD := true;
      when '1'       => RESULT := x"1"; GOOD := true;
      when '2'       => RESULT := x"2"; GOOD := true;
      when '3'       => RESULT := x"3"; GOOD := true;
      when '4'       => RESULT := x"4"; GOOD := true;
      when '5'       => RESULT := x"5"; GOOD := true;
      when '6'       => RESULT := x"6"; GOOD := true;
      when '7'       => RESULT := x"7"; GOOD := true;
      when '8'       => RESULT := x"8"; GOOD := true;
      when '9'       => RESULT := x"9"; GOOD := true;
      when 'A' | 'a' => RESULT := x"A"; GOOD := true;
      when 'B' | 'b' => RESULT := x"B"; GOOD := true;
      when 'C' | 'c' => RESULT := x"C"; GOOD := true;
      when 'D' | 'd' => RESULT := x"D"; GOOD := true;
      when 'E' | 'e' => RESULT := x"E"; GOOD := true;
      when 'F' | 'f' => RESULT := x"F"; GOOD := true;
      when 'Z'       => RESULT := "ZZZZ"; GOOD := true;
      when 'X'       => RESULT := "XXXX"; GOOD := true;
      when others =>
        assert not ISSUE_ERROR
          report
          "STD_LOGIC_1164.HREAD Error: Read a '" & C &
          "', expected a Hex character (0-F)."
          severity error;
        GOOD := false;
    end case;
  end procedure Char2QuadBits;

  procedure HREAD (L    : inout LINE; VALUE : out STD_ULOGIC_VECTOR;
                   GOOD : out   BOOLEAN) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER := (VALUE'length+3)/4;
    constant pad : INTEGER := ne*4 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*4 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    skip_whitespace (L);
    if VALUE'length > 0 then
      read (L, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          GOOD := false;
          return;
        elsif c = '_' then
          if i = 0 then
            GOOD := false;                -- Begins with an "_"
            return;
          elsif lastu then
            GOOD := false;                -- "__" detected
            return;
          else
            lastu := true;
          end if;
        else
          Char2QuadBits(c, sv(4*i to 4*i+3), ok, false);
          if not ok then
            GOOD := false;
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or (sv (0 to pad-1)) = '1' then
        GOOD := false;                           -- vector was truncated.
      else
        GOOD  := true;
        VALUE := sv (pad to sv'high);
      end if;
    else
      GOOD := true;                     -- Null input string, skips whitespace
    end if;
  end procedure HREAD;

  procedure HREAD (L : inout LINE; VALUE : out STD_ULOGIC_VECTOR) is
    variable ok  : BOOLEAN;
    variable c   : CHARACTER;
    constant ne  : INTEGER := (VALUE'length+3)/4;
    constant pad : INTEGER := ne*4 - VALUE'length;
    variable sv  : STD_ULOGIC_VECTOR(0 to ne*4 - 1);
    variable i   : INTEGER;
    variable lastu  : BOOLEAN := false;       -- last character was an "_"
  begin
    VALUE := (VALUE'range => 'U'); -- initialize to a "U"
    skip_whitespace (L);
    if VALUE'length > 0 then           -- non Null input string
      read (L, c, ok);
      i := 0;
      while i < ne loop
        -- Bail out if there was a bad read
        if not ok then
          report "STD_LOGIC_1164.HREAD "
            & "End of string encountered"
            severity error;
          return;
        end if;
        if c = '_' then
          if i = 0 then
            report "STD_LOGIC_1164.HREAD "
              & "String begins with an ""_""" severity error;
            return;
          elsif lastu then
            report "STD_LOGIC_1164.HREAD "
              & "Two underscores detected in input string ""__"""
              severity error;
            return;
          else
            lastu := true;
          end if;
        else
          Char2QuadBits(c, sv(4*i to 4*i+3), ok, true);
          if not ok then
            return;
          end if;
          i := i + 1;
          lastu := false;
        end if;
        if i < ne then
          read(L, c, ok);
        end if;
      end loop;
      if or (sv (0 to pad-1)) = '1' then
        report "STD_LOGIC_1164.HREAD Vector truncated"
          severity error;
      else
        VALUE := sv (pad to sv'high);
      end if;
    end if;
  end procedure HREAD;

  procedure OWRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write (L, TO_OSTRING(VALUE), JUSTIFIED, FIELD);
  end procedure OWRITE;

  procedure HWRITE (L         : inout LINE; VALUE : in STD_ULOGIC_VECTOR;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0) is
  begin
    write (L, TO_HSTRING (VALUE), JUSTIFIED, FIELD);
  end procedure HWRITE;

end package body std_logic_1164;
