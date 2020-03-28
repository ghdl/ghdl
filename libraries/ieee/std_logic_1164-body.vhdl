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
  end resolved;

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
  end "and";

  function "nand" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (not_table (and_table(l, r)));
  end "nand";

  function "or" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (or_table(l, r));
  end "or";

  function "nor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (not_table (or_table(l, r)));
  end "nor";

  function "xor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return (xor_table(l, r));
  end "xor";

  --START-!V87

  function "xnor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01 is
  begin
    return not_table(xor_table(l, r));
  end "xnor";

  --END-!V87

  function "not" (l : STD_ULOGIC) return UX01 is
  begin
    return (not_table(l));
  end "not";

  -------------------------------------------------------------------
  -- and
  -------------------------------------------------------------------
  function "and" (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
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
  end "and";

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
  end "and";

  -------------------------------------------------------------------
  -- nand
  -------------------------------------------------------------------
  function "nand" (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
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
  end "nand";

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
  end "nand";

  -------------------------------------------------------------------
  -- or
  -------------------------------------------------------------------
  function "or" (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
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
  end "or";

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
  end "or";

  -------------------------------------------------------------------
  -- nor
  -------------------------------------------------------------------
  function "nor" (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
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
  end "nor";

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
  end "nor";

  ---------------------------------------------------------------------
  -- xor
  -------------------------------------------------------------------
  function "xor" (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
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
  end "xor";

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
  end "xor";

  -------------------------------------------------------------------
  -- xnor
  -------------------------------------------------------------------
  --START-!V87
  function "xnor" (l, r : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    alias rv        : STD_LOGIC_VECTOR (1 to r'length) is r;
    variable result : STD_LOGIC_VECTOR (1 to l'length);
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
  end "xnor";

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
  end "xnor";
  --END-!V87

  -------------------------------------------------------------------
  -- not
  -------------------------------------------------------------------
  function "not" (l : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias lv        : STD_LOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_LOGIC_VECTOR (1 to l'length) := (others => 'X');
  begin
    for i in result'range loop
      result(i) := not_table(lv(i));
    end loop;
    return result;
  end "not";

  function "not" (l : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias lv        : STD_ULOGIC_VECTOR (1 to l'length) is l;
    variable result : STD_ULOGIC_VECTOR (1 to l'length) := (others => 'X');
  begin
    for i in result'range loop
      result(i) := not_table(lv(i));
    end loop;
    return result;
  end "not";

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
  end To_bit;
  --------------------------------------------------------------------
  function To_bitvector (s : STD_LOGIC_VECTOR; xmap : BIT := '0')
    return BIT_VECTOR
  is
    alias sv        : STD_LOGIC_VECTOR (s'length-1 downto 0) is s;
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
  end To_bitvector;

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
  end To_bitvector;

  --------------------------------------------------------------------
  function To_StdULogic (b : BIT) return STD_ULOGIC is
  begin
    case b is
      when '0' => return '0';
      when '1' => return '1';
    end case;
  end To_StdULogic;
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
  end To_StdLogicVector;
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
  end To_StdLogicVector;
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
  end To_StdULogicVector;
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
  end To_StdULogicVector;

  -------------------------------------------------------------------
  -- strength strippers and type convertors
  -------------------------------------------------------------------
  -------------------------------------------------------------------
  -- to_x01
  -------------------------------------------------------------------
  function To_X01 (s : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias sv        : STD_LOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_LOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_x01 (sv(i));
    end loop;
    return result;
  end To_X01;

  function To_X01 (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias sv        : STD_ULOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_ULOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_x01 (sv(i));
    end loop;
    return result;
  end To_X01;
  --------------------------------------------------------------------
  function To_X01 (s : STD_ULOGIC) return X01 is
  begin
    return (cvt_to_x01(s));
  end To_X01;
  --------------------------------------------------------------------
  function To_X01 (b : BIT_VECTOR) return STD_LOGIC_VECTOR is
    alias bv        : BIT_VECTOR (1 to b'length) is b;
    variable result : STD_LOGIC_VECTOR (1 to b'length);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end To_X01;

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
  end To_X01;

  --------------------------------------------------------------------
  function To_X01 (b : BIT) return X01 is
  begin
    case b is
      when '0' => return('0');
      when '1' => return('1');
    end case;
  end To_X01;
  --------------------------------------------------------------------
  -- to_x01z
  -------------------------------------------------------------------
  function To_X01Z (s : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias sv        : STD_LOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_LOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_x01z (sv(i));
    end loop;
    return result;
  end To_X01Z;

  function To_X01Z (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias sv        : STD_ULOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_ULOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_x01z (sv(i));
    end loop;
    return result;
  end To_X01Z;

  --------------------------------------------------------------------
  function To_X01Z (s : STD_ULOGIC) return X01Z is
  begin
    return (cvt_to_x01z(s));
  end To_X01Z;
  --------------------------------------------------------------------
  function To_X01Z (b : BIT_VECTOR) return STD_LOGIC_VECTOR is
    alias bv        : BIT_VECTOR (1 to b'length) is b;
    variable result : STD_LOGIC_VECTOR (1 to b'length);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end To_X01Z;

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
  end To_X01Z;

  --------------------------------------------------------------------
  function To_X01Z (b : BIT) return X01Z is
  begin
    case b is
      when '0' => return('0');
      when '1' => return('1');
    end case;
  end To_X01Z;
  --------------------------------------------------------------------
  -- to_ux01
  -------------------------------------------------------------------
  function To_UX01 (s : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR is
    alias sv        : STD_LOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_LOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_ux01 (sv(i));
    end loop;
    return result;
  end To_UX01;

  function To_UX01 (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR is
    alias sv        : STD_ULOGIC_VECTOR (1 to s'length) is s;
    variable result : STD_ULOGIC_VECTOR (1 to s'length);
  begin
    for i in result'range loop
      result(i) := cvt_to_ux01 (sv(i));
    end loop;
    return result;
  end To_UX01;

  --------------------------------------------------------------------
  function To_UX01 (s : STD_ULOGIC) return UX01 is
  begin
    return (cvt_to_ux01(s));
  end To_UX01;
  --------------------------------------------------------------------
  function To_UX01 (b : BIT_VECTOR) return STD_LOGIC_VECTOR is
    alias bv        : BIT_VECTOR (1 to b'length) is b;
    variable result : STD_LOGIC_VECTOR (1 to b'length);
  begin
    for i in result'range loop
      case bv(i) is
        when '0' => result(i) := '0';
        when '1' => result(i) := '1';
      end case;
    end loop;
    return result;
  end To_UX01;
  
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
  end To_UX01;

  --------------------------------------------------------------------
  function To_UX01 (b : BIT) return UX01 is
  begin
    case b is
      when '0' => return('0');
      when '1' => return('1');
    end case;
  end To_UX01;

  -------------------------------------------------------------------
  -- edge detection
  -------------------------------------------------------------------
  function rising_edge (signal s : STD_ULOGIC) return BOOLEAN is
  begin
    return (s'event and (To_X01(s) = '1') and
            (To_X01(s'last_value) = '0'));
  end rising_edge;

  function falling_edge (signal s : STD_ULOGIC) return BOOLEAN is
  begin
    return (s'event and (To_X01(s) = '0') and
            (To_X01(s'last_value) = '1'));
  end falling_edge;

  -------------------------------------------------------------------
  -- object contains an unknown
  -------------------------------------------------------------------
  function Is_X (s : STD_LOGIC_VECTOR) return BOOLEAN is
  begin
    for i in s'range loop
      case s(i) is
        when 'U' | 'X' | 'Z' | 'W' | '-' => return true;
        when others                      => null;
      end case;
    end loop;
    return false;
  end Is_X;

  function Is_X (s : STD_ULOGIC_VECTOR) return BOOLEAN is
  begin
    for i in s'range loop
      case s(i) is
        when 'U' | 'X' | 'Z' | 'W' | '-' => return true;
        when others                      => null;
      end case;
    end loop;
    return false;
  end Is_X;

  --------------------------------------------------------------------
  function Is_X (s : STD_ULOGIC) return BOOLEAN is
  begin
    case s is
      when 'U' | 'X' | 'Z' | 'W' | '-' => return true;
      when others                      => null;
    end case;
    return false;
  end Is_X;

end std_logic_1164;
