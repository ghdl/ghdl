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
--             :  (STD_LOGIC_1164 package declaration)
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
-- $Revision: 1228 $
-- $Date: 2008-04-30 10:04:53 +0930 (Wed, 30 Apr 2008) $
-- --------------------------------------------------------------------

package std_logic_1164 is

  -------------------------------------------------------------------
  -- logic state system  (unresolved)
  -------------------------------------------------------------------
  type STD_ULOGIC is ( 'U',             -- Uninitialized
                       'X',             -- Forcing  Unknown
                       '0',             -- Forcing  0
                       '1',             -- Forcing  1
                       'Z',             -- High Impedance
                       'W',             -- Weak     Unknown
                       'L',             -- Weak     0
                       'H',             -- Weak     1
                       '-'              -- Don't care
                       );
  -------------------------------------------------------------------
  -- unconstrained array of std_ulogic for use with the resolution function
  -- and for use in declaring signal arrays of unresolved elements
  -------------------------------------------------------------------
  type STD_ULOGIC_VECTOR is array (NATURAL range <>) of STD_ULOGIC;

  -------------------------------------------------------------------
  -- resolution function
  -------------------------------------------------------------------
  function resolved (s : STD_ULOGIC_VECTOR) return STD_ULOGIC;


  -------------------------------------------------------------------
  -- logic state system  (resolved)
  -------------------------------------------------------------------
  subtype STD_LOGIC is resolved STD_ULOGIC;

  -------------------------------------------------------------------
  -- unconstrained array of resolved std_ulogic for use in declaring
  -- signal arrays of resolved elements
  -------------------------------------------------------------------
  type STD_LOGIC_VECTOR is array (NATURAL range <>) of STD_LOGIC;

  -------------------------------------------------------------------
  -- common subtypes
  -------------------------------------------------------------------
  subtype X01 is resolved STD_ULOGIC range 'X' to '1';    -- ('X','0','1')
  subtype X01Z is resolved STD_ULOGIC range 'X' to 'Z';   -- ('X','0','1','Z')
  subtype UX01 is resolved STD_ULOGIC range 'U' to '1';   -- ('U','X','0','1')
  subtype UX01Z is resolved STD_ULOGIC range 'U' to 'Z';  -- ('U','X','0','1','Z')

  -------------------------------------------------------------------
  -- overloaded logical operators
  -------------------------------------------------------------------

  function "and"  (l : STD_ULOGIC; r : STD_ULOGIC) return UX01;
  function "nand" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01;
  function "or"   (l : STD_ULOGIC; r : STD_ULOGIC) return UX01;
  function "nor"  (l : STD_ULOGIC; r : STD_ULOGIC) return UX01;
  function "xor"  (l : STD_ULOGIC; r : STD_ULOGIC) return UX01;
  function "xnor" (l : STD_ULOGIC; r : STD_ULOGIC) return UX01; --!V87
  function "not"  (l : STD_ULOGIC) return UX01;

  -------------------------------------------------------------------
  -- vectorized overloaded logical operators
  -------------------------------------------------------------------
  function "and"  (l, r : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR;
  function "and"  (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  function "nand" (l, r : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR;
  function "nand" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  function "or"   (l, r : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR;
  function "or"   (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  function "nor"  (l, r : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR;
  function "nor"  (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  function "xor"  (l, r : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR;
  function "xor"  (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  function "xnor" (l, r : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR; --!V87
  function "xnor" (l, r : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR; --!V87

  function "not"  (l    : STD_LOGIC_VECTOR)  return STD_LOGIC_VECTOR;
  function "not"  (l    : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  -------------------------------------------------------------------
  -- conversion functions
  -------------------------------------------------------------------
  function To_bit       (s : STD_ULOGIC; xmap : BIT        := '0') return BIT;
  function To_bitvector (s : STD_LOGIC_VECTOR; xmap : BIT := '0') return BIT_VECTOR;
  function To_bitvector (s : STD_ULOGIC_VECTOR; xmap : BIT := '0') return BIT_VECTOR;

  function To_StdULogic       (b : BIT) return STD_ULOGIC;
  function To_StdLogicVector  (b : BIT_VECTOR) return STD_LOGIC_VECTOR;
  function To_StdLogicVector  (s : STD_ULOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function To_StdULogicVector (b : BIT_VECTOR) return STD_ULOGIC_VECTOR;
  function To_StdULogicVector (s : STD_LOGIC_VECTOR) return STD_ULOGIC_VECTOR;

  -------------------------------------------------------------------
  -- strength strippers and type convertors
  -------------------------------------------------------------------

  function To_X01 (s : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function To_X01 (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function To_X01 (s : STD_ULOGIC) return X01;
  function To_X01 (b : BIT_VECTOR) return STD_LOGIC_VECTOR;
  function To_X01 (b : BIT_VECTOR) return STD_ULOGIC_VECTOR;
  function To_X01 (b : BIT) return X01;

  function To_X01Z (s : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function To_X01Z (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function To_X01Z (s : STD_ULOGIC) return X01Z;
  function To_X01Z (b : BIT_VECTOR) return STD_LOGIC_VECTOR;
  function To_X01Z (b : BIT_VECTOR) return STD_ULOGIC_VECTOR;
  function To_X01Z (b : BIT) return X01Z;

  function To_UX01 (s : STD_LOGIC_VECTOR) return STD_LOGIC_VECTOR;
  function To_UX01 (s : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  function To_UX01 (s : STD_ULOGIC) return UX01;
  function To_UX01 (b : BIT_VECTOR) return STD_LOGIC_VECTOR;
  function To_UX01 (b : BIT_VECTOR) return STD_ULOGIC_VECTOR;
  function To_UX01 (b : BIT) return UX01;

  -------------------------------------------------------------------
  -- edge detection
  -------------------------------------------------------------------
  function rising_edge  (signal s : STD_ULOGIC) return BOOLEAN;
  function falling_edge (signal s : STD_ULOGIC) return BOOLEAN;

  -------------------------------------------------------------------
  -- object contains an unknown
  -------------------------------------------------------------------
  function Is_X (s : STD_ULOGIC_VECTOR) return BOOLEAN;
  function Is_X (s : STD_LOGIC_VECTOR) return BOOLEAN;
  function Is_X (s : STD_ULOGIC) return BOOLEAN;

end std_logic_1164;
