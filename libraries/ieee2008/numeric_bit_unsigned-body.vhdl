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
--   Title     :  Standard VHDL Synthesis Packages
--             :  (NUMERIC_BIT_UNSIGNED package body)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC, and IEEE P1076 Working Group
--             :
--   Purpose   :  This package defines numeric types and arithmetic functions
--             :  for use with synthesis tools. Values of type BIT_VECTOR
--             :  are interpreted as unsigned numbers in vector form.
--             :  The leftmost bit is treated as the most significant bit.
--             :  This package contains overloaded arithmetic operators on
--             :  the BIT_VECTOR type. The package also contains
--             :  useful type conversions functions, clock detection
--             :  functions, and other utility functions.
--             :
--             :  If any argument to a function is a null array, a null array
--             :  is returned (exceptions, if any, are noted individually).
--
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

library ieee;
use ieee.numeric_bit.all;

package body NUMERIC_BIT_UNSIGNED is

  -- Id: A.3
  function "+" (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) + UNSIGNED(R));
  end function "+";

  -- Id: A.3R
  function "+"(L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) + R);
  end function "+";

  -- Id: A.3L
  function "+"(L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L + UNSIGNED(R));
  end function "+";

  -- Id: A.5
  function "+" (L : BIT_VECTOR; R : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) + R);
  end function "+";

  -- Id: A.6
  function "+" (L : NATURAL; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L + UNSIGNED(R));
  end function "+";

  --============================================================================

  -- Id: A.9
  function "-" (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) - UNSIGNED(R));
  end function "-";

  -- Id: A.9R
  function "-"(L : BIT_VECTOR; R : BIT) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) - R);
  end function "-";

  -- Id: A.9L
  function "-"(L : BIT; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L - UNSIGNED(R));
  end function "-";

  -- Id: A.11
  function "-" (L : BIT_VECTOR; R : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) - R);
  end function "-";

  -- Id: A.12
  function "-" (L : NATURAL; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L - UNSIGNED(R));
  end function "-";

  --============================================================================

  -- Id: A.15
  function "*" (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) * UNSIGNED(R));
  end function "*";

  -- Id: A.17
  function "*" (L : BIT_VECTOR; R : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) * R);
  end function "*";

  -- Id: A.18
  function "*" (L : NATURAL; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L * UNSIGNED(R));
  end function "*";

  --============================================================================

  -- Id: A.21
  function "/" (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) / UNSIGNED(R));
  end function "/";

  -- Id: A.23
  function "/" (L : BIT_VECTOR; R : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) / R);
  end function "/";

  -- Id: A.24
  function "/" (L : NATURAL; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L / UNSIGNED(R));
  end function "/";

  --============================================================================

  -- Id: A.27
  function "rem" (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) rem UNSIGNED(R));
  end function "rem";

  -- Id: A.29
  function "rem" (L : BIT_VECTOR; R : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) rem R);
  end function "rem";

  -- Id: A.30
  function "rem" (L : NATURAL; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L rem UNSIGNED(R));
  end function "rem";

  --============================================================================

  -- Id: A.33
  function "mod" (L, R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) mod UNSIGNED(R));
  end function "mod";

  -- Id: A.35
  function "mod" (L : BIT_VECTOR; R : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(L) mod R);
  end function "mod";

  -- Id: A.36
  function "mod" (L : NATURAL; R : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (L mod UNSIGNED(R));
  end function "mod";

  --============================================================================
  -- Id: A.39
  function find_leftmost (ARG: BIT_VECTOR; Y: BIT) return INTEGER is
  begin
    return find_leftmost(UNSIGNED(ARG), Y);
  end function find_leftmost;

  -- Id: A.41
  function find_rightmost (ARG: BIT_VECTOR; Y: BIT) return INTEGER is
  begin
    return find_rightmost(UNSIGNED(ARG), Y);
  end function find_rightmost;

  --============================================================================
  -- Id: C.1
  function ">" (L, R : BIT_VECTOR) return BOOLEAN is
  begin
    return UNSIGNED(L) > UNSIGNED(R);
  end function ">";

  -- Id: C.3
  function ">" (L : NATURAL; R : BIT_VECTOR) return BOOLEAN is
  begin
    return L > UNSIGNED(R);
  end function ">";

  -- Id: C.5
  function ">" (L : BIT_VECTOR; R : NATURAL) return BOOLEAN is
  begin
    return UNSIGNED(L) > R;
  end function ">";

  --============================================================================
  -- Id: C.7
  function "<" (L, R : BIT_VECTOR) return BOOLEAN is
  begin
    return UNSIGNED(L) < UNSIGNED(R);
  end function "<";

  -- Id: C.9
  function "<" (L : NATURAL; R : BIT_VECTOR) return BOOLEAN is
  begin
    return L < UNSIGNED(R);
  end function "<";

  -- Id: C.11
  function "<" (L : BIT_VECTOR; R : NATURAL) return BOOLEAN is
  begin
    return UNSIGNED(L) < R;
  end function "<";

  --============================================================================
  -- Id: C.13
  function "<=" (L, R : BIT_VECTOR) return BOOLEAN is
  begin
    return UNSIGNED(L) <= UNSIGNED(R);
  end function "<=";

  -- Id: C.15
  function "<=" (L : NATURAL; R : BIT_VECTOR) return BOOLEAN is
  begin
    return L <= UNSIGNED(R);
  end function "<=";

  -- Id: C.17
  function "<=" (L : BIT_VECTOR; R : NATURAL) return BOOLEAN is
  begin
    return UNSIGNED(L) <= R;
  end function "<=";

  --============================================================================
  -- Id: C.19
  function ">=" (L, R : BIT_VECTOR) return BOOLEAN is
  begin
    return UNSIGNED(L) >= UNSIGNED(R);
  end function ">=";

  -- Id: C.21
  function ">=" (L : NATURAL; R : BIT_VECTOR) return BOOLEAN is
  begin
    return L >= UNSIGNED(R);
  end function ">=";

  -- Id: C.23
  function ">=" (L : BIT_VECTOR; R : NATURAL) return BOOLEAN is
  begin
    return UNSIGNED(L) >= R;
  end function ">=";

  --============================================================================
  -- Id: C.25
  function "=" (L, R : BIT_VECTOR) return BOOLEAN is
  begin
    return UNSIGNED(L) = UNSIGNED(R);
  end function "=";

  -- Id: C.27
  function "=" (L : NATURAL; R : BIT_VECTOR) return BOOLEAN is
  begin
    return L = UNSIGNED(R);
  end function "=";

  -- Id: C.29
  function "=" (L : BIT_VECTOR; R : NATURAL) return BOOLEAN is
  begin
    return UNSIGNED(L) = R;
  end function "=";

  --============================================================================
  -- Id: C.31
  function "/=" (L, R : BIT_VECTOR) return BOOLEAN is
  begin
    return UNSIGNED(L) /= UNSIGNED(R);
  end function "/=";

  -- Id: C.33
  function "/=" (L : NATURAL; R : BIT_VECTOR) return BOOLEAN is
  begin
    return L /= UNSIGNED(R);
  end function "/=";

  -- Id: C.35
  function "/=" (L : BIT_VECTOR; R : NATURAL) return BOOLEAN is
  begin
    return UNSIGNED(L) /= R;
  end function "/=";

  --============================================================================
  -- Id: C.37
  function MINIMUM (L, R: BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (MINIMUM(UNSIGNED(L), UNSIGNED(R)));
  end function MINIMUM;

  -- Id: C.39
  function MINIMUM (L: NATURAL; R: BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (MINIMUM(L, UNSIGNED(R)));
  end function MINIMUM;

  -- Id: C.41
  function MINIMUM (L: BIT_VECTOR; R: NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (MINIMUM(UNSIGNED(L), R));
  end function MINIMUM;

  --============================================================================
  -- Id: C.43
  function MAXIMUM (L, R: BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (MAXIMUM(UNSIGNED(L), UNSIGNED(R)));
  end function MAXIMUM;

  -- Id: C.45
  function MAXIMUM (L: NATURAL; R: BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (MAXIMUM(L, UNSIGNED(R)));
  end function MAXIMUM;

  -- Id: C.47
  function MAXIMUM (L: BIT_VECTOR; R: NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (MAXIMUM(UNSIGNED(L), R));
  end function MAXIMUM;

  --============================================================================

  -- Id: C.49
  function "?>" (L, R: BIT_VECTOR) return BIT is
  begin
    return UNSIGNED(L) ?> UNSIGNED(R);
  end function "?>";

  -- Id: C.51
  function "?>" (L: NATURAL; R: BIT_VECTOR) return BIT is
  begin
    return L ?> UNSIGNED(R);
  end function "?>";

  -- Id: C.53
  function "?>" (L: BIT_VECTOR; R: NATURAL) return BIT is
  begin
    return UNSIGNED(L) ?> R;
  end function "?>";

  --============================================================================

  -- Id: C.55
  function "?<" (L, R: BIT_VECTOR) return BIT is
  begin
    return UNSIGNED(L) ?< UNSIGNED(R);
  end function "?<";

  -- Id: C.57
  function "?<" (L: NATURAL; R: BIT_VECTOR) return BIT is
  begin
    return L ?< UNSIGNED(R);
  end function "?<";

  -- Id: C.59
  function "?<" (L: BIT_VECTOR; R: NATURAL) return BIT is
  begin
    return UNSIGNED(L) ?< R;
  end function "?<";

  --============================================================================

  -- Id: C.61
  function "?<=" (L, R: BIT_VECTOR) return BIT is
  begin
    return UNSIGNED(L) ?<= UNSIGNED(R);
  end function "?<=";

  -- Id: C.63
  function "?<=" (L: NATURAL; R: BIT_VECTOR) return BIT is
  begin
    return L ?<= UNSIGNED(R);
  end function "?<=";

  -- Id: C.65
  function "?<=" (L: BIT_VECTOR; R: NATURAL) return BIT is
  begin
    return UNSIGNED(L) ?<= R;
  end function "?<=";

  --============================================================================

  -- Id: C.67
  function "?>=" (L, R: BIT_VECTOR) return BIT is
  begin
    return UNSIGNED(L) ?>= UNSIGNED(R);
  end function "?>=";

  -- Id: C.69
  function "?>=" (L: NATURAL; R: BIT_VECTOR) return BIT is
  begin
    return L ?>= UNSIGNED(R);
  end function "?>=";

  -- Id: C.71
  function "?>=" (L: BIT_VECTOR; R: NATURAL) return BIT is
  begin
    return UNSIGNED(L) ?>= R;
  end function "?>=";

  --============================================================================

  -- Id: C.73
  function "?=" (L, R: BIT_VECTOR) return BIT is
  begin
    return UNSIGNED(L) ?= UNSIGNED(R);
  end function "?=";

  -- Id: C.75
  function "?=" (L: NATURAL; R: BIT_VECTOR) return BIT is
  begin
    return L ?= UNSIGNED(R);
  end function "?=";

  -- Id: C.77
  function "?=" (L: BIT_VECTOR; R: NATURAL) return BIT is
  begin
    return UNSIGNED(L) ?= R;
  end function "?=";

  --============================================================================

  -- Id: C.79
  function "?/=" (L, R: BIT_VECTOR) return BIT is
  begin
    return UNSIGNED(L) ?/= UNSIGNED(R);
  end function "?/=";

  -- Id: C.81
  function "?/=" (L: NATURAL; R: BIT_VECTOR) return BIT is
  begin
    return L ?/= UNSIGNED(R);
  end function "?/=";

  -- Id: C.83
  function "?/=" (L: BIT_VECTOR; R: NATURAL) return BIT is
  begin
    return UNSIGNED(L) ?/= R;
  end function "?/=";

  --============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (shift_left (ARG   => UNSIGNED(ARG),
                                   COUNT => COUNT));
  end function SHIFT_LEFT;

  -- Id: S.2
  function SHIFT_RIGHT (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (shift_right (ARG   => UNSIGNED(ARG),
                                    COUNT => COUNT));
  end function SHIFT_RIGHT;

  --============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (rotate_left (ARG   => UNSIGNED(ARG),
                                    COUNT => COUNT));
  end function ROTATE_LEFT;

  -- Id: S.6
  function ROTATE_RIGHT (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (rotate_right (ARG   => UNSIGNED(ARG),
                                     COUNT => COUNT));
  end function ROTATE_RIGHT;

  --============================================================================

  -- Id: S.9
  function "sll" (ARG: BIT_VECTOR; COUNT: INTEGER) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(ARG) sll COUNT);
  end function "sll";

  -- Id: S.11
  function "srl" (ARG: BIT_VECTOR; COUNT: INTEGER) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(ARG) srl COUNT);
  end function "srl";

  -- Id: S.13
  function "rol" (ARG: BIT_VECTOR; COUNT: INTEGER) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(ARG) rol COUNT);
  end function "rol";

  -- Id: S.15
  function "ror" (ARG: BIT_VECTOR; COUNT: INTEGER) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(ARG) ror COUNT);
  end function "ror";

  -- Id: S.17
  function "sla" (ARG: BIT_VECTOR; COUNT: INTEGER) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(ARG) sla COUNT);
  end function "sla";

  -- Id: S.19
  function "sra" (ARG: BIT_VECTOR; COUNT: INTEGER) return BIT_VECTOR is
  begin
    return BIT_VECTOR (UNSIGNED(ARG) sra COUNT);
  end function "sra";

  --============================================================================

  -- Id: R.2
  function RESIZE (ARG : BIT_VECTOR; NEW_SIZE : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (
      resize (arg      => UNSIGNED(ARG),
              NEW_SIZE => NEW_SIZE));
  end function RESIZE;

  function RESIZE (ARG, SIZE_RES : BIT_VECTOR) return BIT_VECTOR is
  begin
    return BIT_VECTOR (
      RESIZE (ARG      => UNSIGNED(ARG),
              NEW_SIZE => SIZE_RES'length));
  end function RESIZE;

  --============================================================================

  -- Id: D.1
  function TO_INTEGER (ARG : BIT_VECTOR) return NATURAL is
  begin
    return TO_INTEGER (UNSIGNED(ARG));
  end function TO_INTEGER;

  -- Id: D.3
  function To_BitVector (ARG, SIZE : NATURAL) return BIT_VECTOR is
  begin
    return BIT_VECTOR (TO_UNSIGNED(ARG, SIZE));
  end function To_BitVector;

  function To_BitVector (ARG : NATURAL; SIZE_RES : BIT_VECTOR)
    return BIT_VECTOR is
  begin
    return BIT_VECTOR (TO_UNSIGNED(ARG, SIZE_RES'length));
  end function To_BitVector;

end package body NUMERIC_BIT_UNSIGNED;
