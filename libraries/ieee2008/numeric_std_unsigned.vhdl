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
--             :  (NUMERIC_STD_UNSIGNED package declaration)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC, and IEEE P1076 Working Group
--             :
--   Purpose   :  This package defines numeric types and arithmetic functions
--             :  for use with synthesis tools. Values of type STD_ULOGIC_VECTOR
--             :  are interpreted as unsigned numbers in vector form.
--             :  The leftmost bit is treated as the most significant bit.
--             :  This package contains overloaded arithmetic operators on
--             :  the STD_ULOGIC_VECTOR type. The package also contains
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

library IEEE;
use IEEE.STD_LOGIC_1164.all;
package NUMERIC_STD_UNSIGNED is
  constant CopyRightNotice : STRING :=
    "Copyright IEEE P1076 WG. Licensed Apache 2.0";

  -- Id: A.3
  function "+" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(MAXIMUM(L'LENGTH, R'LENGTH)-1 downto 0).
  -- Result: Adds two UNSIGNED vectors that may be of different lengths.

  -- Id: A.3R
  function "+"(L : STD_ULOGIC_VECTOR; R : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0)
  -- Result: Similar to A.3 where R is a one bit STD_ULOGIC_VECTOR

  -- Id: A.3L
  function "+"(L : STD_ULOGIC; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Similar to A.3 where L is a one bit UNSIGNED

  -- Id: A.5
  function "+" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0).
  -- Result: Adds an UNSIGNED vector, L, with a non-negative INTEGER, R.

  -- Id: A.6
  function "+" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0).
  -- Result: Adds a non-negative INTEGER, L, with an UNSIGNED vector, R.

  --============================================================================

  -- Id: A.9
  function "-" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: UNSIGNED(MAXIMUM(L'LENGTH, R'LENGTH)-1 downto 0).
  -- Result: Subtracts two UNSIGNED vectors that may be of different lengths.

  -- Id: A.9R
  function "-"(L : STD_ULOGIC_VECTOR; R : STD_ULOGIC) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0)
  -- Result: Similar to A.9 where R is a one bit UNSIGNED

  -- Id: A.9L
  function "-"(L : STD_ULOGIC; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Similar to A.9 where L is a one bit UNSIGNED

  -- Id: A.11
  function "-" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0).
  -- Result: Subtracts a non-negative INTEGER, R, from an UNSIGNED vector, L.

  -- Id: A.12
  function "-" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0).
  -- Result: Subtracts an UNSIGNED vector, R, from a non-negative INTEGER, L.

  --============================================================================

  -- Id: A.15
  function "*" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR((L'LENGTH+R'LENGTH-1) downto 0).
  -- Result: Performs the multiplication operation on two UNSIGNED vectors
  --         that may possibly be of different lengths.

  -- Id: A.17
  function "*" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR((L'LENGTH+L'LENGTH-1) downto 0).
  -- Result: Multiplies an UNSIGNED vector, L, with a non-negative
  --         INTEGER, R. R is converted to an UNSIGNED vector of
  --         SIZE L'LENGTH before multiplication.

  -- Id: A.18
  function "*" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR((R'LENGTH+R'LENGTH-1) downto 0).
  -- Result: Multiplies an UNSIGNED vector, R, with a non-negative
  --         INTEGER, L. L is converted to an UNSIGNED vector of
  --         SIZE R'LENGTH before multiplication.

  --============================================================================
  --
  -- NOTE: If second argument is zero for "/" operator, a severity level
  --       of ERROR is issued.

  -- Id: A.21
  function "/" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0)
  -- Result: Divides an UNSIGNED vector, L, by another UNSIGNED vector, R.

  -- Id: A.23
  function "/" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0)
  -- Result: Divides an UNSIGNED vector, L, by a non-negative INTEGER, R.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.24
  function "/" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Divides a non-negative INTEGER, L, by an UNSIGNED vector, R.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  --============================================================================
  --
  -- NOTE: If second argument is zero for "rem" operator, a severity level
  --       of ERROR is issued.

  -- Id: A.27
  function "rem" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where L and R are UNSIGNED vectors.

  -- Id: A.29
  function "rem" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where L is an UNSIGNED vector and R is a
  --         non-negative INTEGER.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.30
  function "rem" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where R is an UNSIGNED vector and L is a
  --         non-negative INTEGER.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  --============================================================================
  --
  -- NOTE: If second argument is zero for "mod" operator, a severity level
  --       of ERROR is issued.

  -- Id: A.33
  function "mod" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L and R are UNSIGNED vectors.

  -- Id: A.35
  function "mod" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(L'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L is an UNSIGNED vector and R
  --         is a non-negative INTEGER.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.36
  function "mod" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(R'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where R is an UNSIGNED vector and L
  --         is a non-negative INTEGER.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  --============================================================================
  -- Id: A.39
  function find_leftmost (ARG : STD_ULOGIC_VECTOR; Y : STD_ULOGIC) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Finds the leftmost occurrence of the value of Y in ARG.
  --         Returns the index of the occurrence if it exists, or -1 otherwise.

  -- Id: A.41
  function find_rightmost (ARG : STD_ULOGIC_VECTOR; Y : STD_ULOGIC) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Finds the leftmost occurrence of the value of Y in ARG.
  --         Returns the index of the occurrence if it exists, or -1 otherwise.

  --============================================================================
  -- Comparison Operators
  --============================================================================

  -- Id: C.1
  function ">" (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.3
  function ">" (L : NATURAL; R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L is a non-negative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.5
  function ">" (L : STD_ULOGIC_VECTOR; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L is an UNSIGNED vector and
  --         R is a non-negative INTEGER.

  --============================================================================

  -- Id: C.7
  function "<" (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.9
  function "<" (L : NATURAL; R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L is a non-negative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.11
  function "<" (L : STD_ULOGIC_VECTOR; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L is an UNSIGNED vector and
  --         R is a non-negative INTEGER.

  --============================================================================

  -- Id: C.13
  function "<=" (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.15
  function "<=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L is a non-negative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.17
  function "<=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L is an UNSIGNED vector and
  --         R is a non-negative INTEGER.

  --============================================================================

  -- Id: C.19
  function ">=" (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.21
  function ">=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L is a non-negative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.23
  function ">=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L is an UNSIGNED vector and
  --         R is a non-negative INTEGER.

  --============================================================================

  -- Id: C.25
  function "=" (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.27
  function "=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L is a non-negative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.29
  function "=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L is an UNSIGNED vector and
  --         R is a non-negative INTEGER.

  --============================================================================

  -- Id: C.31
  function "/=" (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.33
  function "/=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L is a non-negative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.35
  function "/=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L is an UNSIGNED vector and
  --         R is a non-negative INTEGER.

  --============================================================================

  -- Id: C.37
  function MINIMUM (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR
  -- Result: Returns the lesser of two UNSIGNED vectors that may be
  --         of different lengths.

  -- Id: C.39
  function MINIMUM (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR
  -- Result: Returns the lesser of a nonnegative INTEGER, L, and
  --         an UNSIGNED vector, R.

  -- Id: C.41
  function MINIMUM (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR
  -- Result: Returns the lesser of an UNSIGNED vector, L, and
  --         a nonnegative INTEGER, R.

  --============================================================================

  -- Id: C.43
  function MAXIMUM (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR
  -- Result: Returns the greater of two UNSIGNED vectors that may be
  --         of different lengths.

  -- Id: C.45
  function MAXIMUM (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR
  -- Result: Returns the greater of a nonnegative INTEGER, L, and
  --         an UNSIGNED vector, R.

  -- Id: C.47
  function MAXIMUM (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR
  -- Result: Returns the greater of an UNSIGNED vector, L, and
  --         a nonnegative INTEGER, R.

  --============================================================================
  -- Id: C.49
  function "?>" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.51
  function "?>" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L is a nonnegative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.53
  function "?>" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L is an UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  --============================================================================

  -- Id: C.55
  function "?<" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.57
  function "?<" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L is a nonnegative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.59
  function "?<" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L is an UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  --============================================================================

  -- Id: C.61
  function "?<=" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.63
  function "?<=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L is a nonnegative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.65
  function "?<=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L is an UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  --============================================================================

  -- Id: C.67
  function "?>=" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.69
  function "?>=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L is a nonnegative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.71
  function "?>=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L is an UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  --============================================================================

  -- Id: C.73
  function "?=" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.75
  function "?=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L is a nonnegative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.77
  function "?=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L is an UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  --============================================================================

  -- Id: C.79
  function "?/=" (L, R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L and R are UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.81
  function "?/=" (L : NATURAL; R : STD_ULOGIC_VECTOR) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L is a nonnegative INTEGER and
  --         R is an UNSIGNED vector.

  -- Id: C.83
  function "?/=" (L : STD_ULOGIC_VECTOR; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L is an UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  --============================================================================
  -- Shift and Rotate Functions
  --============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(ARG'LENGTH-1 downto 0)
  -- Result: Performs a shift-left on an UNSIGNED vector COUNT times.
  --         The vacated positions are filled with '0'.
  --         The COUNT leftmost elements are lost.

  -- Id: S.2
  function SHIFT_RIGHT (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR;
  -- Result subtype: UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a shift-right on an UNSIGNED vector COUNT times.
  --         The vacated positions are filled with '0'.
  --         The COUNT rightmost elements are lost.
  --============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(ARG'LENGTH-1 downto 0)
  -- Result: Performs a rotate-left of an UNSIGNED vector COUNT times.

  -- Id: S.6
  function ROTATE_RIGHT (ARG : STD_ULOGIC_VECTOR; COUNT : NATURAL)
    return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(ARG'LENGTH-1 downto 0)
  -- Result: Performs a rotate-right of an UNSIGNED vector COUNT times.


  --============================================================================

  ------------------------------------------------------------------------------
  -- Note: Function S.17 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.17
  function "sla" (ARG : STD_ULOGIC_VECTOR; COUNT : INTEGER) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.19 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.19
  function "sra" (ARG : STD_ULOGIC_VECTOR; COUNT : INTEGER) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_RIGHT(ARG, COUNT)


  --============================================================================
  --   RESIZE Functions
  --============================================================================

  -- Id: R.2
  function RESIZE (ARG : STD_ULOGIC_VECTOR; NEW_SIZE : NATURAL)
    return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(NEW_SIZE-1 downto 0)
  -- Result: Resizes the UNSIGNED vector ARG to the specified size.
  --         To create a larger vector, the new [leftmost] bit positions
  --         are filled with '0'. When truncating, the leftmost bits
  --         are dropped.

  function RESIZE (ARG, SIZE_RES : STD_ULOGIC_VECTOR) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR (SIZE_RES'length-1 downto 0)

  --============================================================================
  -- Conversion Functions
  --============================================================================

  -- Id: D.1
  function TO_INTEGER (ARG : STD_ULOGIC_VECTOR) return NATURAL;
  -- Result subtype: NATURAL. Value cannot be negative since parameter is an
  --             UNSIGNED vector.
  -- Result: Converts the UNSIGNED vector to an INTEGER.

  -- Id: D.3
  function To_StdLogicVector (ARG, SIZE : NATURAL) return STD_LOGIC_VECTOR;
  -- Result subtype: STD_LOGIC_VECTOR(SIZE-1 downto 0)
  -- Result: Converts a non-negative INTEGER to an UNSIGNED vector with
  --         the specified SIZE.

  function To_StdLogicVector (ARG : NATURAL; SIZE_RES : STD_ULOGIC_VECTOR)
    return STD_LOGIC_VECTOR;
  -- Result subtype: STD_LOGIC_VECTOR(SIZE_RES'length-1 downto 0)

  alias To_Std_Logic_Vector is
    To_StdLogicVector[NATURAL, NATURAL return STD_LOGIC_VECTOR];
  alias To_SLV is
    To_StdLogicVector[NATURAL, NATURAL return STD_LOGIC_VECTOR];
  alias To_Std_Logic_Vector is
    To_StdLogicVector[NATURAL, STD_ULOGIC_VECTOR return STD_LOGIC_VECTOR];
  alias To_SLV is
    To_StdLogicVector[NATURAL, STD_ULOGIC_VECTOR return STD_LOGIC_VECTOR];

  -- Id: D.5
  function To_StdULogicVector (ARG, SIZE : NATURAL) return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_ULOGIC_VECTOR(SIZE-1 downto 0)
  -- Result: Converts a non-negative INTEGER to an UNSIGNED vector with
  --         the specified SIZE.

  function To_StdULogicVector (ARG : NATURAL; SIZE_RES : STD_ULOGIC_VECTOR)
    return STD_ULOGIC_VECTOR;
  -- Result subtype: STD_LOGIC_VECTOR(SIZE_RES'length-1 downto 0)

  alias To_Std_ULogic_Vector is
    To_StdULogicVector[NATURAL, NATURAL return STD_ULOGIC_VECTOR];
  alias To_SULV is
    To_StdULogicVector[NATURAL, NATURAL return STD_ULOGIC_VECTOR];
  alias To_Std_ULogic_Vector is
    To_StdULogicVector[NATURAL, STD_ULOGIC_VECTOR return STD_ULOGIC_VECTOR];
  alias To_SULV is
    To_StdULogicVector[NATURAL, STD_ULOGIC_VECTOR return STD_ULOGIC_VECTOR];

end package NUMERIC_STD_UNSIGNED;
