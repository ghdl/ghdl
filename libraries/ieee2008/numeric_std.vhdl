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
--             :  (NUMERIC_STD package declaration)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  IEEE DASC Synthesis Working Group,
--             :  Accellera VHDL-TC, and IEEE P1076 Working Group
--             :
--   Purpose   :  This package defines numeric types and arithmetic functions
--             :  for use with synthesis tools. Two numeric types are defined:
--             :  -- > UNRESOLVED_UNSIGNED: represents an UNSIGNED number
--             :       in vector form
--             :  -- > UNRESOLVED_SIGNED: represents a SIGNED number
--             :       in vector form
--             :  The base element type is type STD_ULOGIC.
--             :  Aliases U_UNSIGNED and U_SIGNED are defined for the types
--             :  UNRESOLVED_UNSIGNED and UNRESOLVED_SIGNED, respectively.
--             :  Two numeric subtypes are defined:
--             :  -- > UNSIGNED: represents UNSIGNED number in vector form
--             :  -- > SIGNED: represents a SIGNED number in vector form
--             :  The element subtypes are the same subtype as STD_LOGIC.
--             :  The leftmost bit is treated as the most significant bit.
--             :  Signed vectors are represented in two's complement form.
--             :  This package contains overloaded arithmetic operators on
--             :  the SIGNED and UNSIGNED types. The package also contains
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

use STD.TEXTIO.all;
library IEEE;
use IEEE.STD_LOGIC_1164.all;

package NUMERIC_STD is
  constant CopyRightNotice : STRING
    := "Copyright © 2008 IEEE. All rights reserved.";


  --============================================================================
  -- Numeric Array Type Definitions
  --============================================================================

  type UNRESOLVED_UNSIGNED is array (NATURAL range <>) of STD_ULOGIC;
  type UNRESOLVED_SIGNED is array (NATURAL range <>) of STD_ULOGIC;

  subtype U_UNSIGNED is UNRESOLVED_UNSIGNED;
  subtype U_SIGNED is UNRESOLVED_SIGNED;

  subtype UNSIGNED is (resolved) UNRESOLVED_UNSIGNED;
  subtype SIGNED is (resolved) UNRESOLVED_SIGNED;

  --============================================================================
  -- Arithmetic Operators:
  --===========================================================================

  -- Id: A.1
  function "abs" (ARG : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Returns the absolute value of an UNRESOLVED_SIGNED vector ARG.

  -- Id: A.2
  function "-" (ARG : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Returns the value of the unary minus operation on a
  --         UNRESOLVED_SIGNED vector ARG.

  --============================================================================

  -- Id: A.3
  function "+" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(MAXIMUM(L'LENGTH, R'LENGTH)-1 downto 0)
  -- Result: Adds two UNRESOLVED_UNSIGNED vectors that may be of different lengths.

  -- Id: A.3R
  function "+"(L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Similar to A.3 where R is a one bit UNRESOLVED_UNSIGNED

  -- Id: A.3L
  function "+"(L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Similar to A.3 where L is a one bit UNRESOLVED_UNSIGNED

  -- Id: A.4
  function "+" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(MAXIMUM(L'LENGTH, R'LENGTH)-1 downto 0)
  -- Result: Adds two UNRESOLVED_SIGNED vectors that may be of different lengths.

  -- Id: A.4R
  function "+"(L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Similar to A.4 where R is bit 0 of a non-negative.

  -- Id: A.4L
  function "+"(L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Similar to A.4 where L is bit 0 of a non-negative.

  -- Id: A.5
  function "+" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Adds an UNRESOLVED_UNSIGNED vector, L, with a nonnegative INTEGER, R.

  -- Id: A.6
  function "+" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Adds a nonnegative INTEGER, L, with an UNRESOLVED_UNSIGNED vector, R.

  -- Id: A.7
  function "+" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Adds an INTEGER, L(may be positive or negative), to an UNRESOLVED_SIGNED
  --         vector, R.

  -- Id: A.8
  function "+" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Adds an UNRESOLVED_SIGNED vector, L, to an INTEGER, R.

  --============================================================================

  -- Id: A.9
  function "-" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(MAXIMUM(L'LENGTH, R'LENGTH)-1 downto 0)
  -- Result: Subtracts two UNRESOLVED_UNSIGNED vectors that may be of different lengths.

  -- Id: A.9R
  function "-"(L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Similar to A.9 where R is a one bit UNRESOLVED_UNSIGNED

  -- Id: A.9L
  function "-"(L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Similar to A.9 where L is a one bit UNRESOLVED_UNSIGNED

  -- Id: A.10
  function "-" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(MAXIMUM(L'LENGTH, R'LENGTH)-1 downto 0)
  -- Result: Subtracts an UNRESOLVED_SIGNED vector, R, from another UNRESOLVED_SIGNED vector, L,
  --         that may possibly be of different lengths.

  -- Id: A.10R
  function "-"(L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Similar to A.10 where R is bit 0 of a non-negative.

  -- Id: A.10L
  function "-"(L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Similar to A.10 where R is bit 0 of a non-negative.

  -- Id: A.11
  function "-" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Subtracts a nonnegative INTEGER, R, from an UNRESOLVED_UNSIGNED vector, L.

  -- Id: A.12
  function "-" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Subtracts an UNRESOLVED_UNSIGNED vector, R, from a nonnegative INTEGER, L.

  -- Id: A.13
  function "-" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Subtracts an INTEGER, R, from an UNRESOLVED_SIGNED vector, L.

  -- Id: A.14
  function "-" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Subtracts an UNRESOLVED_SIGNED vector, R, from an INTEGER, L.

  --============================================================================

  -- Id: A.15
  function "*" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED((L'LENGTH+R'LENGTH-1) downto 0)
  -- Result: Performs the multiplication operation on two UNRESOLVED_UNSIGNED vectors
  --         that may possibly be of different lengths.

  -- Id: A.16
  function "*" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED((L'LENGTH+R'LENGTH-1) downto 0)
  -- Result: Multiplies two UNRESOLVED_SIGNED vectors that may possibly be of
  --         different lengths.

  -- Id: A.17
  function "*" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED((L'LENGTH+L'LENGTH-1) downto 0)
  -- Result: Multiplies an UNRESOLVED_UNSIGNED vector, L, with a nonnegative
  --         INTEGER, R. R is converted to an UNRESOLVED_UNSIGNED vector of
  --         SIZE L'LENGTH before multiplication.

  -- Id: A.18
  function "*" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED((R'LENGTH+R'LENGTH-1) downto 0)
  -- Result: Multiplies an UNRESOLVED_UNSIGNED vector, R, with a nonnegative
  --         INTEGER, L. L is converted to an UNRESOLVED_UNSIGNED vector of
  --         SIZE R'LENGTH before multiplication.

  -- Id: A.19
  function "*" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED((L'LENGTH+L'LENGTH-1) downto 0)
  -- Result: Multiplies an UNRESOLVED_SIGNED vector, L, with an INTEGER, R. R is
  --         converted to an UNRESOLVED_SIGNED vector of SIZE L'LENGTH before
  --         multiplication.

  -- Id: A.20
  function "*" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED((R'LENGTH+R'LENGTH-1) downto 0)
  -- Result: Multiplies an UNRESOLVED_SIGNED vector, R, with an INTEGER, L. L is
  --         converted to an UNRESOLVED_SIGNED vector of SIZE R'LENGTH before
  --         multiplication.

  --============================================================================
  --
  -- NOTE: If second argument is zero for "/" operator, a severity level
  --       of ERROR is issued.

  -- Id: A.21
  function "/" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Divides an UNRESOLVED_UNSIGNED vector, L, by another UNRESOLVED_UNSIGNED vector, R.

  -- Id: A.22
  function "/" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Divides an UNRESOLVED_SIGNED vector, L, by another UNRESOLVED_SIGNED vector, R.

  -- Id: A.23
  function "/" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Divides an UNRESOLVED_UNSIGNED vector, L, by a nonnegative INTEGER, R.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.24
  function "/" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Divides a nonnegative INTEGER, L, by an UNRESOLVED_UNSIGNED vector, R.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  -- Id: A.25
  function "/" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Divides an UNRESOLVED_SIGNED vector, L, by an INTEGER, R.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.26
  function "/" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Divides an INTEGER, L, by an UNRESOLVED_SIGNED vector, R.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  --============================================================================
  --
  -- NOTE: If second argument is zero for "rem" operator, a severity level
  --       of ERROR is issued.

  -- Id: A.27
  function "rem" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where L and R are UNRESOLVED_UNSIGNED vectors.

  -- Id: A.28
  function "rem" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where L and R are UNRESOLVED_SIGNED vectors.

  -- Id: A.29
  function "rem" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where L is an UNRESOLVED_UNSIGNED vector and R is a
  --         nonnegative INTEGER.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.30
  function "rem" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where R is an UNRESOLVED_UNSIGNED vector and L is a
  --         nonnegative INTEGER.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  -- Id: A.31
  function "rem" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where L is UNRESOLVED_SIGNED vector and R is an INTEGER.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.32
  function "rem" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L rem R" where R is UNRESOLVED_SIGNED vector and L is an INTEGER.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  --============================================================================
  --
  -- NOTE: If second argument is zero for "mod" operator, a severity level
  --       of ERROR is issued.

  -- Id: A.33
  function "mod" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L and R are UNRESOLVED_UNSIGNED vectors.

  -- Id: A.34
  function "mod" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L and R are UNRESOLVED_SIGNED vectors.

  -- Id: A.35
  function "mod" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L is an UNRESOLVED_UNSIGNED vector and R
  --         is a nonnegative INTEGER.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.36
  function "mod" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where R is an UNRESOLVED_UNSIGNED vector and L
  --         is a nonnegative INTEGER.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  -- Id: A.37
  function "mod" (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.
  --         If NO_OF_BITS(R) > L'LENGTH, result is truncated to L'LENGTH.

  -- Id: A.38
  function "mod" (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Computes "L mod R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.
  --         If NO_OF_BITS(L) > R'LENGTH, result is truncated to R'LENGTH.

  --============================================================================
  -- Id: A.39
  function find_leftmost (ARG : UNRESOLVED_UNSIGNED; Y : STD_ULOGIC) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Finds the leftmost occurrence of the value of Y in ARG.
  --         Returns the index of the occurrence if it exists, or -1 otherwise.

  -- Id: A.40
  function find_leftmost (ARG : UNRESOLVED_SIGNED; Y : STD_ULOGIC) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Finds the leftmost occurrence of the value of Y in ARG.
  --         Returns the index of the occurrence if it exists, or -1 otherwise.

  -- Id: A.41
  function find_rightmost (ARG : UNRESOLVED_UNSIGNED; Y : STD_ULOGIC) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Finds the leftmost occurrence of the value of Y in ARG.
  --         Returns the index of the occurrence if it exists, or -1 otherwise.

  -- Id: A.42
  function find_rightmost (ARG : UNRESOLVED_SIGNED; Y : STD_ULOGIC) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Finds the leftmost occurrence of the value of Y in ARG.
  --         Returns the index of the occurrence if it exists, or -1 otherwise.

  --============================================================================
  -- Comparison Operators
  --============================================================================

  -- Id: C.1
  function ">" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.2
  function ">" (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.3
  function ">" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.4
  function ">" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L is a INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.5
  function ">" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.6
  function ">" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L > R" where L is an UNRESOLVED_SIGNED vector and
  --         R is a INTEGER.

  --============================================================================

  -- Id: C.7
  function "<" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.8
  function "<" (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.9
  function "<" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.10
  function "<" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.11
  function "<" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.12
  function "<" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L < R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.13
  function "<=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.14
  function "<=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.15
  function "<=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.16
  function "<=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.17
  function "<=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.18
  function "<=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L <= R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.19
  function ">=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.20
  function ">=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.21
  function ">=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.22
  function ">=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.23
  function ">=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.24
  function ">=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L >= R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.25
  function "=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.26
  function "=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.27
  function "=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.28
  function "=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.29
  function "=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.30
  function "=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L = R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.31
  function "/=" (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.32
  function "/=" (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.33
  function "/=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.34
  function "/=" (L : INTEGER; R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.35
  function "/=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.36
  function "/=" (L : UNRESOLVED_SIGNED; R : INTEGER) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: Computes "L /= R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.37
  function MINIMUM (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED
  -- Result: Returns the lesser of two UNRESOLVED_UNSIGNED vectors that may be
  --         of different lengths.

  -- Id: C.38
  function MINIMUM (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED
  -- Result: Returns the lesser of two UNRESOLVED_SIGNED vectors that may be
  --         of different lengths.

  -- Id: C.39
  function MINIMUM (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED
  -- Result: Returns the lesser of a nonnegative INTEGER, L, and
  --         an UNRESOLVED_UNSIGNED vector, R.

  -- Id: C.40
  function MINIMUM (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED
  -- Result: Returns the lesser of an INTEGER, L, and an UNRESOLVED_SIGNED
  --         vector, R.

  -- Id: C.41
  function MINIMUM (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED
  -- Result: Returns the lesser of an UNRESOLVED_UNSIGNED vector, L, and
  --         a nonnegative INTEGER, R.

  -- Id: C.42
  function MINIMUM (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED
  -- Result: Returns the lesser of an UNRESOLVED_SIGNED vector, L, and
  --         an INTEGER, R.

  --============================================================================

  -- Id: C.43
  function MAXIMUM (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED
  -- Result: Returns the greater of two UNRESOLVED_UNSIGNED vectors that may be
  --         of different lengths.

  -- Id: C.44
  function MAXIMUM (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED
  -- Result: Returns the greater of two UNRESOLVED_SIGNED vectors that may be
  --         of different lengths.

  -- Id: C.45
  function MAXIMUM (L : NATURAL; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED
  -- Result: Returns the greater of a nonnegative INTEGER, L, and
  --         an UNRESOLVED_UNSIGNED vector, R.

  -- Id: C.46
  function MAXIMUM (L : INTEGER; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED
  -- Result: Returns the greater of an INTEGER, L, and an UNRESOLVED_SIGNED
  --         vector, R.

  -- Id: C.47
  function MAXIMUM (L : UNRESOLVED_UNSIGNED; R : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED
  -- Result: Returns the greater of an UNRESOLVED_UNSIGNED vector, L, and
  --         a nonnegative INTEGER, R.

  -- Id: C.48
  function MAXIMUM (L : UNRESOLVED_SIGNED; R : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED
  -- Result: Returns the greater of an UNRESOLVED_SIGNED vector, L, and
  --         an INTEGER, R.

  --============================================================================

  -- Id: C.49
  function "?>" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.50
  function "?>" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.51
  function "?>" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.52
  function "?>" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L is a INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.53
  function "?>" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.54
  function "?>" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L > R" where L is an UNRESOLVED_SIGNED vector and
  --         R is a INTEGER.

  --============================================================================

  -- Id: C.55
  function "?<" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.56
  function "?<" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.57
  function "?<" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.58
  function "?<" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.59
  function "?<" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.60
  function "?<" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L < R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.61
  function "?<=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.62
  function "?<=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.63
  function "?<=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.64
  function "?<=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.65
  function "?<=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.66
  function "?<=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L <= R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.67
  function "?>=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.68
  function "?>=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.69
  function "?>=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.70
  function "?>=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.71
  function "?>=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.72
  function "?>=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L >= R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.73
  function "?=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.74
  function "?=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.75
  function "?=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.76
  function "?=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.77
  function "?=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.78
  function "?=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L = R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================

  -- Id: C.79
  function "?/=" (L, R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L and R are UNRESOLVED_UNSIGNED vectors possibly
  --         of different lengths.

  -- Id: C.80
  function "?/=" (L, R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L and R are UNRESOLVED_SIGNED vectors possibly
  --         of different lengths.

  -- Id: C.81
  function "?/=" (L : NATURAL; R : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L is a nonnegative INTEGER and
  --         R is an UNRESOLVED_UNSIGNED vector.

  -- Id: C.82
  function "?/=" (L : INTEGER; R : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L is an INTEGER and
  --         R is an UNRESOLVED_SIGNED vector.

  -- Id: C.83
  function "?/=" (L : UNRESOLVED_UNSIGNED; R : NATURAL) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L is an UNRESOLVED_UNSIGNED vector and
  --         R is a nonnegative INTEGER.

  -- Id: C.84
  function "?/=" (L : UNRESOLVED_SIGNED; R : INTEGER) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC
  -- Result: Computes "L /= R" where L is an UNRESOLVED_SIGNED vector and
  --         R is an INTEGER.

  --============================================================================
  -- Shift and Rotate Functions
  --============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a shift-left on an UNRESOLVED_UNSIGNED vector COUNT times.
  --         The vacated positions are filled with '0'.
  --         The COUNT leftmost elements are lost.

  -- Id: S.2
  function SHIFT_RIGHT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a shift-right on an UNRESOLVED_UNSIGNED vector COUNT times.
  --         The vacated positions are filled with '0'.
  --         The COUNT rightmost elements are lost.

  -- Id: S.3
  function SHIFT_LEFT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a shift-left on an UNRESOLVED_SIGNED vector COUNT times.
  --         The vacated positions are filled with '0'.
  --         The COUNT leftmost elements are lost.

  -- Id: S.4
  function SHIFT_RIGHT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a shift-right on an UNRESOLVED_SIGNED vector COUNT times.
  --         The vacated positions are filled with the leftmost
  --         element, ARG'LEFT. The COUNT rightmost elements are lost.

  --============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a rotate-left of an UNRESOLVED_UNSIGNED vector COUNT times.

  -- Id: S.6
  function ROTATE_RIGHT (ARG : UNRESOLVED_UNSIGNED; COUNT : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a rotate-right of an UNRESOLVED_UNSIGNED vector COUNT times.

  -- Id: S.7
  function ROTATE_LEFT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a logical rotate-left of an UNRESOLVED_SIGNED
  --         vector COUNT times.

  -- Id: S.8
  function ROTATE_RIGHT (ARG : UNRESOLVED_SIGNED; COUNT : NATURAL) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: Performs a logical rotate-right of an UNRESOLVED_SIGNED
  --         vector COUNT times.

  --============================================================================

  --============================================================================

  ------------------------------------------------------------------------------
  --   Note: Function S.9 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.9
  function "sll" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.10 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.10
  function "sll" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  --   Note: Function S.11 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE StdL 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.11
  function "srl" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_RIGHT(ARG, COUNT)

  ------------------------------------------------------------------------------
  --   Note: Function S.12 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.12
  function "srl" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: UNRESOLVED_SIGNED(SHIFT_RIGHT(UNRESOLVED_UNSIGNED(ARG), COUNT))

  ------------------------------------------------------------------------------
  --   Note: Function S.13 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.13
  function "rol" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: ROTATE_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  --   Note: Function S.14 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.14
  function "rol" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: ROTATE_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.15 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.15
  function "ror" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: ROTATE_RIGHT(ARG, COUNT)

  ------------------------------------------------------------------------------
  --   Note: Function S.16 is not compatible with IEEE Std 1076-1987. Comment
  --   out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.16
  function "ror" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: ROTATE_RIGHT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.17 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.17
  function "sla" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.18 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.18
  function "sla" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_LEFT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.19 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.19
  function "sra" (ARG : UNRESOLVED_UNSIGNED; COUNT : INTEGER) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_RIGHT(ARG, COUNT)

  ------------------------------------------------------------------------------
  -- Note: Function S.20 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.20
  function "sra" (ARG : UNRESOLVED_SIGNED; COUNT : INTEGER) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(ARG'LENGTH-1 downto 0)
  -- Result: SHIFT_RIGHT(ARG, COUNT)

  --============================================================================
  --   RESIZE Functions
  --============================================================================

  -- Id: R.1
  function RESIZE (ARG : UNRESOLVED_SIGNED; NEW_SIZE : NATURAL) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(NEW_SIZE-1 downto 0)
  -- Result: Resizes the UNRESOLVED_SIGNED vector ARG to the specified size.
  --         To create a larger vector, the new [leftmost] bit positions
  --         are filled with the sign bit (ARG'LEFT). When truncating,
  --         the sign bit is retained along with the rightmost part.

  -- Id: R.2
  function RESIZE (ARG : UNRESOLVED_UNSIGNED; NEW_SIZE : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(NEW_SIZE-1 downto 0)
  -- Result: Resizes the UNRESOLVED_SIGNED vector ARG to the specified size.
  --         To create a larger vector, the new [leftmost] bit positions
  --         are filled with '0'. When truncating, the leftmost bits
  --         are dropped.

  function RESIZE (ARG, SIZE_RES : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED (SIZE_RES'length-1 downto 0)

  function RESIZE (ARG, SIZE_RES : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED (SIZE_RES'length-1 downto 0)

  --============================================================================
  -- Conversion Functions
  --============================================================================

  -- Id: D.1
  function TO_INTEGER (ARG : UNRESOLVED_UNSIGNED) return NATURAL;
  -- Result subtype: NATURAL. Value cannot be negative since parameter is an
  --             UNRESOLVED_UNSIGNED vector.
  -- Result: Converts the UNRESOLVED_UNSIGNED vector to an INTEGER.

  -- Id: D.2
  function TO_INTEGER (ARG : UNRESOLVED_SIGNED) return INTEGER;
  -- Result subtype: INTEGER
  -- Result: Converts an UNRESOLVED_SIGNED vector to an INTEGER.

  -- Id: D.3
  function TO_UNSIGNED (ARG, SIZE : NATURAL) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(SIZE-1 downto 0)
  -- Result: Converts a nonnegative INTEGER to an UNRESOLVED_UNSIGNED vector with
  --         the specified SIZE.

  -- Id: D.4
  function TO_SIGNED (ARG : INTEGER; SIZE : NATURAL) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(SIZE-1 downto 0)
  -- Result: Converts an INTEGER to a UNRESOLVED_SIGNED vector of the specified SIZE.

  function TO_UNSIGNED (ARG : NATURAL; SIZE_RES : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(SIZE_RES'length-1 downto 0)

  function TO_SIGNED (ARG : INTEGER; SIZE_RES : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(SIZE_RES'length-1 downto 0)

  --============================================================================
  -- Logical Operators
  --============================================================================

  -- Id: L.1
  function "not" (L : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Termwise inversion

  -- Id: L.2
  function "and" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector AND operation

  -- Id: L.3
  function "or" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector OR operation

  -- Id: L.4
  function "nand" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector NAND operation

  -- Id: L.5
  function "nor" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector NOR operation

  -- Id: L.6
  function "xor" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector XOR operation

  -- ---------------------------------------------------------------------------
  -- Note: Function L.7 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  -- ---------------------------------------------------------------------------
  -- Id: L.7
  function "xnor" (L, R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector XNOR operation

  -- Id: L.8
  function "not" (L : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Termwise inversion

  -- Id: L.9
  function "and" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector AND operation

  -- Id: L.10
  function "or" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector OR operation

  -- Id: L.11
  function "nand" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector NAND operation

  -- Id: L.12
  function "nor" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector NOR operation

  -- Id: L.13
  function "xor" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector XOR operation

  -- ---------------------------------------------------------------------------
  -- Note: Function L.14 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  -- ---------------------------------------------------------------------------
  -- Id: L.14
  function "xnor" (L, R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector XNOR operation

  -- Id: L.15
  function "and" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector AND operation

  -- Id: L.16
  function "and" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar AND operation

  -- Id: L.17
  function "or" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector OR operation

  -- Id: L.18
  function "or" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar OR operation

  -- Id: L.19
  function "nand" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector NAND operation

  -- Id: L.20
  function "nand" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar NAND operation

  -- Id: L.21
  function "nor" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector NOR operation

  -- Id: L.22
  function "nor" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar NOR operation

  -- Id: L.23
  function "xor" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector XOR operation

  -- Id: L.24
  function "xor" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar XOR operation

  ------------------------------------------------------------------------------
  -- Note: Function L.25 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.25
  function "xnor" (L : STD_ULOGIC; R : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector XNOR operation

  ------------------------------------------------------------------------------
  -- Note: Function L.26 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.26
  function "xnor" (L : UNRESOLVED_UNSIGNED; R : STD_ULOGIC) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar XNOR operation

  -- Id: L.27
  function "and" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector AND operation

  -- Id: L.28
  function "and" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar AND operation

  -- Id: L.29
  function "or" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector OR operation

  -- Id: L.30
  function "or" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar OR operation

  -- Id: L.31
  function "nand" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector NAND operation

  -- Id: L.32
  function "nand" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar NAND operation

  -- Id: L.33
  function "nor" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector NOR operation

  -- Id: L.34
  function "nor" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar NOR operation

  -- Id: L.35
  function "xor" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector XOR operation

  -- Id: L.36
  function "xor" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar XOR operation

  ------------------------------------------------------------------------------
  -- Note: Function L.37 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.37
  function "xnor" (L : STD_ULOGIC; R : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(R'LENGTH-1 downto 0)
  -- Result: Scalar/Vector XNOR operation

  ------------------------------------------------------------------------------
  -- Note: Function L.38 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.38
  function "xnor" (L : UNRESOLVED_SIGNED; R : STD_ULOGIC) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(L'LENGTH-1 downto 0)
  -- Result: Vector/Scalar XNOR operation

  ------------------------------------------------------------------------------
  -- Note: Function L.39 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.39
  function "and" (L : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of and'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.40 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.40
  function "nand" (L : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of nand'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.41 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.41
  function "or" (L : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of or'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.42 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.42
  function "nor" (L : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of nor'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.43 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.43
  function "xor" (L : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of xor'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.44 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.44
  function "xnor" (L : UNRESOLVED_SIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of xnor'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.45 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.45
  function "and" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of and'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.46 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.46
  function "nand" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of nand'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.47 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.47
  function "or" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of or'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.48 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.48
  function "nor" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of nor'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.49 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.49
  function "xor" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of xor'ing all of the bits of the vector.

  ------------------------------------------------------------------------------
  -- Note: Function L.50 is not compatible with editions of IEEE Std 1076 from
  -- 1987 through 2002. Comment out the function (declaration and body) for
  -- compatibility with these editions.
  ------------------------------------------------------------------------------
  -- Id: L.50
  function "xnor" (L : UNRESOLVED_UNSIGNED) return STD_ULOGIC;
  -- Result subtype: STD_ULOGIC.
  -- Result: Result of xnor'ing all of the bits of the vector.

  --============================================================================
  -- Match Functions
  --============================================================================

  -- Id: M.1
  function STD_MATCH (L, R : STD_ULOGIC) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: terms compared per STD_LOGIC_1164 intent

  -- Id: M.2
  function STD_MATCH (L, R : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: terms compared per STD_LOGIC_1164 intent

  -- Id: M.3
  function STD_MATCH (L, R : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: terms compared per STD_LOGIC_1164 intent

  -- Id: M.5
  function STD_MATCH (L, R : STD_ULOGIC_VECTOR) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: terms compared per STD_LOGIC_1164 intent

  --============================================================================
  -- Translation Functions
  --============================================================================

  -- Id: T.1
  function TO_01 (S : UNRESOLVED_UNSIGNED; XMAP : STD_ULOGIC := '0') return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', and 'L' is translated
  --         to '0'. If a value other than '0'|'1'|'H'|'L' is found,
  --         the array is set to (others => XMAP), and a warning is
  --         issued.

  -- Id: T.2
  function TO_01 (S : UNRESOLVED_SIGNED; XMAP : STD_ULOGIC := '0') return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', and 'L' is translated
  --         to '0'. If a value other than '0'|'1'|'H'|'L' is found,
  --         the array is set to (others => XMAP), and a warning is
  --         issued.

  -- Id: T.3
  function TO_X01 (S : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', 'L' is translated to '0',
  --         and values other than '0'|'1'|'H'|'L' are translated to 'X'.

  -- Id: T.4
  function TO_X01 (S : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', 'L' is translated to '0',
  --         and values other than '0'|'1'|'H'|'L' are translated to 'X'.

  -- Id: T.5
  function TO_X01Z (S : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', 'L' is translated to '0',
  --         and values other than '0'|'1'|'H'|'L'|'Z' are translated to 'X'.

  -- Id: T.6
  function TO_X01Z (S : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', 'L' is translated to '0',
  --         and values other than '0'|'1'|'H'|'L'|'Z' are translated to 'X'.

  -- Id: T.7
  function TO_UX01 (S : UNRESOLVED_UNSIGNED) return UNRESOLVED_UNSIGNED;
  -- Result subtype: UNRESOLVED_UNSIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', 'L' is translated to '0',
  --         and values other than 'U'|'0'|'1'|'H'|'L' are translated to 'X'.

  -- Id: T.8
  function TO_UX01 (S : UNRESOLVED_SIGNED) return UNRESOLVED_SIGNED;
  -- Result subtype: UNRESOLVED_SIGNED(S'RANGE)
  -- Result: Termwise, 'H' is translated to '1', 'L' is translated to '0',
  --         and values other than 'U'|'0'|'1'|'H'|'L' are translated to 'X'.

  -- Id: T.9
  function IS_X (S : UNRESOLVED_UNSIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: TRUE if S contains a 'U'|'X'|'Z'|'W'|'-' value, FALSE otherwise.

  -- Id: T.10
  function IS_X (S : UNRESOLVED_SIGNED) return BOOLEAN;
  -- Result subtype: BOOLEAN
  -- Result: TRUE if S contains a 'U'|'X'|'Z'|'W'|'-' value, FALSE otherwise.

  --============================================================================
  -- string conversion and write operations
  --============================================================================
  -- the following operations are predefined

  -- function TO_STRING (value : UNRESOLVED_UNSIGNED) return STRING;
  -- function TO_STRING (value : UNRESOLVED_SIGNED) return STRING;

  -- explicitly defined operations

  alias TO_BSTRING is TO_STRING [UNRESOLVED_UNSIGNED return STRING];
  alias TO_BSTRING is TO_STRING [UNRESOLVED_SIGNED return STRING];
  alias to_binary_string is TO_STRING [UNRESOLVED_UNSIGNED return STRING];
  alias to_binary_string is TO_STRING [UNRESOLVED_SIGNED return STRING];

  function TO_OSTRING (value : UNRESOLVED_UNSIGNED) return STRING;
  function TO_OSTRING (value : UNRESOLVED_SIGNED) return STRING;
  alias to_octal_string is TO_OSTRING [UNRESOLVED_UNSIGNED return STRING];
  alias to_octal_string is TO_OSTRING [UNRESOLVED_SIGNED return STRING];

  function to_hstring (value : UNRESOLVED_UNSIGNED) return STRING;
  function to_hstring (value : UNRESOLVED_SIGNED) return STRING;
  alias to_hex_string is to_hstring [UNRESOLVED_UNSIGNED return STRING];
  alias to_hex_string is to_hstring [UNRESOLVED_SIGNED return STRING];

  procedure READ(L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED; GOOD : out BOOLEAN);

  procedure READ(L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED);

  procedure READ(L : inout LINE; VALUE : out UNRESOLVED_SIGNED; GOOD : out BOOLEAN);

  procedure READ(L : inout LINE; VALUE : out UNRESOLVED_SIGNED);

  procedure WRITE (L         : inout LINE; VALUE : in UNRESOLVED_UNSIGNED;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  procedure WRITE (L         : inout LINE; VALUE : in UNRESOLVED_SIGNED;
                   JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  alias BREAD is READ [LINE, UNRESOLVED_UNSIGNED, BOOLEAN];
  alias BREAD is READ [LINE, UNRESOLVED_SIGNED, BOOLEAN];

  alias BREAD is READ [LINE, UNRESOLVED_UNSIGNED];
  alias BREAD is READ [LINE, UNRESOLVED_SIGNED];

  alias BINARY_READ is READ [LINE, UNRESOLVED_UNSIGNED, BOOLEAN];
  alias BINARY_READ is READ [LINE, UNRESOLVED_SIGNED, BOOLEAN];

  alias BINARY_READ is READ [LINE, UNRESOLVED_UNSIGNED];
  alias BINARY_READ is READ [LINE, UNRESOLVED_SIGNED];

  procedure OREAD (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED; GOOD : out BOOLEAN);
  procedure OREAD (L : inout LINE; VALUE : out UNRESOLVED_SIGNED; GOOD : out BOOLEAN);

  procedure OREAD (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED);
  procedure OREAD (L : inout LINE; VALUE : out UNRESOLVED_SIGNED);

  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_UNSIGNED, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_SIGNED, BOOLEAN];

  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_UNSIGNED];
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_SIGNED];

  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED; GOOD : out BOOLEAN);
  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_SIGNED; GOOD : out BOOLEAN);

  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_UNSIGNED);
  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_SIGNED);

  alias HEX_READ is HREAD [LINE, UNRESOLVED_UNSIGNED, BOOLEAN];
  alias HEX_READ is HREAD [LINE, UNRESOLVED_SIGNED, BOOLEAN];

  alias HEX_READ is HREAD [LINE, UNRESOLVED_UNSIGNED];
  alias HEX_READ is HREAD [LINE, UNRESOLVED_SIGNED];

  alias BWRITE is WRITE [LINE, UNRESOLVED_UNSIGNED, SIDE, WIDTH];
  alias BWRITE is WRITE [LINE, UNRESOLVED_SIGNED, SIDE, WIDTH];

  alias BINARY_WRITE is WRITE [LINE, UNRESOLVED_UNSIGNED, SIDE, WIDTH];
  alias BINARY_WRITE is WRITE [LINE, UNRESOLVED_SIGNED, SIDE, WIDTH];

  procedure OWRITE (L         : inout LINE; VALUE : in UNRESOLVED_UNSIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  procedure OWRITE (L         : inout LINE; VALUE : in UNRESOLVED_SIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  alias OCTAL_WRITE is OWRITE [LINE, UNRESOLVED_UNSIGNED, SIDE, WIDTH];
  alias OCTAL_WRITE is OWRITE [LINE, UNRESOLVED_SIGNED, SIDE, WIDTH];

  procedure HWRITE (L         : inout LINE; VALUE : in UNRESOLVED_UNSIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  procedure HWRITE (L         : inout LINE; VALUE : in UNRESOLVED_SIGNED;
                    JUSTIFIED : in    SIDE := right; FIELD : in WIDTH := 0);

  alias HEX_WRITE is HWRITE [LINE, UNRESOLVED_UNSIGNED, SIDE, WIDTH];
  alias HEX_WRITE is HWRITE [LINE, UNRESOLVED_SIGNED, SIDE, WIDTH];

end package NUMERIC_STD;
