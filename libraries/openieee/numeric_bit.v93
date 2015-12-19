--  This -*- vhdl -*- file is part of GHDL.
--  IEEE 1076.3 compliant numeric bit package.
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

package NUMERIC_BIT is
  type UNSIGNED is array (natural range <>) of BIT;
  type SIGNED   is array (natural range <>) of BIT;


  function TO_INTEGER (ARG : UNSIGNED) return NATURAL;
  function TO_INTEGER (ARG :   SIGNED) return INTEGER;
  --  Convert ARG to an integer.
  --  Simulation is aborted in case of overflow.
  --  Issue a warning in case of non-logical value.

  function TO_UNSIGNED (ARG, SIZE : NATURAL) return UNSIGNED;
  --  Convert ARG to unsigned.
  --  Result index range is SIZE - 1 downto 0.
  --  Issue a warning if value is truncated.

  function TO_SIGNED (ARG : INTEGER; SIZE : NATURAL) return SIGNED;
  --  Convert ARG to signed.
  --  Result index range is SIZE - 1 downto 0.
  --  Issue a warning if value is truncated.

  function resize (ARG : UNSIGNED; NEW_SIZE: natural) return UNSIGNED;
  function resize (ARG :   SIGNED; NEW_SIZE: natural) return   SIGNED;
  --  Result index range is NEW_SIZE - 1 downto 0 (unless null array).
  --  For SIGNED, the sign of the result is the sign of ARG.

  function "="  (L, R : UNSIGNED) return BOOLEAN;
  function "="  (L : UNSIGNED; R :  NATURAL) return BOOLEAN;
  function "="  (L :  NATURAL; R : UNSIGNED) return BOOLEAN;
  function "/=" (L, R : UNSIGNED) return BOOLEAN;
  function "/=" (L : UNSIGNED; R :  NATURAL) return BOOLEAN;
  function "/=" (L :  NATURAL; R : UNSIGNED) return BOOLEAN;
  function "<"  (L, R : UNSIGNED) return BOOLEAN;
  function "<"  (L : UNSIGNED; R :  NATURAL) return BOOLEAN;
  function "<"  (L :  NATURAL; R : UNSIGNED) return BOOLEAN;
  function "<=" (L, R : UNSIGNED) return BOOLEAN;
  function "<=" (L : UNSIGNED; R :  NATURAL) return BOOLEAN;
  function "<=" (L :  NATURAL; R : UNSIGNED) return BOOLEAN;
  function ">"  (L, R : UNSIGNED) return BOOLEAN;
  function ">"  (L : UNSIGNED; R :  NATURAL) return BOOLEAN;
  function ">"  (L :  NATURAL; R : UNSIGNED) return BOOLEAN;
  function ">=" (L, R : UNSIGNED) return BOOLEAN;
  function ">=" (L : UNSIGNED; R :  NATURAL) return BOOLEAN;
  function ">=" (L :  NATURAL; R : UNSIGNED) return BOOLEAN;

  function "="  (L, R : SIGNED) return BOOLEAN;
  function "="  (L :   SIGNED; R :  INTEGER) return BOOLEAN;
  function "="  (L :  INTEGER; R :   SIGNED) return BOOLEAN;
  function "/=" (L, R : SIGNED) return BOOLEAN;
  function "/=" (L :   SIGNED; R :  INTEGER) return BOOLEAN;
  function "/=" (L :  INTEGER; R :   SIGNED) return BOOLEAN;
  function "<"  (L, R : SIGNED) return BOOLEAN;
  function "<"  (L :   SIGNED; R :  INTEGER) return BOOLEAN;
  function "<"  (L :  INTEGER; R :   SIGNED) return BOOLEAN;
  function "<=" (L, R : SIGNED) return BOOLEAN;
  function "<=" (L :   SIGNED; R :  INTEGER) return BOOLEAN;
  function "<=" (L :  INTEGER; R :   SIGNED) return BOOLEAN;
  function ">"  (L, R : SIGNED) return BOOLEAN;
  function ">"  (L :   SIGNED; R :  INTEGER) return BOOLEAN;
  function ">"  (L :  INTEGER; R :   SIGNED) return BOOLEAN;
  function ">=" (L, R : SIGNED) return BOOLEAN;
  function ">=" (L :   SIGNED; R :  INTEGER) return BOOLEAN;
  function ">=" (L :  INTEGER; R :   SIGNED) return BOOLEAN;
   --  Issue a warning in case of non-logical value.

  function "-" (ARG : SIGNED) return SIGNED;
  --  Compute -ARG.
  --  Result index range is Arg'length - 1 downto 0.

  function "abs" (ARG : SIGNED) return SIGNED;
  --  Compute abs ARG.
  --  Result index range is Arg'length - 1 downto 0.

  function "+" (L, R : UNSIGNED) return UNSIGNED;
  function "+" (L, R :   SIGNED) return   SIGNED;
  function "-" (L, R : UNSIGNED) return UNSIGNED;
  function "-" (L, R :   SIGNED) return   SIGNED;
  --  Compute L +/- R.
  --  Result index range is max (L'Length, R'Length) - 1 downto 0.
  --  Issue a warning in case of non-logical value.

  function "+" (L : UNSIGNED; R :  NATURAL) return UNSIGNED;
  function "+" (L :  NATURAL; R : UNSIGNED) return UNSIGNED;
  function "+" (L :   SIGNED; R :  INTEGER) return   SIGNED;
  function "+" (L :  INTEGER; R :   SIGNED) return   SIGNED;
  function "-" (L : UNSIGNED; R :  NATURAL) return UNSIGNED;
  function "-" (L :  NATURAL; R : UNSIGNED) return UNSIGNED;
  function "-" (L :   SIGNED; R :  INTEGER) return   SIGNED;
  function "-" (L :  INTEGER; R :   SIGNED) return   SIGNED;
  --  Compute L +/- R.
  --  Result index range is V'Length - 1 downto 0, where V is the vector
  --   parameter.
  --  Issue a warning in case of non-logical value.
  --  Issue a warning if value is truncated.

  function "*" (L, R : UNSIGNED) return UNSIGNED;
  function "*" (L, R :   SIGNED) return   SIGNED;
  --  Compute L * R
  --  Result index range is L'Length + R'Length - 1 downto 0.

  function "*" (L : UNSIGNED; R : NATURAL) return UNSIGNED;
  function "*" (L :   SIGNED; R : INTEGER) return   SIGNED;
  --  Compute L * R
  --  R is converted to a vector of length L'length

  function "*" (L : NATURAL; R : UNSIGNED) return UNSIGNED;
  function "*" (L : INTEGER; R :   SIGNED) return   SIGNED;
  --  Compute L * R
  --  L is converted to a vector of length R'length

  function "/"   (L, R : UNSIGNED) return UNSIGNED;
  function "/"   (L, R :   SIGNED) return   SIGNED;
  function "rem" (L, R : UNSIGNED) return UNSIGNED;
  function "rem" (L, R :   SIGNED) return   SIGNED;
  function "mod" (L, R : UNSIGNED) return UNSIGNED;
  function "mod" (L, R :   SIGNED) return   SIGNED;
  --  Compute L op R
  --  Result index range is L'Length - 1 downto 0.
  --  Issue a warning in case of non-logical value.
  --  Issue an error if R is 0.

  function "/"   (L : UNSIGNED; R : NATURAL) return UNSIGNED;
  function "/"   (L :   SIGNED; R : INTEGER) return   SIGNED;
  function "rem" (L : UNSIGNED; R : NATURAL) return UNSIGNED;
  function "rem" (L :   SIGNED; R : INTEGER) return   SIGNED;
  function "mod" (L : UNSIGNED; R : NATURAL) return UNSIGNED;
  function "mod" (L :   SIGNED; R : INTEGER) return   SIGNED;
  --  Compute L op R.
  --  Result index range is L'Length - 1 downto 0.
  --  Issue a warning in case of non-logical value.
  --  Issue an error if R is 0.

  function "/"   (L : NATURAL; R : UNSIGNED) return UNSIGNED;
  function "/"   (L : INTEGER; R :   SIGNED) return   SIGNED;
  function "rem" (L : NATURAL; R : UNSIGNED) return UNSIGNED;
  function "rem" (L : INTEGER; R :   SIGNED) return   SIGNED;
  function "mod" (L : NATURAL; R : UNSIGNED) return UNSIGNED;
  function "mod" (L : INTEGER; R :   SIGNED) return   SIGNED;
  --  Compute L op R.
  --  Result index range is R'Length - 1 downto 0.
  --  Issue a warning in case of non-logical value.
  --  Issue an error if R is 0.
  --  Result may be truncated.

  function "not" (l : UNSIGNED) return UNSIGNED;
  function "not" (l :   SIGNED) return   SIGNED;
  function "and" (l, r : UNSIGNED) return UNSIGNED;
  function "and" (l, r :   SIGNED) return   SIGNED;
  function "nand" (l, r : UNSIGNED) return UNSIGNED;
  function "nand" (l, r :   SIGNED) return   SIGNED;
  function "or" (l, r : UNSIGNED) return UNSIGNED;
  function "or" (l, r :   SIGNED) return   SIGNED;
  function "nor" (l, r : UNSIGNED) return UNSIGNED;
  function "nor" (l, r :   SIGNED) return   SIGNED;
  function "xor" (l, r : UNSIGNED) return UNSIGNED;
  function "xor" (l, r :   SIGNED) return   SIGNED;
  function "xnor" (l, r : UNSIGNED) return UNSIGNED;
  function "xnor" (l, r :   SIGNED) return   SIGNED;
  --  Compute L OP R.
  --  Result index range is L'Length - 1 downto 0.
  --  No specific handling of null array: the index range of the result
  --  would be -1 downto 0 (without warning).  This it not what is specified
  --  in 1076.3, but corresponds to the standard implementation.
  --  No specific handling of non-logical values.  Behaviour is compatible
  --  with std_logic_1164.

  function shift_left  (ARG : UNSIGNED; COUNT: NATURAL) return UNSIGNED;
  function shift_left  (ARG :   SIGNED; COUNT: NATURAL) return   SIGNED;
  function shift_right (ARG : UNSIGNED; COUNT: NATURAL) return UNSIGNED;
  function shift_right (ARG :   SIGNED; COUNT: NATURAL) return   SIGNED;
  --  Result index range is ARG'Length - 1 downto 0.

  function rotate_left  (ARG : UNSIGNED; COUNT: NATURAL) return UNSIGNED;
  function rotate_left  (ARG :   SIGNED; COUNT: NATURAL) return   SIGNED;
  function rotate_right (ARG : UNSIGNED; COUNT: NATURAL) return UNSIGNED;
  function rotate_right (ARG :   SIGNED; COUNT: NATURAL) return   SIGNED;
  --  Result index range is ARG'Length - 1 downto 0.

  function "sll" (ARG : UNSIGNED; COUNT: INTEGER) return UNSIGNED;
  function "sll" (ARG :   SIGNED; COUNT: INTEGER) return   SIGNED;
  function "srl" (ARG : UNSIGNED; COUNT: INTEGER) return UNSIGNED;
  function "srl" (ARG :   SIGNED; COUNT: INTEGER) return   SIGNED;
  --  Result index range is ARG'Length - 1 downto 0.

  function "rol" (ARG : UNSIGNED; COUNT: INTEGER) return UNSIGNED;
  function "rol" (ARG :   SIGNED; COUNT: INTEGER) return   SIGNED;
  function "ror" (ARG : UNSIGNED; COUNT: INTEGER) return UNSIGNED;
  function "ror" (ARG :   SIGNED; COUNT: INTEGER) return   SIGNED;
  --  Result index range is ARG'Length - 1 downto 0.

  function rising_edge (signal s : bit) return boolean;
  function falling_edge (signal s : bit) return boolean;
end NUMERIC_BIT;
