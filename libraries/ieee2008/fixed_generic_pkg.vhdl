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
--   Title     :  Fixed-point package (Generic package declaration)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC and IEEE P1076 Working Group
--             :
--   Purpose   :  This packages defines basic binary fixed point
--             :  arithmetic functions
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

use STD.TEXTIO.all;
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
use IEEE.fixed_float_types.all;

package fixed_generic_pkg is
  generic (
    -- Rounding routine to use in fixed point, fixed_round or fixed_truncate
    fixed_round_style    : fixed_round_style_type    := fixed_round;
    -- Overflow routine to use in fixed point, fixed_saturate or fixed_wrap
    fixed_overflow_style : fixed_overflow_style_type := fixed_saturate;
    -- Extra bits used in divide routines
    fixed_guard_bits     : NATURAL                   := 3;
    -- If TRUE, then turn off warnings on "X" propagation
    no_warning           : BOOLEAN                   := false
    );

  -- Author David Bishop (dbishop@vhdl.org)
  constant CopyRightNotice : STRING :=
    "Copyright IEEE P1076 WG. Licensed Apache 2.0";

  -- base Unsigned fixed point type, downto direction assumed
  type UNRESOLVED_ufixed is array (INTEGER range <>) of STD_ULOGIC;
  -- base Signed fixed point type, downto direction assumed
  type UNRESOLVED_sfixed is array (INTEGER range <>) of STD_ULOGIC;

  alias U_ufixed is UNRESOLVED_ufixed;
  alias U_sfixed is UNRESOLVED_sfixed;

  subtype ufixed is (resolved) UNRESOLVED_ufixed;
  subtype sfixed is (resolved) UNRESOLVED_sfixed;

  --===========================================================================
  -- Arithmetic Operators:
  --===========================================================================

  -- Absolute value, 2's complement
  -- abs sfixed(a downto b) = sfixed(a+1 downto b)
  function "abs" (arg : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Negation, 2's complement
  -- - sfixed(a downto b) = sfixed(a+1 downto b)
  function "-" (arg : UNRESOLVED_sfixed)return UNRESOLVED_sfixed;

  -- Addition
  -- ufixed(a downto b) + ufixed(c downto d)
  --   = ufixed(maximum(a,c)+1 downto minimum(b,d))
  function "+" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed(a downto b) + sfixed(c downto d)
  --   = sfixed(maximum(a,c)+1 downto minimum(b,d))
  function "+" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Subtraction
  -- ufixed(a downto b) - ufixed(c downto d)
  --   = ufixed(maximum(a,c)+1 downto minimum(b,d))
  function "-" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed(a downto b) - sfixed(c downto d)
  --   = sfixed(maximum(a,c)+1 downto minimum(b,d))
  function "-" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Multiplication
  -- ufixed(a downto b) * ufixed(c downto d) = ufixed(a+c+1 downto b+d)
  function "*" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed(a downto b) * sfixed(c downto d) = sfixed(a+c+1 downto b+d)
  function "*" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Division
  -- ufixed(a downto b) / ufixed(c downto d) = ufixed(a-d downto b-c-1)
  function "/" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed(a downto b) / sfixed(c downto d) = sfixed(a-d+1 downto b-c)
  function "/" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Remainder
  -- ufixed (a downto b) rem ufixed (c downto d)
  --   = ufixed (minimum(a,c) downto minimum(b,d))
  function "rem" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed (a downto b) rem sfixed (c downto d)
  --   = sfixed (minimum(a,c) downto minimum(b,d))
  function "rem" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Modulo
  -- ufixed (a downto b) mod ufixed (c downto d)
  --        = ufixed (minimum(a,c) downto minimum(b, d))
  function "mod" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed (a downto b) mod sfixed (c downto d)
  --        = sfixed (c downto minimum(b, d))
  function "mod" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  ----------------------------------------------------------------------------
  -- In these routines the "real" or "natural" (integer)
  -- are converted into a fixed point number and then the operation is
  -- performed.  It is assumed that the array will be large enough.
  -- If the input is "real" then the real number is converted into a fixed of
  -- the same size as the fixed point input.  If the number is an "integer"
  -- then it is converted into fixed with the range (l'high downto 0).
  ----------------------------------------------------------------------------

  -- ufixed(a downto b) + ufixed(a downto b) = ufixed(a+1 downto b)
  function "+" (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;

  -- ufixed(c downto d) + ufixed(c downto d) = ufixed(c+1 downto d)
  function "+" (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) + ufixed(a downto 0) = ufixed(a+1 downto minimum(0,b))
  function "+" (l : UNRESOLVED_ufixed; r : NATURAL) return UNRESOLVED_ufixed;

  -- ufixed(a downto 0) + ufixed(c downto d) = ufixed(c+1 downto minimum(0,d))
  function "+" (l : NATURAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) - ufixed(a downto b) = ufixed(a+1 downto b)
  function "-" (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;

  -- ufixed(c downto d) - ufixed(c downto d) = ufixed(c+1 downto d)
  function "-" (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) - ufixed(a downto 0) = ufixed(a+1 downto minimum(0,b))
  function "-" (l : UNRESOLVED_ufixed; r : NATURAL) return UNRESOLVED_ufixed;

  -- ufixed(a downto 0) + ufixed(c downto d) = ufixed(c+1 downto minimum(0,d))
  function "-" (l : NATURAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) * ufixed(a downto b) = ufixed(2a+1 downto 2b)
  function "*" (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;

  -- ufixed(c downto d) * ufixed(c downto d) = ufixed(2c+1 downto 2d)
  function "*" (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed (a downto b) * ufixed (a downto 0) = ufixed (2a+1 downto b)
  function "*" (l : UNRESOLVED_ufixed; r : NATURAL) return UNRESOLVED_ufixed;

  -- ufixed (a downto b) * ufixed (a downto 0) = ufixed (2a+1 downto b)
  function "*" (l : NATURAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) / ufixed(a downto b) = ufixed(a-b downto b-a-1)
  function "/" (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) / ufixed(a downto b) = ufixed(a-b downto b-a-1)
  function "/" (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed(a downto b) / ufixed(a downto 0) = ufixed(a downto b-a-1)
  function "/" (l : UNRESOLVED_ufixed; r : NATURAL) return UNRESOLVED_ufixed;

  -- ufixed(c downto 0) / ufixed(c downto d) = ufixed(c-d downto -c-1)
  function "/" (l : NATURAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed (a downto b) rem ufixed (a downto b) = ufixed (a downto b)
  function "rem" (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;

  -- ufixed (c downto d) rem ufixed (c downto d) = ufixed (c downto d)
  function "rem" (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed (a downto b) rem ufixed (a downto 0) = ufixed (a downto minimum(b,0))
  function "rem" (l : UNRESOLVED_ufixed; r : NATURAL) return UNRESOLVED_ufixed;

  -- ufixed (c downto 0) rem ufixed (c downto d) = ufixed (c downto minimum(d,0))
  function "rem" (l : NATURAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed (a downto b) mod ufixed (a downto b) = ufixed (a downto b)
  function "mod" (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;

  -- ufixed (c downto d) mod ufixed (c downto d) = ufixed (c downto d)
  function "mod" (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- ufixed (a downto b) mod ufixed (a downto 0) = ufixed (a downto minimum(b,0))
  function "mod" (l : UNRESOLVED_ufixed; r : NATURAL) return UNRESOLVED_ufixed;

  -- ufixed (c downto 0) mod ufixed (c downto d) = ufixed (c downto minimum(d,0))
  function "mod" (l : NATURAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;

  -- sfixed(a downto b) + sfixed(a downto b) = sfixed(a+1 downto b)
  function "+" (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;

  -- sfixed(c downto d) + sfixed(c downto d) = sfixed(c+1 downto d)
  function "+" (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) + sfixed(a downto 0) = sfixed(a+1 downto minimum(0,b))
  function "+" (l : UNRESOLVED_sfixed; r : INTEGER) return UNRESOLVED_sfixed;

  -- sfixed(c downto 0) + sfixed(c downto d) = sfixed(c+1 downto minimum(0,d))
  function "+" (l : INTEGER; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) - sfixed(a downto b) = sfixed(a+1 downto b)
  function "-" (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;

  -- sfixed(c downto d) - sfixed(c downto d) = sfixed(c+1 downto d)
  function "-" (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) - sfixed(a downto 0) = sfixed(a+1 downto minimum(0,b))
  function "-" (l : UNRESOLVED_sfixed; r : INTEGER) return UNRESOLVED_sfixed;

  -- sfixed(c downto 0) - sfixed(c downto d) = sfixed(c+1 downto minimum(0,d))
  function "-" (l : INTEGER; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) * sfixed(a downto b) = sfixed(2a+1 downto 2b)
  function "*" (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;

  -- sfixed(c downto d) * sfixed(c downto d) = sfixed(2c+1 downto 2d)
  function "*" (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) * sfixed(a downto 0) = sfixed(2a+1 downto b)
  function "*" (l : UNRESOLVED_sfixed; r : INTEGER) return UNRESOLVED_sfixed;

  -- sfixed(c downto 0) * sfixed(c downto d) = sfixed(2c+1 downto d)
  function "*" (l : INTEGER; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) / sfixed(a downto b) = sfixed(a-b+1 downto b-a)
  function "/" (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;

  -- sfixed(c downto d) / sfixed(c downto d) = sfixed(c-d+1 downto d-c)
  function "/" (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed(a downto b) / sfixed(a downto 0) = sfixed(a+1 downto b-a)
  function "/" (l : UNRESOLVED_sfixed; r : INTEGER) return UNRESOLVED_sfixed;

  -- sfixed(c downto 0) / sfixed(c downto d) = sfixed(c-d+1 downto -c)
  function "/" (l : INTEGER; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed (a downto b) rem sfixed (a downto b) = sfixed (a downto b)
  function "rem" (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;

  -- sfixed (c downto d) rem sfixed (c downto d) = sfixed (c downto d)
  function "rem" (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed (a downto b) rem sfixed (a downto 0) = sfixed (a downto minimum(b,0))
  function "rem" (l : UNRESOLVED_sfixed; r : INTEGER) return UNRESOLVED_sfixed;

  -- sfixed (c downto 0) rem sfixed (c downto d) = sfixed (c downto minimum(d,0))
  function "rem" (l : INTEGER; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed (a downto b) mod sfixed (a downto b) = sfixed (a downto b)
  function "mod" (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;

  -- sfixed (c downto d) mod sfixed (c downto d) = sfixed (c downto d)
  function "mod" (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- sfixed (a downto b) mod sfixed (a downto 0) = sfixed (a downto minimum(b,0))
  function "mod" (l : UNRESOLVED_sfixed; r : INTEGER) return UNRESOLVED_sfixed;

  -- sfixed (c downto 0) mod sfixed (c downto d) = sfixed (c downto minimum(d,0))
  function "mod" (l : INTEGER; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- This version of divide gives the user more control
  -- ufixed(a downto b) / ufixed(c downto d) = ufixed(a-d downto b-c-1)
  function divide (
    l, r                 : UNRESOLVED_ufixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed;

  -- This version of divide gives the user more control
  -- sfixed(a downto b) / sfixed(c downto d) = sfixed(a-d+1 downto b-c)
  function divide (
    l, r                 : UNRESOLVED_sfixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_sfixed;

  -- These functions return 1/X
  -- 1 / ufixed(a downto b) = ufixed(-b downto -a-1)
  function reciprocal (
    arg                  : UNRESOLVED_ufixed;  -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed;

  -- 1 / sfixed(a downto b) = sfixed(-b+1 downto -a)
  function reciprocal (
    arg                  : UNRESOLVED_sfixed;  -- fixed point input
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_sfixed;

  -- REM function
  -- ufixed (a downto b) rem ufixed (c downto d)
  --   = ufixed (minimum(a,c) downto minimum(b,d))
  function remainder (
    l, r                 : UNRESOLVED_ufixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed;

  -- sfixed (a downto b) rem sfixed (c downto d)
  --   = sfixed (minimum(a,c) downto minimum(b,d))
  function remainder (
    l, r                 : UNRESOLVED_sfixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_sfixed;

  -- mod function
  -- ufixed (a downto b) mod ufixed (c downto d)
  --        = ufixed (minimum(a,c) downto minimum(b, d))
  function modulo (
    l, r                 : UNRESOLVED_ufixed;
    constant round_style : fixed_round_style_type := fixed_round_style;
    constant guard_bits  : NATURAL                := fixed_guard_bits)
    return UNRESOLVED_ufixed;

  -- sfixed (a downto b) mod sfixed (c downto d)
  --        = sfixed (c downto minimum(b, d))
  function modulo (
    l, r                    : UNRESOLVED_sfixed;
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)
    return UNRESOLVED_sfixed;

  -- Procedure for those who need an "accumulator" function.
  -- add_carry (ufixed(a downto b), ufixed (c downto d))
  --         = ufixed (maximum(a,c) downto minimum(b,d))
  procedure add_carry (
    L, R   : in  UNRESOLVED_ufixed;
    c_in   : in  STD_ULOGIC;
    result : out UNRESOLVED_ufixed;
    c_out  : out STD_ULOGIC);

  -- add_carry (sfixed(a downto b), sfixed (c downto d))
  --         = sfixed (maximum(a,c) downto minimum(b,d))
  procedure add_carry (
    L, R   : in  UNRESOLVED_sfixed;
    c_in   : in  STD_ULOGIC;
    result : out UNRESOLVED_sfixed;
    c_out  : out STD_ULOGIC);

  -- Scales the result by a power of 2.  Width of input = width of output with
  -- the binary point moved.
  function scalb (y : UNRESOLVED_ufixed; N : INTEGER) return UNRESOLVED_ufixed;
  function scalb (y : UNRESOLVED_ufixed; N : UNRESOLVED_SIGNED) return UNRESOLVED_ufixed;
  function scalb (y : UNRESOLVED_sfixed; N : INTEGER) return UNRESOLVED_sfixed;
  function scalb (y : UNRESOLVED_sfixed; N : UNRESOLVED_SIGNED) return UNRESOLVED_sfixed;

  function Is_Negative (arg : UNRESOLVED_sfixed) return BOOLEAN;

  --===========================================================================
  -- Comparison Operators
  --===========================================================================

  function ">"  (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function ">"  (l, r : UNRESOLVED_sfixed) return BOOLEAN;
  function "<"  (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function "<"  (l, r : UNRESOLVED_sfixed) return BOOLEAN;
  function "<=" (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function "<=" (l, r : UNRESOLVED_sfixed) return BOOLEAN;
  function ">=" (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function ">=" (l, r : UNRESOLVED_sfixed) return BOOLEAN;
  function "="  (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function "="  (l, r : UNRESOLVED_sfixed) return BOOLEAN;
  function "/=" (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function "/=" (l, r : UNRESOLVED_sfixed) return BOOLEAN;

  function "?="  (l, r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?/=" (l, r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?>"  (l, r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?>=" (l, r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?<"  (l, r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?<=" (l, r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?="  (l, r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?/=" (l, r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?>"  (l, r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?>=" (l, r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?<"  (l, r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?<=" (l, r : UNRESOLVED_sfixed) return STD_ULOGIC;

  function std_match (l, r : UNRESOLVED_ufixed) return BOOLEAN;
  function std_match (l, r : UNRESOLVED_sfixed) return BOOLEAN;

  -- Overloads the default "maximum" and "minimum" function

  function maximum (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function minimum (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function maximum (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function minimum (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  ----------------------------------------------------------------------------
  -- In these compare functions a natural is converted into a
  -- fixed point number of the bounds "maximum(l'high,0) downto 0"
  ----------------------------------------------------------------------------

  function "="  (l : UNRESOLVED_ufixed; r : NATURAL) return BOOLEAN;
  function "/=" (l : UNRESOLVED_ufixed; r : NATURAL) return BOOLEAN;
  function ">=" (l : UNRESOLVED_ufixed; r : NATURAL) return BOOLEAN;
  function "<=" (l : UNRESOLVED_ufixed; r : NATURAL) return BOOLEAN;
  function ">"  (l : UNRESOLVED_ufixed; r : NATURAL) return BOOLEAN;
  function "<"  (l : UNRESOLVED_ufixed; r : NATURAL) return BOOLEAN;

  function "="  (l : NATURAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function "/=" (l : NATURAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function ">=" (l : NATURAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function "<=" (l : NATURAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function ">"  (l : NATURAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function "<"  (l : NATURAL; r : UNRESOLVED_ufixed) return BOOLEAN;

  function "?="  (l : UNRESOLVED_ufixed; r : NATURAL) return STD_ULOGIC;
  function "?/=" (l : UNRESOLVED_ufixed; r : NATURAL) return STD_ULOGIC;
  function "?>=" (l : UNRESOLVED_ufixed; r : NATURAL) return STD_ULOGIC;
  function "?<=" (l : UNRESOLVED_ufixed; r : NATURAL) return STD_ULOGIC;
  function "?>"  (l : UNRESOLVED_ufixed; r : NATURAL) return STD_ULOGIC;
  function "?<"  (l : UNRESOLVED_ufixed; r : NATURAL) return STD_ULOGIC;

  function "?="  (l : NATURAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?/=" (l : NATURAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?>=" (l : NATURAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?<=" (l : NATURAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?>"  (l : NATURAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?<"  (l : NATURAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;

  function maximum (l : UNRESOLVED_ufixed; r : NATURAL)
    return UNRESOLVED_ufixed;
  function minimum (l : UNRESOLVED_ufixed; r : NATURAL)
    return UNRESOLVED_ufixed;
  function maximum (l : NATURAL; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function minimum (l : NATURAL; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  ----------------------------------------------------------------------------
  -- In these compare functions a real is converted into a
  -- fixed point number of the bounds "l'high+1 downto l'low"
  ----------------------------------------------------------------------------

  function "="  (l : UNRESOLVED_ufixed; r : REAL) return BOOLEAN;
  function "/=" (l : UNRESOLVED_ufixed; r : REAL) return BOOLEAN;
  function ">=" (l : UNRESOLVED_ufixed; r : REAL) return BOOLEAN;
  function "<=" (l : UNRESOLVED_ufixed; r : REAL) return BOOLEAN;
  function ">"  (l : UNRESOLVED_ufixed; r : REAL) return BOOLEAN;
  function "<"  (l : UNRESOLVED_ufixed; r : REAL) return BOOLEAN;

  function "="  (l : REAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function "/=" (l : REAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function ">=" (l : REAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function "<=" (l : REAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function ">"  (l : REAL; r : UNRESOLVED_ufixed) return BOOLEAN;
  function "<"  (l : REAL; r : UNRESOLVED_ufixed) return BOOLEAN;

  function "?="  (l : UNRESOLVED_ufixed; r : REAL) return STD_ULOGIC;
  function "?/=" (l : UNRESOLVED_ufixed; r : REAL) return STD_ULOGIC;
  function "?>=" (l : UNRESOLVED_ufixed; r : REAL) return STD_ULOGIC;
  function "?<=" (l : UNRESOLVED_ufixed; r : REAL) return STD_ULOGIC;
  function "?>"  (l : UNRESOLVED_ufixed; r : REAL) return STD_ULOGIC;
  function "?<"  (l : UNRESOLVED_ufixed; r : REAL) return STD_ULOGIC;

  function "?="  (l : REAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?/=" (l : REAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?>=" (l : REAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?<=" (l : REAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?>"  (l : REAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "?<"  (l : REAL; r : UNRESOLVED_ufixed) return STD_ULOGIC;

  function maximum (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;
  function maximum (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function minimum (l : UNRESOLVED_ufixed; r : REAL) return UNRESOLVED_ufixed;
  function minimum (l : REAL; r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  ----------------------------------------------------------------------------
  -- In these compare functions an integer is converted into a
  -- fixed point number of the bounds "maximum(l'high,1) downto 0"
  ----------------------------------------------------------------------------

  function "="  (l : UNRESOLVED_sfixed; r : INTEGER) return BOOLEAN;
  function "/=" (l : UNRESOLVED_sfixed; r : INTEGER) return BOOLEAN;
  function ">=" (l : UNRESOLVED_sfixed; r : INTEGER) return BOOLEAN;
  function "<=" (l : UNRESOLVED_sfixed; r : INTEGER) return BOOLEAN;
  function ">"  (l : UNRESOLVED_sfixed; r : INTEGER) return BOOLEAN;
  function "<"  (l : UNRESOLVED_sfixed; r : INTEGER) return BOOLEAN;

  function "="  (l : INTEGER; r : UNRESOLVED_sfixed) return BOOLEAN;
  function "/=" (l : INTEGER; r : UNRESOLVED_sfixed) return BOOLEAN;
  function ">=" (l : INTEGER; r : UNRESOLVED_sfixed) return BOOLEAN;
  function "<=" (l : INTEGER; r : UNRESOLVED_sfixed) return BOOLEAN;
  function ">"  (l : INTEGER; r : UNRESOLVED_sfixed) return BOOLEAN;
  function "<"  (l : INTEGER; r : UNRESOLVED_sfixed) return BOOLEAN;

  function "?="  (l : UNRESOLVED_sfixed; r : INTEGER) return STD_ULOGIC;
  function "?/=" (l : UNRESOLVED_sfixed; r : INTEGER) return STD_ULOGIC;
  function "?>=" (l : UNRESOLVED_sfixed; r : INTEGER) return STD_ULOGIC;
  function "?<=" (l : UNRESOLVED_sfixed; r : INTEGER) return STD_ULOGIC;
  function "?>"  (l : UNRESOLVED_sfixed; r : INTEGER) return STD_ULOGIC;
  function "?<"  (l : UNRESOLVED_sfixed; r : INTEGER) return STD_ULOGIC;

  function "?="  (l : INTEGER; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?/=" (l : INTEGER; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?>=" (l : INTEGER; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?<=" (l : INTEGER; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?>"  (l : INTEGER; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?<"  (l : INTEGER; r : UNRESOLVED_sfixed) return STD_ULOGIC;

  function maximum (l : UNRESOLVED_sfixed; r : INTEGER)
    return UNRESOLVED_sfixed;
  function maximum (l : INTEGER; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function minimum (l : UNRESOLVED_sfixed; r : INTEGER)
    return UNRESOLVED_sfixed;
  function minimum (l : INTEGER; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  ----------------------------------------------------------------------------
  -- In these compare functions a real is converted into a
  -- fixed point number of the bounds "l'high+1 downto l'low"
  ----------------------------------------------------------------------------

  function "="  (l : UNRESOLVED_sfixed; r : REAL) return BOOLEAN;
  function "/=" (l : UNRESOLVED_sfixed; r : REAL) return BOOLEAN;
  function ">=" (l : UNRESOLVED_sfixed; r : REAL) return BOOLEAN;
  function "<=" (l : UNRESOLVED_sfixed; r : REAL) return BOOLEAN;
  function ">"  (l : UNRESOLVED_sfixed; r : REAL) return BOOLEAN;
  function "<"  (l : UNRESOLVED_sfixed; r : REAL) return BOOLEAN;

  function "="  (l : REAL; r : UNRESOLVED_sfixed) return BOOLEAN;
  function "/=" (l : REAL; r : UNRESOLVED_sfixed) return BOOLEAN;
  function ">=" (l : REAL; r : UNRESOLVED_sfixed) return BOOLEAN;
  function "<=" (l : REAL; r : UNRESOLVED_sfixed) return BOOLEAN;
  function ">"  (l : REAL; r : UNRESOLVED_sfixed) return BOOLEAN;
  function "<"  (l : REAL; r : UNRESOLVED_sfixed) return BOOLEAN;

  function "?="  (l : UNRESOLVED_sfixed; r : REAL) return STD_ULOGIC;
  function "?/=" (l : UNRESOLVED_sfixed; r : REAL) return STD_ULOGIC;
  function "?>=" (l : UNRESOLVED_sfixed; r : REAL) return STD_ULOGIC;
  function "?<=" (l : UNRESOLVED_sfixed; r : REAL) return STD_ULOGIC;
  function "?>"  (l : UNRESOLVED_sfixed; r : REAL) return STD_ULOGIC;
  function "?<"  (l : UNRESOLVED_sfixed; r : REAL) return STD_ULOGIC;

  function "?="  (l : REAL; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?/=" (l : REAL; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?>=" (l : REAL; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?<=" (l : REAL; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?>"  (l : REAL; r : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "?<"  (l : REAL; r : UNRESOLVED_sfixed) return STD_ULOGIC;

  function maximum (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;
  function maximum (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function minimum (l : UNRESOLVED_sfixed; r : REAL) return UNRESOLVED_sfixed;
  function minimum (l : REAL; r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  --===========================================================================
  -- Shift and Rotate Functions.
  -- Note that sra and sla are not the same as the BIT_VECTOR version
  --===========================================================================

  function "sll" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed;
  function "srl" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed;
  function "rol" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed;
  function "ror" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed;
  function "sla" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed;
  function "sra" (ARG : UNRESOLVED_ufixed; COUNT : INTEGER)
    return UNRESOLVED_ufixed;
  function "sll" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed;
  function "srl" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed;
  function "rol" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed;
  function "ror" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed;
  function "sla" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed;
  function "sra" (ARG : UNRESOLVED_sfixed; COUNT : INTEGER)
    return UNRESOLVED_sfixed;
  function SHIFT_LEFT  (ARG : UNRESOLVED_ufixed; COUNT : NATURAL)
    return UNRESOLVED_ufixed;
  function SHIFT_RIGHT (ARG : UNRESOLVED_ufixed; COUNT : NATURAL)
    return UNRESOLVED_ufixed;
  function SHIFT_LEFT  (ARG : UNRESOLVED_sfixed; COUNT : NATURAL)
    return UNRESOLVED_sfixed;
  function SHIFT_RIGHT (ARG : UNRESOLVED_sfixed; COUNT : NATURAL)
    return UNRESOLVED_sfixed;

  ----------------------------------------------------------------------------
  -- logical functions
  ----------------------------------------------------------------------------

  function "not"  (l    : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "and"  (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "or"   (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "nand" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "nor"  (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "xor"  (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "xnor" (l, r : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function "not"  (l    : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function "and"  (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function "or"   (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function "nand" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function "nor"  (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function "xor"  (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function "xnor" (l, r : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- Vector and std_ulogic functions, same as functions in numeric_std
  function "and"  (l : STD_ULOGIC; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function "and"  (l : UNRESOLVED_ufixed; r : STD_ULOGIC)
    return UNRESOLVED_ufixed;
  function "or"   (l : STD_ULOGIC; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function "or"   (l : UNRESOLVED_ufixed; r : STD_ULOGIC)
    return UNRESOLVED_ufixed;
  function "nand" (l : STD_ULOGIC; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function "nand" (l : UNRESOLVED_ufixed; r : STD_ULOGIC)
    return UNRESOLVED_ufixed;
  function "nor"  (l : STD_ULOGIC; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function "nor"  (l : UNRESOLVED_ufixed; r : STD_ULOGIC)
    return UNRESOLVED_ufixed;
  function "xor"  (l : STD_ULOGIC; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function "xor"  (l : UNRESOLVED_ufixed; r : STD_ULOGIC)
    return UNRESOLVED_ufixed;
  function "xnor" (l : STD_ULOGIC; r : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  function "xnor" (l : UNRESOLVED_ufixed; r : STD_ULOGIC)
    return UNRESOLVED_ufixed;
  function "and"  (l : STD_ULOGIC; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function "and"  (l : UNRESOLVED_sfixed; r : STD_ULOGIC)
    return UNRESOLVED_sfixed;
  function "or"   (l : STD_ULOGIC; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function "or"   (l : UNRESOLVED_sfixed; r : STD_ULOGIC)
    return UNRESOLVED_sfixed;
  function "nand" (l : STD_ULOGIC; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function "nand" (l : UNRESOLVED_sfixed; r : STD_ULOGIC)
    return UNRESOLVED_sfixed;
  function "nor"  (l : STD_ULOGIC; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function "nor"  (l : UNRESOLVED_sfixed; r : STD_ULOGIC)
    return UNRESOLVED_sfixed;
  function "xor"  (l : STD_ULOGIC; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function "xor"  (l : UNRESOLVED_sfixed; r : STD_ULOGIC)
    return UNRESOLVED_sfixed;
  function "xnor" (l : STD_ULOGIC; r : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  function "xnor" (l : UNRESOLVED_sfixed; r : STD_ULOGIC)
    return UNRESOLVED_sfixed;

  -- Reduction operators, same as numeric_std functions
  function "and"  (l : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "nand" (l : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "or"   (l : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "nor"  (l : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "xor"  (l : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "xnor" (l : UNRESOLVED_ufixed) return STD_ULOGIC;
  function "and"  (l : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "nand" (l : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "or"   (l : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "nor"  (l : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "xor"  (l : UNRESOLVED_sfixed) return STD_ULOGIC;
  function "xnor" (l : UNRESOLVED_sfixed) return STD_ULOGIC;

  -- returns arg'low-1 if not found
  function find_leftmost (arg : UNRESOLVED_ufixed; y : STD_ULOGIC)
    return INTEGER;
  function find_leftmost (arg : UNRESOLVED_sfixed; y : STD_ULOGIC)
    return INTEGER;

  -- returns arg'high+1 if not found
  function find_rightmost (arg : UNRESOLVED_ufixed; y : STD_ULOGIC)
    return INTEGER;
  function find_rightmost (arg : UNRESOLVED_sfixed; y : STD_ULOGIC)
    return INTEGER;

  --===========================================================================
  --   RESIZE Functions
  --===========================================================================
  -- resizes the number (larger or smaller)
  -- The returned result will be ufixed (left_index downto right_index)
  -- If "round_style" is fixed_round, then the result will be rounded.
  -- If the MSB of the remainder is a "1" AND the LSB of the unrounded result
  -- is a '1' or the lower bits of the remainder include a '1' then the result
  -- will be increased by the smallest representable number for that type.
  -- "overflow_style" can be fixed_saturate or fixed_wrap.
  -- In saturate mode, if the number overflows then the largest possible
  -- representable number is returned.  If wrap mode, then the upper bits
  -- of the number are truncated.

  function resize (
    arg                     : UNRESOLVED_ufixed;  -- input
    constant left_index     : INTEGER;  -- integer portion
    constant right_index    : INTEGER;  -- size of fraction
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  -- "size_res" functions create the size of the output from the indices
  -- of the "size_res" input.  The actual value of "size_res" is not used.
  function resize (
    arg                     : UNRESOLVED_ufixed;  -- input
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  -- Note that in "wrap" mode the sign bit is not replicated.  Thus the
  -- resize of a negative number can have a positive result in wrap mode.
  function resize (
    arg                     : UNRESOLVED_sfixed;  -- input
    constant left_index     : INTEGER;            -- integer portion
    constant right_index    : INTEGER;            -- size of fraction
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  function resize (
    arg                     : UNRESOLVED_sfixed;  -- input
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  --===========================================================================
  -- Conversion Functions
  --===========================================================================

  -- integer (natural) to unsigned fixed point.
  -- arguments are the upper and lower bounds of the number, thus
  -- ufixed (7 downto -3) <= to_ufixed (int, 7, -3);
  function to_ufixed (
    arg                     : NATURAL;  -- integer
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  function to_ufixed (
    arg                     : NATURAL;            -- integer
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  -- real to unsigned fixed point
  function to_ufixed (
    arg                     : REAL;     -- real
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)
    return UNRESOLVED_ufixed;

  function to_ufixed (
    arg                     : REAL;     -- real
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)
    return UNRESOLVED_ufixed;

  -- unsigned to unsigned fixed point
  function to_ufixed (
    arg                     : UNRESOLVED_UNSIGNED;             -- unsigned
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  function to_ufixed (
    arg                     : UNRESOLVED_UNSIGNED;           -- unsigned
    size_res                : UNRESOLVED_ufixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_ufixed;

  -- Performs a conversion.  ufixed (arg'range) is returned
  function to_ufixed (
    arg : UNRESOLVED_UNSIGNED)          -- unsigned
    return UNRESOLVED_ufixed;

  -- unsigned fixed point to unsigned
  function to_unsigned (
    arg                     : UNRESOLVED_ufixed;  -- fixed point input
    constant size           : NATURAL;            -- length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_UNSIGNED;

  -- unsigned fixed point to unsigned
  function to_unsigned (
    arg                     : UNRESOLVED_ufixed;    -- fixed point input
    size_res                : UNRESOLVED_UNSIGNED;  -- used for length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_UNSIGNED;

  -- unsigned fixed point to real
  function to_real (
    arg : UNRESOLVED_ufixed)            -- fixed point input
    return REAL;

  -- unsigned fixed point to integer
  function to_integer (
    arg                     : UNRESOLVED_ufixed;  -- fixed point input
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return NATURAL;

  -- Integer to UNRESOLVED_sfixed
  function to_sfixed (
    arg                     : INTEGER;   -- integer
    constant left_index     : INTEGER;   -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  function to_sfixed (
    arg                     : INTEGER;            -- integer
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  -- Real to sfixed
  function to_sfixed (
    arg                     : REAL;     -- real
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)
    return UNRESOLVED_sfixed;

  function to_sfixed (
    arg                     : REAL;     -- real
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style;
    constant guard_bits     : NATURAL                   := fixed_guard_bits)
    return UNRESOLVED_sfixed;

  -- signed to sfixed
  function to_sfixed (
    arg                     : UNRESOLVED_SIGNED;               -- signed
    constant left_index     : INTEGER;  -- left index (high index)
    constant right_index    : INTEGER                   := 0;  -- right index
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  function to_sfixed (
    arg                     : UNRESOLVED_SIGNED;  -- signed
    size_res                : UNRESOLVED_sfixed;  -- for size only
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_sfixed;

  -- signed to sfixed (output assumed to be size of signed input)
  function to_sfixed (
    arg : UNRESOLVED_SIGNED)            -- signed
    return UNRESOLVED_sfixed;

  -- Conversion from ufixed to sfixed
  function to_sfixed (
    arg : UNRESOLVED_ufixed)
    return UNRESOLVED_sfixed;

  -- signed fixed point to signed
  function to_signed (
    arg                     : UNRESOLVED_sfixed;  -- fixed point input
    constant size           : NATURAL;            -- length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_SIGNED;

  -- signed fixed point to signed
  function to_signed (
    arg                     : UNRESOLVED_sfixed;  -- fixed point input
    size_res                : UNRESOLVED_SIGNED;  -- used for length of output
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return UNRESOLVED_SIGNED;

  -- signed fixed point to real
  function to_real (
    arg : UNRESOLVED_sfixed)            -- fixed point input
    return REAL;

  -- signed fixed point to integer
  function to_integer (
    arg                     : UNRESOLVED_sfixed;  -- fixed point input
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;
    constant round_style    : fixed_round_style_type    := fixed_round_style)
    return INTEGER;

  -- Because of the fairly complicated sizing rules in the fixed point
  -- packages these functions are provided to compute the result ranges
  -- Example:
  -- signal uf1 : ufixed (3 downto -3);
  -- signal uf2 : ufixed (4 downto -2);
  -- signal uf1multuf2 : ufixed (ufixed_high (3, -3, '*', 4, -2) downto
  --                             ufixed_low (3, -3, '*', 4, -2));
  -- uf1multuf2 <= uf1 * uf2;
  -- Valid characters: '+', '-', '*', '/', 'r' or 'R' (rem), 'm' or 'M' (mod),
  --                   '1' (reciprocal), 'a' or 'A' (abs), 'n' or 'N' (unary -)
  function ufixed_high (left_index, right_index   : INTEGER;
                        operation                 : CHARACTER := 'X';
                        left_index2, right_index2 : INTEGER   := 0)
    return INTEGER;

  function ufixed_low (left_index, right_index   : INTEGER;
                       operation                 : CHARACTER := 'X';
                       left_index2, right_index2 : INTEGER   := 0)
    return INTEGER;

  function sfixed_high (left_index, right_index   : INTEGER;
                        operation                 : CHARACTER := 'X';
                        left_index2, right_index2 : INTEGER   := 0)
    return INTEGER;

  function sfixed_low (left_index, right_index   : INTEGER;
                       operation                 : CHARACTER := 'X';
                       left_index2, right_index2 : INTEGER   := 0)
    return INTEGER;

  -- Same as above, but using the "size_res" input only for their ranges:
  -- signal uf1multuf2 : ufixed (ufixed_high (uf1, '*', uf2) downto
  --                             ufixed_low (uf1, '*', uf2));
  -- uf1multuf2 <= uf1 * uf2;
  --
  function ufixed_high (size_res  : UNRESOLVED_ufixed;
                        operation : CHARACTER := 'X';
                        size_res2 : UNRESOLVED_ufixed)
    return INTEGER;

  function ufixed_low (size_res  : UNRESOLVED_ufixed;
                       operation : CHARACTER := 'X';
                       size_res2 : UNRESOLVED_ufixed)
    return INTEGER;

  function sfixed_high (size_res  : UNRESOLVED_sfixed;
                        operation : CHARACTER := 'X';
                        size_res2 : UNRESOLVED_sfixed)
    return INTEGER;

  function sfixed_low (size_res  : UNRESOLVED_sfixed;
                       operation : CHARACTER := 'X';
                       size_res2 : UNRESOLVED_sfixed)
    return INTEGER;

  -- purpose: returns a saturated number
  function saturate (
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed;

  -- purpose: returns a saturated number
  function saturate (
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed;

  function saturate (
    size_res : UNRESOLVED_ufixed)       -- only the size of this is used
    return UNRESOLVED_ufixed;

  function saturate (
    size_res : UNRESOLVED_sfixed)       -- only the size of this is used
    return UNRESOLVED_sfixed;

  --===========================================================================
  -- Translation Functions
  --===========================================================================

  -- maps meta-logical values
  function to_01 (
    s             : UNRESOLVED_ufixed;  -- fixed point input
    constant XMAP : STD_ULOGIC := '0')  -- Map x to
    return UNRESOLVED_ufixed;

  -- maps meta-logical values
  function to_01 (
    s             : UNRESOLVED_sfixed;  -- fixed point input
    constant XMAP : STD_ULOGIC := '0')  -- Map x to
    return UNRESOLVED_sfixed;

  function Is_X    (arg : UNRESOLVED_ufixed) return BOOLEAN;
  function Is_X    (arg : UNRESOLVED_sfixed) return BOOLEAN;
  function to_X01  (arg : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function to_X01  (arg : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function to_X01Z (arg : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function to_X01Z (arg : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;
  function to_UX01 (arg : UNRESOLVED_ufixed) return UNRESOLVED_ufixed;
  function to_UX01 (arg : UNRESOLVED_sfixed) return UNRESOLVED_sfixed;

  -- straight vector conversion routines, needed for synthesis.
  -- These functions are here so that a std_logic_vector can be
  -- converted to and from sfixed and ufixed.  Note that you can
  -- not convert these vectors because of their negative index.

  function to_slv (
    arg : UNRESOLVED_ufixed)            -- fixed point vector
    return STD_LOGIC_VECTOR;
  alias to_StdLogicVector is to_slv [UNRESOLVED_ufixed
                                     return STD_LOGIC_VECTOR];
  alias to_Std_Logic_Vector is to_slv [UNRESOLVED_ufixed
                                       return STD_LOGIC_VECTOR];

  function to_slv (
    arg : UNRESOLVED_sfixed)            -- fixed point vector
    return STD_LOGIC_VECTOR;
  alias to_StdLogicVector is to_slv [UNRESOLVED_sfixed
                                     return STD_LOGIC_VECTOR];
  alias to_Std_Logic_Vector is to_slv [UNRESOLVED_sfixed
                                       return STD_LOGIC_VECTOR];

  function to_sulv (
    arg : UNRESOLVED_ufixed)            -- fixed point vector
    return STD_ULOGIC_VECTOR;
  alias to_StdULogicVector is to_sulv [UNRESOLVED_ufixed
                                      return STD_ULOGIC_VECTOR];
  alias to_Std_ULogic_Vector is to_sulv [UNRESOLVED_ufixed
                                        return STD_ULOGIC_VECTOR];

  function to_sulv (
    arg : UNRESOLVED_sfixed)            -- fixed point vector
    return STD_ULOGIC_VECTOR;
  alias to_StdULogicVector is to_sulv [UNRESOLVED_sfixed
                                      return STD_ULOGIC_VECTOR];
  alias to_Std_ULogic_Vector is to_sulv [UNRESOLVED_sfixed
                                        return STD_ULOGIC_VECTOR];

  function to_ufixed (
    arg                  : STD_ULOGIC_VECTOR;  -- shifted vector
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed;

  function to_ufixed (
    arg      : STD_ULOGIC_VECTOR;       -- shifted vector
    size_res : UNRESOLVED_ufixed)       -- for size only
    return UNRESOLVED_ufixed;

  function to_sfixed (
    arg                  : STD_ULOGIC_VECTOR;  -- shifted vector
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed;

  function to_sfixed (
    arg      : STD_ULOGIC_VECTOR;       -- shifted vector
    size_res : UNRESOLVED_sfixed)       -- for size only
    return UNRESOLVED_sfixed;

  -- As a concession to those who use a graphical DSP environment,
  -- these functions take parameters in those tools format and create
  -- fixed point numbers.  These functions are designed to convert from
  -- a std_logic_vector to the VHDL fixed point format using the conventions
  -- of these packages.  In a pure VHDL environment you should use the
  -- "to_ufixed" and "to_sfixed" routines.

  -- unsigned fixed point
  function to_UFix (
    arg      : STD_ULOGIC_VECTOR;
    width    : NATURAL;                 -- width of vector
    fraction : NATURAL)                 -- width of fraction
    return UNRESOLVED_ufixed;

  -- signed fixed point
  function to_SFix (
    arg      : STD_ULOGIC_VECTOR;
    width    : NATURAL;                 -- width of vector
    fraction : NATURAL)                 -- width of fraction
    return UNRESOLVED_sfixed;

  -- finding the bounds of a number.  These functions can be used like this:
  -- signal xxx : ufixed (7 downto -3);
  -- -- Which is the same as "ufixed (UFix_high (11,3) downto UFix_low(11,3))"
  -- signal yyy : ufixed (UFix_high (11, 3, "+", 11, 3)
  --               downto UFix_low(11, 3, "+", 11, 3));
  -- Where "11" is the width of xxx (xxx'length),
  -- and 3 is the lower bound (abs (xxx'low))
  -- In a pure VHDL environment use "ufixed_high" and "ufixed_low"

  function UFix_high (width, fraction   : NATURAL;
                      operation         : CHARACTER := 'X';
                      width2, fraction2 : NATURAL   := 0)
    return INTEGER;

  function UFix_low (width, fraction   : NATURAL;
                     operation         : CHARACTER := 'X';
                     width2, fraction2 : NATURAL   := 0)
    return INTEGER;

  -- Same as above but for signed fixed point.  Note that the width
  -- of a signed fixed point number ignores the sign bit, thus
  -- width = sxxx'length-1

  function SFix_high (width, fraction   : NATURAL;
                      operation         : CHARACTER := 'X';
                      width2, fraction2 : NATURAL   := 0)
    return INTEGER;

  function SFix_low (width, fraction   : NATURAL;
                     operation         : CHARACTER := 'X';
                     width2, fraction2 : NATURAL   := 0)
    return INTEGER;

  --===========================================================================
  -- string and textio Functions
  --===========================================================================

  -- purpose: writes fixed point into a line
  procedure WRITE (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_ufixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0);

  -- purpose: writes fixed point into a line
  procedure WRITE (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_sfixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0);

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_ufixed);

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_ufixed;
                 GOOD  : out   BOOLEAN);

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_sfixed);

  procedure READ(L     : inout LINE;
                 VALUE : out   UNRESOLVED_sfixed;
                 GOOD  : out   BOOLEAN);

  alias bwrite is WRITE [LINE, UNRESOLVED_ufixed, SIDE, width];
  alias bwrite is WRITE [LINE, UNRESOLVED_sfixed, SIDE, width];
  alias bread is READ [LINE, UNRESOLVED_ufixed];
  alias bread is READ [LINE, UNRESOLVED_ufixed, BOOLEAN];
  alias bread is READ [LINE, UNRESOLVED_sfixed];
  alias bread is READ [LINE, UNRESOLVED_sfixed, BOOLEAN];
  alias BINARY_WRITE is WRITE [LINE, UNRESOLVED_ufixed, SIDE, width];
  alias BINARY_WRITE is WRITE [LINE, UNRESOLVED_sfixed, SIDE, width];
  alias BINARY_READ is READ [LINE, UNRESOLVED_ufixed, BOOLEAN];
  alias BINARY_READ is READ [LINE, UNRESOLVED_ufixed];
  alias BINARY_READ is READ [LINE, UNRESOLVED_sfixed, BOOLEAN];
  alias BINARY_READ is READ [LINE, UNRESOLVED_sfixed];

  -- octal read and write
  procedure OWRITE (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_ufixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0);

  procedure OWRITE (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_sfixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0);

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed);

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed;
                  GOOD  : out   BOOLEAN);

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed);

  procedure OREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed;
                  GOOD  : out   BOOLEAN);
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_ufixed, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_ufixed];
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_sfixed, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_sfixed];
  alias OCTAL_WRITE is OWRITE [LINE, UNRESOLVED_ufixed, SIDE, WIDTH];
  alias OCTAL_WRITE is OWRITE [LINE, UNRESOLVED_sfixed, SIDE, WIDTH];

  -- hex read and write
  procedure HWRITE (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_ufixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0);

  -- purpose: writes fixed point into a line
  procedure HWRITE (
    L         : inout LINE;               -- input line
    VALUE     : in    UNRESOLVED_sfixed;  -- fixed point input
    JUSTIFIED : in    SIDE  := right;
    FIELD     : in    WIDTH := 0);

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed);

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_ufixed;
                  GOOD  : out   BOOLEAN);

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed);

  procedure HREAD(L     : inout LINE;
                  VALUE : out   UNRESOLVED_sfixed;
                  GOOD  : out   BOOLEAN);
  alias HEX_READ is HREAD [LINE, UNRESOLVED_ufixed, BOOLEAN];
  alias HEX_READ is HREAD [LINE, UNRESOLVED_sfixed, BOOLEAN];
  alias HEX_READ is HREAD [LINE, UNRESOLVED_ufixed];
  alias HEX_READ is HREAD [LINE, UNRESOLVED_sfixed];
  alias HEX_WRITE is HWRITE [LINE, UNRESOLVED_ufixed, SIDE, WIDTH];
  alias HEX_WRITE is HWRITE [LINE, UNRESOLVED_sfixed, SIDE, WIDTH];

  -- returns a string, useful for:
  -- assert (x = y) report "error found " & TO_STRING(x) severity error;
  function TO_STRING (value : UNRESOLVED_ufixed) return STRING;

  alias TO_BSTRING is TO_STRING [UNRESOLVED_ufixed return STRING];
  alias TO_BINARY_STRING is TO_STRING [UNRESOLVED_ufixed return STRING];

  function TO_OSTRING (value : UNRESOLVED_ufixed) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [UNRESOLVED_ufixed return STRING];

  function TO_HSTRING (value : UNRESOLVED_ufixed) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [UNRESOLVED_ufixed return STRING];

  function TO_STRING (value : UNRESOLVED_sfixed) return STRING;
  alias TO_BSTRING is TO_STRING [UNRESOLVED_sfixed return STRING];
  alias TO_BINARY_STRING is TO_STRING [UNRESOLVED_sfixed return STRING];

  function TO_OSTRING (value : UNRESOLVED_sfixed) return STRING;
  alias TO_OCTAL_STRING is TO_OSTRING [UNRESOLVED_sfixed return STRING];

  function TO_HSTRING (value : UNRESOLVED_sfixed) return STRING;
  alias TO_HEX_STRING is TO_HSTRING [UNRESOLVED_sfixed return STRING];

  -- From string functions allow you to convert a string into a fixed
  -- point number.  Example:
  --  signal uf1 : ufixed (3 downto -3);
  --  uf1 <= from_string ("0110.100", uf1'high, uf1'low); -- 6.5
  -- The "." is optional in this syntax, however it exist and is
  -- in the wrong location an error is produced.  Overflow will
  -- result in saturation.

  function from_string (
    bstring              : STRING;      -- binary string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed;
  alias from_bstring is from_string [STRING, INTEGER, INTEGER
                                     return UNRESOLVED_ufixed];
  alias from_binary_string is from_string [STRING, INTEGER, INTEGER
                                           return UNRESOLVED_ufixed];

  -- Octal and hex conversions work as follows:
  -- uf1 <= from_hstring ("6.8", 3, -3); -- 6.5 (bottom zeros dropped)
  -- uf1 <= from_ostring ("06.4", 3, -3); -- 6.5 (top zeros dropped)

  function from_ostring (
    ostring              : STRING;      -- Octal string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed;
  alias from_octal_string is from_ostring [STRING, INTEGER, INTEGER
                                           return UNRESOLVED_ufixed];

  function from_hstring (
    hstring              : STRING;      -- hex string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_ufixed;
  alias from_hex_string is from_hstring [STRING, INTEGER, INTEGER
                                         return UNRESOLVED_ufixed];

  function from_string (
    bstring              : STRING;      -- binary string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed;
  alias from_bstring is from_string [STRING, INTEGER, INTEGER
                                     return UNRESOLVED_sfixed];
  alias from_binary_string is from_string [STRING, INTEGER, INTEGER
                                           return UNRESOLVED_sfixed];

  function from_ostring (
    ostring              : STRING;      -- Octal string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed;
  alias from_octal_string is from_ostring [STRING, INTEGER, INTEGER
                                           return UNRESOLVED_sfixed];

  function from_hstring (
    hstring              : STRING;      -- hex string
    constant left_index  : INTEGER;
    constant right_index : INTEGER)
    return UNRESOLVED_sfixed;
  alias from_hex_string is from_hstring [STRING, INTEGER, INTEGER
                                         return UNRESOLVED_sfixed];

  -- Same as above, "size_res" is used for it's range only.
  function from_string (
    bstring  : STRING;                  -- binary string
    size_res : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  alias from_bstring is from_string [STRING, UNRESOLVED_ufixed
                                     return UNRESOLVED_ufixed];
  alias from_binary_string is from_string [STRING, UNRESOLVED_ufixed
                                           return UNRESOLVED_ufixed];

  function from_ostring (
    ostring  : STRING;                  -- Octal string
    size_res : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  alias from_octal_string is from_ostring [STRING, UNRESOLVED_ufixed
                                           return UNRESOLVED_ufixed];

  function from_hstring (
    hstring  : STRING;                  -- hex string
    size_res : UNRESOLVED_ufixed)
    return UNRESOLVED_ufixed;
  alias from_hex_string is from_hstring [STRING, UNRESOLVED_ufixed
                                         return UNRESOLVED_ufixed];

  function from_string (
    bstring  : STRING;                  -- binary string
    size_res : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  alias from_bstring is from_string [STRING, UNRESOLVED_sfixed
                                     return UNRESOLVED_sfixed];
  alias from_binary_string is from_string [STRING, UNRESOLVED_sfixed
                                           return UNRESOLVED_sfixed];

  function from_ostring (
    ostring  : STRING;                  -- Octal string
    size_res : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  alias from_octal_string is from_ostring [STRING, UNRESOLVED_sfixed
                                           return UNRESOLVED_sfixed];

  function from_hstring (
    hstring  : STRING;                  -- hex string
    size_res : UNRESOLVED_sfixed)
    return UNRESOLVED_sfixed;
  alias from_hex_string is from_hstring [STRING, UNRESOLVED_sfixed
                                         return UNRESOLVED_sfixed];

  -- Direct conversion functions.  Example:
  --  signal uf1 : ufixed (3 downto -3);
  --  uf1 <= from_string ("0110.100"); -- 6.5
  -- In this case the "." is not optional, and the size of
  -- the output must match exactly.

  function from_string (
    bstring : STRING)                   -- binary string
    return UNRESOLVED_ufixed;
  alias from_bstring is from_string [STRING return UNRESOLVED_ufixed];
  alias from_binary_string is from_string [STRING return UNRESOLVED_ufixed];

  -- Direct octal and hex conversion functions.  In this case
  -- the string lengths must match.  Example:
  -- signal sf1 := sfixed (5 downto -3);
  -- sf1 <= from_ostring ("71.4") -- -6.5

  function from_ostring (
    ostring : STRING)                   -- Octal string
    return UNRESOLVED_ufixed;
  alias from_octal_string is from_ostring [STRING return UNRESOLVED_ufixed];

  function from_hstring (
    hstring : STRING)                   -- hex string
    return UNRESOLVED_ufixed;
  alias from_hex_string is from_hstring [STRING return UNRESOLVED_ufixed];

  function from_string (
    bstring : STRING)                   -- binary string
    return UNRESOLVED_sfixed;
  alias from_bstring is from_string [STRING return UNRESOLVED_sfixed];
  alias from_binary_string is from_string [STRING return UNRESOLVED_sfixed];

  function from_ostring (
    ostring : STRING)                   -- Octal string
    return UNRESOLVED_sfixed;
  alias from_octal_string is from_ostring [STRING return UNRESOLVED_sfixed];

  function from_hstring (
    hstring : STRING)                   -- hex string
    return UNRESOLVED_sfixed;
  alias from_hex_string is from_hstring [STRING return UNRESOLVED_sfixed];

end package fixed_generic_pkg;
