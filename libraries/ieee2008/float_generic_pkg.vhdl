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
--   Title     :  Floating-point package (Generic package declaration)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  Accellera VHDL-TC and IEEE P1076 Working Group
--             :
--   Purpose   :  This packages defines basic binary floating point
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

package float_generic_pkg is
  generic (
    -- Defaults for sizing routines, when you do a "to_float" this will be
    -- the default size.  Example float32 would be 8 and 23 (8 downto -23)
    float_exponent_width : NATURAL    := 8;
    float_fraction_width : NATURAL    := 23;
    -- Rounding algorithm, "round_nearest" is default, other valid values
    -- are "round_zero" (truncation), "round_inf" (round up), and
    -- "round_neginf" (round down)
    float_round_style    : round_type := round_nearest;
    -- Denormal numbers (very small numbers near zero) true or false
    float_denormalize    : BOOLEAN    := true;
    -- Turns on NAN processing (invalid numbers and overflow) true of false
    float_check_error    : BOOLEAN    := true;
    -- Guard bits are added to the bottom of every operation for rounding.
    -- any natural number (including 0) are valid.
    float_guard_bits     : NATURAL    := 3;
    -- If TRUE, then turn off warnings on "X" propagation
    no_warning           : BOOLEAN    := false;
    package fixed_pkg is new IEEE.fixed_generic_pkg
                           generic map (<>) );

  -- Author David Bishop (dbishop@vhdl.org)
  constant CopyRightNotice : STRING :=
    "Copyright IEEE P1076 WG. Licensed Apache 2.0";

  use fixed_pkg.all;

  -- Note that this is "INTEGER range <>", thus if you use a literal, then the
  -- default range will be (INTEGER'low to INTEGER'low + X)
  type UNRESOLVED_float is array (INTEGER range <>) of STD_ULOGIC;  -- main type
  alias U_float is UNRESOLVED_float;

  subtype float is (resolved) UNRESOLVED_float;
  -----------------------------------------------------------------------------
  -- Use the float type to define your own floating point numbers.
  -- There must be a negative index or the packages will error out.
  -- Minimum supported is "subtype float7 is float (3 downto -3);"
  -- "subtype float16 is float (6 downto -9);" is probably the smallest
  -- practical one to use.
  -----------------------------------------------------------------------------

  -- IEEE 754 single precision
  subtype UNRESOLVED_float32 is UNRESOLVED_float (8 downto -23);
  alias U_float32 is UNRESOLVED_float32;
  subtype float32 is float (8 downto -23);
  -----------------------------------------------------------------------------
  -- IEEE-754 single precision floating point.  This is a "float"
  -- in C, and a FLOAT in Fortran.  The exponent is 8 bits wide, and
  -- the fraction is 23 bits wide.  This format can hold roughly 7 decimal
  -- digits.  Infinity is 2**127 = 1.7E38 in this number system.
  -- The bit representation is as follows:
  -- 1 09876543 21098765432109876543210
  -- 8 76543210 12345678901234567890123
  -- 0 00000000 00000000000000000000000
  -- 8 7      0 -1                  -23
  -- +/-   exp.  fraction
  -----------------------------------------------------------------------------

  -- IEEE 754 double precision
  subtype UNRESOLVED_float64 is UNRESOLVED_float (11 downto -52);
  alias U_float64 is UNRESOLVED_float64;
  subtype float64 is float (11 downto -52);
  -----------------------------------------------------------------------------
  -- IEEE-754 double precision floating point.  This is a "double float"
  -- in C, and a FLOAT*8 in Fortran.  The exponent is 11 bits wide, and
  -- the fraction is 52 bits wide.  This format can hold roughly 15 decimal
  -- digits.  Infinity is 2**2047 in this number system.
  -- The bit representation is as follows:
  --  3 21098765432 1098765432109876543210987654321098765432109876543210
  --  1 09876543210 1234567890123456789012345678901234567890123456789012
  --  S EEEEEEEEEEE FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
  -- 11 10        0 -1                                               -52
  -- +/-  exponent    fraction
  -----------------------------------------------------------------------------

  -- IEEE 854 & C extended precision
  subtype UNRESOLVED_float128 is UNRESOLVED_float (15 downto -112);
  alias U_float128 is UNRESOLVED_float128;
  subtype float128 is float (15 downto -112);
  -----------------------------------------------------------------------------
  -- The 128 bit floating point number is "long double" in C (on
  -- some systems this is a 70 bit floating point number) and FLOAT*32
  -- in Fortran.  The exponent is 15 bits wide and the fraction is 112
  -- bits wide. This number can handle approximately 33 decimal digits.
  -- Infinity is 2**32,767 in this number system.
  -----------------------------------------------------------------------------

  -- purpose: Checks for a valid floating point number
  type valid_fpstate is (nan,           -- Signaling NaN (C FP_NAN)
                         quiet_nan,     -- Quiet NaN (C FP_NAN)
                         neg_inf,       -- Negative infinity (C FP_INFINITE)
                         neg_normal,    -- negative normalized nonzero
                         neg_denormal,  -- negative denormalized (FP_SUBNORMAL)
                         neg_zero,      -- -0 (C FP_ZERO)
                         pos_zero,      -- +0 (C FP_ZERO)
                         pos_denormal,  -- Positive denormalized (FP_SUBNORMAL)
                         pos_normal,    -- positive normalized nonzero
                         pos_inf,       -- positive infinity
                         isx);          -- at least one input is unknown

  -- This deferred constant will tell you if the package body is synthesizable
  -- or implemented as real numbers.
  constant fphdlsynth_or_real : BOOLEAN;  -- deferred constant

  -- Returns the class which X falls into
  function Classfp (
    x           : UNRESOLVED_float;              -- floating point input
    check_error : BOOLEAN := float_check_error)  -- check for errors
    return valid_fpstate;

  -- Arithmetic functions, these operators do not require parameters.
  function "abs" (arg : UNRESOLVED_float) return UNRESOLVED_float;
  function "-"   (arg : UNRESOLVED_float) return UNRESOLVED_float;

  -- These allows the base math functions to use the default values
  -- of their parameters.  Thus they do full IEEE floating point.

  function "+"   (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "-"   (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "*"   (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "/"   (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "rem" (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "mod" (l, r : UNRESOLVED_float) return UNRESOLVED_float;

  -- Basic parameter list
  -- round_style - Selects the rounding algorithm to use
  -- guard - extra bits added to the end if the operation to add precision
  -- check_error - When "false" turns off NAN and overflow checks
  -- denormalize - When "false" turns off denormal number processing

  function add (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function subtract (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function multiply (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function divide (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function remainder (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function modulo (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- reciprocal
  function reciprocal (
    arg                  : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function dividebyp2 (
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- Multiply accumulate  result = l*r + c
  function mac (
    l, r, c              : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant guard       : NATURAL    := float_guard_bits;  -- number of guard bits
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- Square root (all 754 based implementations need this)
  function sqrt (
    arg                  : UNRESOLVED_float;       -- floating point input
    constant round_style : round_type := float_round_style;
    constant guard       : NATURAL    := float_guard_bits;
    constant check_error : BOOLEAN    := float_check_error;
    constant denormalize : BOOLEAN    := float_denormalize)
    return UNRESOLVED_float;

  function Is_Negative (arg : UNRESOLVED_float) return BOOLEAN;

  -----------------------------------------------------------------------------
  -- compare functions
  -- =, /=, >=, <=, <, >, maximum, minimum

  function eq (                               -- equal =
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;
    constant denormalize : BOOLEAN := float_denormalize)
    return BOOLEAN;

  function ne (                               -- not equal /=
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;
    constant denormalize : BOOLEAN := float_denormalize)
    return BOOLEAN;

  function lt (                               -- less than <
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;
    constant denormalize : BOOLEAN := float_denormalize)
    return BOOLEAN;

  function gt (                               -- greater than >
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;
    constant denormalize : BOOLEAN := float_denormalize)
    return BOOLEAN;

  function le (                               -- less than or equal to <=
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;
    constant denormalize : BOOLEAN := float_denormalize)
    return BOOLEAN;

  function ge (                               -- greater than or equal to >=
    l, r                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;
    constant denormalize : BOOLEAN := float_denormalize)
    return BOOLEAN;

  -- Need to overload the default versions of these
  function "="  (l, r : UNRESOLVED_float) return BOOLEAN;
  function "/=" (l, r : UNRESOLVED_float) return BOOLEAN;
  function ">=" (l, r : UNRESOLVED_float) return BOOLEAN;
  function "<=" (l, r : UNRESOLVED_float) return BOOLEAN;
  function ">"  (l, r : UNRESOLVED_float) return BOOLEAN;
  function "<"  (l, r : UNRESOLVED_float) return BOOLEAN;

  function "?="  (l, r : UNRESOLVED_float) return STD_ULOGIC;
  function "?/=" (l, r : UNRESOLVED_float) return STD_ULOGIC;
  function "?>"  (l, r : UNRESOLVED_float) return STD_ULOGIC;
  function "?>=" (l, r : UNRESOLVED_float) return STD_ULOGIC;
  function "?<"  (l, r : UNRESOLVED_float) return STD_ULOGIC;
  function "?<=" (l, r : UNRESOLVED_float) return STD_ULOGIC;

  function std_match (l, r : UNRESOLVED_float) return BOOLEAN;
  function find_rightmost (arg : UNRESOLVED_float; y : STD_ULOGIC)
    return INTEGER;
  function find_leftmost (arg : UNRESOLVED_float; y : STD_ULOGIC)
    return INTEGER;
  function maximum (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function minimum (l, r : UNRESOLVED_float) return UNRESOLVED_float;

  -- conversion functions
  -- Converts one floating point number into another.

  function resize (
    arg                     : UNRESOLVED_float;  -- Floating point input
    constant exponent_width : NATURAL    := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- length of FP output fraction
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant check_error    : BOOLEAN    := float_check_error;
    constant denormalize_in : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant denormalize    : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function resize (
    arg                     : UNRESOLVED_float;  -- Floating point input
    size_res                : UNRESOLVED_float;
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant check_error    : BOOLEAN    := float_check_error;
    constant denormalize_in : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant denormalize    : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  function to_float32 (
    arg                     : UNRESOLVED_float;
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant check_error    : BOOLEAN    := float_check_error;
    constant denormalize_in : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant denormalize    : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float32;

  function to_float64 (
    arg                     : UNRESOLVED_float;
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant check_error    : BOOLEAN    := float_check_error;
    constant denormalize_in : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant denormalize    : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float64;

  function to_float128 (
    arg                     : UNRESOLVED_float;
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant check_error    : BOOLEAN    := float_check_error;
    constant denormalize_in : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant denormalize    : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float128;

  -- Converts an fp into an SLV (needed for synthesis)
  function to_slv (arg : UNRESOLVED_float) return STD_LOGIC_VECTOR;
  alias to_StdLogicVector is to_slv [UNRESOLVED_float return STD_LOGIC_VECTOR];
  alias to_Std_Logic_Vector is to_slv [UNRESOLVED_float return STD_LOGIC_VECTOR];

  -- Converts an fp into an std_ulogic_vector (sulv)
  function to_sulv (arg : UNRESOLVED_float) return STD_ULOGIC_VECTOR;
  alias to_StdULogicVector is to_sulv [UNRESOLVED_float return STD_ULOGIC_VECTOR];
  alias to_Std_ULogic_Vector is to_sulv [UNRESOLVED_float return STD_ULOGIC_VECTOR];

  -- std_ulogic_vector to float
  function to_float (
    arg                     : STD_ULOGIC_VECTOR;
    constant exponent_width : NATURAL := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- length of FP output fraction
    return UNRESOLVED_float;

  -- Integer to float
  function to_float (
    arg                     : INTEGER;
    constant exponent_width : NATURAL    := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- length of FP output fraction
    constant round_style    : round_type := float_round_style)  -- rounding option
    return UNRESOLVED_float;

  -- real to float
  function to_float (
    arg                     : REAL;
    constant exponent_width : NATURAL    := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- length of FP output fraction
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant denormalize    : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- unsigned to float
  function to_float (
    arg                     : UNRESOLVED_UNSIGNED;
    constant exponent_width : NATURAL    := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- length of FP output fraction
    constant round_style    : round_type := float_round_style)  -- rounding option
    return UNRESOLVED_float;

  -- signed to float
  function to_float (
    arg                     : UNRESOLVED_SIGNED;
    constant exponent_width : NATURAL    := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- length of FP output fraction
    constant round_style    : round_type := float_round_style)  -- rounding option
    return UNRESOLVED_float;

  -- unsigned fixed point to float
  function to_float (
    arg                     : UNRESOLVED_ufixed;  -- unsigned fixed point input
    constant exponent_width : NATURAL    := float_exponent_width;  -- width of exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- width of fraction
    constant round_style    : round_type := float_round_style;  -- rounding
    constant denormalize    : BOOLEAN    := float_denormalize)  -- use ieee extensions
    return UNRESOLVED_float;

  -- signed fixed point to float
  function to_float (
    arg                     : UNRESOLVED_sfixed;
    constant exponent_width : NATURAL    := float_exponent_width;  -- length of FP output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- length of FP output fraction
    constant round_style    : round_type := float_round_style;  -- rounding
    constant denormalize    : BOOLEAN    := float_denormalize)  -- rounding option
    return UNRESOLVED_float;

  -- size_res functions
  -- Integer to float
  function to_float (
    arg                  : INTEGER;
    size_res             : UNRESOLVED_float;
    constant round_style : round_type := float_round_style)  -- rounding option
    return UNRESOLVED_float;

  -- real to float
  function to_float (
    arg                  : REAL;
    size_res             : UNRESOLVED_float;
    constant round_style : round_type := float_round_style;  -- rounding option
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- unsigned to float
  function to_float (
    arg                  : UNRESOLVED_UNSIGNED;
    size_res             : UNRESOLVED_float;
    constant round_style : round_type := float_round_style)  -- rounding option
    return UNRESOLVED_float;

  -- signed to float
  function to_float (
    arg                  : UNRESOLVED_SIGNED;
    size_res             : UNRESOLVED_float;
    constant round_style : round_type := float_round_style)  -- rounding option
    return UNRESOLVED_float;

  -- sulv to float
  function to_float (
    arg      : STD_ULOGIC_VECTOR;
    size_res : UNRESOLVED_float)
    return UNRESOLVED_float;

  -- unsigned fixed point to float
  function to_float (
    arg                  : UNRESOLVED_ufixed;  -- unsigned fixed point input
    size_res             : UNRESOLVED_float;
    constant round_style : round_type := float_round_style;  -- rounding
    constant denormalize : BOOLEAN    := float_denormalize)  -- use ieee extensions
    return UNRESOLVED_float;

  -- signed fixed point to float
  function to_float (
    arg                  : UNRESOLVED_sfixed;
    size_res             : UNRESOLVED_float;
    constant round_style : round_type := float_round_style;  -- rounding
    constant denormalize : BOOLEAN    := float_denormalize)  -- rounding option
    return UNRESOLVED_float;

  -- float to unsigned
  function to_unsigned (
    arg                  : UNRESOLVED_float;  -- floating point input
    constant size        : NATURAL;     -- length of output
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error)  -- check for errors
    return UNRESOLVED_UNSIGNED;

  -- float to signed
  function to_signed (
    arg                  : UNRESOLVED_float;  -- floating point input
    constant size        : NATURAL;     -- length of output
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error)  -- check for errors
    return UNRESOLVED_SIGNED;

  -- purpose: Converts a float to unsigned fixed point
  function to_ufixed (
    arg                     : UNRESOLVED_float;  -- fp input
    constant left_index     : INTEGER;  -- integer part
    constant right_index    : INTEGER;  -- fraction part
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;  -- saturate
    constant round_style    : fixed_round_style_type    := fixed_round_style;  -- rounding
    constant check_error    : BOOLEAN                   := float_check_error;  -- check for errors
    constant denormalize    : BOOLEAN                   := float_denormalize)
    return UNRESOLVED_ufixed;

  -- float to signed fixed point
  function to_sfixed (
    arg                     : UNRESOLVED_float;  -- fp input
    constant left_index     : INTEGER;  -- integer part
    constant right_index    : INTEGER;  -- fraction part
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;  -- saturate
    constant round_style    : fixed_round_style_type    := fixed_round_style;  -- rounding
    constant check_error    : BOOLEAN                   := float_check_error;  -- check for errors
    constant denormalize    : BOOLEAN                   := float_denormalize)
    return UNRESOLVED_sfixed;

  -- size_res versions
  -- float to unsigned
  function to_unsigned (
    arg                  : UNRESOLVED_float;  -- floating point input
    size_res             : UNRESOLVED_UNSIGNED;
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error)  -- check for errors
    return UNRESOLVED_UNSIGNED;

  -- float to signed
  function to_signed (
    arg                  : UNRESOLVED_float;  -- floating point input
    size_res             : UNRESOLVED_SIGNED;
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error)  -- check for errors
    return UNRESOLVED_SIGNED;

  -- purpose: Converts a float to unsigned fixed point
  function to_ufixed (
    arg                     : UNRESOLVED_float;  -- fp input
    size_res                : UNRESOLVED_ufixed;
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;  -- saturate
    constant round_style    : fixed_round_style_type    := fixed_round_style;  -- rounding
    constant check_error    : BOOLEAN                   := float_check_error;  -- check for errors
    constant denormalize    : BOOLEAN                   := float_denormalize)
    return UNRESOLVED_ufixed;

  -- float to signed fixed point
  function to_sfixed (
    arg                     : UNRESOLVED_float;  -- fp input
    size_res                : UNRESOLVED_sfixed;
    constant overflow_style : fixed_overflow_style_type := fixed_overflow_style;  -- saturate
    constant round_style    : fixed_round_style_type    := fixed_round_style;  -- rounding
    constant check_error    : BOOLEAN                   := float_check_error;  -- check for errors
    constant denormalize    : BOOLEAN                   := float_denormalize)
    return UNRESOLVED_sfixed;

  -- float to real
  function to_real (
    arg                  : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return REAL;

  -- float to integer
  function to_integer (
    arg                  : UNRESOLVED_float;  -- floating point input
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error)  -- check for errors
    return INTEGER;

  -- For Verilog compatability
  function realtobits (arg : REAL) return STD_ULOGIC_VECTOR;
  function bitstoreal (arg : STD_ULOGIC_VECTOR) return REAL;

  -- Maps metalogical values
  function to_01 (
    arg  : UNRESOLVED_float;            -- floating point input
    XMAP : STD_LOGIC := '0')
    return UNRESOLVED_float;

  function Is_X (arg    : UNRESOLVED_float) return BOOLEAN;
  function to_X01 (arg  : UNRESOLVED_float) return UNRESOLVED_float;
  function to_X01Z (arg : UNRESOLVED_float) return UNRESOLVED_float;
  function to_UX01 (arg : UNRESOLVED_float) return UNRESOLVED_float;

  -- These two procedures were copied out of the body because they proved
  -- very useful for vendor specific algorithm development
  -- Break_number converts a floating point number into it's parts
  -- Exponent is biased by -1

  procedure break_number (
    arg         : in  UNRESOLVED_float;
    denormalize : in  BOOLEAN := float_denormalize;
    check_error : in  BOOLEAN := float_check_error;
    fract       : out UNRESOLVED_UNSIGNED;
    expon       : out UNRESOLVED_SIGNED;  -- NOTE:  Add 1 to get the real exponent!
    sign        : out STD_ULOGIC);

  procedure break_number (
    arg         : in  UNRESOLVED_float;
    denormalize : in  BOOLEAN := float_denormalize;
    check_error : in  BOOLEAN := float_check_error;
    fract       : out UNRESOLVED_ufixed;  -- a number between 1.0 and 2.0
    expon       : out UNRESOLVED_SIGNED;  -- NOTE:  Add 1 to get the real exponent!
    sign        : out STD_ULOGIC);

  -- Normalize takes a fraction and and exponent and converts them into
  -- a floating point number.  Does the shifting and the rounding.
  -- Exponent is assumed to be biased by -1

  function normalize (
    fract                   : UNRESOLVED_UNSIGNED;  -- fraction, unnormalized
    expon                   : UNRESOLVED_SIGNED;   -- exponent - 1, normalized
    sign                    : STD_ULOGIC;         -- sign bit
    sticky                  : STD_ULOGIC := '0';  -- Sticky bit (rounding)
    constant exponent_width : NATURAL    := float_exponent_width;  -- size of output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- size of output fraction
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant denormalize    : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant nguard         : NATURAL    := float_guard_bits)   -- guard bits
    return UNRESOLVED_float;

  -- Exponent is assumed to be biased by -1
  function normalize (
    fract                   : UNRESOLVED_ufixed;   -- unsigned fixed point
    expon                   : UNRESOLVED_SIGNED;   -- exponent - 1, normalized
    sign                    : STD_ULOGIC;         -- sign bit
    sticky                  : STD_ULOGIC := '0';  -- Sticky bit (rounding)
    constant exponent_width : NATURAL    := float_exponent_width;  -- size of output exponent
    constant fraction_width : NATURAL    := float_fraction_width;  -- size of output fraction
    constant round_style    : round_type := float_round_style;  -- rounding option
    constant denormalize    : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant nguard         : NATURAL    := float_guard_bits)   -- guard bits
    return UNRESOLVED_float;

  function normalize (
    fract                : UNRESOLVED_UNSIGNED;    -- unsigned
    expon                : UNRESOLVED_SIGNED;      -- exponent - 1, normalized
    sign                 : STD_ULOGIC;  -- sign bit
    sticky               : STD_ULOGIC := '0';  -- Sticky bit (rounding)
    size_res             : UNRESOLVED_float;   -- used for sizing only
    constant round_style : round_type := float_round_style;  -- rounding option
    constant denormalize : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant nguard      : NATURAL    := float_guard_bits)   -- guard bits
    return UNRESOLVED_float;

  -- Exponent is assumed to be biased by -1
  function normalize (
    fract                : UNRESOLVED_ufixed;      -- unsigned fixed point
    expon                : UNRESOLVED_SIGNED;      -- exponent - 1, normalized
    sign                 : STD_ULOGIC;  -- sign bit
    sticky               : STD_ULOGIC := '0';  -- Sticky bit (rounding)
    size_res             : UNRESOLVED_float;   -- used for sizing only
    constant round_style : round_type := float_round_style;  -- rounding option
    constant denormalize : BOOLEAN    := float_denormalize;  -- Use IEEE extended FP
    constant nguard      : NATURAL    := float_guard_bits)   -- guard bits
    return UNRESOLVED_float;

  -- overloaded versions
  function "+"   (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function "+"   (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function "+"   (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function "+"   (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
  function "-"   (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function "-"   (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function "-"   (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function "-"   (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
  function "*"   (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function "*"   (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function "*"   (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function "*"   (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
  function "/"   (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function "/"   (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function "/"   (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function "/"   (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
  function "rem" (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function "rem" (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function "rem" (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function "rem" (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
  function "mod" (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function "mod" (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function "mod" (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function "mod" (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;

  -- overloaded compare functions
  function "="   (l : UNRESOLVED_float; r : REAL)    return BOOLEAN;
  function "/="  (l : UNRESOLVED_float; r : REAL)    return BOOLEAN;
  function ">="  (l : UNRESOLVED_float; r : REAL)    return BOOLEAN;
  function "<="  (l : UNRESOLVED_float; r : REAL)    return BOOLEAN;
  function ">"   (l : UNRESOLVED_float; r : REAL)    return BOOLEAN;
  function "<"   (l : UNRESOLVED_float; r : REAL)    return BOOLEAN;
  function "="   (l : REAL; r : UNRESOLVED_float)    return BOOLEAN;
  function "/="  (l : REAL; r : UNRESOLVED_float)    return BOOLEAN;
  function ">="  (l : REAL; r : UNRESOLVED_float)    return BOOLEAN;
  function "<="  (l : REAL; r : UNRESOLVED_float)    return BOOLEAN;
  function ">"   (l : REAL; r : UNRESOLVED_float)    return BOOLEAN;
  function "<"   (l : REAL; r : UNRESOLVED_float)    return BOOLEAN;
  function "="   (l : UNRESOLVED_float; r : INTEGER) return BOOLEAN;
  function "/="  (l : UNRESOLVED_float; r : INTEGER) return BOOLEAN;
  function ">="  (l : UNRESOLVED_float; r : INTEGER) return BOOLEAN;
  function "<="  (l : UNRESOLVED_float; r : INTEGER) return BOOLEAN;
  function ">"   (l : UNRESOLVED_float; r : INTEGER) return BOOLEAN;
  function "<"   (l : UNRESOLVED_float; r : INTEGER) return BOOLEAN;
  function "="   (l : INTEGER; r : UNRESOLVED_float) return BOOLEAN;
  function "/="  (l : INTEGER; r : UNRESOLVED_float) return BOOLEAN;
  function ">="  (l : INTEGER; r : UNRESOLVED_float) return BOOLEAN;
  function "<="  (l : INTEGER; r : UNRESOLVED_float) return BOOLEAN;
  function ">"   (l : INTEGER; r : UNRESOLVED_float) return BOOLEAN;
  function "<"   (l : INTEGER; r : UNRESOLVED_float) return BOOLEAN;
  function "?="  (l : UNRESOLVED_float; r : REAL)    return STD_ULOGIC;
  function "?/=" (l : UNRESOLVED_float; r : REAL)    return STD_ULOGIC;
  function "?>"  (l : UNRESOLVED_float; r : REAL)    return STD_ULOGIC;
  function "?>=" (l : UNRESOLVED_float; r : REAL)    return STD_ULOGIC;
  function "?<"  (l : UNRESOLVED_float; r : REAL)    return STD_ULOGIC;
  function "?<=" (l : UNRESOLVED_float; r : REAL)    return STD_ULOGIC;
  function "?="  (l : REAL; r : UNRESOLVED_float)    return STD_ULOGIC;
  function "?/=" (l : REAL; r : UNRESOLVED_float)    return STD_ULOGIC;
  function "?>"  (l : REAL; r : UNRESOLVED_float)    return STD_ULOGIC;
  function "?>=" (l : REAL; r : UNRESOLVED_float)    return STD_ULOGIC;
  function "?<"  (l : REAL; r : UNRESOLVED_float)    return STD_ULOGIC;
  function "?<=" (l : REAL; r : UNRESOLVED_float)    return STD_ULOGIC;
  function "?="  (l : UNRESOLVED_float; r : INTEGER) return STD_ULOGIC;
  function "?/=" (l : UNRESOLVED_float; r : INTEGER) return STD_ULOGIC;
  function "?>"  (l : UNRESOLVED_float; r : INTEGER) return STD_ULOGIC;
  function "?>=" (l : UNRESOLVED_float; r : INTEGER) return STD_ULOGIC;
  function "?<"  (l : UNRESOLVED_float; r : INTEGER) return STD_ULOGIC;
  function "?<=" (l : UNRESOLVED_float; r : INTEGER) return STD_ULOGIC;
  function "?="  (l : INTEGER; r : UNRESOLVED_float) return STD_ULOGIC;
  function "?/=" (l : INTEGER; r : UNRESOLVED_float) return STD_ULOGIC;
  function "?>"  (l : INTEGER; r : UNRESOLVED_float) return STD_ULOGIC;
  function "?>=" (l : INTEGER; r : UNRESOLVED_float) return STD_ULOGIC;
  function "?<"  (l : INTEGER; r : UNRESOLVED_float) return STD_ULOGIC;
  function "?<=" (l : INTEGER; r : UNRESOLVED_float) return STD_ULOGIC;
  -- minimum and maximum overloads
  function maximum (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function minimum (l : UNRESOLVED_float; r : REAL)    return UNRESOLVED_float;
  function maximum (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function minimum (l : REAL; r : UNRESOLVED_float)    return UNRESOLVED_float;
  function maximum (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function minimum (l : UNRESOLVED_float; r : INTEGER) return UNRESOLVED_float;
  function maximum (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
  function minimum (l : INTEGER; r : UNRESOLVED_float) return UNRESOLVED_float;
----------------------------------------------------------------------------
  -- logical functions
  ----------------------------------------------------------------------------

  function "not"  (l    : UNRESOLVED_float) return UNRESOLVED_float;
  function "and"  (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "or"   (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "nand" (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "nor"  (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "xor"  (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  function "xnor" (l, r : UNRESOLVED_float) return UNRESOLVED_float;
  -- Vector and std_ulogic functions, same as functions in numeric_std
  function "and" (l : STD_ULOGIC; r : UNRESOLVED_float)
    return UNRESOLVED_float;
  function "and" (l : UNRESOLVED_float; r : STD_ULOGIC)
    return UNRESOLVED_float;
  function "or" (l : STD_ULOGIC; r : UNRESOLVED_float)
    return UNRESOLVED_float;
  function "or" (l : UNRESOLVED_float; r : STD_ULOGIC)
    return UNRESOLVED_float;
  function "nand" (l : STD_ULOGIC; r : UNRESOLVED_float)
    return UNRESOLVED_float;
  function "nand" (l : UNRESOLVED_float; r : STD_ULOGIC)
    return UNRESOLVED_float;
  function "nor" (l : STD_ULOGIC; r : UNRESOLVED_float)
    return UNRESOLVED_float;
  function "nor" (l : UNRESOLVED_float; r : STD_ULOGIC)
    return UNRESOLVED_float;
  function "xor" (l : STD_ULOGIC; r : UNRESOLVED_float)
    return UNRESOLVED_float;
  function "xor" (l : UNRESOLVED_float; r : STD_ULOGIC)
    return UNRESOLVED_float;
  function "xnor" (l : STD_ULOGIC; r : UNRESOLVED_float)
    return UNRESOLVED_float;
  function "xnor" (l : UNRESOLVED_float; r : STD_ULOGIC)
    return UNRESOLVED_float;
  -- Reduction operators, same as numeric_std functions
  function "and"  (l : UNRESOLVED_float) return STD_ULOGIC;
  function "nand" (l : UNRESOLVED_float) return STD_ULOGIC;
  function "or"   (l : UNRESOLVED_float) return STD_ULOGIC;
  function "nor"  (l : UNRESOLVED_float) return STD_ULOGIC;
  function "xor"  (l : UNRESOLVED_float) return STD_ULOGIC;
  function "xnor" (l : UNRESOLVED_float) return STD_ULOGIC;

  -- Note: "sla", "sra", "sll", "slr", "rol" and "ror" not implemented.

  -----------------------------------------------------------------------------
  -- Recommended Functions from the IEEE 754 Appendix
  -----------------------------------------------------------------------------

  -- returns x with the sign of y.
  function Copysign (x, y : UNRESOLVED_float) return UNRESOLVED_float;

  -- Returns y * 2**n for integral values of N without computing 2**n
  function Scalb (
    y                    : UNRESOLVED_float;  -- floating point input
    N                    : INTEGER;     -- exponent to add
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- Returns y * 2**n for integral values of N without computing 2**n
  function Scalb (
    y                    : UNRESOLVED_float;  -- floating point input
    N                    : UNRESOLVED_SIGNED;      -- exponent to add
    constant round_style : round_type := float_round_style;  -- rounding option
    constant check_error : BOOLEAN    := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN    := float_denormalize)  -- Use IEEE extended FP
    return UNRESOLVED_float;

  -- returns the unbiased exponent of x
  function Logb (x : UNRESOLVED_float) return INTEGER;
  function Logb (x : UNRESOLVED_float) return UNRESOLVED_SIGNED;

  -- returns the next representable neighbor of x in the direction toward y
  function Nextafter (
    x, y                 : UNRESOLVED_float;  -- floating point input
    constant check_error : BOOLEAN := float_check_error;  -- check for errors
    constant denormalize : BOOLEAN := float_denormalize)
    return UNRESOLVED_float;

  -- Returns TRUE if X is unordered with Y.
  function Unordered (x, y : UNRESOLVED_float) return BOOLEAN;
  function Finite (x       : UNRESOLVED_float) return BOOLEAN;
  function Isnan (x        : UNRESOLVED_float) return BOOLEAN;

  -- Function to return constants.
  function zerofp (
    constant exponent_width : NATURAL := float_exponent_width;  -- exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- fraction
    return UNRESOLVED_float;
  function nanfp (
    constant exponent_width : NATURAL := float_exponent_width;  -- exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- fraction
    return UNRESOLVED_float;
  function qnanfp (
    constant exponent_width : NATURAL := float_exponent_width;  -- exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- fraction
    return UNRESOLVED_float;
  function pos_inffp (
    constant exponent_width : NATURAL := float_exponent_width;  -- exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- fraction
    return UNRESOLVED_float;
  function neg_inffp (
    constant exponent_width : NATURAL := float_exponent_width;  -- exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- fraction
    return UNRESOLVED_float;
  function neg_zerofp (
    constant exponent_width : NATURAL := float_exponent_width;  -- exponent
    constant fraction_width : NATURAL := float_fraction_width)  -- fraction
    return UNRESOLVED_float;
  -- size_res versions
  function zerofp (
    size_res : UNRESOLVED_float)        -- variable is only use for sizing
    return UNRESOLVED_float;
  function nanfp (
    size_res : UNRESOLVED_float)        -- variable is only use for sizing
    return UNRESOLVED_float;
  function qnanfp (
    size_res : UNRESOLVED_float)        -- variable is only use for sizing
    return UNRESOLVED_float;
  function pos_inffp (
    size_res : UNRESOLVED_float)        -- variable is only use for sizing
    return UNRESOLVED_float;
  function neg_inffp (
    size_res : UNRESOLVED_float)        -- variable is only use for sizing
    return UNRESOLVED_float;
  function neg_zerofp (
    size_res : UNRESOLVED_float)        -- variable is only use for sizing
    return UNRESOLVED_float;

  --===========================================================================
  -- string and textio Functions
  --===========================================================================

  -- writes S:EEEE:FFFFFFFF
  procedure WRITE (
    L         : inout LINE;              -- access type (pointer)
    VALUE     : in    UNRESOLVED_float;  -- value to write
    JUSTIFIED : in    SIDE  := right;    -- which side to justify text
    FIELD     : in    WIDTH := 0);       -- width of field

  -- Reads SEEEEFFFFFFFF, "." and ":" are ignored
  procedure READ (L    : inout LINE; VALUE : out UNRESOLVED_float);
  procedure READ (L    : inout LINE; VALUE : out UNRESOLVED_float;
                  GOOD : out   BOOLEAN);

  alias BREAD is READ [LINE, UNRESOLVED_float, BOOLEAN];
  alias BREAD is READ [LINE, UNRESOLVED_float];
  alias BWRITE is WRITE [LINE, UNRESOLVED_float, SIDE, WIDTH];
  alias BINARY_READ is READ [LINE, UNRESOLVED_float, BOOLEAN];
  alias BINARY_READ is READ [LINE, UNRESOLVED_float];
  alias BINARY_WRITE is WRITE [LINE, UNRESOLVED_float, SIDE, WIDTH];

  procedure OWRITE (
    L         : inout LINE;              -- access type (pointer)
    VALUE     : in    UNRESOLVED_float;  -- value to write
    JUSTIFIED : in    SIDE  := right;    -- which side to justify text
    FIELD     : in    WIDTH := 0);       -- width of field

  -- Octal read with padding, no separators used
  procedure OREAD (L    : inout LINE; VALUE : out UNRESOLVED_float);
  procedure OREAD (L    : inout LINE; VALUE : out UNRESOLVED_float;
                   GOOD : out   BOOLEAN);
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_float, BOOLEAN];
  alias OCTAL_READ is OREAD [LINE, UNRESOLVED_float];
  alias OCTAL_WRITE is OWRITE [LINE, UNRESOLVED_float, SIDE, WIDTH];

  -- Hex write with padding, no separators
  procedure HWRITE (
    L         : inout LINE;              -- access type (pointer)
    VALUE     : in    UNRESOLVED_float;  -- value to write
    JUSTIFIED : in    SIDE  := right;    -- which side to justify text
    FIELD     : in    WIDTH := 0);       -- width of field

  -- Hex read with padding, no separators used
  procedure HREAD (L : inout LINE; VALUE : out UNRESOLVED_float);
  procedure HREAD (L    : inout LINE; VALUE : out UNRESOLVED_float;
                   GOOD : out   BOOLEAN);
  alias HEX_READ is HREAD [LINE, UNRESOLVED_float, BOOLEAN];
  alias HEX_READ is HREAD [LINE, UNRESOLVED_float];
  alias HEX_WRITE is HWRITE [LINE, UNRESOLVED_float, SIDE, WIDTH];

  -- returns "S:EEEE:FFFFFFFF"
  function to_string (value : UNRESOLVED_float) return STRING;
  alias TO_BSTRING is TO_STRING [UNRESOLVED_float return STRING];
  alias TO_BINARY_STRING is TO_STRING [UNRESOLVED_float return STRING];

  -- Returns a HEX string, with padding
  function to_hstring (value : UNRESOLVED_float) return STRING;
  alias TO_HEX_STRING is to_hstring [UNRESOLVED_float return STRING];

  -- Returns and octal string, with padding
  function to_ostring (value : UNRESOLVED_float) return STRING;
  alias TO_OCTAL_STRING is to_ostring [UNRESOLVED_float return STRING];

  function from_string (
    bstring                 : STRING;   -- binary string
    constant exponent_width : NATURAL := float_exponent_width;
    constant fraction_width : NATURAL := float_fraction_width)
    return UNRESOLVED_float;
  alias from_bstring is from_string [STRING, NATURAL, NATURAL
                                     return UNRESOLVED_float];
  alias from_binary_string is from_string [STRING, NATURAL, NATURAL
                                           return UNRESOLVED_float];
  function from_ostring (
    ostring                 : STRING;   -- Octal string
    constant exponent_width : NATURAL := float_exponent_width;
    constant fraction_width : NATURAL := float_fraction_width)
    return UNRESOLVED_float;
  alias from_octal_string is from_ostring [STRING, NATURAL, NATURAL
                                           return UNRESOLVED_float];

  function from_hstring (
    hstring                 : STRING;   -- hex string
    constant exponent_width : NATURAL := float_exponent_width;
    constant fraction_width : NATURAL := float_fraction_width)
    return UNRESOLVED_float;
  alias from_hex_string is from_hstring [STRING, NATURAL, NATURAL
                                         return UNRESOLVED_float];

  function from_string (
    bstring  : STRING;                  -- binary string
    size_res : UNRESOLVED_float)        -- used for sizing only
    return UNRESOLVED_float;
  alias from_bstring is from_string [STRING, UNRESOLVED_float
                                     return UNRESOLVED_float];
  alias from_binary_string is from_string [STRING, UNRESOLVED_float
                                           return UNRESOLVED_float];

  function from_ostring (
    ostring  : STRING;                  -- Octal string
    size_res : UNRESOLVED_float)        -- used for sizing only
    return UNRESOLVED_float;
  alias from_octal_string is from_ostring [STRING, UNRESOLVED_float
                                           return UNRESOLVED_float];

  function from_hstring (
    hstring  : STRING;                  -- hex string
    size_res : UNRESOLVED_float)        -- used for sizing only
    return UNRESOLVED_float;
  alias from_hex_string is from_hstring [STRING, UNRESOLVED_float
                                         return UNRESOLVED_float];

end package float_generic_pkg;
