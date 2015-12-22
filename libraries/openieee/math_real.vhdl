--  This -*- vhdl -*- file is part of GHDL.
--  IEEE 1076.2 math_real package.
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

package MATH_REAL is
  --  The values were computed with at least 40 digits and rounded to
  --  20 digits after the dot.  They were checked with the original ieee
  --  specification (log2_of_e has an extra digit from the spec).
  constant math_e        : real := 2.71828_18284_59045_23536;
  constant math_1_over_e : real := 0.36787_94411_71442_321596;

  constant math_pi        : real := 3.14159_26535_89793_23846;
  constant math_2_pi      : real := 6.28318_53071_79586_47693;
  constant math_pi_over_2 : real := 1.57079_63267_94896_61923;
  constant math_pi_over_3 : real := 1.04719_75511_96597_74615;
  constant math_pi_over_4 : real := 0.78539_81633_97448_30962;
  constant math_3_pi_over_2 : real := 4.71238_89803_84689_85769;

  constant math_log_of_2   : real := 0.69314_71805_59945_30942;
  constant math_log_of_10  : real := 2.30258_50929_94045_68402;
  constant math_log2_of_e  : real := 1.44269_50408_88963_40736;
  constant math_log10_of_e : real := 0.43429_44819_03251_82765;

  constant math_sqrt_2        : real := 1.41421_35623_73095_04880;
  constant math_1_over_sqrt_2 : real := 0.70710_67811_86547_52440;
  constant math_sqrt_pi       : real := 1.77245_38509_05516_02730;

  constant math_deg_to_rad    : real := 0.01745_32925_19943_29577;
  constant math_rad_to_deg    : real := 57.29577_95130_82320_87680;

  function SIGN (X : REAL) return REAL;

  function CEIL (X : REAL) return REAL;
  attribute foreign of ceil : function is "VHPIDIRECT ceil";

  function FLOOR (X : REAL) return REAL;
  attribute foreign of floor : function is "VHPIDIRECT floor";

  function ROUND (X : REAL) return REAL;
  attribute foreign of round : function is "VHPIDIRECT round";

  function TRUNC (X : REAL) return REAL;
  attribute foreign of trunc : function is "VHPIDIRECT trunc";

  function "mod" (X, Y : REAL) return REAL;
  --  Contrary to fmod, the sign of the result is the sign of Y.

  function REALMAX (X, Y : REAL) return REAL;
  attribute foreign of REALMAX : function is "VHPIDIRECT fmax";

  function REALMIN (X, Y : REAL) return REAL;
  attribute foreign of REALMIN : function is "VHPIDIRECT fmin";

  procedure UNIFORM (SEED1, SEED2 : inout POSITIVE; X : out REAL);
  --  Algorithm from: Pierre L'Ecuyer, CACM June 1988 Volume 31 Number 6
  --  page 747 figure 3.

  function SQRT (X : REAL) return REAL;
  attribute foreign of SQRT : function is "VHPIDIRECT sqrt";

  function CBRT (X : REAL) return REAL;
  attribute foreign of CBRT : function is "VHPIDIRECT cbrt";

  function "**" (X : INTEGER; Y : REAL) return REAL;

  function "**" (X : REAL; Y : REAL) return REAL;
  attribute foreign of "**" [ REAL, REAL return REAL ]: function is
    "VHPIDIRECT pow";

  function EXP (X : REAL) return REAL;
  attribute foreign of EXP : function is "VHPIDIRECT exp";

  function LOG (X : REAL) return REAL;
  attribute foreign of LOG [ REAL return REAL ] : function is "VHPIDIRECT log";

  function LOG2 (X : REAL) return REAL;
  attribute foreign of LOG2 : function is "VHPIDIRECT log2";

  function LOG10 (X : REAL) return REAL;
  attribute foreign of LOG10 : function is "VHPIDIRECT log10";

  function LOG (X : REAL; BASE : REAL) return REAL;

  function SIN (X : REAL) return REAL;
  attribute foreign of SIN : function is "VHPIDIRECT sin";

  function COS (X : REAL) return REAL;
  attribute foreign of COS : function is "VHPIDIRECT cos";

  function TAN (X : REAL) return REAL;
  attribute foreign of TAN : function is "VHPIDIRECT tan";

  function ARCSIN (X : REAL) return REAL;
  attribute foreign of ARCSIN : function is "VHPIDIRECT asin";

  function ARCCOS (X : REAL) return REAL;
  attribute foreign of ARCCOS : function is "VHPIDIRECT acos";

  function ARCTAN (Y : REAL) return REAL;
  attribute foreign of ARCTAN [ REAL return REAL ]: function is
    "VHPIDIRECT atan";

  function ARCTAN (Y, X : REAL) return REAL;
  attribute foreign of ARCTAN [ REAL, REAL return REAL ]: function is
    "VHPIDIRECT atan2";

  function SINH (X : REAL) return REAL;
  attribute foreign of SINH : function is "VHPIDIRECT sinh";

  function COSH (X : REAL) return REAL;
  attribute foreign of COSH : function is "VHPIDIRECT cosh";

  function TANH (X : REAL) return REAL;
  attribute foreign of TANH : function is "VHPIDIRECT tanh";

  function ARCSINH (X : REAL) return REAL;
  attribute foreign of ARCSINH : function is "VHPIDIRECT asinh";

  function ARCCOSH (X : REAL) return REAL;
  attribute foreign of ARCCOSH : function is "VHPIDIRECT acosh";

  function ARCTANH (Y : REAL) return REAL;
  attribute foreign of ARCTANH : function is "VHPIDIRECT atanh";
end MATH_REAL;
