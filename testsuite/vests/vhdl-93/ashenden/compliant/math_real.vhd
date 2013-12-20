
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: math_real.vhd,v 1.2 2001-10-26 16:29:37 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

--------------------------------------------------------------- 
--
-- This source file may be used and distributed without restriction.
-- No declarations or definitions shall be included in this package. 
--
--   ****************************************************************
--   *                                                              *
--   *                      W A R N I N G		 	    *
--   *								    *
--   *   This DRAFT version IS NOT endorsed or approved by IEEE     *
--   *								    *
--   ****************************************************************
--
-- Title:    PACKAGE MATH_REAL
--
-- Library:  This package shall be compiled into a library 
--           symbolically named IEEE.
--
-- Purpose:  VHDL declarations for mathematical package MATH_REAL
--	     which contains common real constants, common real
--	     functions, and real trascendental functions.
--
-- Author:   Based on work by IEEE VHDL Math Package Study Group 
--
-- Notes:
-- 	The package body shall be considered the formal definition of 
-- 	the semantics of this package. Tool developers may choose to implement 
-- 	the package body in the most efficient manner available to them.
--
-- History:
--	Version 0.4		JAT	4/15/93
-------------------------------------------------------------
Library IEEE;

Package MATH_REAL is
--synopsys synthesis_off

  constant  MATH_E :         real := 2.71828_18284_59045_23536;  
                                        -- value of e
  constant  MATH_1_E:        real := 0.36787_94411_71442_32160;
                                        -- value of 1/e
  constant  MATH_PI :        real := 3.14159_26535_89793_23846;  
                                        -- value of pi
  constant  MATH_1_PI :      real := 0.31830_98861_83790_67154;  
                                        -- value of 1/pi
  constant  MATH_LOG_OF_2:   real := 0.69314_71805_59945_30942;
                                        -- natural log of 2
  constant  MATH_LOG_OF_10:  real := 2.30258_50929_94045_68402;
                                        -- natural log of10
  constant  MATH_LOG2_OF_E:  real := 1.44269_50408_88963_4074;
                                        -- log base 2 of e
  constant  MATH_LOG10_OF_E: real := 0.43429_44819_03251_82765;
                                        -- log base 10 of e
  constant  MATH_SQRT2:      real := 1.41421_35623_73095_04880; 
                                        -- sqrt of 2
  constant  MATH_SQRT1_2:    real := 0.70710_67811_86547_52440; 
                                        -- sqrt of 1/2
  constant  MATH_SQRT_PI:    real := 1.77245_38509_05516_02730; 
                                        -- sqrt of pi
  constant  MATH_DEG_TO_RAD: real := 0.01745_32925_19943_29577;
  -- conversion factor from degree to radian
  constant  MATH_RAD_TO_DEG: real := 57.29577_95130_82320_87685;
  -- conversion factor from radian to degree

  --
  -- attribute for functions whose implementation is foreign (C native)
  --
  -- attribute FOREIGN: string;  -- predefined attribute in VHDL-1992
  --

  function SIGN (X: real ) return real;
  -- returns 1.0 if X > 0.0; 0.0 if X == 0.0; -1.0 if X < 0.0

  function CEIL (X : real ) return real;
  -- returns smallest integer value (as real) not less than X

  function FLOOR (X : real ) return real;
  -- returns largest integer value (as real) not greater than X

  function ROUND (X : real ) return real;
  -- returns FLOOR(X + 0.5) if X > 0.0;
  -- return CEIL(X - 0.5) if X < 0.0
  
  function FMAX (X, Y : real ) return real;
  -- returns the algebraically larger of X and Y

  function FMIN (X, Y : real ) return real;
  -- returns the algebraically smaller of X and Y

  function SRAND (seed: in integer ) return integer;
  -- attribute FOREIGN of SRAND: function is "C_NATIVE"; 
  -- for VHDL-1992 standard
  --
  -- sets value of seed for sequence of pseudo-random numbers.
  -- returns the value of the seed.
  -- It uses the native C function srand().

  function RAND return integer;		
  -- attribute FOREIGN of RAND: function is "C_NATIVE"; 
  -- for VHDL-1992 standard
  --
  -- returns an integer pseudo-random number with uniform distribution.
  -- It uses the native C function rand(). 
  -- Seed for the sequence is initialized with the
  -- SRAND() function and value of the seed is changed every
  -- time SRAND() is called,  but it is not visible.
  -- The range of generated values is platform dependent.

  function GET_RAND_MAX return integer;		
  -- attribute FOREIGN of GET_RAND_MAX: function is "C_NATIVE"; 
  -- for VHDL-1992 standard
  --
  -- returns the upper bound of the range of the
  -- pseudo-random numbers generated by  RAND().
  -- The support for this function is platform dependent.
  -- It may not be available in some platforms.
  -- Note: the value of (RAND() / GET_RAND_MAX()) is a
  --       pseudo-random number distributed between 0 & 1.

  function SQRT (X : real ) return real;
  -- returns square root of X;  X >= 0.0

  function CBRT (X : real ) return real;
  -- returns cube root of X

  function "**" (X : integer; Y : real) return real;
  -- returns Y power of X ==>  X**Y;
  -- error if X = 0 and Y <= 0.0
  -- error if X < 0 and Y does not have an integral value

  function "**" (X : real; Y : real) return real;
  -- returns Y power of X ==>  X**Y;
  -- error if X = 0.0 and Y <= 0.0
  -- error if X < 0.0 and Y does not have an integral value

  function EXP  (X : real ) return real;
  -- returns e**X; where e = MATH_E

  function LOG (X : real ) return real;
  -- returns natural logarithm of X; X > 0

  function LOG (BASE: positive; X : real) return real;
  -- returns logarithm base BASE of X; X > 0

  function  SIN (X : real ) return real;
  -- returns sin X; X in radians

  function  COS ( X : real ) return real;
  -- returns cos X; X in radians

  function  TAN (X : real ) return real;
  -- returns tan X; X in radians
  -- X /= ((2k+1) * PI/2), where k is an integer

  function  ASIN (X : real ) return real; 
  -- returns  -PI/2 < asin X < PI/2; | X | <= 1.0

  function  ACOS (X : real ) return real;
  -- returns  0 < acos X < PI; | X | <= 1.0

  function  ATAN (X : real) return real;
  -- returns  -PI/2 < atan X < PI/2

  function  ATAN2 (X : real; Y : real) return real;
  -- returns  atan (X/Y); -PI < atan2(X,Y) < PI; Y /= 0.0

  function SINH (X : real) return real;
  -- hyperbolic sine; returns (e**X - e**(-X))/2

  function  COSH (X : real) return real;
  -- hyperbolic cosine; returns (e**X + e**(-X))/2

  function  TANH (X : real) return real;
  -- hyperbolic tangent; -- returns (e**X - e**(-X))/(e**X + e**(-X))
  
  function ASINH (X : real) return real;
  -- returns ln( X + sqrt( X**2 + 1))

  function ACOSH (X : real) return real;
  -- returns ln( X + sqrt( X**2 - 1));   X >= 1.0

  function ATANH (X : real) return real;
  -- returns (ln( (1 + X)/(1 - X)))/2 ; | X | < 1.0

--synopsys synthesis_on
end  MATH_REAL;
