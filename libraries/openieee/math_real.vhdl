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
  constant math_pi : real := 3.14159_26535_89793_23846;
  function SIGN (X : REAL) return REAL;

  function CEIL (X : REAL) return REAL;
  attribute foreign of ceil : function is "VHPIDIRECT ceil";

  function FLOOR (X : REAL) return REAL;
  attribute foreign of floor : function is "VHPIDIRECT floor";

  function ROUND (X : REAL) return REAL;
  attribute foreign of round : function is "VHPIDIRECT round";

  function TRUNC (X : REAL) return REAL;
  attribute foreign of trunc : function is "VHPIDIRECT trunc";

  procedure UNIFORM (SEED1, SEED2 : inout POSITIVE; X : out REAL);
  --  Algorithm from: Pierre L'Ecuyer, CACM June 1988 Volume 31 Number 6
  --  page 747 figure 3.

  function SIN (X : REAL) return REAL;
  attribute foreign of SIN : function is "VHPIDIRECT sin";

  function COS (X : REAL) return REAL;
  attribute foreign of COS : function is "VHPIDIRECT cos";
end MATH_REAL;
