--  This -*- vhdl -*- file is part of GHDL.
--  IEEE 1076.2 math_real package body.
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

package body MATH_REAL is
  function SIGN (X : REAL) return REAL is
  begin
    if X > 0.0 then
      return 1.0;
    elsif X < 0.0 then
      return -1.0;
    else
      return 0.0;
    end if;
  end SIGN;

  function CEIL (X : REAL) return REAL is
  begin
    assert false severity failure;
  end CEIL;

  function FLOOR (X : REAL) return REAL is
  begin
    assert false severity failure;
  end FLOOR;

  function ROUND (X : REAL) return REAL is
  begin
    assert false severity failure;
  end ROUND;

  function TRUNC (X : REAL) return REAL is
  begin
    assert false severity failure;
  end TRUNC;

  procedure UNIFORM (SEED1, SEED2 : inout POSITIVE; X : out REAL)
  is
    variable z, k : Integer;
  begin
    k := seed1 / 53668;
    seed1 := 40014 * (seed1 - k * 53668) - k * 12211;
    if seed1 < 0 then
      seed1 := seed1 + 2147483563;
    end if;

    k := seed2 / 52774;
    seed2 := 40692 * (seed2 - k * 52774) - k * 3791;
    if seed2 < 0 then
      seed2 := seed2 + 2147483399;
    end if;

    z := seed1 - seed2;
    if z < 1 then
      z := z + 2147483562;
    end if;

    x := real (z) * 4.656613e-10;
  end UNIFORM;

  function SIN (X : REAL) return REAL is
  begin
    assert false severity failure;
  end SIN;

  function COS (X : REAL) return REAL is
  begin
    assert false severity failure;
  end COS;


end MATH_REAL;
