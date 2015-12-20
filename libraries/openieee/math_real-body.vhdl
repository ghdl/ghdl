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
  end;

  function fmod (X, Y : REAL) return REAL;
  attribute foreign of fmod : function is "VHPIDIRECT fmod";

  function fmod (X, Y : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function "mod" (X, Y : REAL) return REAL
  is
    variable res : real;
  begin
    assert y /= 0.0 report "ieee.math_real.""mod"": dividend is 0.0"
      severity failure;
    res := fmod (x, y);
    if res /= 0.0 then
      if x > 0.0 xor y > 0.0 then
        res := res + y;
      end if;
    end if;
    return res;
  end "mod";

  function REALMAX (X, Y : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function REALMIN (X, Y : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  procedure UNIFORM (SEED1, SEED2 : inout POSITIVE; X : out REAL)
  is
    variable z, k : Integer;
    variable s1, s2 : Integer;
  begin
    k := seed1 / 53668;
    s1 := 40014 * (seed1 - k * 53668) - k * 12211;
    if s1 < 0 then
      seed1 := s1 + 2147483563;
    else
      seed1 := s1;
    end if;

    k := seed2 / 52774;
    s2 := 40692 * (seed2 - k * 52774) - k * 3791;
    if s2 < 0 then
      seed2 := s2 + 2147483399;
    else
      seed2 := s2;
    end if;

    z := seed1 - seed2;
    if z < 1 then
      z := z + 2147483562;
    end if;

    x := real (z) * 4.656613e-10;
  end UNIFORM;

  function SQRT (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function CBRT (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function "**" (X : INTEGER; Y : REAL) return REAL is
  begin
    return real (x) ** y;
  end "**";

  function "**" (X : REAL; Y : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function EXP (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function LOG (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function LOG2 (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function LOG10 (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function LOG (X : REAL; BASE : REAL) return REAL is
  begin
    return log (x) / log (base);
  end log;

  function SIN (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function COS (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function TAN (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCSIN (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCCOS (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCTAN (Y : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCTAN (Y, X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function SINH (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function COSH (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function TANH (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCSINH (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCCOSH (X : REAL) return REAL is
  begin
    assert false severity failure;
  end;

  function ARCTANH (Y : REAL) return REAL is
  begin
    assert false severity failure;
  end;

end MATH_REAL;
