
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

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
-- $Id: tc2964.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s03b00x00p03n01i02964ent IS
END c02s03b00x00p03n01i02964ent;

ARCHITECTURE c02s03b00x00p03n01i02964arch OF c02s03b00x00p03n01i02964ent IS
  function f1a(constant c1 : in integer) return integer is
  begin
    return 12;
  end;
  function f1a(constant c1,c2 : in integer) return integer is
  begin
    return 25;
  end;
  function f2b(constant c1 : in integer) return integer is
  begin
    return 22;
  end;
  function f2b(constant c1 : in real) return integer is
  begin
    return 28;
  end;
  function f3c(constant c0:integer; constant c1:real) return integer is
  begin
    return 32;
  end;
  function f3c(constant c1:real; constant c0:integer) return integer is
  begin
    return 38;
  end;
  function f4d(constant c1 : in integer) return integer is
  begin
    return 42;
  end;
  function f4d(constant c1 : in integer) return real is
  begin
    return 48.0;
  end;
  function f5e(constant c1 : in integer) return integer is
  begin
    return 52;
  end;
  procedure f5e(constant c1 : in integer) is
  begin
    return;
  end;
  function f6f(constant c0 : in real;constant c1 : in integer) return integer is
  begin
    return 62;
  end;
  function f6f(constant c2 : in integer;constant c3 : in real) return integer is
  begin
    return 68;
  end;
BEGIN
  TESTING: PROCESS
    variable i1 : integer;
    variable r1 : real;
    variable k  : integer := 0;
  BEGIN
    i1 := 8;
    if (i1 /= 8) then
      k := 1;
    end if;
    assert (i1=8)
      report "Error in initial conditions detected"
      severity failure;
    i1:= f1a(4);
    if (i1 /= 12) then
      k := 1;
    end if;
    assert (i1=12)
      report "Error differentiating overloaded subprog by number of formals"
      severity failure;
    i1:=f1a(16,23);
    if (i1 /= 25) then
      k := 1;
    end if;
    assert (i1=25)
      report "Error differentiating overloaded subprog by number of formals"
      severity failure;
    i1:= f2b(4);
    if (i1 /= 22) then
      k := 1;
    end if;
    assert (i1=22)
      report "Error differentiating overloaded subprog by type of formals"
      severity failure;
    i1:=f2b(4.0);
    if (i1 /= 28) then
      k := 1;
    end if;
    assert (i1=28)
      report "Error differentiating overloaded subprog by type of formals"
      severity failure;
    i1:= f3c(4,4.0);
    if (i1 /= 32) then
      k := 1;
    end if;
    assert (i1=32)
      report "Error differentiating overloaded subprog by order of formals"
      severity failure;
    i1:= f3c(4.0,4);
    if (i1 /= 38) then
      k := 1;
    end if;
    assert (i1=38)
      report "Error differentiating overloaded subprog by order of formals"
      severity failure;
    i1:= f4d(4);
    if (i1 /= 42) then
      k := 1;
    end if;
    assert (i1=42)
      report "Error differentiating overloaded subprog by return type"
      severity failure;

    r1:= f4d(4);
    if (r1 /= 48.0) then
      k := 1;
    end if;
    assert (r1=48.0)
      report "Error differentiating overloaded subprog by return type"
      severity failure;
    i1:= f5e(4);
    if (i1 /= 52) then
      k := 1;
    end if;
    assert (i1=52)
      report "Error differentiating overloaded subprog by having a return"
      severity failure;
    i1:= f6f(c1 => 4, c0 => 4.4);
    if (i1 /= 62) then
      k := 1;
    end if;
    assert (i1=62)
      report "Error differentiating overloaded subprog by name of formals"
      severity failure;
    i1:= f6f(c3 => 4.4, c2 => 4);
    if (i1 /= 68) then
      k := 1;
    end if;
    assert (i1=68)
      report "Error differentiating overloaded subprog by name of formals"
      severity failure;
    wait for 5 ns;
    assert NOT( k=0 )
      report "***PASSED TEST: c02s03b00x00p03n01i02964"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c02s03b00x00p03n01i02964 - Overload subprogram call test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b00x00p03n01i02964arch;
