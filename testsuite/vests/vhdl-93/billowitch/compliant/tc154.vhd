
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
-- $Id: tc154.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b02x02p16n01i00154pkg is
  procedure P1 (a  : in integer; b: inout integer);
  function  F1 (I1 : in integer) return real;
  function  F2 (I2 : in real) return integer;
end c04s03b02x02p16n01i00154pkg;

package body c04s03b02x02p16n01i00154pkg is
  procedure P1 (a  : in integer; b: inout integer) is
  begin
    b := a;
  end P1;

  function  F1 (I1 : in integer) return real is
  begin
    return 10.0;
  end F1;

  function  F2 (I2 : in real) return integer is
  begin
    return 10;
  end F2;
end c04s03b02x02p16n01i00154pkg;


use work.c04s03b02x02p16n01i00154pkg.all;
ENTITY c04s03b02x02p16n01i00154ent IS
END c04s03b02x02p16n01i00154ent;

ARCHITECTURE c04s03b02x02p16n01i00154arch OF c04s03b02x02p16n01i00154ent IS

BEGIN

  TESTING: PROCESS
    variable x : real := 1.0;
  BEGIN
    P1 (10, F1(b) => F2(x));  -- No_failure_here
    assert NOT(F2(x) = 10)
      report "***PASSED TEST: c04s03b02x02p16n01i00154"
      severity NOTE;
    assert (F2(x) = 10)
      report "***FAILED TEST: c04s03b02x02p16n01i00154 - Types of the actuals match those of the formals test failed.."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p16n01i00154arch;
