
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
-- $Id: tc2979.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s03b01x00p07n01i02979ent IS
END c02s03b01x00p07n01i02979ent;

ARCHITECTURE c02s03b01x00p07n01i02979arch OF c02s03b01x00p07n01i02979ent IS
  type newt is (one,two,three,four);
  function "mod" (constant c1,c2 : in integer) return newt is
  begin
    assert (c1=10)
      report "Error in association of left operator"
      severity failure;
    assert (c2=20)
      report "Error in association of right operator"
      severity failure;
    return three;
  end;
BEGIN
  TESTING: PROCESS
    variable n1 : newt;
  BEGIN
    wait for 5 ns;
    n1 := two;
    assert (n1=two)
      report "Error in initial conditions detected"
      severity failure;
    n1:= "mod"(10,20);
    assert NOT( n1=three )
      report "***PASSED TEST: c02s03b01x00p07n01i02979"
      severity NOTE;
    assert ( n1=three )
      report "***FAILED TEST: c02s03b01x00p07n01i02979 - Error in call to operloaded operator."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b01x00p07n01i02979arch;
