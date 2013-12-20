
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
-- $Id: tc2968.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s03b01x00p02n02i02968ent IS
END c02s03b01x00p02n02i02968ent;

ARCHITECTURE c02s03b01x00p02n02i02968arch OF c02s03b01x00p02n02i02968ent IS
  type newt is (one,two,three,four);
  function "abs" (constant c1 : in integer) return newt is
  begin
    assert (c1=10)
      report "Error in association of right operator"
      severity failure;
    assert NOT( c1=10 )
      report "***PASSED TEST: c02s03b01x00p02n02i02968"
      severity NOTE;
    assert ( c1=10 )
      report "***FAILED TEST: c02s03b01x00p02n02i02968 - Error in association of operands."
      severity ERROR;
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
    n1:= abs 10;
    assert (n1=three)
      report "Error in call to operloaded operator"
      severity failure;
    wait;
  END PROCESS TESTING;

END c02s03b01x00p02n02i02968arch;
