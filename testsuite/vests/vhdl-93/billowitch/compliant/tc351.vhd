
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
-- $Id: tc351.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p02n01i00351ent IS
END c03s02b01x01p02n01i00351ent;

ARCHITECTURE c03s02b01x01p02n01i00351arch OF c03s02b01x01p02n01i00351ent IS
  type bit_vctor is array (-1 to 8) of integer;  --Expect_success
BEGIN
  TESTING: PROCESS
    variable k : bit_vctor;
  BEGIN
    k(5) := 5;
    assert NOT(k(5)=5) 
      report "***PASSED TEST: c03s02b01x01p02n01i00351" 
      severity NOTE;
    assert (k(5)=5) 
      report "***FAILED TEST: c03s02b01x01p02n01i00351 - An implicit conversion to the predefined type INTEGER is assumed if each bound is either a numeric literal or an attribute, and the type of both bounds(prior to implicit conversion) is the type universal_integer." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p02n01i00351arch;
