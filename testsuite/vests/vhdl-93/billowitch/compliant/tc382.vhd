
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
-- $Id: tc382.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p04n01i00382ent IS
END c03s02b01x01p04n01i00382ent;

ARCHITECTURE c03s02b01x01p04n01i00382arch OF c03s02b01x01p04n01i00382ent IS
  type days is (sun, mon, tue, wed, thu, fri, sat);
  type bit_vctor is array (days range mon to fri) of integer;
BEGIN
  TESTING: PROCESS
    variable k : bit_vctor;
  BEGIN
    k(mon) := 1;
    k(tue) := 2;
    k(wed) := 3;
    k(thu) := 4;
    k(fri) := 5;
    assert NOT (   k(mon) = 1 and 
                   k(tue) = 2 and
                   k(wed) = 3 and 
                   k(thu) = 4 and 
                   k(fri) = 5 )
      report "***PASSED TEST: c03s02b01x01p04n01i00382"
      severity NOTE;
    assert  (   k(mon) = 1 and 
                k(tue) = 2 and
                k(wed) = 3 and 
                k(thu) = 4 and 
                k(fri) = 5 )
      report "***FAILED TEST: c03s02b01x01p04n01i00382 - An index constraint is compatible with the type denoted by the type mark if and only if the constraint defined by each discrete range is compatible with the corresponding subtype." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p04n01i00382arch;
