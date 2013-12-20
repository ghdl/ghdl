
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
-- $Id: tc392.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p06n01i00392ent IS
END c03s02b01x01p06n01i00392ent;

ARCHITECTURE c03s02b01x01p06n01i00392arch OF c03s02b01x01p06n01i00392ent IS
  type   M1 is array (positive range <>) of integer;
  signal S1 : M1(3 to 30) ; -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    S1(3)  <= 3  after 3 ns;
    S1(30) <= 30 after 3 ns;
    wait for 10 ns;
    assert NOT(S1(3)=3 and S1(30)=30) 
      report "***PASSED TEST: c03s02b01x01p06n01i00392" 
      severity NOTE;
    assert (S1(3)=3 and S1(30)=30) 
      report "***FAILED TEST: c03s02b01x01p06n01i00392 - Subtype indication of array object declaration must denote a constrained array."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p06n01i00392arch;
