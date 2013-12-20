
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
-- $Id: tc393.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p06n01i00393ent IS
END c03s02b01x01p06n01i00393ent;

ARCHITECTURE c03s02b01x01p06n01i00393arch OF c03s02b01x01p06n01i00393ent IS
  type   M1 is array (positive range <>) of integer;
BEGIN
  TESTING: PROCESS
    variable V1 : M1(4 to 10) ;  -- No_failure_here
  BEGIN
    V1(4)   := 4;
    V1(10)   := 10;
    assert NOT(V1(4)=4 and V1(10)=10)
      report "***PASSED TEST: c03s02b01x01p06n01i00393" 
      severity NOTE;
    assert (V1(4)=4 and V1(10)=10)
      report "***FAILED TEST: c03s02b01x01p06n01i00393 - Subtype indication of array object declaration must denote a constrained array."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p06n01i00393arch;
