
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
-- $Id: tc397.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p06n02i00397ent IS
END c03s02b01x01p06n02i00397ent;

ARCHITECTURE c03s02b01x01p06n02i00397arch OF c03s02b01x01p06n02i00397ent IS
  type I1 is range 1 to 5;
  type M1 is array (positive range 1 to 6) of integer;
  type M2 is array (I1'(1) to I1'(2)) of M1; -- No_failure_here
BEGIN
  TESTING: PROCESS
    variable k : M2;
  BEGIN
    k(1) := (1,2,3,4,5,6);
    k(2) := (7,8,9,10,11,12);
    assert NOT(k(1)=(1,2,3,4,5,6) and k(2)=(7,8,9,10,11,12)) 
      report "***PASSED TEST: c03s02b01x01p06n02i00397" 
      severity NOTE;
    assert (k(1)=(1,2,3,4,5,6) and k(2)=(7,8,9,10,11,12)) 
      report "***FAILED TEST: c03s02b01x01p06n02i00397 - Array element cannot be an unconstrained array." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p06n02i00397arch;
