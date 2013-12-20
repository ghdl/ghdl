
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
-- $Id: tc301.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b04x00p03n01i00301ent IS
END c03s01b04x00p03n01i00301ent;

ARCHITECTURE c03s01b04x00p03n01i00301arch OF c03s01b04x00p03n01i00301ent IS
  type REAL1 is range REAL'LOW to REAL'HIGH;
BEGIN
  TESTING: PROCESS
    variable k : REAL1 := 6.0;
  BEGIN
    k := 5.0;
    assert NOT(k=5.0) 
      report "***PASSED TEST: c03s01b04x00p03n01i00301" 
      severity NOTE;
    assert (k=5.0) 
      report "***FAILED TEST: c03s01b04x00p03n01i00301 - The range of the type defined by the floating point type is within the range given by the floating point defintion." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b04x00p03n01i00301arch;
