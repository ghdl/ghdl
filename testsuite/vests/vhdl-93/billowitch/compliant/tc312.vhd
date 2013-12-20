
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
-- $Id: tc312.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b04x00p07n03i00312ent IS
END c03s01b04x00p07n03i00312ent;

ARCHITECTURE c03s01b04x00p07n03i00312arch OF c03s01b04x00p07n03i00312ent IS
  type R1 is range -10.0 to 10.0;
  type R2 is range REAL'LOW to REAL'HIGH;
BEGIN
  TESTING: PROCESS
    variable V1 : real := 0.000001;
    variable V2 : real := 0.000002;
    variable V3 : real ;
  BEGIN
    V3 := V2 - V1;
    assert NOT(V3 = 0.000001)
      report "***PASSED TEST: c03s01b04x00p07n03i00312"
      severity NOTE;
    assert ( V3 = 0.000001 )
      report "***FAILED TEST: c03s01b04x00p07n03i00312 -  A minimum of six digits of precision is included in the representation of floating point types."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b04x00p07n03i00312arch;
