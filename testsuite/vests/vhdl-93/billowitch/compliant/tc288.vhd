
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
-- $Id: tc288.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p13n01i00288ent IS
END c03s01b03x00p13n01i00288ent;

ARCHITECTURE c03s01b03x00p13n01i00288arch OF c03s01b03x00p13n01i00288ent IS
  type distance is range 0 to 2e9
    units
      -- base unit
      mil;
      inch = 1000 mil;
      ft = 12 inch;
      yd = 3 ft;
      fm = 6 ft;
      mi = 5280 ft;
    end units;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT((880 * (yd + yd)) = 5280 * ft)
      report "***PASSED TEST: c03s01b03x00p13n01i00288"
      severity NOTE;
    assert ((880 * (yd + yd)) = 5280 * ft)
      report "***FAILED TEST: c03s01b03x00p13n01i00288 - The same arithmetic operations are defined for all physical types."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p13n01i00288arch;
