
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
-- $Id: tc287.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b03x00p12n03i00287ent IS
END c03s01b03x00p12n03i00287ent;

ARCHITECTURE c03s01b03x00p12n03i00287arch OF c03s01b03x00p12n03i00287ent IS
  type UPLE is range 1 to 8
    units
      single;
      double    = 2 single;
      quadruple    = 2 double;
      octuple    = 2 quadruple;
    end units;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT((UPLE'POS(3 double) = 3 * UPLE'POS(double)) and (UPLE'POS(0.5 octuple) = 4))
      report "***PASSED TEST: c03s01b03x00p12n03i00287"
      severity NOTE;
    assert ((UPLE'POS(3 double) = 3 * UPLE'POS(double)) and (UPLE'POS(0.5 octuple) = 4))
      report "***FAILED TEST: c03s01b03x00p12n03i00287 - The position number of a physical literal with an abstract literal part is rounded up to the nearest integer of the product of the abstract literal part of physical literal and the position number of its unit name."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b03x00p12n03i00287arch;
