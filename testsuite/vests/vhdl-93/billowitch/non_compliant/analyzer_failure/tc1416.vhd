
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
-- $Id: tc1416.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b01x00p01n01i01416ent IS
END c08s05b01x00p01n01i01416ent;

ARCHITECTURE c08s05b01x00p01n01i01416arch OF c08s05b01x00p01n01i01416ent IS

BEGIN
  TESTING: PROCESS
    type ARAY_1 is array (INTEGER range <>) of BIT;
    subtype SUB_ONE      is ARAY_1 (1 to 10);
    subtype SUB_TWO    is ARAY_1 (1 to 100);
    subtype SUB_THREE    is ARAY_1 (41 to 60);
    variable V1 : SUB_ONE;
    variable V2 : SUB_TWO;
    variable V3 : SUB_THREE;
  BEGIN
    V1 := V3;
    assert FALSE 
      report "***FAILED TEST: c08s05b01x00p01n01i01416 - The number of components has to be the same." 
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c08s05b01x00p01n01i01416arch;
