
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
-- $Id: tc2199.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p01n01i02199ent IS
END c07s02b06x00p01n01i02199ent;

ARCHITECTURE c07s02b06x00p01n01i02199arch OF c07s02b06x00p01n01i02199ent IS

BEGIN
  TESTING: PROCESS
    variable y : real := -9.0 * (-3.0); -- y should be 27.0
  BEGIN
    assert NOT(y=27.0)
      report "***PASSED TEST: c07s02b06x00p01n01i02199"
      severity NOTE;
    assert ( y=27.0 )
      report "***FAILED TEST: c07s02b06x00p01n01i02199 - The  operators * and / are predefined for any integer type and any floating point type and have their convertional meaning."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p01n01i02199arch;
