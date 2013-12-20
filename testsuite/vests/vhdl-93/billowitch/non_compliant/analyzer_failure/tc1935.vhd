
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
-- $Id: tc1935.vhd,v 1.2 2001-10-26 16:30:14 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p01n04i01935ent IS
END c07s02b01x00p01n04i01935ent;

ARCHITECTURE c07s02b01x00p01n04i01935arch OF c07s02b01x00p01n04i01935ent IS

BEGIN
  TESTING: PROCESS
    type array_one is array (positive range <>) of boolean;
    variable x : array_one( 1 to 10);
    variable y : array_one(1 to 5);
    variable z : array_one(1 to 10);
    type array_two is array (positive range <>) of bit;
    variable a : array_two( 1 to 10);
    variable b : array_two(1 to 5);
    variable c : array_two(1 to 10);
  BEGIN
    z := (x nand  y);  -- Failure_here
    assert FALSE 
      report "***FAILED TEST: c07s02b01x00p01n04i01935 - Operands should be arrays of the same length."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p01n04i01935arch;
