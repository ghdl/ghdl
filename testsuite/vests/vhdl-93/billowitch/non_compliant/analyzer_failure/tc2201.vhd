
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
-- $Id: tc2201.vhd,v 1.2 2001-10-26 16:30:16 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p01n01i02201ent IS
END c07s02b06x00p01n01i02201ent;

ARCHITECTURE c07s02b06x00p01n01i02201arch OF c07s02b06x00p01n01i02201ent IS

BEGIN
  TESTING: PROCESS
    type array_one is array (1 to 10) of boolean;
    type array_two is array (1 to 20) of boolean;
    variable x : array_one;
    variable y : array_two;
    variable z : integer;
  BEGIN
    z := x * y;  -- Failure_here
    -- multiplying operator cannot operate on array types.
    assert FALSE 
      report "***FAILED TEST: c07s02b06x00p01n01i02201 - Multiplying operators are predefined only for integer and floating point types." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p01n01i02201arch;
