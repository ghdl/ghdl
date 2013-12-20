
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
-- $Id: tc1995.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p07n01i01995ent IS
END c07s02b02x00p07n01i01995ent;

ARCHITECTURE c07s02b02x00p07n01i01995arch OF c07s02b02x00p07n01i01995ent IS

BEGIN
  TESTING: PROCESS
    type PHYS is range 1 to 260
      units
        A;
        B = 10 A;
        C = 10 B;
      end units;
    variable k : integer    := 0;
    variable m : PHYS    := 10 A; 
  BEGIN
    if (m /= 2 B) then
      k := 5;
    else
      k := 3;
    end if;
    assert NOT(k=5) 
      report "***PASSED TEST: c07s02b02x00p07n01i01995"
      severity NOTE;
    assert (k=5) 
      report "***FAILED TEST: c07s02b02x00p07n01i01995 - Inequality operators are not defined for file types."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p07n01i01995arch;
