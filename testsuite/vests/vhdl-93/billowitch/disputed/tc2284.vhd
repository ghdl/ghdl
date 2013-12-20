
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
-- $Id: tc2284.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p14n01i02284ent IS
END c07s02b06x00p14n01i02284ent;

ARCHITECTURE c07s02b06x00p14n01i02284arch OF c07s02b06x00p14n01i02284ent IS
  signal   SS : TIME;
BEGIN
  TESTING: PROCESS
    variable A : TIME := 199 ns;
    variable R : REAL := 7.9999;
    variable S : INTEGER := 199;
  BEGIN
    SS <= A * R / S;                
    wait for 10 ns;
    assert NOT(SS = 7.9999 ns)
      report "***PASSED TEST: c07s02b06x00p14n01i02284" 
      severity NOTE;
    assert (SS = 7.9999 ns)
      report "***FAILED TEST: c07s02b06x00p14n01i02284 - Incompatible operands: May not be multiplied or divided." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p14n01i02284arch;
