
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
-- $Id: tc2151.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p21n01i02151ent IS
END c07s02b04x00p21n01i02151ent;

ARCHITECTURE c07s02b04x00p21n01i02151arch OF c07s02b04x00p21n01i02151ent IS

  TYPE     real_v       is array (integer range <>) of real;
  SUBTYPE     real_1       is real_v (1 to 1);
  SUBTYPE     real_null    is real_v (1 to 0);

BEGIN
  TESTING: PROCESS
    variable result    : real_1;
    variable l_operand : real_null;
    variable r_operand : real := 12.345;
  BEGIN
--
-- The element is treated as an implicit single element array !
--
    result := l_operand & r_operand;
    wait for 5 ns;
    assert NOT(result(1) = 12.345)
      report "***PASSED TEST: c07s02b04x00p21n01i02151"
      severity NOTE;
    assert (result(1) = 12.345)
      report "***FAILED TEST: c07s02b04x00p21n01i02151 - Concatenation of null and REAL array failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p21n01i02151arch;
