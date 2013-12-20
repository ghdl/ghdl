
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
-- $Id: tc2272.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p14n01i02272ent IS
END c07s02b06x00p14n01i02272ent;

ARCHITECTURE c07s02b06x00p14n01i02272arch OF c07s02b06x00p14n01i02272ent IS
  signal SS : TIME;
BEGIN
  TESTING: PROCESS
    variable A : TIME    := 3 * 11 ns;
    variable R : REAL    := 7.9999;
    variable S : INTEGER    := 1;
  BEGIN
    SS <= R * ( S * A );            -- context 4
    wait for 10 ns;
    assert NOT((-0.01 ns < (SS - 3*11*7.9999 ns)) and ((SS - 3*11*7.9999 ns) < 0.01 ns))
      report "***PASSED TEST: c07s02b06x00p14n01i02272"
      severity NOTE;
    assert ((-0.01 ns < (SS - 3*11*7.9999 ns)) and ((SS - 3*11*7.9999 ns) < 0.01 ns))
      report "***FAILED TEST: c07s02b06x00p14n01i02272 - The left operand of the multiplication operation can be an integer type and the right operand of physical type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p14n01i02272arch;
