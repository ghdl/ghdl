
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
-- $Id: tc341.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x00p09n02i00341ent IS
END c03s02b01x00p09n02i00341ent;

ARCHITECTURE c03s02b01x00p09n02i00341arch OF c03s02b01x00p09n02i00341ent IS

BEGIN
  TESTING: PROCESS
    type       T_A1_S       is ARRAY(INTEGER range <>) of INTEGER;
    subtype    ST_A1_S    is T_A1_S(INTEGER range 1 to 3);
    type       T_A1_A1_S    is ARRAY(INTEGER range <>) of ST_A1_S;
    subtype    ST_A1_A1_S    is T_A1_A1_S(INTEGER range 6 downto 4);

    variable V_A1_A1_S    : ST_A1_A1_S;
  BEGIN
    V_A1_A1_S(6)(1) := 61;
    V_A1_A1_S(6)(2) := 62;
    V_A1_A1_S(6)(3) := 63;
    V_A1_A1_S(5)(1) := 51;
    V_A1_A1_S(5)(2) := 52;
    V_A1_A1_S(5)(3) := 53;
    V_A1_A1_S(4)(1) := 41;
    V_A1_A1_S(4)(2) := 42;
    V_A1_A1_S(4)(3) := 43;
    wait for 5 ns;
    assert NOT(    V_A1_A1_S(6)(1) = 61   and
                   V_A1_A1_S(6)(2) = 62   and
                   V_A1_A1_S(6)(3) = 63   and
                   V_A1_A1_S(5)(1) = 51   and
                   V_A1_A1_S(5)(2) = 52   and
                   V_A1_A1_S(5)(3) = 53   and
                   V_A1_A1_S(4)(1) = 41   and
                   V_A1_A1_S(4)(2) = 42   and
                   V_A1_A1_S(4)(3) = 43)
      report "***PASSED TEST: c03s02b01x00p09n02i00341"
      severity NOTE;
    assert (    V_A1_A1_S(6)(1) = 61   and
                V_A1_A1_S(6)(2) = 62   and
                V_A1_A1_S(6)(3) = 63   and
                V_A1_A1_S(5)(1) = 51   and
                V_A1_A1_S(5)(2) = 52   and
                V_A1_A1_S(5)(3) = 53   and
                V_A1_A1_S(4)(1) = 41   and
                V_A1_A1_S(4)(2) = 42   and
                V_A1_A1_S(4)(3) = 43)
      report "***FAILED TEST: c03s02b01x00p09n02i00341 - For each possible sequence of index values that can be formed by selecting one value for each index for a multimensioal array, there is a distinct element."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x00p09n02i00341arch;
