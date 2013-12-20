
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
-- $Id: tc346.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x00p10n05i00346ent IS
END c03s02b01x00p10n05i00346ent;

ARCHITECTURE c03s02b01x00p10n05i00346arch OF c03s02b01x00p10n05i00346ent IS
  type MEM is array(INTEGER range <>) of BIT;
BEGIN
  TESTING: PROCESS
    variable S1 : MEM(1 to 5);
    variable S2 : MEM(28 downto 7);
  BEGIN
    S1(1 to 5) := "11111";
    S2(28 downto 21) := "00001111";
    assert NOT(S1(1 to 5) = "11111" and S2(28 downto 21)= "00001111")
      report "***PASSED TEST: c03s02b01x00p10n05i00346"
      severity NOTE;
    assert (S1(1 to 5) = "11111" and S2(28 downto 21)= "00001111")
      report "***FAILED TEST: c03s02b01x00p10n05i00346 - Different objects of the same unconstrained array type can have different bounds and direction."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x00p10n05i00346arch;
