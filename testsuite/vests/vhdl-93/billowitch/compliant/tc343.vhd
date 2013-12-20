
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
-- $Id: tc343.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x00p09n03i00343ent IS
END c03s02b01x00p09n03i00343ent;

ARCHITECTURE c03s02b01x00p09n03i00343arch OF c03s02b01x00p09n03i00343ent IS
  type M1 is array (1 to 4) of BIT;
  signal X1 : M1;
BEGIN
  TESTING: PROCESS
  BEGIN
    X1(1) <= '0' after 10 ns;
    X1(2) <= '1' after 20 ns;
    X1(3) <= '1' after 30 ns;
    X1(4) <= '0' after 40 ns;   -- No_failure_here
    wait for 50 ns;
    assert NOT(X1(4)='0' and X1(3)='1' and X1(2)='1' and X1(1)='0')
      report "***PASSED TEST: c03s02b01x00p09n03i00343"
      severity NOTE;
    assert (X1(4)='0' and X1(3)='1' and X1(2)='1' and X1(1)='0')
      report "***FAILED TEST: c03s02b01x00p09n03i00343 - The values in the given index range are not the values that belong to the corresponding range."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x00p09n03i00343arch;
