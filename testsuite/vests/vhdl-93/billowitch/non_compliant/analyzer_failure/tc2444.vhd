
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
-- $Id: tc2444.vhd,v 1.2 2001-10-26 16:30:18 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p01n01i02444ent IS
  type idx    is range 0 to 15;
  type aray    is array (idx) of positive;
END c07s03b02x02p01n01i02444ent;

ARCHITECTURE c07s03b02x02p01n01i02444arch OF c07s03b02x02p01n01i02444ent IS
  signal sig : aray;
BEGIN
  TESTING: PROCESS
  BEGIN
    sig <= (others => 0);
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c07s03b02x02p01n01i02444 - An aggregate of a one-dimensional array type the expression of each element association must be of the element type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p01n01i02444arch;
