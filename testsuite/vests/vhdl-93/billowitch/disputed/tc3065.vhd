
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
-- $Id: tc3065.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s04b02x00p02n01i03065ent IS
END c12s04b02x00p02n01i03065ent;

ARCHITECTURE c12s04b02x00p02n01i03065arch OF c12s04b02x00p02n01i03065ent IS
  type   intvector is array (natural range <>) of integer;
  signal   V2 : intvector(1 to 5);
  signal   V0 : integer := 66;
BEGIN
  FG2: for i in V2'range generate
    IG1: if i = V2'left generate
      V2(i) <= V0 after 1 ns;
    end generate;
    IG2: if i /= V2'left generate
      V2(i) <= V2(i-1) after 1 ns;
    end generate;
    -- ..., V2(2) <= V2(1), V2(1) <= V0
  end generate;
  TESTING: PROCESS
  BEGIN
    wait for 50 ns;
    assert NOT( V2 = (66,66,66,66,66) )
      report "***PASSED TEST: c12s04b02x00p02n01i03065"
      severity NOTE;
    assert ( V2 = (66,66,66,66,66) )
      report "***FAILED TEST: c12s04b02x00p02n01i03065 - Generate statement semantic test failed."
      severity ERROR;
  END PROCESS TESTING;

END c12s04b02x00p02n01i03065arch;
