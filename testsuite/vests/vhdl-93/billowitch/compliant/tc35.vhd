
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
-- $Id: tc35.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p01n01i00035ent IS
END c04s03b01x01p01n01i00035ent;

ARCHITECTURE c04s03b01x01p01n01i00035arch OF c04s03b01x01p01n01i00035ent IS
  type large is range 0 to 2_000_000_000      --   < 2**31-1
    units
      sbu; 
      lbu = 2000000000 sbu;
    end units;
  constant   SC : large := sbu;
  constant   LC : large := lbu;
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( LC = 2000000000 * SC )
      report "***PASSED TEST: c04s03b01x01p01n01i00035"
      severity NOTE;
    assert ( LC = 2000000000 * SC )
      report "***FAILED TEST: c04s03b01x01p01n01i00035 - Large physical type declaration test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p01n01i00035arch;
