
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
-- $Id: tc1261.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s02b00x00p05n01i01261ent IS
END c08s02b00x00p05n01i01261ent;

ARCHITECTURE c08s02b00x00p05n01i01261arch OF c08s02b00x00p05n01i01261ent IS

BEGIN
  TESTING: PROCESS
  BEGIN

    -- Print out the NOTE message:
    assert (FALSE)
      report "Verify that the following says 'Assertion violation'."
      severity NOTE;

    -- Print out the default message.
    assert (FALSE)
      severity WARNING;

    assert FALSE 
      report "***PASSED TEST: c08s02b00x00p05n01i01261 - This test needs manual check. Messages as NOTE: Verify that the following says 'Asserion violation' and WARNING: Assertion violation should appear."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c08s02b00x00p05n01i01261arch;
