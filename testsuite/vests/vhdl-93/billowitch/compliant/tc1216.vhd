
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
-- $Id: tc1216.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p26n02i01216ent IS
END c08s01b00x00p26n02i01216ent;

ARCHITECTURE c08s01b00x00p26n02i01216arch OF c08s01b00x00p26n02i01216ent IS
  signal   A : BIT;
BEGIN
  TESTING: PROCESS
    -- Local variables.
    variable ShouldBeTime : TIME;
    variable I            : INTEGER;
  BEGIN
    -- First, wait for 1fs;
    wait for 1 fs;

    assert FALSE
      report "***PASSED TEST: c08s01b00x00p26n02i01216 - This test needs manual check. Assertion Failure Note should not appear."
      severity NOTE;
    -- Then, wait until the end of time.
    wait;
    assert (FALSE)
      report "Should never have executed this statement."  
      severity FAILURE;
  END PROCESS TESTING;

END c08s01b00x00p26n02i01216arch;
