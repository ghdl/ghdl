
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
-- $Id: tc1211.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p25n01i01211ent IS
END c08s01b00x00p25n01i01211ent;

ARCHITECTURE c08s01b00x00p25n01i01211arch OF c08s01b00x00p25n01i01211ent IS
  -- Local signals.
  signal A : BIT;
BEGIN
  TESTING: PROCESS
    -- Local variables.
    variable ShouldBeTime : TIME;
  BEGIN
    -- Check a condition clause that is NEVER true.
    -- Should suspend until the timeout_clause is reached.
    A <= '1' after 1 ns;
    ShouldBeTime := NOW + 20 ns;
    wait on A until (FALSE) for 20 ns;
    assert NOT(ShouldBeTime = NOW)
      report "***PASSED TEST: c08s01b00x00p25n01i01211"
      severity NOTE;
    assert (ShouldBeTime = NOW)
      report "***FAILED TEST: c08s01b00x00p25n01i01211 - If the condition specified by the condition clause is FALSE, the wait statement will suspend itself again." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p25n01i01211arch;
