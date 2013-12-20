
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
-- $Id: tc37.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p02n01i00037ent IS
END c04s03b01x01p02n01i00037ent;

ARCHITECTURE c04s03b01x01p02n01i00037arch OF c04s03b01x01p02n01i00037ent IS
  constant C1 : Boolean   := true; -- No_failure_here
  constant C2 : bit       := '0';  -- No_failure_here
  constant C3 : integer   := 123;  -- No_failure_here
  constant C4 : positive  := 34;   -- No_failure_here
  constant C5 : natural   := 12;   -- No_failure_here
  constant C6 : real      := 1.20; -- No_failure_here
  constant C7 : character := 'C';  -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(    C1   = true   and
                   C2   = '0'   and
                   C3   = 123   and
                   C4   = 34   and
                   C5   = 12   and
                   C6   = 1.20   and
                   C7   = 'C'   )   
      report "***PASSED TEST: c04s03b01x01p02n01i00037"
      severity NOTE;
    assert (    C1   = true   and
                C2   = '0'   and
                C3   = 123   and
                C4   = 34   and
                C5   = 12   and
                C6   = 1.20   and
                C7   = 'C'   )   
      report "***FAILED TEST: c04s03b01x01p02n01i00037 - Constant declaration syntactic format test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p02n01i00037arch;
