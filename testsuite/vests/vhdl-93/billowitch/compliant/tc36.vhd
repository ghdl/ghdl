
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
-- $Id: tc36.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x01p02n01i00036ent IS
END c04s03b01x01p02n01i00036ent;

ARCHITECTURE c04s03b01x01p02n01i00036arch OF c04s03b01x01p02n01i00036ent IS
  constant a  : positive := 1;              -- No_failure_here
  constant b  : natural  := 1;              -- No_failure_here
  constant a1 : positive := a + 1;          -- No_failure_here
  constant a2 : positive := a + a;          -- No_failure_here
  constant a3 : positive := a * (a/a + 1);  -- No_failure_here
  constant b1 : natural  := b + 1;          -- No_failure_here
  constant b2 : natural  := b + b;          -- No_failure_here
  constant b3 : natural  := b * (b/b + 1);  -- No_failure_here
  constant b4 : natural  := b - b;          -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT(    a   = 1   and
                   b   = 1   and
                   a1   = 2   and
                   a2   = 2   and
                   a3   = 2   and
                   b1   = 2   and
                   b2   = 2   and
                   b3   = 2   and
                   b4   = 0   )
      report "***PASSED TEST: c04s03b01x01p02n01i00036"
      severity NOTE;
    assert (    a   = 1   and
                b   = 1   and
                a1   = 2   and
                a2   = 2   and
                a3   = 2   and
                b1   = 2   and
                b2   = 2   and
                b3   = 2   and
                b4   = 0   )
      report "***FAILED TEST: c04s03b01x01p02n01i00036 - Constant declaration syntactic format test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x01p02n01i00036arch;
