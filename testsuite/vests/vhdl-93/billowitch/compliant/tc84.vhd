
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
-- $Id: tc84.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x03p02n01i00084ent IS
END c04s03b01x03p02n01i00084ent;

ARCHITECTURE c04s03b01x03p02n01i00084arch OF c04s03b01x03p02n01i00084ent IS

BEGIN
  TESTING: PROCESS
    variable x1 : bit ;                         -- No_failure_here
    variable x2 : character ;                   -- No_failure_here
    variable x3 : integer := 1000;                  -- No_failure_here
    variable x4 : real := 1.001;                    -- No_failure_here
    variable x5 : boolean ;                     -- No_failure_here
    variable x6 : time  := 10 ns;                   -- No_failure_here
    variable x7 : string(1 to 10) := "abcdefghij";    -- No_failure_here
    variable x8 : bit_vector (10 downto 1);     -- No_failure_here
  BEGIN
    assert NOT(    x1 = '0'      and
                   x2 = NUL      and
                   x3 = 1000      and
                   x4 = 1.001      and
                   x5 = false      and
                   x6 = 10 ns      and
                   x7 = "abcdefghij"   and
                   x8 = "0000000000"   ) 
      report "***PASSED TEST:c04s03b01x03p02n01i00084"
      severity NOTE;
    assert (    x1 = '0'      and
                x2 = NUL      and
                x3 = 1000      and
                x4 = 1.001      and
                x5 = false      and
                x6 = 10 ns      and
                x7 = "abcdefghij"   and
                x8 = "0000000000"   ) 
      report "***FAILED TEST: c04s03b01x03p02n01i00084 - Variable assignment test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x03p02n01i00084arch;
