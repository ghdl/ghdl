
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
-- $Id: tc64.vhd,v 1.2 2001-10-26 16:29:58 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x02p02n01i00064ent IS
END c04s03b01x02p02n01i00064ent;

ARCHITECTURE c04s03b01x02p02n01i00064arch OF c04s03b01x02p02n01i00064ent IS
  signal C1 : Boolean      := TRUE;          -- No_failure_here
  signal C2 : bit          := '1';          -- No_failure_here
  signal C3 : integer      := 12345;          -- No_failure_here
  signal C4 : positive     := 54321;          -- No_failure_here
  signal C5 : natural      := 12121;          -- No_failure_here
  signal C6 : real         := 1.345;          -- No_failure_here
  signal C7 : character    := 'N';          -- No_failure_here
  signal C8 : time         := 100 ns;          -- No_failure_here
  signal C9 : String    (1 to 8)    := "AAAAAAAA";  -- No_failure_here
  signal C10 : bit_vector(0 to 7)    := "11111111";  -- No_failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 10 ns;
    assert NOT(    C1   = TRUE   and
                   C2   = '1'   and
                   C3   = 12345   and
                   C4   = 54321   and
                   C5   = 12121   and
                   C6   = 1.345   and
                   C7   = 'N'   and
                   C8   = 100 ns   and
                   C9   = "AAAAAAAA"   and
                   C10   = "11111111"   ) 
      report "***PASSED TEST:c04s03b01x02p02n01i00064"
      severity NOTE;
    assert (    C1   = TRUE   and
                C2   = '1'   and
                C3   = 12345   and
                C4   = 54321   and
                C5   = 12121   and
                C6   = 1.345   and
                C7   = 'N'   and
                C8   = 100 ns   and
                C9   = "AAAAAAAA"   and
                C10   = "11111111"   ) 
      report "***FAILED TEST: c04s03b01x02p02n01i00064 - Syntactic test for signal assignment failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b01x02p02n01i00064arch;
