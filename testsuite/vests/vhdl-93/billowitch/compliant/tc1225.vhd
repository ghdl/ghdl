
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
-- $Id: tc1225.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p28n01i01225ent IS
END c08s01b00x00p28n01i01225ent;

ARCHITECTURE c08s01b00x00p28n01i01225arch OF c08s01b00x00p28n01i01225ent IS
  -- Local signals.
  signal A : BIT;
BEGIN
  TESTING: PROCESS
    -- Local variables.
    variable ShouldBeTime    : TIME;
    variable I               : INTEGER;
    variable k       : integer := 0;
  BEGIN
    -- Make sure it takes an EVENT to trigger the WAIT statement.
    A <= A after 2 ns,           -- NOT an event.
         (not A) after 4 ns;      -- an event.
    ShouldBeTime := NOW + 4 ns;   -- Should wait for event.   
    wait on A;
    if (ShouldBeTime /= Now) then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW)
      report "Did not wait for 4ns";

    -- If the value of the condition is FALSE, resuspend.
    -- If the value is TRUE, the process will resume.
    A <= '1' after 2 ns,
         '0' after 4 ns;

    -- Make sure that we wait until the second one for
    -- the following wait statement to resume.
    ShouldBeTime := NOW + 4 ns;
    wait until (A = '0');
    if (ShouldBeTime /= Now and A /= '0') then
      k := 1;
    end if;
    assert (ShouldBeTime = NOW)
      report "Did not wait for 4ns";
    assert (A = '0')
      report "Did not assign the correct value.";
    
    -- Such resuspension does not involve the recalculation of the timeout interval.
    -- If the value of the condition is FALSE, resuspend.
    -- IF the value is TRUE, the process will resume.
    A <= '1' after 2 ns,
         '0' after 4 ns;
    
    -- Make sure that we wait until the second one for
    -- the following wait statement to resume.
    ShouldBeTime := NOW + 3 ns;
    wait until (A = '0') for 3 ns;
    if (ShouldBeTime /= Now and A /= '1') then
      k := 1;   
    end if;
    assert (ShouldBeTime = NOW)
      report "Did not wait for 3ns";
    assert (A = '1')
      report "Did not assign the correct value to A.";
    assert NOT( k=0 )
      report "***PASSED TEST: c08s01b00x00p28n01i01225"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c08s01b00x00p28n01i01225 - The process will resume if the result of an event occuring on sentivity set is TRUE."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p28n01i01225arch;
