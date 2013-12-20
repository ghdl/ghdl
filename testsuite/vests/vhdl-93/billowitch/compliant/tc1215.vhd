
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
-- $Id: tc1215.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s01b00x00p26n01i01215ent IS
END c08s01b00x00p26n01i01215ent;

ARCHITECTURE c08s01b00x00p26n01i01215arch OF c08s01b00x00p26n01i01215ent IS
  -- Local signals.
  signal A : BIT;
BEGIN
  TESTING: PROCESS
    -- Local variables.
    variable ShouldBeTime    : TIME;
    variable I               : INTEGER;
    variable k      : integer := 0;
  BEGIN
    -- Given that a particular condition will never be TRUE,
    -- verify that we always wait for the desired amount of time.
    for I in 0 to 100 loop
      -- Perform the assignment.
      if  ((I mod 2) = 0) then
        A <= '1' after 1 ns;
      else
        A <= '0' after 1 ns;
      end if;

      -- Compute the time we should end the wait statement.
      ShouldBeTime := NOW + 2 ns;

      -- Wait the desired amount of time.  Note that the condition
      -- will never be TRUE.
      if  ((I mod 2) = 0) then
        wait until (A = '0') for 2 ns;
      else
        wait until (A = '1') for 2 ns;
      end if;
      
      -- Assert that we ended on time.
      assert (ShouldBeTime = NOW);
      if (ShouldBeTime /= NOW) then
        k := 1;
      end if;
    end loop;
    assert NOT(k=0)
      report "***PASSED TEST: c08s01b00x00p26n01i01215"
      severity NOTE;
    assert (k=0)
      report "***FAILED TEST: c08s01b00x00p26n01i01215 - The timeout clause specifies the maximum amount of time the process will remain suspended at this wait statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s01b00x00p26n01i01215arch;
