
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
-- $Id: tc1606.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s11b00x00p04n01i01606ent IS
END c08s11b00x00p04n01i01606ent;

ARCHITECTURE c08s11b00x00p04n01i01606arch OF c08s11b00x00p04n01i01606ent IS

BEGIN
  TESTING: PROCESS
    -- local variables
    variable GONE_THROUGH_ONCE : BOOLEAN := FALSE;
    variable k : integer := 0;
  BEGIN
    for I in 0 to 10 loop
      -- Check to see if we have gone through this more than once.
      if  (not(GONE_THROUGH_ONCE)) then
        GONE_THROUGH_ONCE := TRUE;
      else
        assert (FALSE)
          report "Going through loop more than once.";
      end if;

      -- Exit the loop.
      exit when TRUE;
      k := 1;
      -- The following should never be executed.
      assert (FALSE)
        report "This statement should NEVER be executed.";
    end loop;

    -- Verify that we went through at least once.
    assert( GONE_THROUGH_ONCE )
      report "Did not go through the loop at all.";

    assert NOT(k=0) 
      report "***PASSED TEST: c08s11b00x00p04n01i01606"
      severity NOTE;
    assert (k=0) 
      report "***FAILED TEST: c08s11b00x00p04n01i01606 - The loop should terminate when the condition is TRUE." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s11b00x00p04n01i01606arch;
