
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
-- $Id: tc1576.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s10b00x00p04n01i01576ent IS
END c08s10b00x00p04n01i01576ent;

ARCHITECTURE c08s10b00x00p04n01i01576arch OF c08s10b00x00p04n01i01576ent IS

BEGIN
  TESTING: PROCESS
    -- Local variables
    variable DIDIT    : BOOLEAN;
    variable CONSTONE : INTEGER := 1;
    variable   k     : integer := 0;
  BEGIN
    -- The following loop should never fail its assertion.
    DIDIT := FALSE;
    for I in 0 to 10 loop
      -- Make sure that the last statement of loop is executed.
      if  (I /= 0) then
        if (DIDIT /= true) then
          k := 1;
        end if;
        assert (DIDIT)
          report "Did not execute statement after 'next when FALSE'";
        DIDIT := FALSE;
      end if;
      
      -- This condition is NEVER true.
      next when FALSE;
      
      -- This statement should always be executed.
      DIDIT := TRUE;
    end loop;
    
    -- The following loop should never fail its assertion.
    DIDIT := FALSE;
    for I in 0 to 10 loop
      -- Make sure that the last statement of loop is executed.
      if  (I /= 0) then
        if (DIDIT /= true) then
          k := 1;
        end if;
        assert (DIDIT)
          report "Did not execute statement after 'next when FALSE'";
        DIDIT := FALSE;
      end if;
      
      -- This condition is NEVER true.
      next when (CONSTONE /= 1);
      
      -- This statement should always be executed.
      DIDIT := TRUE;
    end loop;

    assert NOT( k=0 ) 
      report "***PASSED TEST: c08s10b00x00p04n01i01576" 
      severity NOTE;
    assert ( k=0 ) 
      report "***FAILED TEST: c08s10b00x00p04n01i01576 - If the condition in the next statement is FALSE, it should execute the sequence of statements enclosed within the loop condition with the next statement." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s10b00x00p04n01i01576arch;
