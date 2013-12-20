
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
-- $Id: tc1572.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s10b00x00p03n01i01572ent IS
END c08s10b00x00p03n01i01572ent;

ARCHITECTURE c08s10b00x00p03n01i01572arch OF c08s10b00x00p03n01i01572ent IS

BEGIN
  TESTING: PROCESS
    -- Local variables
    variable DIDIT    : BOOLEAN;
    variable CONSTONE : INTEGER := 1;
    variable k    : integer := 0;
  BEGIN
    -- TEST1:  Should always go to outer loop.
    OUTERLOOP:
    for I in 1 to 10 loop

      INNERLOOP:
      for J in 1 to 10 loop
        -- Skip to next iteration of outerloop.
        next OUTERLOOP;

        k := 1;
        -- This should never be executed.
        assert (FALSE)
          report "Statement should never be executed.";
      end loop INNERLOOP;

      k := 1;
      -- This should never be executed.
      assert (FALSE)
        report "Statement should never be executed.";
    end loop OUTERLOOP;
    
    -- TEST2:  Should always go to inner loop.
    -- Set the flag initially.
    DIDIT := TRUE;
    
    -- Execute the loops.
    OUTERLOOP2:
    for I in 1 to 10 loop
      
      INNERLOOP2:
      for J in 1 to 10 loop
        -- Check that last statement of OUTERLOOP2 got done.
        if  (J = 1) then
          assert (DIDIT)
            report "Last statement of OUTERLOOP2 was not executed.";
          if (DIDIT /= true) then   
            k := 1;
          end if;
          DIDIT := FALSE;
        end if;
        
        -- Skip to next iteration of outerloop.
        next INNERLOOP2;
        k := 1; 
        -- This should never be executed.
        assert (FALSE)
          report "Statement should never be executed.";
      end loop INNERLOOP2;
      
      -- This should ALWAYS be executed.
      DIDIT := TRUE;
    end loop OUTERLOOP2;
    assert NOT(k=0) 
      report "***PASSED TEST: c08s10b00x00p03n01i01572"
      severity NOTE;
    assert (k=0) 
      report "***FAILED TEST: c08s10b00x00p03n01i01572 - The NEXT statement did not properly associated with the loop whose label it matchs." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s10b00x00p03n01i01572arch;
