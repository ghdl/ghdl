
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
-- $Id: tc1523.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s09b00x00p07n01i01523pkg is

  -- Global procedure.
  procedure proc1;

  -- Global function.
  function  func1 return INTEGER;

end c08s09b00x00p07n01i01523pkg;

package body c08s09b00x00p07n01i01523pkg is

  procedure proc1 is
    -- Local variables
    variable  INTV  : INTEGER := 0;

  begin
    -- Check initialization.
    assert (INTV = 0);

    -- Loop until the indicated condition has been met.
    loop
      -- Execute some meaningful function.
      null;

      -- Increment the counter.
      INTV := INTV + 1;

      -- If the condition has been met, terminate the loop.
      if  (INTV = 10) then
        return;
      end if;

      -- Verify that we have not exceeded the limits of the loop.
      assert (INTV < 10);
    end loop;
    
    -- Should NEVER get to this step.
    assert (FALSE)
      report "Return has not exited the procedure.";
  end proc1;
  
  function func1 return INTEGER is
    -- Local variables
    variable  INTV  : INTEGER := 0;
    
  begin
    -- Check initialization.
    assert (INTV = 0);
    
    -- Loop until the indicated condition has been met.
    loop
      -- Execute some meaningful function.
      null;
      -- Increment the counter.
      INTV := INTV + 1;
      
      -- If the condition has been met, terminate the loop.
      if  (INTV = 10) then
        return( INTV );
      end if;
      
      -- Verify that we have not exceeded the limits of the loop.
      assert (INTV < 10);
    end loop;
    
    -- Should NEVER get to this step.
    assert (FALSE)
      report "Return has not exited the procedure.";
  end func1;
  
end c08s09b00x00p07n01i01523pkg;

use work.c08s09b00x00p07n01i01523pkg.all;
ENTITY c08s09b00x00p07n01i01523ent IS
END c08s09b00x00p07n01i01523ent;

ARCHITECTURE c08s09b00x00p07n01i01523arch OF c08s09b00x00p07n01i01523ent IS

BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    -- Call procedure to loop/return.
    proc1;
    
    assert NOT(func1=10)
      report "***PASSED TEST: c08s09b00x00p07n01i01523" 
      severity NOTE;
    assert (func1=10)
      report "***PASSED TEST: c08s09b00x00p07n01i01523 - Function did not return proper value." 
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p07n01i01523arch;
