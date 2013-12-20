
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
-- $Id: tc168.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b03x00p01n01i00168ent IS
END c04s03b03x00p01n01i00168ent;

ARCHITECTURE c04s03b03x00p01n01i00168arch OF c04s03b03x00p01n01i00168ent IS

BEGIN
  TESTING: PROCESS
    variable    V1    : INTEGER := 1;
    alias       V1_A1    : INTEGER is V1;
    -- scalar alias of scalar
    -- alias of variable
    alias       V1_A2    : INTEGER is V1_A1;
    -- alias of alias
    
    variable   pass   : integer := 0;
  BEGIN
    assert V1    = 1;
    assert V1_A1    = 1;
    assert V1_A2    = 1;
    if (V1 /= 1 or V1_A1 /= 1 or V1_A2 /= 1) then   
      pass := 1;
    end if;
    
    V1 := 2;         -- change value...
    assert V1    = 2;
    assert V1_A1    = 2;      -- ... check read
    assert V1_A2    = 2;      -- ... check read
    if (V1 /= 2 or V1_A1 /= 2 or V1_A2 /= 2) then   
      pass := 1;
    end if;
    
    V1_A1 := 3;         -- change value using alias
    assert V1    = 3;      -- ... check that value changed
    assert V1_A1    = 3;
    assert V1_A2    = 3;
    if (V1 /= 3 or V1_A1 /= 3 or V1_A2 /= 3) then   
      pass := 1;
    end if;

    V1_A2 := 4;         -- change value using alias
    assert V1    = 4;      -- ... check that value changed
    assert V1_A1    = 4;
    assert V1_A2    = 4;
    if (V1 /= 4 or V1_A1 /= 4 or V1_A2 /= 4) then   
      pass := 1;
    end if;

    wait for 5 ns;
    assert NOT(   pass = 0   ) 
      report "***PASSED TEST: c04s03b03x00p01n01i00168"
      severity NOTE;
    assert (   pass = 0   ) 
      report "***FAILED TEST: c04s03b03x00p01n01i00168 - Alias of alias variable test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b03x00p01n01i00168arch;
