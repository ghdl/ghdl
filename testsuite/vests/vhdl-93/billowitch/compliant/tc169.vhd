
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
-- $Id: tc169.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b03x00p01n01i00169ent IS
END c04s03b03x00p01n01i00169ent;

ARCHITECTURE c04s03b03x00p01n01i00169arch OF c04s03b03x00p01n01i00169ent IS

BEGIN
  TESTING: PROCESS
    variable    V2 : STRING(1 to 5) := "Hello";
    alias       a3 : STRING(1 to 5) is V2;   -- composite alias of composite
    alias       a2 : STRING(4 downto 1) is V2(1 to 4);
    
    alias       a4 : CHARACTER is V2(4);   -- scalar alias of composite
  BEGIN
    assert V2    = "Hello";
    assert A2    = "Hell";
    assert V2(1)    = 'H';
    assert A2(4)    = 'H';
    assert V2(2)    = 'e';
    assert A2(3)    = 'e';
    assert A3    = "Hello";
    assert A4    = 'l';

    wait for 5 ns;
    assert NOT(   V2       = "Hello"   and 
                  A2       = "Hell"   and 
                  V2(1)    = 'H'      and 
                  A2(4)    = 'H'      and 
                  V2(2)    = 'e'      and 
                  A2(3)    = 'e'      and 
                  A3       = "Hello"   and 
                  A4       = 'l'      )
      report "***PASSED TEST: c04s03b03x00p01n01i00169"
      severity NOTE;
    assert (   V2       = "Hello"   and 
               A2       = "Hell"   and 
               V2(1)    = 'H'      and 
               A2(4)    = 'H'      and 
               V2(2)    = 'e'      and 
               A2(3)    = 'e'      and 
               A3       = "Hello"   and 
               A4       = 'l'      )
      report "***FAILED TEST: c04s03b03x00p01n01i00169 - Alias of alias composite type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b03x00p01n01i00169arch;
