
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
-- $Id: tc2261.vhd,v 1.2 2001-10-26 16:29:46 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p05n01i02261ent IS
END c07s02b06x00p05n01i02261ent;

ARCHITECTURE c07s02b06x00p05n01i02261arch OF c07s02b06x00p05n01i02261ent IS
  
  constant s4p : real := 4.0;
  constant s4n : real := (-4.0);
  constant s5p : real := 5.0;
  constant s5n : real := (-5.0);
  
BEGIN
  TESTING: PROCESS
    variable m1 : real :=   4.0  *   5.0 ;
    variable m2 : real :=   4.0  * (-5.0);
    variable m3 : real := (-4.0) *   5.0 ;
    variable m4 : real := (-4.0) * (-5.0);
    
    variable d1 : real :=   4.0  /   5.0 ;
    variable d2 : real :=   4.0  / (-5.0);
    variable d3 : real := (-4.0) /   5.0 ;
    variable d4 : real := (-4.0) / (-5.0);
    
    variable Em1 : real :=  s4p  *  s5p;
    variable Em2 : real :=  s4p  *  s5n;
    variable Em3 : real :=  s4n  *  s5p;
    variable Em4 : real :=  s4n  *  s5n;
    
    variable Ed1 : real :=  s4p  /  s5p;
    variable Ed2 : real :=  s4p  /  s5n;
    variable Ed3 : real :=  s4n  /  s5p;
    variable Ed4 : real :=  s4n  /  s5n;
  BEGIN
    assert m1 = Em1;
    assert m2 = Em2;
    assert m3 = Em3;
    assert m4 = Em4;
    
    assert d1 = Ed1;
    assert d2 = Ed2;
    assert d3 = Ed3;
    assert d4 = Ed4;

    assert NOT((m1 = Em1)   and
               ( m2 = Em2)   and
               ( m3 = Em3)   and
               ( m4 = Em4)   and
               ( d1 = Ed1)   and
               ( d2 = Ed2)   and
               ( d3 = Ed3)   and
               ( d4 = Ed4)   )
      report "***PASSED TEST: c07s02b06x00p05n01i02261"
      severity NOTE;
    assert (( m1 = Em1)   and
            ( m2 = Em2)   and
            ( m3 = Em3)   and
            ( m4 = Em4)   and
            ( d1 = Ed1)   and
            ( d2 = Ed2)   and
            ( d3 = Ed3)   and
            ( d4 = Ed4)   )
      report "***FAILED TEST: c07s02b06x00p05n01i02261 - Constant real type multiplication and division test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p05n01i02261arch;
