
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
-- $Id: tc1120.vhd,v 1.2 2001-10-26 16:30:03 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p03n01i01120ent IS
  type idx   is range 1 to 10;
  type aray1 is array (idx) of bit;
  type aray2 is array (idx range <>) of aray1;
END c06s05b00x00p03n01i01120ent;

ARCHITECTURE c06s05b00x00p03n01i01120arch OF c06s05b00x00p03n01i01120ent IS

BEGIN
  TESTING: PROCESS
    variable v1, v2 : aray1;
    variable v3     : aray2(1 to 2);
    variable v4     : aray2(1 to 3);
  BEGIN
    --
    -- Try slices consisting of slice names
    --
    v1 := "1111111111";
    v1 := v3(1)(idx);           -- slice is a whole array
    assert v2 = v1
      report "Slice of a slice name as a value failed."
      severity note ;
    
    v1 := "1111111111";
    v4(2)(idx) := v1;           -- slice is a whole array
    assert v4(2) = v1
      report "Slice of a slice name as a target failed."
      severity note ;
    
    v2(1) := v3(1)(1 to 1)(1 to 1)(1);       -- a one element slice
    assert v3(1)(1) = v2(1)
      report "One element slice of a slice name as a value failed."    
      severity note ;
    
    v3(1)(1 to 1)(1 to 1)(1) := v1(1);       -- a one element slice
    assert v3(1)(1) = v1(1)
      report "One element slice of a slice name as a target failed." 
      severity note ;

    assert NOT(   v1       = "1111111111"   and
                  v4(2)       = "1111111111"   and
                  v2(1)      = '0'      and
                  v3(1)(1)   = '1') 
      report "***PASSED TEST: c06s05b00x00p03n01i01120" 
      severity NOTE;
    assert (   v1       = "1111111111"   and
               v4(2)       = "1111111111"   and
               v2(1)      = '0'      and
               v3(1)(1)   = '1') 
      report "***FAILED TEST: c06s05b00x00p03n01i01120 - The prefix of a slice may be a slice name." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p03n01i01120arch;
