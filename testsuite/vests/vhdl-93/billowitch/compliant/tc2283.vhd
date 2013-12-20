
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
-- $Id: tc2283.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b06x00p14n01i02283ent IS
END c07s02b06x00p14n01i02283ent;

ARCHITECTURE c07s02b06x00p14n01i02283arch OF c07s02b06x00p14n01i02283ent IS
BEGIN
  TESTING: PROCESS
    type PHYS is range 0 to 80000
      units
        PHYS1;
        PHYS2 = 2 PHYS1;
        PHYS10 = 10 PHYS1;
        PHYS100 = 10 PHYS10;
      end units;
    function G ( A : PHYS; B : INTEGER; C : REAL ) return PHYS is
    begin
      return A / B * C;       
    end G;
    variable B : PHYS := 11 PHYS1;
    variable R : REAL := 7.9999;
  BEGIN
    B := G(B,4,1.3) / 4 * 0.0 ;
    assert NOT(B = 0 PHYS1) 
      report "***PASSED TEST: c07s02b06x00p14n01i02283" 
      severity NOTE;
    assert (B = 0 PHYS1) 
      report "***FAILED TEST: c07s02b06x00p14n01i02283 - Incompatible operands: May not be multiplied or divided." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b06x00p14n01i02283arch;
